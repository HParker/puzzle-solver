#lang racket/base

;(require (planet gcr/riot))
;(require (planet soegaard/gzip:2:2))
;(require file/gzip)
;(require file/gunzip)
(require racket/list
         racket/format
         racket/vector
         racket/system
         ;racket/fixnum
         racket/set
         data/heap
         ;rnrs/sorting-6
         racket/place
         racket/place/define-remote-server
         remote-shell/ssh
         )

(require "stpconfigs/configenv.rkt"
         "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt"
         "myvectorsort.rkt" ;; use rnrs/sorting-6 which seems to be almost 3 times faster
         )
;(require profile)
;(instrumenting-enabled #t)
;(profiling-enabled #t)

(provide *found-goal*
         distributed-expand-fringe
         remote-expand-part-fringe
         distributed-merge-proto-fringe-slices)

(current-directory *stp-home-path*)

(define *found-goal* #f)

;;------------------------------------------
;; FRINGE SLICING: (proto-)fringe slicing

(define *num-fringe-slices* (* (length *worker-hosts*) *workers-per-host*))

;; define the fixed hash-code bounds to be used for repsonsibility ranges and proto-fringe slicing
(define *fringe-slice-bounds* (compute-segment-bounds *num-fringe-slices*))

;; get-slice-num: fixnum [low number] [hi number] -> number
;; use binary search to find index for given hash-code within ranges defined by *fringe-slice-bounds*
(define (get-slice-num phc (low 0) (hi *num-fringe-slices*))
  (let ([mid (floor (/ (+ low hi) 2))])
    (cond [(= low mid)
           (when (>= phc (vector-ref *fringe-slice-bounds* (add1 mid)))
             (error (format "get-slice-num: missed the mark with index ~a for hc=~a~%" mid phc)))
           mid]
          [(< phc (vector-ref *fringe-slice-bounds* mid))
           (get-slice-num phc low mid)]
          [else (get-slice-num phc mid hi)])))

;; make-vector-ranges: int -> (listof (list int int)
;; create the pairs of indices into the current-fringe-vector that will specify the part of the fringe each worker tackles
(define (make-vector-ranges vlength)
  (if (< vlength 10)
      (list (list 0 vlength))
      (let ((start-list (build-list *num-fringe-slices*
                                    (lambda (i) (floor (* i (/ vlength *num-fringe-slices*)))))))
        (foldr (lambda (x r) (cons (list x (first (first r))) r)) 
               (list (list (last start-list) vlength)) 
               (drop-right start-list 1)))))

;; dynamic-slice-ranges: (listof filespec) -> (listof (list int int))
;; for when changing from one number of slices at one level to another number of slices at the next level
;; ASSUMES that *num-fringe-slices* is an exact multiple of the number of slices in the given list-of-filespec
(define (dynamic-slice-ranges lofspec) 
  (let*-values ([(growth-factor) (quotient *num-fringe-slices* (length lofspec))]
                [(ranges total-count)
                 (for/fold ([res empty]
                            [start 0])
                   ([fs lofspec])
                   (values (append res
                                   (for/list ([i growth-factor]) (list (+ start (* i (/ 1 growth-factor) (filespec-pcount fs)))
                                                                       (+ start (* (add1 i) (/ 1 growth-factor) (filespec-pcount fs))))))
                           (+ start (filespec-pcount fs))))])
    (printf "total-positions ~a and summed start count ~a and last slice pcount ~a~%"
            (foldl + 0 (map filespec-pcount lofspec)) total-count (filespec-pcount (last lofspec)))
    ranges))

;; simple-ranges: (listof filespec) -> (listof (list int int)
;; just use the fringe-segments
(define (make-simple-ranges lofspec)
  (let ([start-range 0])
    (for/list ([fs (in-list lofspec)])
      (set! start-range (+ start-range (filespec-pcount fs)))
      (list (- start-range (filespec-pcount fs)) start-range))))


;;----------------------------------------------------------------------------------------
;; DISTRIBUTED EXPANSION AND MERGING OF FRINGES

;; a sampling-stat is a (vector int fixnum fixnum (vectorof fixnum) boolean string (vectorof int) int int real real)
;; where the elements are:s
;; 0. total number of duplicate-free positions summed over slices
;; 1. number of positions discarded because duplicate with current or prev-fringes
;; 2. number of positions discarded because duplicate with other partial-expansion file
;; 3. vector of numbers counting duplicate-free positions in each respective slice (provide pcount if needed for fspec)
;; 4. boolean goal-found if found when expanding the assigned positions
;; 5. output file name prefix, e.g., proto-fringe-dXX-NN, without slice id which is assumed to be added when needed
;; 6. vector of slice file sizes
;; 7. total number of positions processed to give rise to duplicate free positions in index 0
;; 8. number of duplicate positions eliminated while generating the partial-expansions
;; 9. cumulative sort-time from partial-expansion files phase1
;; 10. cumulative write-time from partial-expansion files phase1
;; 11. other-expand-time (i.e., non-sort and write, or basically the successor-generation)

;; ---------------------------------------------------------------------------------------
;; EXPANSION .....


;; remove-dupes: fringe fringe (listof filespec) int string int int float float float (vectorof worker) -> sampling-stat
;; Running in distributed worker processes:
;; Remove duplicate positions from the list of expand-fspec files (i.e., partial-expansion...),
;; for any positions that appear in multiple partial expansion files.
;; All of the partial-expansion files are sorted, so we can write the merged result to slices as we go.
;; Write the non-duplicate positions to a "proto-fringe-dXX-NNN-SSS" file for XX depth, NNN generating process-id, SSS target segment/slice
(define (remove-dupes pf cf lo-expand-fspec wid ofile-name depth partial-exp-dupes part-sort-time part-write-time other-expand-time placeless-workers)
  ;; the ofile-name is just the file-name -- no *local-store* path where needed
  #|(printf "EXPAND PHASE 2 (REMOVE DUPLICATES) pf: ~a~%cf: ~a~%all of lo-expand-fspec: ~a~%ofile-name: ~a~%depth: ~a~%"
          pf cf lo-expand-fspec ofile-name depth);|#
  ;; EXPAND PHASE 2 (REMOVE DUPLICATES)
  (let* ([pffh (and (not *late-duplicate-removal*) (fh-from-fringe pf))]
         [cffh (and (not *late-duplicate-removal*) (fh-from-fringe cf))]
         [n-pos-to-process (for/sum ([an-fspec (in-list lo-expand-fspec)]) (filespec-pcount an-fspec))]
         [lo-effh (for/list ([an-fspec (in-list lo-expand-fspec)]) (fh-from-filespec an-fspec))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (hcposition<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap lo-effh)
                          lheap)]
         [proto-slice-num 0]
         [slice-upper-bound (vector-ref *fringe-slice-bounds* (add1 proto-slice-num))]
         [proto-slice-ofile 
          (open-output-file (string-append *local-store* ofile-name "-" (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))
                            #:exists 'replace)]
         [slice-counts (make-vector *num-fringe-slices* 0)]
         [sample-stats 
          (vector 0 ; number of positions preserved for further merging
                  0 ; number of positions discarded because duplicate with prev- or current-fringes
                  0 ; number of positions discarded because duplicate with another partial-expansion
                  slice-counts
                  #f 
                  ofile-name ;; here, use the stem of the shared ofile-name 
                  0
                  n-pos-to-process
                  partial-exp-dupes 
                  part-sort-time 
                  part-write-time
                  other-expand-time)]
         [last-pos-bs #"NoLastPos"]
         [remote-hosts (vector-map (lambda (pw) (remote #:host (placeless-worker-host pw))) placeless-workers)]
         )
    ;; locally merge the pre-proto-fringes, removing internal duplicates and maybe dupes found in prev- or current-fringes
    (for ([an-fhead (in-heap/consume! heap-o-fheads)])
      (let ([efpos (fringehead-next an-fhead)])
        (unless ;; efpos is a duplicate
            (or (and (bytes=? (hc-position-bs efpos) last-pos-bs) ; duplicate from most recently written 
                     (vector-set! sample-stats 2 (add1 (vector-ref sample-stats 2))))
                (and
                  (not *late-duplicate-removal*)
                  (or (and (position-in-fhead? efpos pffh) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1))))
                      (and (position-in-fhead? efpos cffh) (vector-set! sample-stats 1 (add1 (vector-ref sample-stats 1)))))))
          (set! last-pos-bs (hc-position-bs efpos))
          (unless (< (hc-position-hc efpos) slice-upper-bound)
            ;; if efpos-hc is >= to the slice-upper-bound, advance the proto-slice-num/ofile/upper-bound until it is not
            (close-output-port proto-slice-ofile)
            ;; synchronously, start copying the proto-slice to the appropriate host; BUT: later do this asynchronously using process
            (unless (string=? (placeless-worker-host (vector-ref placeless-workers proto-slice-num))
                              (placeless-worker-host (vector-ref placeless-workers wid)))
              (scp (vector-ref remote-hosts proto-slice-num)
                   (format "~a~a-~a" *local-store* ofile-name (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))
                   (at-remote (vector-ref remote-hosts proto-slice-num) *local-store*))
              #|
              (system (format "scp -pq4 ~a ~a:~a"
                              (format "~a~a-~a" *local-store* ofile-name (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))
                              (placeless-worker-host (vector-ref placeless-workers proto-slice-num))
                              *local-store*))
              |#
              )
            (set! proto-slice-num (add1 proto-slice-num))
            (set! proto-slice-ofile
                  (open-output-file (string-append *local-store* ofile-name "-" (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right)) 
                                    #:exists 'replace))
            (set! slice-upper-bound (vector-ref *fringe-slice-bounds* (add1 proto-slice-num))))
          (write-bytes  (hc-position-bs efpos) proto-slice-ofile)
          (when (is-goal? efpos) (vector-set! sample-stats 4 (hc-position-bs efpos)))
          (vector-set! slice-counts proto-slice-num (add1 (vector-ref slice-counts proto-slice-num))))
        (advance-fhead! an-fhead)
        (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
          (heap-add! heap-o-fheads an-fhead))))
    ;(printf "remote-expand-part-fringe: HAVE EXPANSIONS:~%")
    ;; close input and output ports
    (for ([fh (in-list (if *late-duplicate-removal* lo-effh (cons pffh (cons cffh lo-effh))))])
      (close-input-port (fringehead-iprt fh)))
    (close-output-port proto-slice-ofile)
    ;; copy last proto-fringe-segment if necessary
    (unless (string=? (placeless-worker-host (vector-ref placeless-workers proto-slice-num))
                      (placeless-worker-host (vector-ref placeless-workers wid)))
      (scp (vector-ref remote-hosts proto-slice-num)
           (format "~a~a-~a" *local-store* ofile-name (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))
           (at-remote (vector-ref remote-hosts proto-slice-num) *local-store*))
      #|
      (system (format "scp -pq4 ~a ~a:~a"
                      (format "~a~a-~a" *local-store* ofile-name (~a proto-slice-num #:left-pad-string "0" #:width 3 #:align 'right))
                      (placeless-worker-host (vector-ref placeless-workers proto-slice-num))
                      *local-store*))
      |#
      )
    ;; copy empty proto-fringe-segments if necessary
    (for ([i (in-range (add1 proto-slice-num) *num-fringe-slices*)])
      (touch (string-append *local-store* ofile-name "-" (~a i #:left-pad-string "0" #:width 3 #:align 'right)))
      (unless (string=? (placeless-worker-host (vector-ref placeless-workers i))
                        (placeless-worker-host (vector-ref placeless-workers wid)))
        (scp (vector-ref remote-hosts i)
                   (format "~a~a-~a" *local-store* ofile-name (~a i #:left-pad-string "0" #:width 3 #:align 'right))
                   (at-remote (vector-ref remote-hosts i) *local-store*))
        #|
        (system (format "scp -pq4 ~a ~a:~a"
                        (format "~a~a-~a" *local-store* ofile-name (~a i #:left-pad-string "0" #:width 3 #:align 'right))
                        (placeless-worker-host (vector-ref placeless-workers i))
                        *local-store*))
        |#
        ))
    ;; complete the sampling-stat
    (vector-set! sample-stats 0 (for/sum ([i (vector-ref sample-stats 3)]) i))
    (vector-set! sample-stats 6 (for/vector ([i *num-fringe-slices*]) 
                                  (file-size (format "~a~a-~a" *local-store* ofile-name (~a i #:left-pad-string "0" #:width 3 #:align 'right)))))
    ;; delete files that are no longer needed
    ;; partial-expansions
    (for ([efspec (in-list lo-expand-fspec)]) (delete-file (filespec-fullpathname efspec)))
    ;; proto-fringe-segments that have been copied to remote hosts -- NOTE: once we change the copy to async this will have to come at the end!!!
    (for ([i (in-range *num-fringe-slices*)]
          #:unless (string=? (placeless-worker-host (vector-ref placeless-workers i))
                             (placeless-worker-host (vector-ref placeless-workers wid))))
      (delete-file (format "~a~a-~a" *local-store* ofile-name (~a i #:left-pad-string "0" #:width 3 #:align 'right))))
    sample-stats))

;; dump-partial-expansion: int string int (listof fspec) float float -> (values (listof fspec) int float float)
;; given the count of pending expanded positions to write, the out-file template, the out-file counter, and the previously written filespecs,
;; sort and write the specified number of positions to the appropriately opened new file,
;; returning two values: the list with the new filespec added and the number of duplicates eliminated at this phase
(define (dump-partial-expansion pcount ofile-template ofile-counter ofiles prev-dupes sort-time0 write-time0)
  (let* ([hc-to-scrub 'hcpos-to-scrub]
         [f (format "~a-~a" ofile-template ofile-counter)]
         [fullpath (string-append *local-store* f)]
         [this-batch 0]
         [sort-time 0]
         [write-time 0])
    ;; scrub the last part of the vector with bogus positions
    #| Need this code active for rnrs/sorting-6
    (for ([i (in-range pcount (vector-length *expansion-space*))])
      (set! hc-to-scrub (vector-ref *expansion-space* i))
      (set-hc-position-hc! hc-to-scrub *most-positive-fixnum*)    ;; make vector-sort! put these at the very end, but if a positions has *most-positive-fixnum* ...
      (bytes-copy! (hc-position-bs hc-to-scrub) 0 #"~~IgnoreMe")) ;; #\~ (ASCII character 126) is greater than any of our positions
    |#
    ;; sort the vector
    (set! sort-time (current-milliseconds))
    ;(vector-sort! hcposition<? *expansion-space*)
    (vector-sort! *expansion-space* hcposition<? 0 pcount)
    (set! sort-time (- (current-milliseconds) sort-time))
    ;; write the first pcount positions to the file
    (set! write-time (current-milliseconds))
    (set! this-batch (write-fringe-to-disk *expansion-space* fullpath pcount #t))
    (set! write-time (- (current-milliseconds) write-time))
    ;; return the two values: augmented list of filespecs, and the incremented number of duplicates eliminated during writing
    (values (cons (make-filespec f this-batch (file-size fullpath) *local-store*)
                  ofiles)
            (+ prev-dupes (- pcount this-batch))
            (+ sort-time sort-time0)
            (+ write-time write-time0))))

;; remote-expand-part-fringe: (list int int) int fringe fringe int (vectorof placeless-worker) -> {whatever returned by remove-dupes}
;; given a pair of indices into the current-fringe that should be expanded by this process, a worker-id (wid),
;; and the prev- and current-fringes, and the depth ...
;; expand the positions in the indices range, ignoring duplicates other than within the new fringe being constructed.
(define (remote-expand-part-fringe ipair wid pf cf depth placeless-workers)
  ;; prev-fringe spec points to default shared directory; current-fringe spec points to *local-store* folder
  ;(printf "remote-expand-part-fringe: starting with pf: ~a, and cf: ~a~%" pf cf)
  ;; EXPAND PHASE 1
  (let* ([expand-part-time (current-milliseconds)]
         ;; file naming convention: partial-expansionPPP-NNN for PPP process-id and NNN expansion file count
         [pre-ofile-template-fname (format "partial-expansion~a" (~a wid #:left-pad-string "0" #:width 3 #:align 'right))]
         [pre-ofile-counter 0]
         [pre-ofiles empty]
         ;; *** Dynamically choose the size of the pre-proto-fringes to keep the number of files below 500 ***
         [start (first ipair)]
         [end (second ipair)]
         [assignment-count (- end start)]
         [expanded-phase1 1];; technically, not yet, but during initialization in pre-resv do loop
         ;; make the fringehead advanced to the right place
         [cffh (fh-from-fringe cf start)]
         [dupes-caught-here 0]
         [sort-time 0.0]
         [write-time 0.0]
         )
    ;; do the actual expansions
    (do ([i 1 (add1 i)]
         [expansion-ptr (expand* (fringehead-next cffh) 0)
                        (expand* (fringehead-next cffh) expansion-ptr)])
      ((>= i assignment-count)
       (set!-values (pre-ofiles dupes-caught-here sort-time write-time)
                    (dump-partial-expansion expansion-ptr pre-ofile-template-fname pre-ofile-counter pre-ofiles dupes-caught-here sort-time write-time)))
      ;; When we have collected the max number of expansions, create another pre-proto-fringe file
      (when (>= expansion-ptr EXPAND-SPACE-SIZE)
        (set!-values (pre-ofiles dupes-caught-here sort-time write-time)
                     (dump-partial-expansion expansion-ptr pre-ofile-template-fname pre-ofile-counter pre-ofiles dupes-caught-here sort-time write-time))
        (set! pre-ofile-counter (add1 pre-ofile-counter))
        (set! expansion-ptr 0))
      (advance-fhead! cffh)
      (set! expanded-phase1 (add1 expanded-phase1)))
    ;(printf "remote-exp-part-fringe: PHASE 1: expanding ~a positions of assigned ~a~%" expanded-phase1 assignment-count)
    (when (< expanded-phase1 assignment-count)
      (error 'remote-expand-part-fringe
             (format "only expanded ~a of the assigned ~a (~a-~a) positions" expanded-phase1 assignment-count start end)))
    (close-input-port (fringehead-iprt cffh))
    ;; PHASE 2: now pass through the proto-fringe expansion file(s) as well as prev-fringe and current-fringe to remove duplicates
    (remove-dupes pf cf pre-ofiles wid
                  (format "proto-fringe-d~a-~a" depth (~a wid #:left-pad-string "0" #:width 3 #:align 'right)) ;; ofile-name
                  depth
                  dupes-caught-here sort-time write-time (- (current-milliseconds) expand-part-time) placeless-workers)))


;; remote-expand-fringe: (listof (list fixnum fixnum)) fringe fringe int (vectorof worker) -> (listof sampling-stat)
;; trigger the distributed expansion according to the given ranges
;; In theory, it shouldn't matter where the files pointed to by the fringe are located.
(define (remote-expand-fringe ranges pf cf depth workers)
  ;(printf "remote-expand-fringe: current-fringe of ~a split as: ~a~%" (fringe-pcount cf) ranges ;(map (lambda (pr) (- (second pr) (first pr))) ranges)
   ;       )
  ;; Now initiate the actual expansions
  (let* ([just-start-things (for ([range-pair (in-list ranges)]
                                  [i (in-range (length ranges))]
                                  [wp (vector-map worker-place workers)])
                              (stp-worker-expand-slice wp (list range-pair i pf cf depth
                                                                (vector-map strip-place workers)))
                              ;(remote-expand-part-fringe range-pair i pf cf depth)
                              )]
         ;[pmsg1 (printf "kicked off the expand-slice at the places~%")]
         [distrib-results (for/list ([range (in-range (length ranges))]
                                     [wp (vector-map worker-place workers)])
                            range
                            (wait-for-results wp)
                            (stp-worker-get-expand-results wp)
                            )])
    ;(printf "remote-expand-fringe: respective expansion counts: ~a~%" (map (lambda (ssv) (vector-ref ssv 0)) distrib-results))
    distrib-results))


;; ------------------------------------------------------------------------------------------
;; MERGING .....
;; merge proto-fringe-segments that were generated by various workers for each particular segment range.
;; the result of the merge is a bona fide fringe-segment with naming convention fringe-segment-d<N>-<seg#>


;; distributed-merge-proto-fringe-slices: (list number number) (vectorof fspec) int string fringe fringe int -> (list string number number)
;; given a list of filespecs pointing to the proto-fringe-segments (PFSs) assigned to this worker and needing to be merged, copy the PFSs
;; and merge into a single fringe-segment that will participate in the new fringe, removing duplicates among slices,
;; and depending on value of *late-duplicate-removal* possibly also removing duplicates 
;; from the corresponding segment of the previous/current fringe.
;; Returns: list of file-name, position-count in that file, and file-size of that file.
(define (distributed-merge-proto-fringe-slices range slice-fspecs depth ofile-name pf cf which-slice)
  ;(define (remote-merge-proto-fringes my-range expand-files-specs depth ofile-name)
  ;; expand-files-specs are of pattern: "proto-fringe-dXX-NNN" for depth XX and proc-id NNN, pointing to working (shared) directory 
  ;; ofile-name is of pattern: "fringe-segment-dX-NNN", where the X is the depth and the NN is a slice identifier
  ;(printf "distributed-merge-proto-fringe-slices: which-slice given as ~a~%" which-slice)
  ;(printf "prev-fring: ~a~%curr-fringe: ~a~%" pf cf) 
  ;(printf "distributed-merge-proto-fringe-slices: ")
  ;(for ([fs slice-fspecs]) (printf "~a: ~a;  " (filespec-fname fs) (filespec-pcount fs))) (printf "~%")
  (let* ([slice-fspecs-fbase (filespec-fbase (vector-ref slice-fspecs 0))]
         [mrg-segment-oport (open-output-file (format "~a~a" *local-store* ofile-name) #:exists 'replace)]
         #|
         [local-protofringe-fspecs (for/vector ([fs slice-fspecs]
                                                #:unless (zero? (filespec-pcount fs))) (rebase-filespec fs *local-store*))]
         |#
         ;[local-protofringe-fspecs (for/list ([fs (in-vector slice-fspecs)] #:unless (zero? (filespec-pcount fs))) fs)]
         ;[pmsg1 (printf "distmerge-debug1: ~a fspecs in ~a~%" (vector-length slice-fspecs) local-protofringe-fspecs)]
         ;******
         ;****** move the fringehead creation inside the heap-o-fhead construction in order to avoid the short-lived list allocation *******
         [to-merge-fheads 
          (for/list ([exp-fspec (in-vector slice-fspecs)])
            (fh-from-filespec exp-fspec))]
         ;[pmsg2 (printf "distmerge-debug2: made 'to-merge-fheads' having ~a fringeheads~%" (length to-merge-fheads))]
         [heap-o-fheads (let ([lheap (make-heap (lambda (fh1 fh2) (hcposition<? (fringehead-next fh1) (fringehead-next fh2))))])
                          (heap-add-all! lheap 
                                         (filter-not (lambda (fh) (eof-object? (fringehead-next fh))) to-merge-fheads))
                          lheap)]
         ;[pmsg2-5 (printf "distmerge-debug2-5: made heap with ~a fheads'~%" (heap-count heap-o-fheads))]
         #|
         [pffh (fh-from-fringe pf (if (= (length (fringe-segments pf)) 1) 0
                                      (for/sum ([i which-slice]
                                                [fspec (fringe-segments pf)]) (filespec-pcount fspec))))]
         |#
         [pffh (fh-from-filespec (if (= (length (fringe-segments pf)) 1)
                                     (first (fringe-segments pf))
                                     (list-ref (fringe-segments pf) which-slice)))]
         ;[pmsg2-7 (printf "debug2-7: made the pffh okay~%")]
         ;[cffh (fh-from-fringe cf (first range))]
         [cffh (fh-from-filespec (if (= (length (fringe-segments cf)) 1)
                                     (first (fringe-segments cf))
                                     (list-ref (fringe-segments cf) which-slice)))]
         ;[pmsg3 (printf "distmerge-debug3: made the heap with ~a frigeheads in it~%" (heap-count heap-o-fheads))]
         ;****** log duplicate eliminations here
         [segment-size (let ([last-pos (make-hcpos (bytes 49 49 49 49))]
                             [keep-pos (void)]
                             [num-written 0])
                         (for ([an-fhead (in-heap/consume! heap-o-fheads)])
                           (set! keep-pos (fringehead-next an-fhead))
                           (unless (or (and (bytes=? (hc-position-bs keep-pos) (hc-position-bs last-pos))) ;; don't write duplicates
                                       (and
                                         *late-duplicate-removal*
                                         (or (position-in-fhead? keep-pos pffh)
                                             (position-in-fhead? keep-pos cffh))))
                             (write-bytes (hc-position-bs keep-pos) mrg-segment-oport)
                             (set! num-written (add1 num-written))
                             (set! last-pos keep-pos))
                           (advance-fhead! an-fhead)
                           (unless (fhdone? an-fhead) ;;(eof-object? (peek-byte (fringehead-iprt an-fhead) 1))
                             (heap-add! heap-o-fheads an-fhead))
                           )
                         num-written)])
    (close-output-port mrg-segment-oport)
    (for ([fhead (in-list to-merge-fheads)]) (close-input-port (fringehead-iprt fhead)))
    ;; delete the proto-fringe-segments after use
    (for ([fspc (in-vector slice-fspecs)]) 
      (delete-file (filespec-fullpathname fspc)))
    (list ofile-name segment-size (file-size (string-append *local-store* ofile-name)))))

;; remote-merge: (listof (list number number)) (vectorof (vectorof fspec)) int fringe fringe (vectorof worker) -> (listof (list string int))
;; merge the proto-fringes from the workers and, depending on value of *late-dulicate-removal*, 
;; remove duplicate positions appearing in either prev- or current-fringes at the same time.
;; ranges is a list of pairs for position indices to be covered by the corresponding slice
;; expand-files-specs (proto-fringe-specs) is vector of vector of filespecs, the top-level has one for each slice,
;; each one containing as many proto-fringes as expanders, all of which need to be merged
(define (remote-merge ranges expand-files-specs depth pf cf workers)
  (unless (= (length ranges) (vector-length expand-files-specs) *num-fringe-slices*)
    (error 'remote-merge "mis-match between ranges, expand-files-specs, and/or *num-fringe-slices*"))
  ;; print the merging fringe work to be done
  #|
  (printf "remote-merge: fringe-slice proto-sizes prior to merging at depth ~a: ~a~%" depth 
          (for/list ([expand-fspecs-slice (in-vector expand-files-specs)])
            (for/sum ([fspec expand-fspecs-slice]) (filespec-pcount fspec))))
  |#
  (let* ([just-start-things (for ([i (in-range *num-fringe-slices*)]
                                  [range ranges]
                                  [expand-fspecs-slice (in-vector expand-files-specs)]
                                  [wp (vector-map worker-place workers)])
                              (let ([ofile-name (format "fringe-segment-d~a-~a" depth (~a i #:left-pad-string "0" #:width 3 #:align 'right))])
                                (stp-worker-merge-slices wp (list range expand-fspecs-slice depth ofile-name pf cf i))))]
         [merge-results (for/list ([w workers]
                                   [i (in-range *num-fringe-slices*)])
                          ;(printf "waiting for merge-results worker ~a at host ~a~%" i (worker-host w))
                          (wait-for-results (worker-place w))
                          ;(printf "finished waiting for worker ~a at host ~a~%" i (worker-host w))
                          (stp-worker-get-merge-results (worker-place w)))])
    ;;(printf "distributed-expand-fringe: merge-range = ~a~%~a~%" merge-range merged-responsibility-range)    
    ;; print the sizes of the merged fringe-slices
    #|
    (printf "remote-merge: fringe-slice sizes after merging at depth ~a: ~a~%" depth (map second merge-results))
    (printf "remote-merge: fringe-slice duplicate removal at depth ~a: ~a~%" depth
            (for/list ([expand-fspecs-slice (in-vector expand-files-specs)]
                       [reduced-slice-and-size merge-results])
              (- (for/sum ([fspec expand-fspecs-slice]) (filespec-pcount fspec)) (second reduced-slice-and-size))))
    |#
    merge-results))


;; ------------------------------------------------------------------------------------------
;; DISTRIBUTED EXPAND AND MERGE

;; distributed-expand-fringe: fringe fringe int (vectorof worker) -> (list string int int)
;; Distributed version of expand-fringe
;; given prev and current-fringes and the present depth, expand and merge the current fringe,
;; returning the fringe-spec of the newly expanded and merged fringe.
(define (distributed-expand-fringe pf cf depth workers)
  #|(printf "distributed-expand-fringe: at depth ~a, pf-definespec: ~a; cf-spec: ~a~%" 
          depth pf-spec cf-spec)|#
  (let* (;; EXPAND
         [start-expand (current-seconds)]
         [first-time? (= (length (fringe-segments cf)) 1)]
         ;; --- Distribute fringes (if necessary) ---------------------------
         ;; NEED TO (MAYBE) DISTRIBUTE FRINGE SEGMENTS TO RESPECTIVE WORKERS
         [unique-worker-hosts (remove-duplicates (for/list ([w workers]) (worker-host w)))]
         [maybe-distrib-fringes (when first-time?
                                  (begin (distribute-fringe cf unique-worker-hosts)
                                         (distribute-fringe pf unique-worker-hosts)))]
         ;[ranges (make-vector-ranges (fringe-pcount cf))]
         [ranges (if (or #t (= (length (fringe-segments cf)) *num-fringe-slices*))
                     (make-simple-ranges (fringe-segments cf))
                     (dynamic-slice-ranges (fringe-segments cf)))]
         ;; --- Distribute the actual expansion work ------------------------
         [pmsg1 (printf "starting distributed expand at depth ~a~%" depth)]
         [sampling-stats (remote-expand-fringe ranges pf cf depth workers)]
         [end-expand (current-seconds)]
         ;; -----------------------------------------------------------------
         [check-for-goal (for/first ([ss (in-list sampling-stats)]
                                     #:when (vector-ref ss 4))
                           (set! *found-goal* (vector-ref ss 4)))]
         ;; make filespecs for proto-fringe-dXX-NN slices the relevant data in the sampling-stats
         [proto-fringe-fspecs (for/vector ([i (in-range *num-fringe-slices*)]);; for each index to a slice...
                                ;; pull out the info from each sampling-stat
                                (for/vector ([ss (in-list sampling-stats)]) 
                                  (make-filespec (string-append (vector-ref ss 5) "-" (~a i #:left-pad-string "0" #:width 3 #:align 'right)) ;; fname
                                                 (vector-ref (vector-ref ss 3) i) ;pcount
                                                 (vector-ref (vector-ref ss 6) i) ;file-size
                                                 *local-store*)))]
         ;; MERGE
         ;; --- Distribute the merging work ----------
         ;[pmsg2 (printf "starting distributed merge at depth ~a w/ proto-specs: ~a~%" depth proto-fringe-fspecs)]
         [sorted-segment-fspecs 
          (remote-merge (if first-time?
                            (make-list *num-fringe-slices* (car ranges))
                            ranges)
                        proto-fringe-fspecs depth pf cf workers)]
         [merge-end (current-seconds)]
         ;[pmsg3 (printf "finished distributed merge at depth ~a~%" depth)]
         ;; -------------------------------------------
         ;; delete previous fringe now that duplicates have been removed
         [delete-previous-fringe (unless *preserve-prior-fringes*
                                   (delete-fringe pf))]
         [sorted-expansion-files (map first sorted-segment-fspecs)]
         [sef-lengths (map second sorted-segment-fspecs)]
         [sef-sizes (map third sorted-segment-fspecs)]
         )
    ;; create the _new_ current-fringe
    #|
    (for ([f sorted-expansion-files])
      ;(printf "distributed-expand-fringe: concatenating ~a~%" f)
      (system (format "cat ~a >> fringe-d~a" f depth)))|#
    ;;--- delete files we don't need anymore ---------
    ;; delete proto-fringe-segments on the *share-store*
    #|
    (for* ([fspecs (in-vector proto-fringe-fspecs)]
           [fspec (in-vector fspecs)])
      (delete-file (filespec-fullpathname fspec)))
    |#
    ;(system "rm partial-expansion* partial-merge*")
    ;(unless (string=? *master-name* "localhost") (delete-file (fspec-fname cf-spec)))
    ;; file-copy, expansion, merge, total
    (printf "expand-merge-times: ~a\t~a\t~a\t~a~%"
            depth
            (- end-expand start-expand)         ;expansion
            (- merge-end end-expand)            ;merge
            (- (current-seconds) start-expand)) ;total
    ;; report the cumulative node sort and write time during expansion phase1
    (printf "node-sort-write: ~a\t~a\t~a\t~a\t~a\t~a~%"
            depth
            (for/sum ([ss (in-list sampling-stats)]) (vector-ref ss 9))  ; sum of worker sort-times
            (for/sum ([ss (in-list sampling-stats)]) (vector-ref ss 10)) ; sum of worker write-times
            (length ranges)                                    ; number of workers for computing average
            (- end-expand start-expand)                        ; total elapsed expansion time for estimation of successor generation
            (for/sum ([ss (in-list sampling-stats)]) (vector-ref ss 11)) ; time mainly for successor generation (non- sort and write)
            )
    ;; report the duplicate elimination data
    (let ([counts (vector 0 0 0 0)])
      (for ([ss (in-list sampling-stats)]) 
        (vector-set! counts 0 (+ (vector-ref ss 0) (vector-ref counts 0)))
        (vector-set! counts 1 (+ (vector-ref ss 1) (vector-ref counts 1)))
        (vector-set! counts 2 (+ (vector-ref ss 2) (vector-ref counts 2)))
        (vector-set! counts 3 (+ (vector-ref ss 8) (vector-ref counts 3))))
      (printf "duplicate-elimination-data: ~a\t~a\t~a\t~a\t~a\t~a\t~a~%"
              depth
              (vector-ref counts 0) ; sum of duplicate-free positions written to proto-fringes -- pre-merge
              (vector-ref counts 1) ; duplicates eliminated because prev- or current-fringe
              (vector-ref counts 2) ; duplicates eliminated because other partial-expansion at current depth
              (vector-ref counts 3) ; duplicates eliminated before first writing to partial-expansion
              (for/sum ([n counts]) n) ; total number of expanded positions handled at this level
              (for/sum ([n (in-list sef-lengths)]) n))) ; number of positions in the new fringe
    ;; make the new fringe to return
    (make-fringe *local-store*
                 (for/list ([segmentfile (in-list sorted-expansion-files)]
                            [length (in-list sef-lengths)]
                            [size (in-list sef-sizes)])
                   (make-filespec segmentfile length size *local-store*))
                 (for/sum ([len (in-list sef-lengths)]) len))
    ))


;;----------- RPC-SERVER SECTION --------------

;; wait-for-results: place -> (void)
(define wait-for-results
  (let ([wait-time 0.01])
    (lambda (wp)
      (cond [(stp-worker-ready? wp) (set! wait-time 0.01)]
            [else (sleep wait-time)
                  (set! wait-time (* 2 wait-time))
                  (wait-for-results wp)]))))

;; the core of the remote worker that can do expansion and merging of responsibility ranges
(define-remote-server stp-worker
  ;(define-state mylog null)
  (define-state MYID -1)
  ;; flag for whether the worker is done with a task and has results available
  (define-state ready #f)
  ;; the results from either expand or merge, respectively
  (define-state expand-results null)
  (define-state merge-results null)

  ;; initialize this worker's id
  (define-cast (init id)
    (set! MYID id)
    ;(set! mylog (open-output-file (format "worker-~a" id) #:exists 'replace))
    )
  ;; report this worker's id
  (define-rpc (getid) MYID)

  ;; are results available?
  (define-rpc (ready?) ready)

  ;; get the results from expansion (assuming available)
  (define-rpc (get-expand-results)
    expand-results)

  ;; get the results from merging (assuming available)
  (define-rpc (get-merge-results)
    merge-results)

  ;; initiate the expansion of a responsibility-range (but do not wait for completion)
  (define-cast (expand-slice args)
    (set! ready #f)
    (let ([range-pair (first args)]
          [i (second args)]
          [pf (third args)]
          [cf (fourth args)]
          [depth (fifth args)]
          [workers (sixth args)])
      (set! expand-results (remote-expand-part-fringe range-pair i pf cf depth workers))
      (set! ready #t)
      ))

  ;; initiate the merging of expansion results for this responsibility range (but do not wait)
  (define-cast (merge-slices args)
    (set! ready #f)
    (let ([range (first args)]
          [expand-fspecs-slice (second args)]
          [depth (third args)]
          [ofile-name (fourth args)]
          [pf (fifth args)]
          [cf (sixth args)]
          [i (seventh args)])
      (set! merge-results (distributed-merge-proto-fringe-slices range expand-fspecs-slice depth ofile-name pf cf i))
      (set! ready #t)))

  ;; return the log port
  #|
  (define-rpc (get-logger)
    mylog)
  |#
  
  ;; close the log file
  #|(define-cast (close-log)
    (close-output-port mylog))
  |#
  )
