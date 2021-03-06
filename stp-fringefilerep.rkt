#lang racket/base

(require mzlib/string
         racket/vector
         racket/port
         racket/file
         racket/format)

(require 
  ;racket/generator
  "stpconfigs/configenv.rkt"
  "stp-init.rkt"
  "stp-solve-base.rkt")

(provide (all-defined-out))

#|====================================================================================
stp-fringefilerep: Utilities for processing fringes that are stored in files

When fringes are stored on files, need to support creation/access/manipulation/etc. 
and provide uniform interface for solvers to access fringes as if they were simple sequence.

Data definitions included here:
- fringe: (vector full-path-to-file (listof segment-spec) number-of-positions)
- filespec:
- fhead (short for fringehead) is a structure (make-fringehead position input-port readcount totalcount)

findex (short for fringe-index): (listof segment-spec) [assumes the list of segment-specs is sorted]
|#

;; -----------------------------------------------------------------------------------
;; --- FRINGE ------------------------------------------------------------------------

;; a fringe is a vector: (vector string (listof filespec) int)
;; where the first string is the base-path to the files, the list of segment-specs describe the segments,
;; and the ints are the count of positions summed over the segments
;; NOTE: an fspec must be a list or vector because they are passed over the riot layer
(define (make-fringe fbase segments pcount) (vector fbase segments pcount))
(define (fringe-fbase an-fs) (vector-ref an-fs 0))
(define (fringe-segments an-fs) (vector-ref an-fs 1))
(define (fringe-fullpathnames an-fs)
  (for/list ([seg (in-list (fringe-segments an-fs))]) (string-append (fringe-fbase an-fs) (filespec-fname seg))))
(define (fringe-pcount an-fs) (vector-ref an-fs 2))


;; -----------------------------------------------------------------------------------
;; --- FILESPEC (SEGMENT) ------------------------------------------------------------

;; a filespec is a vector: (vector {tentative-removal: min-hashcode max-hashcode} fname position-count file-size basepath)
(define (make-filespec ;minhc maxhc 
                       fname pcount fsize fbase) 
  (vector ;minhc maxhc
          fname pcount fsize fbase))
;(define (filespec-minhc fs) (vector-ref fs 0))
;(define (filespec-maxhc fs) (vector-ref fs 1))
(define (filespec-fname fs) (vector-ref fs 0)); was 2
(define (filespec-pcount fs) (vector-ref fs 1));was 3
(define (filespec-fsize fs) (vector-ref fs 2)); was 4
(define (filespec-fbase fs) (vector-ref fs 3)); was 5
(define (filespec-fullpathname fs) (string-append (filespec-fbase fs) (filespec-fname fs)))
(define (rebase-filespec fs newbase) (let ([copyfs (vector-copy fs)]) (vector-set! copyfs 3 newbase) copyfs)); 3 was 5
(define (rebase-filespec! fs newbase) (vector-set! fs 3 newbase) fs); 3 was 5


;; -----------------------------------------------------------------------------------
;; --- FRINGEHEAD --------------------------------------------------------------------

;; a fringehead in a struct
(struct fringehead (next iprt filespecs readcount total) #:mutable)
;; where next is a position, iprt is the current input-port, filespecs is the list of filespecs for the segments making up this fringe
;; with the first corresponding to the current intput-port, readcount is the number of positions read from this fringehead
;; and total is the number of positions expected to be able to read
;; NOTE on readcounts: A readcount value of 'n' should imply that the current fringehead-next value is the n'th position in the fringe

;; fhdone?: fringehead -> boolean
;; #t if readcount >= total for the given fringehead -- that is, this fringehead is exhausted.
;; Note: readcount starts at 1, 
(define (fhdone? fh)
  (when (and (eof-object? (fringehead-next fh)) (null? (cdr (fringehead-filespecs fh))) (<= (fringehead-readcount fh) (fringehead-total fh)))
    ;; try to reset 
    (error 'fhdone? (format "hit end of fringe reading only ~a of ~a positions~%" (fringehead-readcount fh) (fringehead-total fh))))
  (or (> (fringehead-readcount fh) (fringehead-total fh))
      (and (eof-object? (fringehead-next fh))
           (null? (cdr (fringehead-filespecs fh))))))

;; advance-fhead!: fringehead -> position OR void
;; move to the next position, but check to make sure something is available if expected
(define (advance-fhead! fh)
  (when (< (fringehead-readcount fh) (filespec-pcount (car (fringehead-filespecs fh))))
    (do ([sleep-time 0.01 (* sleep-time 2)])
      ((not (eof-object? (peek-bytes 1 1 (fringehead-iprt fh)))) 'proceed)
      (printf "advance-fhead!: sleeping while waiting on ~a after reading ~a of ~a positions~%" 
              (filespec-fullpathname (car (fringehead-filespecs fh))) (fringehead-readcount fh) (fringehead-total fh))
      (sleep sleep-time)))
  (unless (fhdone? fh)
    (set-fringehead-next! fh (read-bs->hcpos (fringehead-iprt fh)))
    (set-fringehead-readcount! fh (add1 (fringehead-readcount fh)))
    (when (and (eof-object? (fringehead-next fh)) (not (fhdone? fh))) ; advance to next segment
      (set-fringehead-filespecs! fh (cdr (fringehead-filespecs fh)))
      (close-input-port (fringehead-iprt fh))
      (set-fringehead-iprt! fh (open-input-file (filespec-fullpathname (car (fringehead-filespecs fh)))))
      (set-fringehead-next! fh (read-bs->hcpos (fringehead-iprt fh))))
    (fringehead-next fh)))

;; position-in-fhead?: position fringehead -> boolean
;; determines if given position is present in the fringe headed by the given fringehead
;; side-effect: advances the fringehead, assuming no position _less-than_ thi given position will subsequently be queried
;; if the given position is less than the head of the fringe, then we'll not find it further in the fringe
;; that is, advance the fh while it is strictly less-than the given position
(define (position-in-fhead? p fh)
  (do ([fhp (fringehead-next fh) (advance-fhead! fh)])
    ((or (fhdone? fh)
         (not (hcposition<? fhp p)))
     (and (hc-position? fhp)
          (bytes=? (hc-position-bs fhp) (hc-position-bs p))))))

;; fh-from-fringe: fringe [int 0] -> fringehead
;; create a fringehead from a given fringe and advance it to the requested start point,
;; that is, fringehead-next is holding the skip+1st position, which is also the readcount, and skip were skipped
;; *** the open port must be closed by the requestor of this fringehead
(define (fh-from-fringe f [skip 0])
  ;(printf "fh-from-fringe: starting~%")
  (let*-values ([(active-fspecs dropped) (drop-some-maybe (fringe-segments f) skip)]
                [(firstfullpathname) (filespec-fullpathname (car active-fspecs))]
                [(inprt) (open-input-file firstfullpathname)]
                [(new-fh) (fringehead (read-bs->hcpos inprt) inprt active-fspecs (add1 dropped) (fringe-pcount f))])
    (for ([i (in-range (- skip dropped))]) (advance-fhead! new-fh))
    #|(printf "fh-from-fringe: leaving, looking at ~a w/ fh-next = ~a, fh-readcount = ~a, asked to advance to ~a~%" 
            (filespec-fullpathname (first (fringehead-filespecs new-fh))) (fringehead-next new-fh) (fringehead-readcount new-fh) skip)|#
    new-fh))

;; fh-from-filespec: filespec -> fringehead
;; create a simple fringehead for a single filespec.  create a dummy fringe wrapper and use fh-from-fringe
(define (fh-from-filespec fspec)
  (fh-from-fringe (make-fringe (filespec-fbase fspec)
                               (list fspec)
                               (filespec-pcount fspec))))

;; drop-some-maybe: (listof filespec) int -> (values (listof filespec) int)
;; drop leading filespecs in the fringe that would be open-and-closed when skipping over skip
;; returning two values: remaining list-of filespecs, and int, the number of positions dropped in filespecs
;; so that the caller can decide how many positions still need to be passed-over to make the given skip count
;; **** filespecs must be non-zero length
(define (drop-some-maybe lofspec skip)
  ;(printf "drop-some-maybe: entering with skip=~a~%" skip)
  (do ([i skip (- i (filespec-pcount (car lof)))]
       [dropped 0 (+ dropped (filespec-pcount (car lof)))]
       [lof lofspec (cdr lof)]
       )
    ((< i (filespec-pcount (car lof))) 
     ;(printf "drop-some-maybe: about to return two values lof=~a, and dropped=~a~%" lof dropped)
     (values lof dropped))))

;; -----------------------------------------------------------------------------------
;; --- BULK FRINGE READING/WRITING ---------------------------------------------------

;; bs->compressed-bs: bytes -> bytes
;; compress the given bytes -- currently just a stub
(define (bs->compressed-bs bs)
  bs)

;; write-pos: bytes output-port -> number
;; write the given bytestring to the output port after compressing, returning number of bytes written
(define (write-pos bs oprt)
  (write-bytes (bs->compressed-bs bs) oprt))

;; read-pos: input-port -> hc-position
;; read the proper number of bytes from the input-port and convert to hc-position
(define (read-compressed-bs->bs iprt)
  (read-bytes *num-pieces* iprt))

;; write-fringe-to-disk: (listof or vectorof hc-position) string -> int
;; writes the bytestring portions of the hc-positions from the given fringe (whether list or vector) into a file with given file-name.
;; return the number written, not counting duplicates if remove-dupes is non-false
(define (write-fringe-to-disk fringe file-name [how-many -1] [remove-dupes #f])
  (let ([my-output (open-output-file file-name #:exists 'replace)]
        [stop-at (if (negative? how-many)
                     (or (and (vector? fringe) (vector-length fringe))
                         (length fringe))
                     how-many)]
        [last-pos #"NoLastPos"]
        [num-written 0])
    (for ([i (in-range stop-at)]
          [hcposition (in-vector fringe)])
      (let ([hc-bs (hc-position-bs hcposition)])
        (cond [remove-dupes
               (unless (bytes=? hc-bs last-pos)
                 (set! last-pos hc-bs)
                 (write-bs->file hc-bs my-output)
                 (set! num-written (add1 num-written)))]
              [else (write-bs->file hc-bs my-output)
                    (set! num-written (add1 num-written))])))
    (close-output-port my-output)
    num-written))

;; read-fringe-from-file: string -> (listof position)
;; reads a file from a file path (if you are in the current directory just simply the file-name)
;; and returns the fringe made up of hc-positions that was in that file.
(define (read-fringe-from-file file-name)
  (let* ([iport (open-input-file file-name)]
         [the-fringe (port->list read-bs->hcpos
                                 iport)])
    (close-input-port iport)
    the-fringe))


;; -----------------------------------------------------------------------------------
;; --- MISC UTILITIES ----------------------------------------------------------------

;; write-bs->file: byte-string [output-port] [number] -> void or error
;; writes the bs to the ofile and checks to make sure the optionally-specified number of bytes were written
(define (write-bs->file bspos [oprt (current-output-port)] [num-bytes *num-pieces*])
  (unless (= (write-bytes bspos oprt) num-bytes)
    (error (string-append "write-bs->file: failed to write an exact position: "
                          (bytes->string/utf-8 (if (= num-bytes *num-pieces*) bspos (subbytes bspos 1)))))))

;; read-bs->hcpos: input-port [number] -> hc-position
;; read a bytestring from the given input-port and create hc-position
(define (read-bs->hcpos in [num-bytes *num-pieces*])
  (let ([bspos (read-bytes num-bytes in)
               ;(read-compressed-bs->bs in)
               ])
    (if (eof-object? bspos) bspos (make-hcpos bspos))))

;; fringe-exists?: fringe -> boolean
;; report if all the files in the fringe are present (for now, ignore the sizes)
(define (fringe-exists? f)
  (for/and ([segment (fringe-segments f)])
    (file-exists? (filespec-fullpathname segment))))

;; rebase-fringe: fringe string -> fringe
;; replace the fbase for all segments (and the fringe itself)
(define (rebase-fringe f newbase)
  (make-fringe newbase
               (for/list ([fspec (fringe-segments f)])
                 (rebase-filespec fspec newbase))
               (fringe-pcount f)))

;; copy-fringe: fringe string -> fringe
;; copy the files in the given fringe to the target, returning a new fringe
(define (copy-fringe f target)
  (make-fringe target
               (for/list ([fspec (in-list (fringe-segments f))])
                 (let ([remote-name  (string-append target (filespec-fullpathname fspec))])
                   (unless (file-exists? remote-name)
                     (copy-file (filespec-fullpathname fspec) remote-name))
                   (rebase-filespec fspec target)))
               (fringe-pcount f)))
                 
;; resegment-fringe: fringe number string -> symbol
;; given a fringe, redistribute it over the given number of segments
;; CURRENTLY only a utility function that can be used prior to initiating a run
(define (resegment-fringe f n [new-fringe-folder-name "newfringe/"])
  (unless (directory-exists? (string-append *share-store* new-fringe-folder-name))
    (make-directory (string-append *share-store* new-fringe-folder-name)))
  (let* (;; current segment boundaries
         (current-slice-bounds (compute-segment-bounds (length (fringe-segments f))))
         ;; determine new segment boundaries based on n
         (new-slice-bounds (compute-segment-bounds n))
         ;; create the fringehead for the given fringe
         (fh (fh-from-fringe f))
         ;; segment-base-name
         (seg-base (substring (filespec-fname (car (fringe-segments f)))
                              0 (- (string-length (filespec-fname (car (fringe-segments f)))) 3)))
         ;; vector of file-names (eventual filespecs) for creating the new fringe
         (fnames (make-vector n))
         )
    ;; go through each position and switch output files when needed
    (do ([i 0] ;; output-segment currently written
         [fsout (open-output-file (string-append *share-store* new-fringe-folder-name seg-base (~a 0 #:left-pad-string "0" #:width 3 #:align 'right))
                                  #:exists 'replace)] ;; current output-port
         [fspecs null] ;; build the list of filespecs
         [pos (fringehead-next fh) (advance-fhead! fh)])
      ((fhdone? fh) 
       (close-output-port fsout)
       (vector-set! fnames i (string-append new-fringe-folder-name seg-base (~a i #:left-pad-string "0" #:width 3 #:align 'right)))
       fnames)
      (when (>= (hc-position-hc pos) (vector-ref new-slice-bounds (add1 i)))
        ;; close current output
        (close-output-port fsout)
        ;; create filespec
        (vector-set! fnames i (string-append new-fringe-folder-name seg-base (~a i #:left-pad-string "0" #:width 3 #:align 'right)))
        ;; advance counter
        (set! i (add1 i))
        ;; open next output
        (set! fsout (open-output-file (string-append *share-store* new-fringe-folder-name seg-base (~a i #:left-pad-string "0" #:width 3 #:align 'right))
                                      #:exists 'replace))
        )
      (write-bytes (hc-position-bs pos) fsout)
      )))

;; compute-segment-bounds: number -> (vectorof number)
;; determine the segment bounds for the given number of slices
(define (compute-segment-bounds n)
  (let* ([slice-width (floor (/ (- *most-positive-fixnum* *most-negative-fixnum*) n))]
         [slices (for/vector #:length (add1 n)
                   ([i n])
                   (+ *most-negative-fixnum* (* i slice-width)))])
    (vector-set! slices n (add1 *most-positive-fixnum*))
    slices))


;; delete-fringe: fringe -> void
;; remove all the files that make up the given fringe
(define (delete-fringe f [fbase (fringe-fbase f)])
  (for ([seg (in-list (fringe-segments f))]
        #:when (file-exists? (string-append fbase (filespec-fname seg))))
    (delete-file (string-append fbase (filespec-fname seg)))))

;; fringe-file-not-ready?: string string int [check-alt-flag #f] -> boolean
;; determine if the single given file exists on disk and has the appropriate size
;; with optional check-alt-flag will look on the nfs share and copy if found
(define (fringe-file-not-ready? basepath fname fsize [check-alt-flag #f])
  (let ([fullname (string-append basepath fname)])
    (when (and check-alt-flag
               (not (file-exists? fullname))
               (file-exists? fname) ;; check working (shared) directory
               (not (string=? fullname fname)))
      (copy-file fname fullname)) ;; YUCK!
    (or (not (file-exists? fullname))
        (< (file-size fullname) fsize))))
      

;; wait-for-files: (listof fspec) [check-alt-flag #f] -> 'ready
;; given a list of fringe-specs, wait until the file is present in the specified location
;; with the specified size.  if check-alt-flag is true, then drop the fbase and see if the file is available via NFS (copy if so!)
;;******* CAUTION: not yet reflecting intent to move wait closer to seeking next position
(define (wait-for-files lo-fspecs [check-alt-flag #f])
  (do ([fspecs (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) lo-fspecs)
               (filter (lambda (fspec) (fringe-file-not-ready? fspec check-alt-flag)) fspecs)]
       [sleep-time 0.01 (* sleep-time 2)])
    ((null? fspecs) 'ready)
    (printf "wait-for-files: waiting for ~a files such as ~a ... and sleeping ~a~%" (length fspecs) (car (fringe-fullpathnames (car fspecs))) sleep-time)
    (sleep sleep-time)))

;; position-count-in-file: string -> number
;; reports the number of positions in the given fringe file assuming the file was written with write-fringe-to-disk
(define (position-count-in-file f)
  (/ (file-size f) *num-pieces*))

;; check-sorted-fringe?: string -> boolean
;; assuming the string, f, points to a sorted file of positions, check to make sure they are sorted
(define (check-sorted-fringe? f)
  (let* ([myin (open-input-file f)]
         [prevpos (read-bs->hcpos myin)]
         [bool-res (for/and ([pos (in-port read-bs->hcpos myin)])
                     (let ([res (hcposition<? prevpos pos)])
                       (set! prevpos pos)
                       res))])
    (close-input-port myin)
    bool-res))

;; touch: string -> void
;; create the file with given name
(define (touch fname) (display-to-file "" fname))


;; make-fringe-from-files: string int -> fringe
;; given a base-string representing the fringe-segment file name -- except for the segment number --
;; and given the number of processors (actually, segments), and optionally the path to these fringe segments (expected to be *share-store*),
;; create the fringe structure for these segments
(define (make-fringe-from-files base-string n-seg [path-to-fringe-segments ""])
  (let ([pcount 0])
    (make-fringe 
     path-to-fringe-segments ;;*share-store*
     (for/list ([i (in-range n-seg)])
       (let* ([f (format "~a~a" base-string (~a i #:left-pad-string "0" #:width 3 #:align 'right))]
              [lpcount (position-count-in-file (string-append path-to-fringe-segments f))])
         (set! pcount (+ pcount lpcount))
         (make-filespec f lpcount (file-size (string-append path-to-fringe-segments f)) path-to-fringe-segments)))
     pcount)))

;; make-fringe-from-file: string [string] -> fringe
;; when a fringe is stored in a single file (instead of being spread over a number of segments),
;; create and return the fringe structure consisting of the given file, found in the optional path (expected to be *share-store*)
(define (make-fringe-from-file file [path-to-fringefile ""])
  (let ([filepcount (position-count-in-file file)])
    (make-fringe path-to-fringefile
                 (list (make-filespec file filepcount (file-size file) path-to-fringefile))
                 filepcount)))

