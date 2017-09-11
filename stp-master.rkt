#lang racket/base

(require racket/place/distributed
         racket/place
         racket/runtime-path
         racket/list
         racket/set
         racket/vector
         racket/system
         "stpconfigs/configenv.rkt"
         "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt"
         "stp-solve-cluster.rkt"
         )

(define *local-found-goal* #f)
(define *diy-threshold* 50000) ;;**** this must be significantly less than EXPAND-SPACE-SIZE 
(define *level-start-time* 0)
(define *worker-nodes* null)
(define *workers* (make-vector (* (length *worker-hosts*) *workers-per-host*) null)) ;; list-of worker structs
(define-runtime-path *worker-path* "stp-solve-cluster.rkt")


;; expand-fringe-self: fringe fringe int -> fringe
;; within the master process, expand the current-fringe and remove duplicates in the expansion and repeats from prev-fringe
;; returning the new fringe
(define (expand-fringe-self pf cf depth)
  (let* ([prev-fringe-set (for/fold ([the-fringe (set)])
                            ([sgmnt (fringe-segments pf)])
                            (set-union the-fringe
                                       (list->set (read-fringe-from-file (filespec-fullpathname sgmnt)))))] ; pf- and cf-spec's in expand-fringe-self should have empty fbase
         [current-fringe-vec 
          (list->vector (for/fold ([the-fringe empty])
                          ([sgmnt (reverse (fringe-segments cf))])
                          (append (read-fringe-from-file (filespec-fullpathname sgmnt)) the-fringe)))]
         [new-cf-name (format "fringe-d~a" depth)]
         [new-cf-fullpath (format "~a~a" *share-store* new-cf-name)]
         ;[prntmsg (printf "finished reading the fringes~%")]
         [exp-ptr 0]
         [expand-them (for ([p-to-expand current-fringe-vec])
                        (set! exp-ptr (expand* p-to-expand exp-ptr)))]
         [res (set->list (for/set ([i exp-ptr]
                                   #:unless (or (set-member? prev-fringe-set (vector-ref *expansion-space* i))
                                                (position-in-vec? current-fringe-vec (vector-ref *expansion-space* i))))
                           (when (is-goal? (vector-ref *expansion-space* i)) (set! *local-found-goal* (vector-ref *expansion-space* i)))
                           (vector-ref *expansion-space* i)))]
         )
    #|(printf "Finished the work packet generating a set of ~a positions~%" (set-count res))
    (for ([p res])
      (printf "pos: ~a~%~a~%" (stringify p) p))|#
    (unless *preserve-prior-fringes*
      (for ([sgmnt (fringe-segments pf)]) (delete-file (filespec-fullpathname sgmnt))))
    (write-fringe-to-disk (list->vector (sort res hcposition<?)) new-cf-fullpath)
    (make-fringe *share-store*
                 (list (make-filespec new-cf-name (length res) (file-size new-cf-fullpath) *share-store*))
                 (length res))))


;; expand-fringe: fringe fringe int -> fringe
;; Given the prev- and current-fringes, and the current depth of search,
;; do the expansions and merges as appropriate, returning the new fringe
(define (expand-fringe prev-fringe current-fringe depth)
  (if (< (fringe-pcount current-fringe) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self prev-fringe current-fringe depth)
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe prev-fringe current-fringe depth *workers*)))


;; cfs-file: fringe fringe int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using given previous and current fringes
(define (cfs-file prev-fringe current-fringe depth)
  (set! *level-start-time* (current-seconds))
  (cond [(or (zero? (fringe-pcount current-fringe)) (> depth *max-depth*)) #f]
        [(or *local-found-goal* *found-goal*)
         (print "found goal")
         (if *local-found-goal*
             *local-found-goal*
             *found-goal*)]
        [else (let ([new-fringe (expand-fringe prev-fringe current-fringe depth)])
                (printf "At depth ~a: current-fringe has ~a positions (and new-fringe ~a) in ~a (~a)~%" 
                        depth (fringe-pcount current-fringe) (fringe-pcount new-fringe)
                        (- (current-seconds) *level-start-time*) (seconds->time (- (current-seconds) *level-start-time*)))
                (flush-output)
                ;;(for ([p current-fringe]) (displayln p))
                (cfs-file current-fringe ;; use current-fringe as prev-fringe at next level
                          new-fringe
                          (add1 depth)))]))

;; start-distributed-search: hc-position -> ...
(define (start-distributed-search start-position)
  ;; initialization of fringe files
  (let ([d-1 (format "~afringe-d-1" *share-store*)]
        [d0 (format "~afringe-d0" *share-store*)])
    (for ([f (directory-list *share-store*)] #:when (regexp-match "^fringe-" (path->string f)))
      (delete-file (build-path *share-store* f)))
    (write-fringe-to-disk (vector) d-1)
    (write-fringe-to-disk (vector start-position) d0)
    (cfs-file (make-fringe *share-store* (list (make-filespec "fringe-d-1" 0 (file-size d-1) *share-store*)) 0)
              (make-fringe *share-store* (list (make-filespec "fringe-d0" 1 (file-size d0) *share-store*)) 1)
              1)))

;(compile-ms-array! *piece-types* *bh* *bw*)
;(compile-spaceindex (format "~a-spaceindex.rkt" *puzzle-name*))

;; canonicalize the *start* blank-configuration
(let* ([spacelist (bwrep->list (intify (hc-position-bs *start*) 0 4))]
       [cbref (rcpair->rcbyte (loc-to-cell (car spacelist)))]
       [canonical-spaces (apply canonize spacelist)])
  (bytes-set! (hc-position-bs *start*) 0 cbref)
  (bytes-copy! (hc-position-bs *start*) 1 canonical-spaces)
  (hc-position-bs *start*))


;; init-worker: number remote-node -> worker-place
;; run on worker -- just cause the workers to load the spaceindex
(define (init-worker i node)
  (printf "init-worker: about to try to create a worker-place~%")
  (let ([a-worker-place (supervise-place-at node *worker-path* 'make-stp-worker)])
    (printf "  have the worker, now call the init function on it~%")
    (stp-worker-init a-worker-place i)
    (printf "Initialized worker ~a as reported by worker purporting to be ~a~%" i (stp-worker-getid a-worker-place))
    a-worker-place))

;; init-workers!: -> (void)
;; initiate the remote-nodes and places, get the workers to load the spaceindex, etc.
;; first try hard-coding four workers on localhost 
(define (init-workers!)
  (printf "init-workers!: about to spawn-remote-racket-node~%")
  (set! *worker-nodes* (for/list ([host *worker-hosts*])
                         (spawn-remote-racket-node host #:listen-port 6344)
                         ;(create-place-node host #:listen-port 6344)
                         ))
  (printf "init-workers!: done spawn; init-worker~%")
  (for ([wrk-at-host *workers-per-host*])
    (for ([node *worker-nodes*]
          [host *worker-hosts*]
          [idbase (in-range (* wrk-at-host (length *worker-hosts*))
                            (* (add1 wrk-at-host) (length *worker-hosts*)))])
      (let ([wplace (init-worker idbase node)])
        (vector-set! *workers* idbase
                     (worker host idbase wplace))
                     )))
  (for ([w *workers*])
    (printf "Worker ~a at host ~a~%" (worker-id w) (worker-host w)))
  )


(module+ main
  ;(set! WORKERS (init-workers))
  (logdebug "main: about to call init-workers!")
  (init-workers!)
  ;(printf "workers: ~s~%workers-w/out-places: ~s~%" *workers* (vector-map strip-place *workers*))
  (define search-result (time (start-distributed-search *start*)))
  #|
  (define search-result (time (cfs-file (make-fringe-from-files "fringe-segment-d142-" 12 "/space/bigspace/fringefiles/")
                                        (make-fringe-from-files "fringe-segment-d143-" 12 "/space/bigspace/fringefiles/")
                                        144)))
  |#
  #|
  (define search-result (time (cfs-file (make-fringe-from-files "fringe-segment-d29-" 4 "fringefiles/")
                                        (make-fringe-from-files "fringe-segment-d30-" 4 "fringefiles/")
                                        31)))
  |#
  #|
  (define search-result (time (cfs-file (make-fringe-from-file "c12d59fringe" "fill-in-path-to-fringe-file")
                                        (make-fringe-from-file "c12d58fringe" "fill-in-path-to-fringe-file")
                                        1)))
  |#
  (print search-result)
  ;(for ([w *workers*]) (stp-worker-close-log (worker-place w)))
  (for ([wn *worker-nodes*]) (node-send-exit wn))
  )

;(time (start-distributed-search *start*))

