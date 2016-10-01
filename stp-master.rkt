#lang racket/base

;(require (planet gcr/riot))
(require racket/place
         "stpconfigs/configenv.rkt"
         "stp-init.rkt"
         "stp-solve-base.rkt"
         "stp-fringefilerep.rkt"
         "stp-spaceindex.rkt"
         "stp-solve-cluster.rkt"
         ;"stp-worker.rkt"
         )

(define *diy-threshold* 5000) ;;**** this must be significantly less than EXPAND-SPACE-SIZE 
(define *level-start-time* 0)
(define WORKERS null)

;; expand-fringe: fringe fringe int -> fringe
;; Given the prev- and current-fringes, and the current depth of search,
;; do the expansions and merges as appropriate, returning the new fringe
(define (expand-fringe prev-fringe current-fringe depth)
  (if (< (fringe-pcount current-fringe) *diy-threshold*)
      ;; do it myself
      (expand-fringe-self prev-fringe current-fringe depth)
      ;; else call distributed-expand, which will farm out to workers
      (distributed-expand-fringe prev-fringe current-fringe depth WORKERS)))


;; cfs-file: fringe fringe int -> position
;; perform a file-based cluster-fringe-search at given depth
;; using given previous and current fringes
(define (cfs-file prev-fringe current-fringe depth)
  (set! *level-start-time* (current-seconds))
  (cond [(or (zero? (fringe-pcount current-fringe)) (> depth *max-depth*)) #f]
        [*found-goal*
         (print "found goal")
         *found-goal*]
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
(compile-spaceindex (format "~a-spaceindex.rkt" *puzzle-name*))

;; canonicalize the *start* blank-configuration
(let* ([spacelist (bwrep->list (intify (hc-position-bs *start*) 0 4))]
       [cbref (rcpair->rcbyte (loc-to-cell (car spacelist)))]
       [canonical-spaces (apply canonize spacelist)])
  (bytes-set! (hc-position-bs *start*) 0 cbref)
  (bytes-copy! (hc-position-bs *start*) 1 canonical-spaces)
  (hc-position-bs *start*))


;; init-worker: number -> worker-place
;; run on worker -- just cause the workers to load the spaceindex
(define (init-worker i)
  (let ([a-worker-place (dynamic-place "stp-worker.rkt" 'worker-main)])
    (place-channel-put a-worker-place 'init)
    (place-channel-put a-worker-place i)
    a-worker-place))

;; init-workers: -> (listof worker-places)
;; initiate the remote-nodes and places, get the workers to load the spaceindex, etc.
;; first try hard-coding four workers on localhost 
(define (init-workers)
  (for/list ([i (in-range 4)])
    (init-worker i)))

(define (main)
  (set! WORKERS (init-workers))
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
  )

;(time (start-distributed-search *start*))

