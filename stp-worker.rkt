#lang racket

(require racket/match
         racket/place/define-remote-server
         "stpconfigs/configenv.rkt"
         "stp-init.rkt"
         "stp-solve-cluster.rkt")

(provide worker-main)

(define MYID -1)


  ;; initialize:
  ;; - set puzzle parameters
  ;; - load spaceindex
  ;; - determine this worker's identity
  ;; setup listener(s) for other nodes
  ;; wait for work ...

;  )

(define (expand-slice args)
  (let ([range-pair (first args)]
        [i (second args)]
        [pf (third args)]
        [cf (fourth args)]
        [depth (fifth args)])
    (remote-expand-part-fringe range-pair i pf cf depth)))

(define (merge-slices args)
  (let ([range (first args)]
        [expand-fspecs-slice (second args)]
        [depth (third args)]
        [ofile-name (fourth args)]
        [pf (fifth args)]
        [cf (sixth args)]
        [i (seventh args)])
    (distributed-merge-proto-fringe-slices range expand-fspecs-slice depth ofile-name pf cf i)))

(define (worker-main pch)
  (case (place-channel-get pch)
    [(init) (set! MYID (place-channel-get pch))]
    [(expand-slice) (place-channel-put pch (expand-slice (place-channel-get pch)))]
    [(merge-slices) (place-channel-put pch (merge-slices (place-channel-get pch)))]
    [else (error 'worker-main "unknown message")])
  (worker-main pch))