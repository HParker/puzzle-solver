#lang racket/base

(provide vector-sort!)


;; vector-sort!: (vectorof X) (X X -> boolean) [N] [N] -> void
;; sort the vector according to the compare between start [0] and end [vector-length]
(define (vector-sort! v compare [start 0] [end (vector-length v)])
  (letrec ([k (/ (log (vector-length v)) (* 2 (log 2)))]
           ;; qsort-aux : (vectorof X) (X X -> boolean) N N  ->  (vectorof number)
           ;; effect: sort the interval [left,right] (inclusive) of vector V
           [qsort-aux (lambda (left right)
                        (cond
                          ;[(>= left right) v]
                          [(< (- right left) k) (void)]
                          [else (let ([new-pivot-position (alt-partition left right (+ left (random (add1 (- right left)))))])
                                  (begin (qsort-aux left (sub1 new-pivot-position))
                                         (qsort-aux (add1 new-pivot-position) right)))]))]
           ;; alt-partition: (vectorof X) (X X -> boolean) N N N -> N
           ;; from wikipedia, but looks like the same pseudo-code from Cormen et al.
           [alt-partition (lambda (l r pividx)
                            (let ([pivot-value (vector-ref v pividx)]
                                  [store-index l])
                              (swap pividx r) ; move pivot-value to end of array
                              (for ([i (in-range l r)])
                                (when (compare (vector-ref v i) pivot-value)
                                  (swap i store-index)
                                  (set! store-index (add1 store-index))))
                              (swap store-index r)
                              store-index))]
           ;; partition : (vectorof X) (X X -> boolean) N N  ->  N
           ;; to determine the proper position p of the pivot-item 
           ;; effect: rearrange the vector V so that 
           ;; -- all items in V in [left,p) are "smaller" than the pivot item
           ;; -- all items of V in (p,right] are not-smaller than the pivot item
           ;; generative recursion
           #|
           [partition (lambda (left right)
                        (letrec ([pivot-position left]
                                 [the-pivot (vector-ref v left)]
                                 [partition-aux 
                                  (lambda (left right)
                                    (let ((new-right (find-new-right v compare the-pivot left right))
                                          (new-left (find-new-left v compare the-pivot left right)))
                                      (cond [(>= new-left new-right)
                                             (swap v pivot-position new-right)
                                             new-right]
                                            [else ; (< new-left new-right)
                                             (swap v new-left new-right)
                                             (partition-aux new-left new-right)])))])
                          (partition-aux left right)))]
           |#
           ;; swap : N N -> void 
           [swap (lambda (i j)
                   (let ((temp (vector-ref v i)))
                     (vector-set! v i (vector-ref v j))
                     (vector-set! v j temp)))]
           )
    (qsort-aux start (sub1 end))
    (insertion-sort v compare)))


;; After looking in the rnrs/sorting-6 code for vector-sort!, which uses vector->list, list->vector, and vector-copy!,
;; decided to compare performance to the in-place qsort provided in HtDP




;; find-new-side : (vectorof X) (X X -> boolean) number N N [>= left]  ->  N
;; to determine an index i between left and right (inclusive)
;; such that (compare (vector-ref V i) the-pivot) holds on range [i+1,right]
#|
(define (find-new-right V compare the-pivot left right)
  (cond
    [(= right left) right]
    [else (cond
	    [(compare (vector-ref V right) the-pivot) right]
	    [else (find-new-right V compare the-pivot left (sub1 right))])]))

(define (find-new-left V compare the-pivot left right)
  (cond
    [(= right left) left]
    [else (cond
            [(not (compare (vector-ref V left) the-pivot)) left]
            [else (find-new-left V compare the-pivot (add1 left) right)])]))
|#

;; Insertion Sort

;; insertion-sort : (vectorof X) (X X -> boolean) -> void
;; for use in sorting a "nearly" sorted vector from quicksort with relaxed stopping criterion
(define (insertion-sort v compare)
  (letrec ([vl (vector-length v)]
          ;; everything from i to vector-length is now sorted
          [isrt (lambda (i)
                  (cond [(zero? i) (void)]
                        [else (insert i (vector-ref v (sub1 i)))
                              (isrt (sub1 i))]))]
          ;; insert j into j+1 to vector-length (which is completely sorted)
          [insert (lambda (j val)
                    (cond [(or (= j vl) 
                               (compare val (vector-ref v j)))
                           (vector-set! v (sub1 j) val)]
                          [else (vector-set! v (sub1 j) (vector-ref v j))
                                (insert (add1 j) val)]))])
    (isrt vl)))
                                           


(define myv1 (vector 5 14 11 14 8 0 2 3 7 7))
(define myv2 (vector #\x #\u #\d #\m #\a #\k))
(set! myv1 myv1)

(vector-sort! myv1 <)
;(insertion-sort myv1 <)
myv1