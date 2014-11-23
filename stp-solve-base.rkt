#lang typed/racket/base

(require
 ;srfi/25 ;; multi-dimensional arrays
 math/array
 racket/list
 ;racket/set
 racket/vector
 "stpconfigs/configenv.rkt"
 "stp-datatypes.rkt"
 "stp-init.rkt"
 )
                  
#|
(provide hcposition<?
         blexi<?
         *ms-array*
         compile-ms-array!
         position-in-vec?
         expand
         is-goal?
         seconds->time)
|#
(provide (all-defined-out))

#| merge-conflict
;; a vector of mutable pairs holding piece-type and location
(define *expandbuf* (build-vector (* (vector-ref *piece-type-template* 0) *num-pieces*) (lambda (_) (mcons 0 (make-bytes *num-pieces*)))))
(define *piecelocvec* (make-vector *bsz* #f));; vector boolean representing used move locations where the index is the location to which a single piece was moved
|#

;; hcposition<?: hc-position hc-position -> boolean
(define: (hcposition<? [p1 : hc-position] [p2 : hc-position]) : Boolean
  (or (< (hc-position-hc p1) (hc-position-hc p2))
      (and (= (hc-position-hc p1) (hc-position-hc p2))
           (bytes<? (hc-position-bs p1) (hc-position-bs p2)))))

;; blexi<?: hc-position hc-position -> boolean
;; lexicographic fallback for hash collision
(define: (blexi<? [p1 : hc-position] [p2 : hc-position]) : Boolean
  (bytes<? (hc-position-bs p1) (hc-position-bs p2)))


;;-------------------------------------------------------------------------------
;; COMMON UTILITIES TO BOTH GENERIC FRINGE-SEARCH AND CLUSTER-FRINGE-SEARCH

;; Set-like Operations on Lists

;; list-subtract: (listof X) (listof X) (X X -> boolean) -> (listof X)
;; ASSUME lists are sorted
(: list-subtract : (All (A) (Listof A) (Listof A) ( A A -> Boolean ) -> (Listof A)))
(define (list-subtract l1 l2 comp?)
  (cond [(or (empty? l1) (empty? l2)) l1]
        [(equal? (first l1) (first l2)) (list-subtract (rest l1) l2 comp?)]
        [(comp? (first l1) (first l2)) (cons (first l1) (list-subtract (rest l1) l2 comp?))]
        [else (list-subtract l1 (rest l2) comp?)]))


;;------------------------------------------------------------------------------------------------------
;; Compiling move-schemas

;; a ms-array is an array indexed by piece-type, location, move-direction (0-3 starting with up)
;; where each location contains a move-schema produced by basic-move-schema


;; move-schema-array for compiling move requirements
;(define: *ms-array* : (Array (U BMS False)) (array #[#f] : (U BMS False)))
(define *ms-array*  (array #[#f] : (U BMS False)))
;(define *ms-array* (array #[#f]))

;; compile-ms-array!: (vectorof (setof cell)) int int -> void
;; where the vector is the piece-type specification, *piece-types*, and the ints are the width and height
#|
(define: (compile-ms-array! [piece-type-specs : (Vectorof (Setof Cell))] [bh : Byte] [bw : Byte]) : Void
  (when (or (zero? bh) (zero? bw)) (error 'compile-ms-array "must be called after an appropriate initialization call"))
  (let* ((bsz *bsz*)
         (a (make-array (list 'shape 1 (vector-length piece-type-specs) 0 bsz 0 4))))
    (for ([piece-type-spec (in-vector (vector-drop piece-type-specs 1))]
          [pti (in-range 1 (vector-length piece-type-specs))])
      (for ([loc bsz])
        (for ([dir (in-range *num-prim-move-translations*)]
              [dir-trans (in-list *prim-move-translations*)])
          (let* ([loc-cell (loc-to-cell loc)]
                 [start-spots (translate-piece piece-type-spec loc-cell)]
                 [moved-spots (translate-piece piece-type-spec (translate-cell loc-cell dir-trans))])
            (array-set! a pti loc dir
                        (if (and (andmap onboard? start-spots)
                                 (andmap onboard? moved-spots))
                            (better-move-schema (loc-to-cell loc) dir-trans start-spots moved-spots)
                            #f))))))
    (set! *ms-array* a)))
|#
(define: (compile-ms-array! [piece-type-specs : (Vectorof (Setof CellRef))] [bh : Byte] [bw : Byte]) : Void
  (when (or (zero? bh) (zero? bw)) (error 'compile-ms-array "must be called after an appropriate initialization call"))
  (let*: ((bsz : Byte *bsz*)
          (ds ((inst vector Index) (assert (sub1 (vector-length piece-type-specs)) byte?)
                                   (assert bsz byte?) 4))
          (a (for*/array: #:shape ds ([pt (in-range (sub1 (vector-length piece-type-specs)))]
                                      [loc (in-range bsz)]
                                      [dir (in-range 4)]) : (U BMS False) #f)))
    (for ([piece-type-spec (in-vector (vector-drop piece-type-specs 1))]
          [pti (in-range (sub1 (vector-length piece-type-specs)))])
      (for ([loc bsz])
        (for ([dir (in-range *num-prim-move-translations*)]
              [dir-trans (in-list *prim-move-translations*)])
          (let* ([loc-cell (loc-to-cell loc)]
                 [start-spots (translate-piece piece-type-spec loc-cell)]
                 [moved-spots (translate-piece piece-type-spec (translate-cell loc-cell dir-trans))])
            (array-set! a ((inst vector Index) (assert pti byte?) loc (assert dir byte?))
                        (if (and (andmap onboard? start-spots)
                                 (andmap onboard? moved-spots))
                            (better-move-schema (loc-to-cell loc) dir-trans (cast start-spots (Listof Cell)) (cast moved-spots (Listof Cell)))
                            #f))))))
    (set! *ms-array* a)))

;; translate-loc: loc trans-spec -> loc
#|(define: (translate-loc [l : Loc] [trans : Cell]) : Loc
  (cell-to-loc (translate-cell (loc-to-cell l) trans)))|#

;; translate-cell: cell trans-spec -> cell
;; given a trans-spec as (delta-row . delta-col) pair, return a new pair
(define: (translate-cell [c : CellRef] [trans : CellRef]) : CellRef
  (cons (assert (+ (car c) (car trans)) fixnum?)
        (assert (+ (cdr c) (cdr trans)) fixnum?)))

;; translate-piece: (listof cells) trans-spec -> (listof cells)
(define: (translate-piece [cell-list : (Setof CellRef)] [trans : CellRef]) : (Listof CellRef)
  (for/list ([cell cell-list])
    (translate-cell cell trans)))


;; better-move-schema: cell trans-spec (listof cell) (listof cell) -> (list int int int int)
;; a better-move-schema (better-ms) is a list:
;; first:   bit-rep of space preconditions
;; second:  xor of space preconditions and space postconditions ("changed-blanks": all that change)
;; third:   xor of current location and translated location (origin) of the piece
;; fourth:  new location of the moved tile's origin

(define: (better-move-schema [cell : Cell] [trans : CellRef] [start-cell-list : (Listof Cell)] [moved-cell-list : (Listof Cell)]) : BMS
  (let* ([current-loc-list (sort (map cell-to-loc start-cell-list) <)]
         [loc-list-to (sort (map cell-to-loc moved-cell-list) <)])
    (list (list->bwrep (list-subtract loc-list-to  current-loc-list <))
          (bitwise-xor (list->bwrep (list-subtract loc-list-to  current-loc-list <))
                       (list->bwrep (list-subtract current-loc-list loc-list-to <)))
          (list->bwrep (list (cell-to-loc cell)
                             (cell-to-loc (cast (translate-cell cell trans) Cell))))
          (cell-to-loc (cast (translate-cell cell trans) Cell)))))

;; position-in-vec?: (vectorof position) position -> boolean
;; determine if given position is in vector of positions
(define: (position-in-vec? [v : (Vectorof hc-position)] [p : hc-position]) : Boolean
  (vec-member? v p hcposition<?))

#|
;; find-pos-index: fixnum (vectorof position) [number] [number] -> int
;; find the index of the *FIRST* position (if present) or of the first position greater than where it would be
;; *** THIS IS NOT _EXACTLY_ CORRECT: assumes only used to pick responsibility-ranges
(: find-pos-index : (->* ( Fixnum (Vectorof hc-position) ) (Fixnum Fixnum) Integer))
(define (find-pos-index pos-hashcode vop [low 0] [high (vector-length vop)])
  (error "find-pos-index: cannot process hc-positions")
  (let* ([mid (quotient (+ low high) 2)]
         [mid-hashcode (and (< mid (vector-length vop)) (equal-hash-code (vector-ref vop mid)))])
    (cond [(>= low high) low]
          [(= pos-hashcode mid-hashcode)
           (or (for/last ([index (in-range mid -1 -1)]
                          #:when (= (equal-hash-code (vector-ref vop index)) mid-hashcode))
                 index)
               0)]
          [(< pos-hashcode mid-hashcode) (find-pos-index pos-hashcode vop low mid)]
          [else (find-pos-index pos-hashcode vop (add1 mid) high)])))
|#

;; vec-member?: (vectorof hc-position) hc-position (hc-position hc-position -> boolean) [int] [int] -> boolean
;; determine if the given item appears in the SORTED vector of positions
(: vec-member? : (->* ( (Vectorof hc-position) hc-position (-> hc-position hc-position Boolean) ) (Integer Integer) Boolean))
(define (vec-member? v x compare? [low 0] [high (vector-length v)])
  (let ([mid (quotient (+ low high) 2)])
    (cond [(>= low high) #f]
          [(= (hc-position-hc x) (hc-position-hc (vector-ref v mid))) #t]
          [(compare? x (vector-ref v mid)) (vec-member? v x compare? low mid)]
          [else (vec-member? v x compare? (add1 mid) high)])))

;; bw-valid-move?: number number -> boolean
;; determine if the current location of the spaces supports a move's prerequisites given as space-prereq
(define: (bw-valid-move? [space-int : Integer] [space-prereq : Integer]) : Boolean
  (= (bitwise-and space-int space-prereq)
     space-prereq))
  
;; onboard?: cell -> boolean
(define: (onboard? [c : CellRef]) : Boolean
  (and (< -1 (car c) *bh*)
       (< -1 (cdr c) *bw*)
       (not (invalid-cell? c))
       ;(number? (cell-to-loc (cast c Cell)))
       ))
;; loc-onboard?: loc -> boolean
(define: (loc-onboard? [loc : Loc]) : Boolean
  (< -1 loc *bsz*))

;;------------------------------------------------------------------------------------

;; is-goal?: hc-position -> boolean
;;****** relies on special-case of goal where single tile of type with only one tile needs to be in certain location
(define: (is-goal? [hcp : hc-position]) : Boolean
  ;(and #f
       (= (bytes-ref (hc-position-bs hcp) (car *target*))
          (cdr *target*))
  ;)
)

;; seconds->time: int -> string
;; format the given number of seconds as hours (if non-zero), minutes (if non-zero), and seconds
(define: (seconds->time [ts : Integer]) : String
  (let* ([hrs (floor (/ ts 3600))]
         [min (floor (/ (- ts (* hrs 3600)) 60))]
         [sec (- ts (* hrs 3600) (* min 60))])
    (cond [(and (zero? hrs) (zero? min)) (format "~a sec." sec)]
          [(and (zero? hrs) (positive? min)) (format "~a min., ~a sec." min sec)]
          [else (format "~a hrs., ~a min., ~a sec." hrs min sec)])))

;(compile-ms-array! *piece-types* *bh* *bw*)
;(expand (get-*start*))
