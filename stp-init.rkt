#lang typed/racket/base

(require math/array
         racket/list
         ;racket/set
         racket/vector
         )

(require/typed racket/set
               [list->set (-> (Listof Any) (Setof Any))])

(require "stpconfigs/configenv.rkt"
         "stp-datatypes.rkt"
         "stp-puzzle-data.rkt")

(provide EXPAND-SPACE-SIZE
         (struct-out hc-position)
         ;hc-position hc-position-hc hc-position-bs hc-position? set-hc-position-hc!
         make-hcpos
         *num-prim-move-translations*
         *prim-move-translations*
         *charify-offset*
         *piece-types* get-*piece-types*
         *num-pieces* get-*num-pieces*
         *bs-ptype-index* get-*bs-ptype-index*
         *target* get-*target*
         *bw* get-*bw*
         *bh* get-*bh*
         *bsz* get-*bsz*
         ;*expansion-space* get-*expansion-space*
         *start* get-*start*
         *piece-type-template* get-*piece-type-template*
         *num-spaces* get-*num-spaces*
         *piecelocvec* get-*piecelocvec*
         charify-int 
         intify
         ;old-positionify ;** temp for testing
         list->bwrep ;; used only during initialization in compile-ms-array! via better-move-schema
         bwrep-direct
         bwrep->list
         ;bwrep->list
         cell-to-loc
         loc-to-cell
         invalid-cell?
         ;register-loc-to-pair
         ;locs->rcbyte
         ;rcpair->rcbyte
         ;rcbyte->rcpair
         ;rc+
         ;rc-
         block10-init
         climb12-init
         ;climb15-init
         ;climbpro24-init
         )

(define: (get-*piece-types*) : (Vectorof (Setof CellRef)) *piece-types*)
(define: (get-*num-pieces*) : Integer *num-pieces*)
(define: (get-*bs-ptype-index*) : (Vectorof Byte) *bs-ptype-index*)
(define: (get-*target*) : (Pairof Byte Byte) *target*)
(define: (get-*bw*) : Byte *bw*)
(define: (get-*bh*) : Byte *bh*)
(define: (get-*bsz*) : Byte *bsz*)
;(define: (get-*expansion-space*) : (Vectorof hc-position) *expansion-space*)
(define: (get-*start*) : hc-position  *start*)
(define: (get-*piece-type-template*) : (Vectorof Byte) *piece-type-template*)
(define: (get-*num-spaces*) : Byte *num-spaces*)
(define: (get-*piecelocvec*) : (Vectorof Boolean) *piecelocvec*)


;; INITIALIZE STUFF FOR SLIDING-TILE-SOLVER

(define EXPAND-SPACE-SIZE 500000)
;(define EXPAND-SPACE-SIZE 2000000)

;; move trans for up, right, down and left respectively
(define: *num-prim-move-translations* : Integer 4)
(define: *prim-move-translations* : (Listof (Pairof Fixnum Fixnum)) '((-1 . 0) (0 . 1) (1 . 0) (0 . -1)))
(define: *charify-offset* : Byte 48)
(define: *max-board-size* : Byte 64)

;; puzzle specific parameters
(define: *invalid-cells* : (Listof Cell) empty)
(define: *num-piece-types* : Byte 0)
(define: *piece-types* : (Vectorof (Setof CellRef)) (vector))
(define: *num-pieces* : Byte 0)
(define: *start* : hc-position (hc-position 0 #"uninitialized"))
(define: *piece-type-template* : (Vectorof Byte) (vector)) ; for each piece-type index, stores how many blocks of that type there are
(define: *num-spaces* : Byte 0)
(define: *bs-ptype-index* : (Vectorof Byte) (vector)) ;; for a byte's index in a position, store the byte's piece-type
(define: *target* : (Pairof Byte Byte) (cons 0 0))
(define: *bw* : Byte 0)
(define: *bh* : Byte 0)
(define: *bsz* : Byte 0)
;(define: *expansion-space* : (Vectorof hc-position) (vector))
;(define *bsbuffer* #"") ;; a reusable buffer for holding expansions of a given position
(define: *cell-to-loc* : (Array (U Byte False)) (array 0 : (U Byte False)))
(define: *loc-to-cell* : (Vectorof Cell) (vector))
(define: *piecelocvec* : (Vectorof Boolean) (make-vector 42 #f));; vector boolean representing used move locations where the index is the location to which a single piece was moved
;; a vector of mutable pairs holding piece-type and location
;;(define: *expandbuf* : (Vectorof (MPairof Byte Bytes)) (vector (mcons 1 #"0")))



;; init-all!: piece-type-vector prepos target N N (listof (N . N)) string -> void
;; generic setter for use by puzzle-specific initialization functions
(define: (init-all! [ptv : (Vectorof (Setof CellRef))] [s : prepos] [t : TileSpec] [nrow : Byte] [ncol : Byte] [invalid : (Listof Cell)]) : Void
  (set! *bh* nrow)
  (set! *bw* ncol)
  (set! *bsz* (assert (- (* nrow ncol) (length invalid)) byte?))
  (init-cell-loc-maps! nrow ncol invalid)
  (set! *num-piece-types* (assert (vector-length ptv) byte?)) ;; including spaces -- must come before bw-positionify/(pre-compress)
  (set! *piecelocvec* (ann (make-vector *bsz* #f) (Vectorof Boolean)))
  (set! *piece-types* ptv)
  ;(set! *piece-types* (ann (vector-map list->set ptv) (Vectorof (Setof CellRef))));****
  #|(set! *piece-types* (for/vector: : (Vectorof (Setof Any)) ([cell-specs : (Listof CellRef) ptv])
                        (list->set cell-specs)))|#
  (set! *invalid-cells* invalid)
  (set! *num-pieces* (assert (+ (length (prepos-tspecs s)) (length (prepos-spaces s))) byte?)) ;; includes spaces -- may be used as length of position bytestring instead of bytes-length
  (set! *start* (make-hcpos (charify (bw-positionify (pre-compress s)))))
  (set! *piece-type-template* (for/vector: : (Vectorof Byte)
                                ([pt : (Listof Loc) (old-positionify (bw-positionify (pre-compress s)))])
                                (assert (length pt) byte?)))
  (set! *num-spaces* (vector-ref *piece-type-template* 0))
  ;;(set! *expansion-space* (build-vector (+ EXPAND-SPACE-SIZE *bsz*) (lambda (_) (hc-position 0 (make-bytes *num-pieces*))))) ;(Vectorof hc-position)))
  #|
  (set! *expandbuf* (cast (build-vector (* (vector-ref *piece-type-template* 0) *num-pieces*)
                                        (lambda (_) (cast (mcons 0 (make-bytes *num-pieces*)) (MPairof Byte Bytes)))) 
                          (Vectorof (MPairof Byte Bytes))))
|#
  (set! *bs-ptype-index*
        (ann
         (list->vector
          (for/fold: : (Listof Byte) ([res : (Listof Byte) '()])
            ([ptype-count : Byte (in-vector *piece-type-template*)]
             [index : Byte *num-piece-types*])
            (append res (for/list: : (Listof Byte) ([i : Byte ptype-count]) index))))
         (Vectorof Byte)))
        
  ;; should set *target* to a bytestring index and an expected location for that indexed value
  ;;******** this only works for a single goal-spec for a tile-type with only one instance, but ....
  (set! *target* (cons (assert (for/sum: : Integer ([ntypes : Byte (in-vector *piece-type-template*)]
                                                    [i : Byte (car t)])
                                 (assert ntypes byte?)) byte?)
                       (assert (+ (cell-to-loc (cdr t)) *charify-offset*) byte?)))
  )

;;---------------------------------------------------------------------------------
;; cells are (row . col) pairs; locations are semi-row-major index

;; init-cell-loc-maps!: N N (listof (N . N)) -> void
;; called only at initialization: init the cell-to-loc and loc-to-cell arrays
(define: (init-cell-loc-maps! [nrow : Byte] [ncol : Byte] [invalid : (Listof Cell)]) : Void
  (set! *loc-to-cell* (for*/vector: : (Vectorof Cell) ([r : Byte nrow][c : Byte ncol] #:unless (member (cons r c) invalid))
                        (cons r c)))
  (let ((invalid-skipped 0))
    (set! *cell-to-loc* (for/array: #:shape (vector nrow ncol) ([i (* *bh* *bw*)]) : (U Byte False)
                                    (if (member (slow-loc-to-cell (assert i byte?)) invalid)
                                        (begin (set! invalid-skipped (add1 invalid-skipped))
                                               #f)
                                        (assert (- i invalid-skipped) byte?))))))

;; cell-to-loc: cell -> int
;; convert ordered pair to row-major-order rank location
(define: (cell-to-loc [pair : Cell]) : Loc
  (let: ([maybe-loc : (U Byte False) (array-ref *cell-to-loc* (vector (car pair) (cdr pair)))])
    (if (boolean? maybe-loc)
        (begin (printf "given cell: ~a~%and maybe-loc: ~a~%" pair maybe-loc)
               (error 'cell-to-loc "attempt to access loc for invalid cell"))
        maybe-loc)))

;; slow version of loc-to-cell for use during population of *cell-to-loc*
(define: (slow-loc-to-cell (i : Byte)) : Cell
  (cons (assert (quotient i *bw*) byte?)
        (remainder i *bw*)))

;; loc-to-cell: int -> cell
(define: (loc-to-cell [i : Loc]) : Cell
  (vector-ref *loc-to-cell* i))

(define: (invalid-cell? [c : CellRef]) : Boolean
  (cons? (member c *invalid-cells*)))


;;--------------------------------------------------------------------------------

;; charify: bw-position -> bytestring
;; convert a bitwise represented position into a series of bytes
(define: (charify [bw-p : (Vectorof Integer)]) : Bytes
  (for/fold: : Bytes ([res : Bytes #""])
    ([pt (in-vector bw-p)])
    (bytes-append res (charify-int pt))))

;; charify-int: int -> bytestring
;; convert a single int to a bytestring rep of each 1 appearing in the int's binary representation
;; that is, the resulting bytestring will be as long as the number of 1's in the given int
(define: (charify-int [i : Integer]) : Bytes
  (for/fold: : Bytes ([res #""])
    ([b (integer-length i)]
     #:when (bitwise-bit-set? i b))
    (bytes-append res (bytes (assert (+ b *charify-offset*) byte?)))))

;; intify: bytestring [int] [int] -> int
;; convert a given series of bytes to a bitwise overlay of their corresponding positions
#|(define (intify bs (start 0) (end (bytes-length bs)))
  (for/fold ([newnum 0])
    ([ploc bs])
    (+ newnum (arithmetic-shift 1 (- ploc *charify-offset*)))))|#
(: intify (->* (Bytes) (Byte Byte) Integer))
(define (intify bs (start 0) (end *num-pieces*))
  (for/sum ([pref (in-range start end)])
    (arithmetic-shift 1 (- (bytes-ref bs pref) *charify-offset*))))

;; bw-positionify: (listof (cons tile-id (listof cell))) -> bw-position
;; create a bitwise-'position' representation of a board state based on the given start-list pre-position format
(define: (bw-positionify [old-position : (Listof (Pairof Byte (Listof Cell)))]) : BW-Position
  (for/vector: : BW-Position ([pspec : (Pairof Byte (Listof Cell)) (in-list old-position)]
                              [i : Byte *num-piece-types*])
    (unless (= i (car pspec)) (error 'positionify "mis-matched piece-type in vector representation of position"))
    (list->bwrep (map cell-to-loc (ann (cdr pspec) (Listof Cell))))))

;; old-positionify: bw-position -> old-position
(define: (old-positionify [bw-position : BW-Position]) : (Vectorof (Listof Loc))
  (for/vector: : (Vectorof (Listof Loc)) ([bwrep : Integer bw-position])
    (bwrep->list (assert bwrep positive?))))

;; list->bwrep: (listof loc) -> int
;; convert the list of locations into a bitwise representation
(define: (list->bwrep [lo-loc : (Listof Loc)]) : Integer
  #|(foldl (lambda (a-loc bwint)
           (+ (arithmetic-shift 1 a-loc) bwint))
         0
         lo-loc)|#
  (for/sum ([a-loc (in-list lo-loc)]) (arithmetic-shift 1 a-loc)))

;; bwrep-direct: N N N N -> fixnum
;; get the blank-int directly from the locations of the four blanks
(define: (bwrep-direct [b1 : Byte] [b2 : Byte] [b3 : Byte] [b4 : Byte]) : Integer
  (+ (arithmetic-shift 1 b1)
     (arithmetic-shift 1 b2)
     (arithmetic-shift 1 b3)
     (arithmetic-shift 1 b4)))


;; bwrep->list: int -> (listof loc)
;; extract the locs encoded in the given int
(define: (bwrep->list [n : Positive-Integer]) : (Listof Loc)
  (for/list ([i (in-range (integer-length n))]
             #:when (bitwise-bit-set? n i))
    (assert i byte?)))    


;; pre-compress: prepos -> (listof (cons tile-id (listof cell)))
;; collapse pieces of the same type and give spaces their unique id of -1
(define: (pre-compress [p : prepos]) : (Listof (Pairof Byte (Listof Cell)))
  (cons (ann (cons 0 (prepos-spaces p))
             (Pairof Byte (Listof Cell)))
        (for/list: : (Listof (Pairof Byte (Listof Cell))) ([i : Integer (in-range 1 (assert *num-piece-types* byte?))])
          (ann (cons (assert i byte?)
                     (for/list: : (Listof Cell) ([a-piece : tspec (prepos-tspecs p)]
                                                 #:when (= i (tspec-tiletype a-piece)))
                       (tspec-origin a-piece)))
               (Pairof Byte (Listof Cell))))))


;;------------------------------------------------------------------------------------------------------

(define (block10-init)
  (init-all! *block10-piece-types* *block10-start* *block10-target* 6 4 *block10-invalid-cells*))

(define (climb12-init)
  (init-all! *climb12-piece-types* *climb12-start* *climb12-target* 6 5 *climb12-invalid-cells*))
#|
(define (climb15-init)
  (init-all! *climb15-piece-types* *climb15-start* *climb15-target* 8 5 *climb15-invalid-cells*))

(define (climbpro24-init)
  (init-all! *climbpro24-piece-types* *climbpro24-start* *climbpro24-target* 10 7 *climbpro24-invalid-cells*))
|#
(case *puzzle-name*
  (("climb12") (climb12-init))
  ;(("climb15") (climb15-init))
  ;(("climbpro24") (climbpro24-init))
  (else (error 'stp-init "puzzle-name missing or unknown in stpconfigs/configenv.rkt")))
