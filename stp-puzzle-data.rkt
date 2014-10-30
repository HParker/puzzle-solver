#lang typed/racket/base

(require racket/set 
         "stp-datatypes.rkt")

(provide (all-defined-out))


;; INITIALIZE STUFF FOR SLIDING-TILE-SOLVER

;;------------------------------------------------------------------------------------------------------
;; BLOCK-10 PUZZLE INIT (variant 12)
(define: *block10-piece-types* : (Vectorof (Setof CellRef))
  (vector (ann (set) (Setof CellRef))     ; 0 reserved for spaces in actual position representation
          (set '(0 . 0)'(0 . 1)'(1 . 0)'(1 . 1))  ; 1  2x2
          (set '(0 . 0)'(0 . 1)'(1 . 0))         ; 2  Upper Left pointing L
          (set '(0 . 0)'(1 . -1)'(1 . 0))        ; 3  Lower Right pointing L
          (set '(0 . 0)'(1 . 0))                ; 4  2x1 vertical rectangle
          (set '(0 . 0))))                     ; 5  1x1 unit square

(define: *block10-start* : prepos ; variant 12
  (prepos (list (tspec 1 '(4 . 1))
                (tspec 2 '(3 . 0))
                (tspec 3 '(1 . 3))
                (tspec 4 '(1 . 0))
                (tspec 4 '(3 . 3))
                (tspec 5 '(2 . 1))
                (tspec 5 '(3 . 2))
                (tspec 5 '(5 . 0))
                (tspec 5 '(5 . 3)))
          '((0 . 1) (0 . 2) (1 . 1) (1 . 2)) ; spaces
          ))

(define: *block10-target* : TileSpec (cons 1 (cons 0 1)))
(define *block10-invalid-cells* '((0 . 0) (0 . 3)))

;;------------------------------------------------------------------------------------------------------
;; CLIMB-12 PUZZLE INIT
;; piece-type is implicit in position within list, each pair specifies the cells of the piece
;; and their location relative to the (arbitrary) origin of that piece, (0 0).
(define: *climb12-piece-types* : (Vectorof (Setof CellRef))
  (vector (ann (set) (Setof CellRef))                                        ; 0 reserved for spaces
          (set '(0 . 0)'(1 . -1)'(1 . 0)'(1 . 1))           ; 1  4 square T (stem up)
          (set '(0 . 0)'(0 . 1)'(1 . 0))                   ; 2  Upper Left pointing L
          (set '(0 . 0)'(1 . -1)'(1 . 0))                  ; 3  Lower Right pointing L
          (set '(0 . 0)'(1 . 0))                          ; 4  2x1 vertical rectangle
          (set '(0 . 0)'(0 . 1))                          ; 5  1x2 horizontal rectangle
          (set '(0 . 0))))                               ; 6  1x1 unit square

;; specify board-state by triples: piece-type, board-row, board-col
(define: *climb12-start* : prepos
  (prepos (list (tspec 1 '(4 . 2))
                (tspec 2 '(2 . 1))
                (tspec 3 '(2 . 3))
                (tspec 4 '(1 . 0))
                (tspec 4 '(1 . 4))
                (tspec 5 '(4 . 0))
                (tspec 5 '(4 . 3))
                (tspec 6 '(3 . 0))
                (tspec 6 '(3 . 4))
                (tspec 6 '(5 . 0))
                (tspec 6 '(5 . 4)))
          '((0 . 2) (1 . 1) (1 . 2) (1 . 3))  ; spaces
          ))

;; specify target as triple: piece-type, board-row, board-col
(define: *climb12-target* : TileSpec (cons 1 (cons 0 2)))
(define *climb12-invalid-cells* '((0 . 0) (0 . 1) (0 . 3) (0 . 4)))
#|
;;------------------------------------------------------------------------------------------------------
;; CLIMB-15 PUZZLE INIT
;; (variation 1: 104 moves)
(define: *climb15-piece-types* : (Vectorof (U Null (Listof CellRef)))
  '#(()                                        ; 0 reserved for spaces
     ((0 . 0)(1 . -1)(1 . 0)(1 . 1))           ; 1  4 square T (stem up)
     ((0 . 0)(0 . 1)(1 . 0))                   ; 2  Upper Left pointing L
     ((0 . 0)(1 . -1)(1 . 0))                  ; 3  Lower Right pointing L
     ((0 . 0)(1 . 0)(1 . 1))                   ; 4  Lower Left pointing L
     ((0 . 0)(0 . 1)(1 . 1))                   ; 5  Upper Right pointing L
     ((0 . 0)(1 . 0))                          ; 6  2x1 vertical rectangle
     ((0 . 0)(0 . 1))                          ; 7  1x2 horizontal rectangle
     ((0 . 0))                                 ; 8  1x1 unit square
     ((0 . 0)(0 . 1)(1 . 0)(1 . 1))))          ; 9  2x2 square
     
(define: *climb15-start* : prepos
  (prepos (list (tspec 1 '(6 . 2))
                (tspec 2 '(2 . 0))
                (tspec 3 '(2 . 2))
                (tspec 4 '(4 . 2))
                (tspec 5 '(4 . 3))
                (tspec 6 '(2 . 3))
                (tspec 6 '(2 . 4))
                (tspec 7 '(6 . 0))
                (tspec 7 '(6 . 3))
                (tspec 8 '(1 . 0))
                (tspec 8 '(1 . 4))
                (tspec 8 '(7 . 0))
                (tspec 8 '(7 . 4))
                (tspec 9 '(4 . 0)))
          '((0 . 2)(1 . 1)(1 . 2)(1 . 3))
          ))

(define: *climb15-target* : TileSpec (cons 1 (cons 0 2)))
(define *climb15-invalid-cells* '((0 . 0) (0 . 1) (0 . 3) (0 . 4)))

;;------------------------------------------------------------------------------------------------------
;; CLIMB-24-PRO PUZZLE INIT
;; 22? moves
;;  From Minoru Abe -- http://www.johnrausch.com/slidingblockpuzzles/abe.htm
;;           _
;; 0   _____|x|_____
;; 1  |___|x x x|___|
;; 2  |   |_____|   |
;; 3  |___| |_  |___|
;; 4  |_  |___|_|  _|
;; 5  | |_| |_| |_| |
;; 6  |_| |_|_|_| |_|
;; 7  |___|_____|___|
;; 8  |   |_| |_|   |
;; 9  |___|_____|___|

(define: *climbpro24-target* : TileSpec (cons 1 (cons 0 3)))
(define *climbpro24-invalid-cells* '((0 . 0)(0 . 1)(0 . 2)(0 . 4)(0 . 5)(0 . 6)))

(define: *climbpro24-piece-types* : (Vectorof (U Null (Listof CellRef)))
  '#(()                                        ; 0 reserved for spaces
     ((0 . 0)(1 . -1)(1 . 0)(1 . 1))           ; 1  4 square T (stem up)
     ((0 . 0)(0 . 1)(1 . 0)(1 . 1))            ; 2  2x2 square
     ((0 . 0)(0 . 1)(1 . 0))                   ; 3  Upper Left pointing L
     ((0 . 0)(0 . 1)(1 . 1))                   ; 4  Upper Right pointing L
     ((0 . 0)(1 . -1)(1 . 0))                  ; 5  Lower Right pointing L
     ((0 . 0)(1 . 0)(1 . 1))                   ; 6  Lower Left pointing L
     ((0 . 0)(1 . 0))                          ; 7  2x1 vertical rectangle
     ((0 . 0)(0 . 1))                          ; 8  1x2 horizontal rectangle
     ((0 . 0)(0 . 1)(0 . 2))                   ; 9  1x3 horizontal rectangle
     ((0 . 0))))                               ; 10 1x1 unit square

(define: *climbpro24-start* : prepos
  (prepos (list (tspec 1 '(8 . 3))    ; T piece
                (tspec 2 '(2 . 0))    ; 2x2
                (tspec 2 '(2 . 5))    ; 2x2
                (tspec 2 '(8 . 0))    ; 2x2
                (tspec 2 '(8 . 5))    ; 2x2
                (tspec 3 '(4 . 5))    ; up-left L
                (tspec 4 '(3 . 3))    ; up-right L
                (tspec 4 '(4 . 0))    ; up-right L
                (tspec 5 '(6 . 1))    ; down-right L
                (tspec 6 '(3 . 2))    ; down-left L
                (tspec 6 '(6 . 5))    ; down-left L
                (tspec 7 '(5 . 0))    ; 2x1 (vertical)
                (tspec 7 '(5 . 2))    ; 2x1 (vertical)
                (tspec 7 '(5 . 4))    ; 2x1 (vertical)
                (tspec 7 '(5 . 6))    ; 2x1 (vertical)
                (tspec 8 '(1 . 0))    ; 1x2 (horizontal)
                (tspec 8 '(1 . 5))    ; 1x2 (horizontal)
                (tspec 9 '(2 . 2))    ; 1x3 (horizontal)
                (tspec 9 '(7 . 2))    ; 1x3 (horizontal)
                (tspec 10 '(5 . 3))   ; 1x1
                (tspec 10 '(6 . 3))   ; 1x1
                (tspec 10 '(8 . 2))   ; 1x1
                (tspec 10 '(8 . 4))   ; 1x1
                )
          '((0 . 3)(1 . 2)(1 . 3)(1 . 4))
          ))

|#
;;------------------------------------------------------------------------------------------------------
; for local testing
;(block10-init)
;(climb12-init)
;(climb15-init)
;(climbpro24-init)
