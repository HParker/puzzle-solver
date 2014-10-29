#lang typed/racket/base

(provide (all-defined-out))

;; ******************************************************************************
;; DATA DEFINITIONS

;; a cell is a pair, (list r c), for row and column r and c
(define-type Cell (Pairof Byte Byte))

;; a location (loc for short) is an int, representing the row-major rank of a cell
(define-type-alias Loc Byte)

;; a tile-spec is a triple, (cons a c), where a is the tile-type and c is the cell of the piece-type's origin
(define-type TileSpec (Pairof Byte Cell))
(struct: tspec ([tiletype : Byte] [origin : Cell]))

;; a tile-loc-spec (tlspec), is a (list t l), where t is the tile-type and l is the loc of that tile

;; a pre-position is a (append (listof tile-spec) (listof cell))
(struct: prepos ([tspecs : (Listof tspec)] [spaces : (Listof Cell)]))

;; a old-position is a (vectorof (listof int))
;; where the index of the top-level vectors reflect the piece-type as given in the init,
;; and the ints in the secondary vectors are the SORTED locations of the pieces of that type

;; a bw-position is a (vector int)
;; where each int is a bitwise representation of the locations of the pieces of that type
(define-type-alias BW-Position (Vectorof Integer))

;; a bs-position is a bytestring, where each byte represents the location of the corresponding tile

;; a hc-position (hcpos for short) is a structure: (make-hc-position hc bwrep)
;; where hc is the equal-hash-code for the bytestring of the position, bwrep


;; ******************************************************************************

(struct: hc-position ([hc : Fixnum] [bs : Bytes]) #:transparent #:mutable)
;; the hc is the hashcode of the bytestring

;; make-hcpos: bs-position -> hc-position
;; wrapper for the position rep augmented with the hashcode
(define: (make-hcpos [bsrep : Bytes]) : hc-position (hc-position (cast (equal-hash-code bsrep) Fixnum) bsrep))


