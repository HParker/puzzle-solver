#lang racket/base

(provide (all-defined-out))


#| Run Environment Configuration for LOCALHOST

Provide defining values for the following global variables:
  *master-name*  : typically either "localhost" or "wcp"
  *local-store*  : where to write partial expansions, e.g., "/state/partition1/fringefiles/" on cluster
  *share-store*  : where to find current, prev, and proto fringes and where to write segments, e.g., "/share/data2/fringefiles/" on cluster
  *n-processors* : the number of processors to use
  *late-duplicate-removal* : (boolean) remove duplicates after [after what again?]

This file is under version control.  Create a _copy_ named "configenv.rkt" to this file if on local machine

|#

;;--- Environment values -----------
(define *master-name* "localhost")
(define *local-store* "fringefiles/")
(define *share-store* "fringefiles/")
(define *n-processors* 4)

;;--- Search values ----------------
#| *late-duplicate-removal*: whether duplicates with prev- and current-fringes are checked on initial local-merge (#f)
      or whether duplicates are forwarded to merge phase and only checked on final merge (#t)
      Tests (and analysis) show that when the number of processors gets high enough (36 is sufficiently high),
      late duplicate removal (trueDDD) has a significant advantage  [see Tasks and Accomplishments: Sat Nov. 22, 2014]
      For local n-core runs, set this #f.
|#
(define *late-duplicate-removal* #f)
(define *preserve-prior-fringes* #f)

;;--- Puzzle values ----------------
(define *puzzle-name* "climb12") ;; one of climb12, climb15, climbpro24
(define *max-depth* 60)
