#lang racket/base

(provide (all-defined-out))


#| Run Environment Configuration for CLUSTER

*** This file is under version control.  Create a _copy_ named "configenv.rkt" to this file if on cluster ***

Provide defining values for the following global variables:
  *stp-home-path*: the full path to stp-master.rkt
  *master-name*  : typically either "localhost" or "wcp"
  *local-store*  : where to write partial expansions, e.g., "/state/partition1/fringefiles/" on cluster
  *share-store*  : [DEPRECATED] where to find current, prev, and proto fringes and where to write segments, e.g., "/share/data2/fringefiles/" on cluster
  *worker-hosts* : a list of strings of the hostnames for where workers will run
  *workers-per-host*: how many workers to run on each remote host

|#

;;--- Environment values -----------
(define *master-name* "wcpkneel")
(define *local-store* "/state/partition1/fringefiles/")
(define *share-store* "/share/bigspace/fringefiles/")
(define *worker-hosts*
  (for/list ([i 45])
    (string-append "compute-0-" (number->string i))))
(define *workers-per-host* 4)

;;--- Search values ----------------
#| *late-duplicate-removal*: whether duplicates with prev- and current-fringes are checked on initial local-merge (#f)
      or whether duplicates are forwarded to merge phase and only checked on final merge (#t)
      Tests (and analysis) show that when the number of processors gets high enough (36 is sufficiently high),
      late duplicate removal (trueDDD) has a significant advantage  [see Tasks and Accomplishments: Sat Nov. 22, 2014]
      For local n-core runs, set this #f.
   *preserve-prior-fringes*: if #t, does not delete prior fringes once done with them.
      Use #t if you want to reconstruct the solution or perhaps restart search from somewhere
|#
(define *late-duplicate-removal* #t)
(define *preserve-prior-fringes* #t)

;;--- Puzzle values ----------------
(define *puzzle-name* "climb12") ;; one of climb12, climb15, climbpro24
(define *max-depth* 240)



