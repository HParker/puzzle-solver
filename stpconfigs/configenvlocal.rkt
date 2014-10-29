#lang typed/racket/base

(provide (all-defined-out))


#| Run Environment Configuration for LOCALHOST

Provide defining values for the following global variables:
  *master-name*  : typically either "localhost" or "wcp"
  *local-store*  : where to write partial expansions, e.g., "/state/partition1/fringefiles/" on cluster
  *share-store*  : where to find current, prev, and proto fringes and where to write segments, e.g., "/share/data2/fringefiles/" on cluster
  *n-processors* : the number of processors to use
  *late-duplicate-removal* : (boolean) remove duplicates after [after what again?]
  *puzzle-name*  : name of the puzzle (String)

Create a link named "configenv.rkt" to this file if on local machine

|#

(define: *master-name* : String "localhost")
(define: *local-store* : String "fringefiles/")
(define: *share-store* : String "fringefiles/")
(define: *n-processors* : Number 4)
(define: *late-duplicate-removal* : Boolean #f)
(define: *puzzle-name* : String "climb12")
;(define: *puzzle-name* : String "climb15")
;(define: *puzzle-name* : String "climbpro24")