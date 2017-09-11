# Parallel Search in Sliding-tile Puzzles

Research code for exploring parallel search algorithms
applied to sliding-tile puzzles that give rise to
extremely large search spaces.

To date, implemented search methods include Breadth-first Search,
Fringe Breadth-first Search, and A\*.

Parallelism has been tested on a 45-node cluster
using Racket's distributed places.
Having difficulties with numbers of worker nodes upwards of a dozen or so.
Had some (intermittent?) success with smaller numbers of workers.

## Overview

- stp-init.rkt (global parameters, data-definitions,
functions for converting between formats, puzzle initialization data, etc)
- stp-solve-base.rkt (functions that are common to any search method,
whether run on a single computer or on a cluster)
- stp-fringefilerep.rkt (file and I/O functions
for reading and writing fringes from and to files)
- stp-solve-cluster.rkt
(the main file with code for distributing the search across multiple processes;
the place code appears at the bottom of the file;
- (stp-spaceindex.rkt is also used and plays an important role,
but is pretty complex so you should ignore it initially)
- the folder stpconfigs contains configuration files for global variables 
whose settings depend on whether you are running on a cluster or a single machine
(although maybe with multiple cores);
you also set identifiers for number of worker-hosts, their names, the number of workers to start on each,
pathnames for where fringe files get stored,
and global identifiers for which puzzle you want to solve, etc.;
once you run the software, this folder will also contain an 'index' file
for each puzzle that you run.
- the folder fringefiles will contain stored fringes from searches

## Running

Processes must be run in the source folder; 
for example, `/home/username/puzzle-solver`.
Before starting, you must also create `<source-home>/stpconfigs/configenv.rkt`
for wherever you placed the source code.
Usually, you can simply create a symbolic link to `./stpconfigs/configenvlocal.rkt`
but you can create a copy and edit it to your particular situation.

In a shell, run `racket stp-master.rkt`
You may optionally redirect output to a log file.


## To-do

In the wplace branch:
- make fringe-segment copying asynchronous
- consider using compression for the copying
- explore faster alternatives to scp for copying
