ParallelLogger
==============

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ParallelLogger)](https://cran.r-project.org/package=ParallelLogger)
[![CRAN_Status_Badge](http://cranlogs.r-pkg.org/badges/ParallelLogger)](https://cran.r-project.org/package=ParallelLogger)

Introduction
============
An R package with tools to be used in the other OHDSI R packages

Features
========
- Functions for parallel computation
- Functions for logging
- Functions used for automating analyses

Examples
===========

```r
# Run a function in parallel:
fun <- function(x) {
  return (x^2)
}

cluster <- makeCluster(numberOfThreads = 3)
result <- clusterApply(cluster, 1:10, fun)
stopCluster(cluster)

# Create a file logger:
addDefaultFileLogger("log.txt")
logTrace("Hello world")
```

Technology
============
ParallelLogger is an R package.

System Requirements
============
Requires R (version 3.1.0 or higher)

Dependencies
============
None

Getting Started
===============
1. In R, use the following commands to download and install ParallelLogger:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/ParallelLogger")
  ```

Getting Involved
=============
* Vignette: [Logging using ParallelLogger](https://raw.githubusercontent.com/OHDSI/ParallelLogger/master/inst/doc/Logging.pdf)
* Vignette: [Parallel execution using ParallelLogger](https://raw.githubusercontent.com/OHDSI/ParallelLogger/master/inst/doc/Parallel.pdf)
* Package manual: [ParallelLogger.pdf](https://raw.githubusercontent.com/OHDSI/ParallelLogger/master/extras/ParallelLogger.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
ParallelLogger is licensed under Apache License 2.0

Development
===========
ParallelLogger is being developed in R Studio.

### Development status
[![Build Status](https://travis-ci.org/OHDSI/ParallelLogger.svg?branch=master)](https://travis-ci.org/OHDSI/ParallelLogger)
[![codecov.io](https://codecov.io/github/OHDSI/ParallelLogger/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/ParallelLogger?branch=master)

Ready for use

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
