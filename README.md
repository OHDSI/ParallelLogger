ParallelLogger
==============

[![Build Status](https://github.com/OHDSI/ParallelLogger/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/ParallelLogger/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/ParallelLogger/coverage.svg?branch=main)](https://app.codecov.io/github/OHDSI/ParallelLogger?branch=main)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ParallelLogger)](https://cran.r-project.org/package=ParallelLogger)
[![CRAN_Status_Badge](https://cranlogs.r-pkg.org/badges/ParallelLogger)](https://cran.r-project.org/package=ParallelLogger)

ParallelLogger is part of [HADES](https://ohdsi.github.io/Hades/).

Introduction
============
Support for parallel computation with progress bar, and option to stop or proceed on errors. Also provides logging to console and disk, and the logging persists in the parallel threads. Additional functions support function call automation with delayed execution (e.g. for executing functions in parallel).

Features
========
- Functions for parallel computation.
- Functions for logging, including automated logging for errors and warnings.
- Functions used for automating analyses.

Examples
========

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
Requires R (version 4.0.0 or higher)

Getting Started
===============
In R, to install the latest stable version, install from CRAN:

```r
install.packages("ParallelLogger")
```
  
To install the latest development version, install from the develop branch in GitHub:

```r
install.packages("remotes")
library(remotes)
install_github("ohdsi/ParallelLogger", ref = "develop")
```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/ParallelLogger/).

PDF versions of the documentation is also available:
* Vignette: [Logging using ParallelLogger](https://raw.githubusercontent.com/OHDSI/ParallelLogger/main/inst/doc/Logging.pdf)
* Vignette: [Parallel execution using ParallelLogger](https://raw.githubusercontent.com/OHDSI/ParallelLogger/main/inst/doc/Parallel.pdf)
* Package manual: [ParallelLogger.pdf](https://raw.githubusercontent.com/OHDSI/ParallelLogger/main/extras/ParallelLogger.pdf)

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/ParallelLogger/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
ParallelLogger is licensed under Apache License 2.0

Development
===========
ParallelLogger is being developed in R Studio.

### Development status

Ready for use

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.
