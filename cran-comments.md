This is a repackaging of the 'OhdsiRTools' package that was not allowed in CRAN. I have removed all functions for package developers and communicating with an OHDSI WebAPI, and instead only include the functions for parallel processing, logging, and function call automation. There is a large number of other packages we would like to move to CRAN that rely on this functionality.

As a reminder: we use 'snow' instead of 'parallel' because we need several low-level functions that are exported by 'snow' but not by 'parallel' (e.g. 'sendCall' and 'recvOneResult'). Note that 'parallel' itself also calls the 'snow' package.

---

## Test environments
* Ubuntu 14.04.5 LTS (Travis), R 3.5.0
* Windows 7, R 3.4.4

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

There are no downstream dependencies.