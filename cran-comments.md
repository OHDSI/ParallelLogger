Dropped all uses of 'dontrun' as requested. Examples now use temp files, so don't write to user's home space.

We use 'snow' instead of 'parallel' because we need several low-level functions that are exported by 'snow' but not by 'parallel' (e.g. 'sendCall' and 'recvOneResult'). I looked at 'parallel' and 'foreach', but am not able to use them to replace 'snow'. Alternative solutions to having a dependency on 'snow' are: incorporating the required 'snow' code in this package (really just a few lines), or having ':::' calls to 'parallel'. I'm happy to implement either if you prefer it to a dependency on 'snow'.

---

## Test environments
* Ubuntu 14.04.5 LTS (Travis), R 3.5.0
* Windows 7, R 3.4.4

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

There are no downstream dependencies.