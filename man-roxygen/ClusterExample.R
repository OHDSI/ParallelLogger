#' @examples 
#' fun <- function(x) {
#'   return (x^2)
#' }
#' 
#' cluster <- makeCluster(numberOfThreads = 3)
#' clusterApply(cluster, 1:10, fun)
#' stopCluster(cluster)
