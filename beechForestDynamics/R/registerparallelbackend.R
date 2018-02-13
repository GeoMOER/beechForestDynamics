#' register parallel backend
#' @title register parallelization information
#' @details Creates a set of copies of R running in parallel and communicating over sockets &
#' register the parallel backend with the foreach package.
#'
#' @usage
#' @param cores defines the number of cores used for calculating
#' @References R Core Team
#' @author HammerLe, Kleebaue

register <- function() {
cores = 8
cl = parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)
 }
