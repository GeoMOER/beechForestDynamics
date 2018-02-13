#' Deregister parallel backend
#' @title deregister parallel backend
#' @details if the number of cores is greater than 1L, the parallel backend will be deregistered
#' @References  R Core Team
#'
#'@author HammerLe, Kleebaue

deregister <- function(){
  if (cores > 1L)
    parallel::stopCluster(cl)

}
