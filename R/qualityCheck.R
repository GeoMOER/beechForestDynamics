#'@title Check quality of raster data
#'@description Checks raster data with a quality layer. Replaces values with bad quality with NA.
#'@export qualityCheck
#'@examples
#' \dontrun{
#' quality_check(p = c("NDVI.tif$", "reliability.tif$"), path_modis_prj = path_modis_prj, path_modis_quality_checked = path_modis_quality_checked)
#' }
#'@param rstck_values \code{raster::stack} with data values
#'@param rstck_quality \code{raster::stack} with ranked quality values
#'@param outputfilepathes name and path of output files
#'@return stack of quality checked data values
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schmidt, Tiziana Koch, Niklas Balzer
#'

qualityCheck <- function(rstck_values, rstck_quality, outputfilepathes){

  qa = foreach(i = raster::unstack(rstck_values), j = rstck_quality, k = outputfilepathes,
               .packages = c("doParallel", "raster", "rgdal", "GSODTools"),
               .export = ls(envir = globalenv())) %dopar% {
                 raster::overlay(i, j, fun = function(x, y) {
                   x[!y[] %in% c(0:2)] = NA
                   return(x)
                 }, filename = k,
                 format = "GTiff", overwrite = TRUE)
               }


  qa = stack(qa)
  return(qa)
}
