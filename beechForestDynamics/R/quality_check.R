#'quality_check
#'@title Quality assurance
#'@description Checks raster data with a quality layer. Replaces values with bad quality with NA.
#'@examples
#' \dontrun{
#' quality_check(p = c("NDVI.tif$", "reliability.tif$"), path_modis_prj = path_modis_prj, path_modis_quality_checked = path_modis_quality_checked)
#'          }
#'@param ndvi.rst rasterstack with ndvi raster and reliability raster
#'@param filename_output name and path of output file
#'@return stack of quality checked NDVI
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schmidt, Tiziana Koch, Niklas Balzer
#'

quality_check <- function(ndvi.rst, filename_output){
  
  lst_ndvi_qa = foreach(i = raster::unstack(ndvi.rst[[1]]), j = ndvi.rst[[2]],k=filename_output,
                        .packages = c("doParallel", "raster", "rgdal", "GSODTools"),
                        .export = ls(envir = globalenv())) %dopar% {
                          raster::overlay(i, j, fun = function(x, y) {
                            x[!y[] %in% c(0:2)] = NA
                            return(x)
                          }, filename = k,
                          format = "GTiff", overwrite = TRUE)
                        }
  
  
  ndvi.rst.qa = stack(lst_ndvi_qa)
  return(ndvi.rst.qa)
}
