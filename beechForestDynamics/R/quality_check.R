#'quality_check
#'@title Quality assurance
#'@description Checks raster data with a quality layer. Replaces values with bad quality with NA.  
#'@examples
#' \dontrun{
#' quality_check(p = c("NDVI.tif$", "reliability.tif$"), path_modis_proj = path_modis_prj)
#'          }
#'@param p names to identify raster files of NDVI and the quality layer with $ for a direct access
#'@param path_modis_proj name of path directed to the raster files
#'@return stack of quality checked NDVI
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schmidt, Tiziana Koch
quality_check <- function(p, path_modis_prj){
  ndvi.rst = lapply(p, function(p){
  ndvi = stack(list.files(path_modis_prj, pattern = p, full.names = TRUE))
})

if (!dir.exists(path_modis_quality_checked))
  dir.create(path_modis_quality_checked)

suppressWarnings(
  lst_ndvi_qa = foreach(i = unstack(ndvi.rst[[1]]), j = ndvi.rst[[2]],
                        .packages = lib,
                        .export = ls(envir = globalenv())) %dopar% {
                          raster::overlay(i, j, fun = function(x, y) {
                            x[!y[] %in% c(0:2)] = NA
                            return(x)
                          }, filename = paste0(path_modis_quality_checked, "QA_", names(i)),
                          format = "GTiff", overwrite = TRUE)
                        }
)

ndvi.rst.qa = stack(lst_ndvi_qa)
return(ndvi.rst.qa)
}