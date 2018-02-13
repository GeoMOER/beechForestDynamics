#' Scale raster data
#' @title scaleRaster
#' @aliases Scaling
#' @author Santwoski, A. & C. Weber
#' @description The scaling function takes up a \code{raster::stack} and scales it.
#' The function changes the scaling (default = /10000) and rejects inconsistend values (default = NA).
#' The function writes scaled GeoTiffs and returns also a raster stack.
#'
#' @param rstck rstck raster stack
#' @param scalefac Set scaling factor (default= 10000), factor to divide initial raster values
#' @param incon Dealing with rejected values (default = NA)
#' @param outputfilepathes Set output file path
#'
#' @export scaleRaster
#'
#' @example
#' /donotrun{
#' fn <-
#' scaling(rstck, scalefac = 1000, incon = NA)}
#'
scaleRaster <- function(rstck, scalefac = 10000, incon = NA, outputfilepathes){
  rstck_scaled = foreach(i = unstack(rstck), j = as.list(outputfilepathes),
                         .packages = c("raster", "rgdal"),
                         .export = ls(envir = globalenv())) %dopar% {

                           ## scale factor
                           rst = i / scalefac

                           # set inconsistent values to NA
                           id = which(rst[] < -1 | rst[] > 1)
                           if (length(id) > 0) {
                             rst[id] = incon
                           }

                           # store data in .tif-file
                           raster::writeRaster(rst, filename = j,
                                               format = "GTiff",
                                               overwrite = TRUE)
                         }
  rstck_scaled <- raster::stack(rstck_scaled)
  return(rstck_scaled)
}
