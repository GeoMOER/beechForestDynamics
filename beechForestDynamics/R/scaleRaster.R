#'Scale Raster
#'@title scaleRaster
#'@aliases Scaling
#'@author Santwoski, A. & C. Weber
#'@description The scaling function takes up a raster-stack [raster::stack()] and scales it.
#'The function changes the scaling (default = /10000) and rejects inconsistend values (default = NA).
#'The function returns new calculated .tif-files with pattern /SCL_ into output file path and returns also a raster stack.
#'
#'@param input Input raster stack
#'@param scalefac Set scaling factor (default= 10000), factor to divide initial MODIS-NDVI values
#'@param incon Dealing with rejacted values (default = NA)
#'@param outputpath Set output file path
#'
#'@export scaleRaster
#'
#'@example
#'/donotrun{
#'fn <-
#'scaling(input, scalefac = 10000, incon = NA)}
#'
scaleRaster <- function(input, scalefac = 10000, incon = NA, outputpath){
  ## Input data:
  p = "^.*_NDVI_.*\\.tif$"
  fls_wht = base::list.files(path=input,pattern = p, full.names = TRUE)
  rst_wht =  raster::stack(fls_wht)
  fls_scl = base::paste0(outputpath, "SCL_", names(rst_wht))
  lst_scl = foreach(i = unstack(rst_wht), j = as.list(fls_scl),
                    .packages = c("raster", "rgdal"),
                    .export = ls(envir = globalenv())) %dopar% {

                      ## scale factor (default 10000)
                      rst = i
                      rst = rst / scalefac

                      # rejection of inconsistent values
                      id = which(rst[] < -1 | rst[] > 1)

                      if (length(id) > 0) {
                        rst[id] = incon ##default NA
                      }

                      # store data in .tif-file
                      raster::writeRaster(rst, filename = j, format = "GTiff", overwrite = TRUE)
                    }
  lst_scl_sc <- raster::stack(lst_scl)
  base::return(lst_scl_sc)
}
