#'Scaling MODIS-Data
#'@title Scaling
#'@aliases Scaling
#'@author Santwoski, A. & C. Weber
#'@description The scaling function takes up the previously created .tif-files that a  Whittaker smoother ran through.
#'The function changes the scaling of the NDVI modis data (default = /10000), rejects inconsistend values (default = NA).
#'The function returns new calculated .tif-files with pattern /SCL_ into the modis_scaled_tiles file. The function requires
#'the "raster" and "rgdal" packages. The function returns also a raster stack.
#'
#'@param input Set input directory
#'@param cores Set cores for parallel working (default = 1 core)
#'@param scalefac Set scaling factor (default= 10000), factor to divide initial MODIS-NDVI values
#'@param incon Dealing with rejacted values (default = NA)
#'@param output Set output directory
#'
#'@example
#'/donotrun{
#'scaling(input,cores = 1, scalefac = 10000, incon = NA)}
#'
scaling <- function(input,cores = 1, scalefac = 10000, incon = NA, output){
  base::require(raster)
  base::require(rgdal)
  base::require(doParallel)
  ## Input data:
  p = "^.*_NDVI_.*\\.tif$"
  fls_wht = base::list.files(path=input,pattern = p, full.names = TRUE)
  rst_wht =  raster::stack(fls_wht)
  fls_scl = base::paste0(output, "SCL_", names(rst_wht))
  ## Do parallel
  lib = c("doParallel", "raster", "rgdal", "GSODTools")
  cl = parallel::makeCluster(cores) ## cores (default = 1)
  doParallel::registerDoParallel(cl)
  lst_scl = foreach(i = unstack(rst_wht), j = as.list(fls_scl),
                    .packages = c("raster", "rgdal"),
                    .export = ls(envir = globalenv())) %dopar% {

                      ## scale factor (default 10000 --> NDVI factor MODIS 0,00001)
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
  base::detach("package:raster")
  base::detach("package:rgdal")
}
