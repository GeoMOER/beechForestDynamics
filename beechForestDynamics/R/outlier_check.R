#' Additional outlier check 
#' @title Additional outlier check
#' @details this is an example function named 'Additional outlier check'
#' which checks for statistical outliers
#' @usage outlier_check(input, lq, uq)
#' @param input, path where input is
#' @param lq, Numeric, lower quantile limit
#' @param uq, Numeric, upper quantile limit 
#' 
#' @return geo-tiff data

  outlier_check <- function(input, lq, uq){
  
  #list input folder
  dirs = list.dirs(input)[-1]
  #dirs = dirs[-seq(7)]
  
  #load all Tiffs, which are named with _NDVI_, stack them and write them as matrix
  for(dir in dirs){
    p =  "^.*_NDVI_.*\\.tif$"
    files = list.files(dir, pattern = p, full.names = TRUE)
    ndvi.rst.qa = raster::stack(files)
    ndvi_mat_qa = raster::as.matrix(ndvi.rst.qa)
    ndvi_rst_sd = ndvi.rst.qa
  
  #take the matix, go through all rows and calculate the outliers 
  #based on the lower and upper quantile  
    ndvi_lst_sd =
      foreach(i = 1:nrow(ndvi_mat_qa), .packages = lib,
              .export = ls(envir = globalenv())) %dopar% {
                val = ndvi_mat_qa[i, ]
                if(length(which(!is.na(val))) > 2){
                  id = GSODTools::tsOutliers(val, lower_quantile = lq,
                                             upper_quantile = uq, index = TRUE)
                  val[id] = NA
                }
                return(matrix(val, ncol = length(val), byrow = TRUE))
              }
    
    #bind rows of the outliers together
    ndvi_mat_sd = do.call("rbind", ndvi_lst_sd)
    rm(ndvi_lst_sd)
    rm(ndvi_mat_qa)
    gc()
    
    #assign outliers to layer
    for(l in seq(nlayers(ndvi_rst_sd))){
      ndvi_rst_sd[[l]] = raster::setValues(ndvi_rst_sd[[l]], ndvi_mat_sd[, l])
    }
    
    #name files and set output directory
    subpath = paste0(path_modis_outliers_tiles, basename(dir), "/")
    if (!dir.exists(subpath))
      dir.create(subpath, recursive = TRUE)
    
    #write results as geotiff
    ndvi_rst_sd = writeRaster(ndvi_rst_sd, format = "GTiff",
                              filename = subpath,
                              bylayer = TRUE, suffix = names(ndvi.rst.qa),
                              overwrite = TRUE)
    rm(ndvi.rst.qa)
    rm(ndvi_rst_sd)
    gc()
    }
  }


  outlier_check(input = path_modis_quality_checked_tiles,
               lq = 0.4,
               uq = 0.9)