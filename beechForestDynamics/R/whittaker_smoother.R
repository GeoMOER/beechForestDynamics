#' Whittaker smoother 
#' @title Whittaker smoother
#' @details this function smoothes a time series vegetation index based on satellite data 
#' @usage whittaker_smoother(input, begin, end, lambda, nIter, threshold)
#' @param input, path where input is
#' @param begin, Character, first date of time series in format "YYYY-MM-DD"
#' @param end, Character, last date of time series in format "YYYY-MM-DD"
#' @param lambda, integer, strength of smoothing
#' @param nIter, integer, number of iteration for the upper envelope fitting
#' @param threshold, integer, threshold for outlier values
#' 
#' @return geo-tiff data


##### Whittaker smoother #####   Leonie Hahn, Andreas Hanzl
whittaker_smoother <- function(input, begin, end, lambda, nIter, threshold){

dirs = list.dirs(input)[-1]

for(dir in dirs){
  p =  "^.*_NDVI_.*\\.tif$"
  vi = preStack(path=dir, pattern=p)
  
  dirw = paste0(path_modis_qua_tiles, basename(dir), "/")
  p =  "^.*_Quality_.*\\.tif$"
  w = preStack(path=dirw, pattern = p)
  
  dirt = paste0(path_modis_doy_tiles, basename(dir), "/")
  p =  "^.*_day_.*\\.tif$"
  t = preStack(path=dirt, pattern = p)
  
  timeInfo = orgTime(basename(vi), nDays="asIn", begin=format(as.Date(begin), '%Y%j'), 
                     end=format(as.Date(end), '%Y%j'), pillow = 0, pos1 = 14, pos2 = 20)
  vi = preStack(files=vi, timeInfo=timeInfo)
  
  
  if(all(substr(basename(vi), 14, 20) == substr(basename(w), 10, 16)) &
     all(substr(basename(vi), 14, 20) == substr(basename(t), 10, 16))){
    
    subpath = paste0(path_modis_whittaker_tiles, basename(dir), "/")
    if (!dir.exists(subpath))
      dir.create(subpath, recursive = TRUE)
    
    whittaker.raster(vi = vi, w = w, t = t,
                     timeInfo = timeInfo,
                     lambda = lambda, nIter = nIter,
                     prefixSuffix = c("MYD13Q1", substr(basename(vi[1]), 21, 60)),
                     outDirPath = subpath,
                     outlierThreshold = threshold,
                     overwrite = TRUE, format = "raster")
    
  } else {
    stop
  }
  
}
}

whittaker_smoother(input = path_modis_outliers_tiles,
                  begin = "2002-07-04",
                  end = "2017-12-11",
                  lambda = 6000,
                  nIter = 3,
                  threshold = 2000)
