#' Whittaker smoother 
#' @title Whittaker smoother
#' @details this function smoothes a time series vegetation index based on satellite data 
#' @usage whittaker_smoother(input, quality_rst, doy_rst, begin, end, whittaker_files, lambda, nIter, threshold)
#' dependent on package "MODIS"
#' @param ndvi_oc_rst, stacked vegetation index files
#' @param quality_rst, stacked quality checked files
#' @param doy_rst, stacked day of year files
#' @param whittaker_files, path where whittaker output files shall be stored
#' @param lambda, integer, strength of smoothing 
#' @param nIter, integer, number of iteration for the upper envelope fitting
#' @param threshold, integer, threshold for outlier values
#' @param prefix, character, file name prefix
#' @param suffix, character, file name suffix
#' 
#' @return geo-tiff data
#' 
#' @references Nauss, T., Detsch, F.
#' @author Hanzl, A., Hahn, L.
#'
#'@examples
#' \dontrun{
#' whittaker_smoother(ndvi_oc_rst = ndvi_oc_rst, quality_rst = path_modis_qua_tiles,
#'                    doy_rst = path_modis_doy_tiles, whittaker_files = path_modis_whittaker_tiles, 
#'                    lambda = 6000, nIter = 3, threshold = 2000
#'                    prefix = "ab", suffix = "ws")
#'}

##### Whittaker smoother #####  
whittaker_smoother <- function(ndvi_oc_rst, quality_rst, doy_rst, output_subdirectory, 
                               lambda, nIter, threshold, prefix, suffix){
  outfilepath <- compileOutFilePath(input_filepath = ndvi_oc_rst,
                     output_subdirectory = output_subdirectory, prefix = prefix, suffix = suffix)
  
      whittaker.raster(vi = ndvi_oc_rst, w = quality_rst, t = doy_rst,
                       timeInfo = orgTime(vi),
                       lambda = lambda, nIter = nIter,
                       prefixSuffix = c(prefix, suffix),
                       outDirPath = outfilepath,
                       outlierThreshold = threshold,
                       overwrite = TRUE, format = "raster")
}
