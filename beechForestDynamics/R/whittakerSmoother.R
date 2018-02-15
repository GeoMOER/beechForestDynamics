#' @title Whittaker smoother
#' @details this function smoothes a time series vegetation index based on satellite data
#' @usage whittakerSmoother(input, quality_rst, doy_rst, begin, end, whittaker_files, lambda, nIter, threshold)
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
#' @export whittakerSmoother
#'
#' @references Nauss, T., Detsch, F.
#' @author Hanzl, A., Hahn, L.
#'
#'@examples
#' \dontrun{
#' whittakerSmoother(ndvi_oc_rst = ndvi_oc_rst, quality_rst = path_modis_qua_tiles,
#'                    doy_rst = path_modis_doy_tiles, whittaker_files = path_modis_whittaker_tiles,
#'                    lambda = 6000, nIter = 3, threshold = 2000)
#'}

##### Whittaker smoother #####
whittakerSmoother <- function(vi, names_vi = NA,
                              pos1=10, pos2=16,
                              begin=NULL, end=NULL,
                              quality_stck=NULL,
                              doy_stck=NULL,
                              prefixSuffix = c("MCD", "ndvi"),
                              outfilepath,
                              lambda, nIter, threshold, pillow=0){

  if(is.na(names_vi[1])){
    names_vi = names(vi)
  }

  names_vi = names(vi)
  timeInfo = MODIS::orgTime(basename(names_vi),nDays="asIn",begin=begin,end=end,pillow=pillow,pos1=pos1,pos2=pos2)

  wrst = MODIS::whittaker.raster(vi = vi, w = quality_stck, t = doy_stck,
                          removeOutlier = TRUE,
                          threshold = 2000,
                          timeInfo = timeInfo,
                          lambda = lambda, nIter = nIter,
                          prefixSuffix = prefixSuffix,
                          outDirPath = outfilepath,
                          overwrite = TRUE, format = "raster")

  # Rename files
  lambda_str = paste0("yL", lambda, ".")

  outfiles = list.files(outfilepath,pattern = glob2rx("*.tif"), full.names = TRUE)
  newnames = gsub("MYD13Q1.", "MYD13Q1.A", outfiles)
  newnames = gsub(lambda_str, "", newnames)
  file.rename(outfiles, newnames)

  wrst = raster::stack(newnames)
  return(wrst)
}
