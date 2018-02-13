#'@title Temporal aggregation
#'@aliases temporalAggregation
#'@author Santwoski, A. & C. Weber
#'@description The temporal function takes up the previously created scaled .tif-files (scaled-function).
#'The function changes the input .tif-files to a stack with temporal composite. The function returns new calculated
#'.tif-files with pattern /day_of_the_year into the modis_doy_tiles file (The file must be created beforehand).
#'The function requires the "raster" package.
#'
#'@param input Set input file (default= path_modis_whittaker_tiles, from 00_set_environment call)
#'
#'@export temporalAggregation
#'
#'@examples
#'\dontrun{
#'temporalAggregation(input)
#'}

temporalAggregation=function(input){
  ## Input data:
  tiflist=list.dirs(input)[-1]
  files = list.files(tiflist, pattern = "^.*_NDVI_.*\\.tif$", full.names = TRUE)
  rst_scl = stack(files)
  temp=substr(tiflist,1, nchar((tiflist))-nchar(basename(tiflist)))
  temp2=substr(tiflist,1, nchar((temp))-nchar(basename(temp))-1)
  subpath = paste0(temp2,"modis_doy_tiles/", basename(tiflist), "/")
  files_doy = list.files(subpath, pattern = "*day_of_the_year.*\\.tif", full.names = TRUE)
  ## Temporal composite:
  start = which(substr(basename(files_doy), 10, 16) %in% substr(names(rst_scl)[1], 13, 19))
  end = which(substr(basename(files_doy), 10, 16) %in% substr(names(rst_scl)[nlayers(rst_scl)], 13, 19))
  ## Stack temporal composite:
  rst_doy = stack(files_doy[start:end])
}
