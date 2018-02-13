#'@title Fill gaps by linear interpolation
#'@description Subsets NAs by spacial linear interpolation
#'@examples
#' \dontrun{
#' fillGapsLin(rst_fn, out_path)
#' }
#'
#'@param rst_fn raster stack of scaled and temporal aggregated rasters = output of previous function (3rd function of temporal aggregation),
#'              following folder structure of 00_set_environment.R this would mean: all rasters in path "path_modis_temp_agg_tiles"
#'              rst_fn = stack(list.files(path_modis_temp_agg_tiles, pattern = glob2rx("*.tif"), full.names = TRUE))
#'@param out_path path to folder, where output should be stored
#'                following folder structure of 00_set_environment.R this would mean: out_path = paste0(path_modis_filled_tiles)
#'
#'@return writes out rasters with no NA values
#'@export fillGapsLin
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schnell, Laura Giese

fillGapsLin = function(rst_fn, out_path){
  lib = c("doParallel", "raster", "rgdal", "GSODTools")
  rst_fn_mat = raster::as.matrix(rst_fn)
  # 43701
  rst_fn_mat_filled =
    foreach(i = 1:nrow(rst_fn_mat), .packages = lib,
            .export = ls(envir = globalenv())) %dopar% {
              val = rst_fn_mat[i, ]
              val_length=length(val)
              if(sum(is.na(val))/val_length < 0.5){
                nas = rle(is.na(val))
                nas_lg = which(nas$lengths & nas$values)
                for(l in nas_lg){
                  l_size = nas$lengths[l]
                  nas_lg_pos = sum(nas$lengths[1:l])
                  sm = nas_lg_pos - l_size
                  lg = nas_lg_pos + 1
                  if(sm <= 0){
                    sm = 1
                    val[sm] = val[lg]
                  }
                  if(lg >= val_length){
                    lg = val_length
                    val[lg] = val[sm]
                  }
                  val[sm:lg]
                  gap_length = lg-sm+1
                  gap_values = approx(c(val[sm], val[lg]), method = "linear",
                                      n = gap_length)
                  val[(sm+1):(lg-1)] = gap_values$y[2:(gap_length-1)]
                }
              }
              return(matrix(val, ncol = length(val), byrow = TRUE))
            }
  rst_fn_mat_filled = do.call("rbind", rst_fn_mat_filled)
  rst_fn_filled = rst_fn
  rm(rst_fn)
  gc()

  for(l in seq(nlayers(rst_fn_filled))){
    rst_fn_filled[[l]] = raster::setValues(rst_fn_filled[[l]], rst_fn_mat_filled[, l])
  }


  names(rst_fn_filled) = paste0("FLD_", names(rst_fn_filled))
  #subpath = paste0(path_modis_filled_tiles, basename(dir), "/")
  out_path -> subpath
  if (!dir.exists(subpath))
    dir.create(subpath, recursive = TRUE)
  fls_fn_filled = paste0(subpath, names(rst_fn_filled))

  lst_fn_filled = foreach(i = raster::unstack(rst_fn_filled), j = as.list(fls_fn_filled)) %do% {
    raster::writeRaster(i, filename = j, format = "GTiff", overwrite = TRUE)
  }


}
