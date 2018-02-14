#'@title fill gaps by linear interpolation
#'@description subsets NAs by spacial linear interpolation
#'@usage dependent of package "raster", "foreach", "doParallel", "raster", "rgdal", "GSODTools"
#'@examples
#' \dontrun{
#' fill_gaps_lin(raster_stack, outfilepath)
#' }
#'
#'@param raster_stack raster stack of scaled and temporal aggregated rasters
#'@param out_path path to folder, where output should be stored
#'
#'@return writes out rasters with no NA values
#'@export fill_gaps_lin
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schnell, Laura Giese

fill_gaps_lin = function(raster_stack, outfilepath){
  lib = c("foreach", "doParallel", "raster", "rgdal", "GSODTools")
  raster_stack_mat = raster::as.matrix(raster_stack)
  # 43701
  raster_stack_mat_filled =
    foreach(i = 1:nrow(raster_stack_mat), .packages = lib,
            .export = ls(envir = globalenv())) %dopar% {
              val = raster_stack_mat[i, ]
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
  raster_stack_mat_filled = do.call("rbind", raster_stack_mat_filled)
  raster_stack_filled = raster_stack
  rm(raster_stack)
  gc()

  for(l in seq(nlayers(raster_stack_filled))){
    raster_stack_filled[[l]] = raster::setValues(raster_stack_filled[[l]], raster_stack_mat_filled[, l])
  }


  #names(raster_stack_filled) = paste0(names_fill, names(raster_stack_filled))
  #subpath = paste0(path_modis_filled_tiles, basename(dir), "/")
  #input_filepath -> subpath
  if (!dir.exists(outfilepath))
    dir.create(dirname(outfilepath[1]), recursive = TRUE)
  fls_fn_filled = paste0(outfilepath, names(raster_stack_filled))

  lst_fn_filled = foreach(i = raster::unstack(raster_stack_filled), j = as.list(outfilepath)) %do% {
    raster::writeRaster(i, filename = j, format = "GTiff", overwrite = TRUE)
  }


}
