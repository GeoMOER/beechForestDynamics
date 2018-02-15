#' @title Temporal aggregation
#' @aliases temporalAggregation
#' @author Santwoski, A. & C. Weber
#' @description The function is a wrapper arround MODIS::temporalComposit.
#' It is hardly usefull since it only adds the writing.
#'
#' @param rstack Raster stack of MODIS composite files
#' @param rstack_doy Raster stack of associated day of year information to rstack
#' @param pos1 Start position of date information in names(rstack) (format: YYYYJJJ)
#' @param pos2 End position of date information in names(rstack) (format: YYYYJJJ)
#' @param outputfilepathes Full path and names of all output layers to be written to disk
#' @param interval Aggregation interval (see MODIS::temporalComposite),
#' @param fun Aggregation function (see MODIS::temporalComposite)
#' @param na.rm NA handling (see MODIS::temporalComposite)
#' @param cores Paralleliziation (see MODIS::temporalComposite)
#'
#'@export temporalAggregation
#'
#'@examples
#'\dontrun{
#'temporalAggregation(rstack, rstack_doy, layer_dates, outputfilepathes,
#'interval = "fortnight", fun = max, na.rm = TRUE,
#'cores = 4L)
#'}

temporalAggregation = function(rstack, rstack_doy, pos1 = 10, pos2 = 16, outputfilepathes,
                               interval = "fortnight", fun = max, na.rm = TRUE,
                               cores = 4L){

  layer_dates = MODIS::extractDate(rstack, pos1 = pos1, pos2 = pos2, asDate =TRUE)$inputLayerDates

  rst = MODIS::temporalComposite(x = rstack, y = rstack_doy,
                                 timeInfo = layer_dates, interval = interval,
                                 fun = fun, na.rm = TRUE, cores = cores)

  outfilepath = basename(outputfilepathes[1])
  outfilepath = paste0(dirname(outputfilepathes[1]), "/",
                       substr(outfilepath, 1, pos1-2),
                       names(rst),
                       substr(outfilepath, pos2+1, nchar(outfilepath)))

  foreach(i = raster::unstack(rst), j = as.list(outfilepath)) %dopar% {
    raster::writeRaster(i, filename = j, format = "GTiff", overwrite = TRUE)
  }

  writeRaster(rst_fn, filename = rst_fn_names,
              format = "GTiff", bylayer = TRUE, overwrite = TRUE)

  return(rst)
}




