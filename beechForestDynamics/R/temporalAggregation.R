#' @title Temporal aggregation
#' @aliases temporalAggregation
#' @author Santwoski, A. & C. Weber
#' @description The function is a wrapper arround MODIS::temporalComposit.
#' It is hardly usefull since it only adds the writing.
#'
#' @param rstack Raster stack of MODIS composite files
#' @param rstack_doy Raster stack of associated day of year information to rstack
#' @param layer_dates Date vector corresponding to input layers (see MODIS::temporalComposite)
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

temporalAggregation = function(rstack, rstack_doy, layer_dates, outputfilepathes,
                               interval = "fortnight", fun = max, na.rm = TRUE,
                               cores = 4L){

  rst_fn = MODIS::temporalComposite(x = rstack, y = rstack_doy,
                             timeInfo = layer_dates, interval = interval,
                             fun = fun, na.rm = na.rm, cores = cores)

  writeRaster(rst_fn, filename = outputfilepathes,
              format = "GTiff", bylayer = TRUE, overwrite = TRUE)

}




