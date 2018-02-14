#' @title Remove seasonality from raster time series
#' @author Johannes Schnell, Laura Giese
#'
#' @description Removes seasonality from time series of scaled, temporal
#' aggregated and filled (= NA-free) rasters (f.ex. NDVI-time-series)
#'
#' @param rstack raster stack of scaled, temporal aggregated and filled
#' (= NA-free) rasters = output of function "fill_gaps_lin" following folder
#' structure of 00_set_environment.R this would mean: all rasters in path
#' "path_modis_filled_tiles"
#'
#' @param outFilePath see compileOutFilePath()
#'
#' @param cycle.window as integer (with "L" in the end of the number, f.ex.: 12L),
#' 12L means monthly anomalies, 24L := half a months, 6L := half a year, etc.
#'
#' @return writes out deseasonalized rasters
#'
#' @export deseason
#'
#' @references Nauss, T., Detsch, F.
#'
#' @examples
#' \dontrun{
#' deseason(your_raster_stack, outFilePath, 12L)
#' }
#'


deseason = function(rstack,
                    outFilePath,
                    cycle.window = 24L) {


  rst_dsn = remote::deseason(rstack,
                             cycle.window,
                             use.cpp = TRUE)


  if (!dir.exists(dirname(outFilePath[1])))
    dir.create(dirname(outFilePath[1]), recursive = TRUE)


  foreach::foreach(i = raster::unstack(rst_dsn), j = as.list(outFilePath)) %do% {
    raster::writeRaster(i,
                        filename = j,
                        format = "GTiff",
                        overwrite = TRUE)
  }
  return(rst_dsn)
}
