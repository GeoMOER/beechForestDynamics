#'@title remove seasonality from raster time series
#'@description removes seasonality from time series of scaled, temporal aggregated and filled (= NA-free) rasters (f.ex. NDVI-time-series)
#'@examples for monthly annomalies
#' \dontrun{
#' deseason(your_raster_stack, outFilePath, 12L)
#'          }
#'@param rstack raster stack of scaled, temporal aggregated and filled (= NA-free) rasters = output of function "fill_gaps_lin"
#'                    following folder structure of 00_set_environment.R this would mean: all rasters in path "path_modis_filled_tiles"
#'@param outFilePath see compileOutFilePath()
#'
#'@param cycle.window as integer (with "L" in the end of the number, f.ex.: 12L),
#'                   12L means monthly anomalies,
#'                   24L := half a months,
#'                   6L := half a year,
#'                   etc.
#'
#'@return writes out deseasonalized rasters
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schnell, Laura Giese


deseason = function(rstack,
                    outFilePath,
                    cycle.window) {


  rst_dsn = remote::deseason(rstack,
                             cycle.window,
                             use.cpp = TRUE)


  if (!dir.exists(outFilePath[1]))
    dir.create(outFilePath[1], recursive = TRUE)


  forech::foreach(i = raster::unstack(rst_dsn), j = as.list(outFilePath)) %do% {
    raster::writeRaster(i,
                        filename = j,
                        format = "GTiff",
                        overwrite = TRUE)
  }
  return(rst_dsn)
}
