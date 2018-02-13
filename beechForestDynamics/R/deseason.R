##### deseason ####
<<<<<<< HEAD
# files = list.files(subpath, pattern = "^.*_NDVI_.*\\.tif$", full.names = TRUE)
# rst_fn_filled = stack(files)
# start_char = "2003001"
# end_char = "2017001"
#'@format  XXX_char sytanx "YYYY|TAGDESJAHRES 3stellig"
#'@param start_char charakter den start beschreibend
#'@param end_char charakter das ende beschreibend
#'@param rst_fn_filled wird von einer anderen funktion übergeben
#'@param out_path speicherpfad
#'@param cycle.window als integer (mit "L" am ende der zahl bsp 12L) angeben, bei 12L werden monatliche annomalien errechnet bei 24L halbmonatliche bei 6L halbjährliche usw
=======
#'@title remove seasonality from raster time series
#'@description removes seasonality from time series of scaled, temporal aggregated and filled (= NA-free) rasters (f.ex. NDVI-time-series)
#'@usage input:
#'       files = list.files(paste0(path_modis_filled_tiles), pattern = "FLD_.*\\.tif$", full.names = TRUE)
#'       rst_fn_filled = stack(files)
#'
#'       dependent of package "raster" and "remote"
#'
#'@examples
#' \dontrun{
#' deseason(start_char= "2003001", end_char = "2017001", rst_fn_filled = rst_fn_filled,
#'          out_path = paste0(path_modis_deseason_tiles), cycle.window = 24L)
#'          with no default
#'          }
#'
#'@format  XXX_char sytanx "YYYYDDD" Day of the year
#'         start_char = "2003001" = 1. Jan 2003 (or "2003365" would be 31. Dec 2003)
#'         end_char = "2017001" = 1. Jan 2017
#'@param start_char charakter which describes start date in Julian Days (Day of the year)
#'@param end_char charakter which describes end date in Julian Days (Day of the year)
#'@param rst_fn_filed raster stack of scaled, temporal aggregated and filled (= NA-free) rasters = output of function "fill_gaps_lin"
#'                    following folder structure of 00_set_environment.R this would mean: all rasters in path "path_modis_filled_tiles"
#'@param out_path path to folder, where output should be stored
#'                following folder structure of 00_set_environment.R this would mean: out_path = paste0(path_modis_deseason_tiles)
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
>>>>>>> 8a0a91726314c03a2d2252b4fd44d57f873affa1

deseason = function(start_char,
                    end_char,
                    rst_fn_filled,
                    out_path,
                    cycle.window) {
  start = grep(start_char, names(rst_fn_filled))
  end = grep(end_char, names(rst_fn_filled)) - 1

  rst_dsn = remote::deseason(rst_fn_filled[[start:end]],
                             cycle.window,
                             use.cpp = TRUE)

  names(rst_dsn) = paste0("DSN_", names(rst_fn_filled[[start:end]]))
  #subpath = paste0(path_modis_deseason_tiles, basename(dir), "/")
  out_path -> subpath
  if (!dir.exists(subpath))
    dir.create(subpath, recursive = TRUE)
  fls_dsn = paste0(subpath, names(rst_dsn))

  lst_dsn = foreach(i = raster::unstack(rst_dsn), j = as.list(fls_dsn)) %do% {
    raster::writeRaster(i,
                        filename = j,
                        format = "GTiff",
                        overwrite = TRUE)
  }

}
