##### deseason ####
#'@title remove seasonality from raster time series
#'@description removes seasonality from time series of scaled, temporal aggregated and filled (= NA-free) rasters (f.ex. NDVI-time-series)
#'@usage input:
#'       files = list.files(paste0(path_modis_filled_tiles), pattern = "FLD_.*//.tif$", full.names = TRUE)
#'       rst_fn_filled = stack(files)
#'
#'       dependent of package "raster" and "remote"
#'
#'@examples
#' /dontrun{
#' deseason(start_char= "2003001", end_char = "2017001", rst_fn_filled = rst_fn_filled,
#'          out_path = paste0(path_modis_deseason_tiles), cycle.window = 24L)
#'          with no default
#'          }
#'
#'@format  XXX_char syntax "YYYYDDD" Year and Day of the year
#'         start_char = "2003001" = 1. Jan 2003 (or "2003365" would be 31. Dec 2003)
#'         end_char = "2017001" = 1. Jan 2017
#'@param rst raster stack of scaled, temporal aggregated and filled (= NA-free) rasters = output of function "fill_gaps_lin"
#'                    following folder structure of 00_set_environment.R this would mean: all rasters in path "path_modis_filled_tiles"
#'@param out_path path to folder, where output should be stored
#'                following folder structure of 00_set_environment.R this would mean: out_path = paste0(path_modis_deseason_tiles)
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schnell, Laura Giese

deseason = function(rst, out_path) {
  rst = remote::deseason(rst)
  out_path -> subpath
  if (!dir.exists(subpath))
    dir.create(subpath, recursive = TRUE)
  }

