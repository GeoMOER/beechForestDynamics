##### deseason ####
# files = list.files(subpath, pattern = "^.*_NDVI_.*\\.tif$", full.names = TRUE)
# rst_fn_filled = stack(files)
# start_char = "2003001"
# end_char = "2017001"
#'@format  XXX_char sytanx "YYYY|TAGDESJAHRES 3stellig"
#'@param start_char charakter den start beschreibend
#'@param end_char charakter das ende beschreibend
#'@param rst_fn_filled wird von einer anderen funktion übergeebn
#'@param out_path speicherpfad
#'@param cycle.window als integer (mit "L" am ende der zahl bsp 12L) angeben, bei 12L werden monatliche annomalien errechnet bei 24L halbmonatliche bei 6L halbjährliche usw

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
