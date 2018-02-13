#' Bug
#' @title Bug
#' @detail
#' @param rst_doy Variable, die gestackte Tiffs "16_days_composite_day_of_the_year" enth?lt
#' @param rst_scl Variable, die gestackte Tiffs von scaling, temporal aggregation, deseasoning and trend computation enth?lt
#'
#' @return rst_fn Raster, das sich aus dem Funktionsaufruf temporalComposite() ergbit

#'#
#'substr(Datei die reingegeben wird, Startposition, Endposition) W?hlt Tiffs von 1. bis 22. Position der Liste aus
#'extractDate
#'temporalComposite


# names(rst_doy) = substr(names(rst_doy), 1, nchar(names(rst_doy))-22)
# names(rst_doy) = paste0(substr(names(rst_doy), 1, 17), "h18v03.005.2010239071130.hdf")
#
# layer_dates = extractDate(rst_scl, pos1 = 13, pos2 = 19, asDate =TRUE)$inputLayerDates
#
# rst_fn = temporalComposite(x = rst_scl, y = rst_doy,
#                            timeInfo = layer_dates, interval = "fortnight",
#                            fun = max, na.rm = TRUE, cores = 4L)
