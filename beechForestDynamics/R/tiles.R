#'tiles
#'@title Create tiles
#'@description Subset MODIS files into tiles  
#'@examples
#' \dontrun{
#' tiles(p = c("NDVI.tif$"), q= c("composite_day_of_the_year.tif$"), r = c("Quality.tif$"), path_modis_quality_checked = path_modis_quality_checked, 
#' path_modeis_prj = path_modeis_prj, tilenbr = c(12,10), overlap = 10, outpath_NDVI = path_modis_quality_checked_tiles, outpath_daynumber = path_modis_doy_tiles, 
#' outpath_qualitycheck = path_modis_qua_tiles
#'  )
#'          }
#'@param p names to identify raster file of NDVI
#'@param q names to identify raster file of composite day of the year
#'@param r names to identify raster file of Quality
#'@param path_modis_quality_checked name of path directed to the raster files of quality checked data
#'@param path_modis_prj name of path directed to the raster files
#'@param tilenbr size of tiles
#'@param overlap overlapt of the creating tiles
#'@param outpath_NDVI outpath of quality checked tiles
#'@param outpath_daynumber outpath of composite day of the year tiles
#'@param outpath_qualitycheck outpath of the quality layers
#'
#'@references Nauss, T., Detsch, F.
#'@author Johannes Schmidt, Tiziana Koch
tiles <- function(p, q, r, path_modis_quality_checked, path_modis_prj, tilenbr, overlap, outpath_NDVI, outpath_daynumber, outpath_qualitycheck){
  
  ndvi.rst.qa = raster::stack(list.files(path_modis_quality_checked, pattern = p, full.names = TRUE))
  satelliteTools::tileRaster(raster = ndvi.rst.qa, tilenbr, overlap, outpath_NDVI)
  
  ndvi.rst.doy = raster::stack(list.files(path_modis_prj, pattern = p, full.names = TRUE))
  satelliteTools::tileRaster(raster = ndvi.rst.doy, tilenbr, overlap, outpath_daynumber)
  
  ndvi.rst.doy = raster::stack(list.files(path_modis_prj, pattern = p, full.names = TRUE))
  satelliteTools::tileRaster(raster = ndvi.rst.doy, tilenbr, overlap, outpath_qualitycheck)
 
}