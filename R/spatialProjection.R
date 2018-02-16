#' @title spatial projection
#' @author A. Santowski & L. Giese
#' @details  Projects a tif files into another projection from a given tif file.
#' @usage spatialProjection=function(rst_from,prst_to,outfilepath)
#' dependent on package "raster" and "rgdal"
#' @param rst_from List of input tif files
#' @param prst_to List of designated projected tif files
#' @param outfilepath path where output tif files shall be stored
#' @return geo tif data 
#' @export spatialProjection
#' @aliases spatialProjection
#' @examples
#' \dontrun{
#' spatialProjection=function(rst_from,prst_to,outfilepath)
#' }
#'
spatialProjectionion=function(rst_from,prst_to,outfilepath){
  for (i in seq(1:length(rst_from))){
    mwesp_stck= raster::stack(rst_from[which(i==substr(basename(path = rst_from),10,13))])#aktuell 16,19
    for (j in seq(1:length(prst_to)))
      
      proj_stck= raster::stack(prst_to[which(i==substr(basename(path = prst_to),10,13))])
    
    projectRaster(from=mwesp_stck, to=proj_stck,
                  method="bilinear", 
                  filename=outputfilepath,format="GTiff", overwrite = T) 
    
  }
}