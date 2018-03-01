#' @title spatial projection
#' @author A. Santowski & L. Giese
#' @details  Projects a tif files into another projection from a given tif file.
#' @usage spatialProjection=function(rst_from,prst_to,outfilepath)
#' dependent on package "raster" and "rgdal"
#' @param list_rst_from charachter string: List of input tif files
#' @param list_prst_to charachter string: List of designated projected tif files
#' @param outfilepath path where output tif files shall be stored
#' @param areaname three letters shortcut for study area
#' @return geo tif data
#' @export spatialProjection
#' @aliases spatialProjection
#' @examples
#' \dontrun{
#' spatialProjection=function(rst_from,prst_to,outfilepath)
#' }
#'


###function
spatialProjection = function(list_rst_from, list_prst_to, outfilepath, areaname=NULL, a=12, b=15){

  years=unique(as.numeric(substr(basename(path = list_rst_from), a, b)))

  foreach(i=years,
  .export=ls(envir = globalenv()),
  .packages = c("raster", "rgdal")) %dopar%{
    mswep_stack=raster::stack(list_rst_from[which(i==substr(basename(path = list_rst_from), a, b))])

    proj_tile=raster(list_prst_to)

    if (!dir.exists(outfilepath))
      dir.create(outfilepath, recursive = TRUE)

    projectRaster(from=mswep_stack, to=proj_tile,
                  method="bilinear",
                  filename=paste0(outfilepath, "/MSWEP_",areaname, i,"_projected"),
                  format="GTiff", overwrite = T, bylayer=F)

  }

}
