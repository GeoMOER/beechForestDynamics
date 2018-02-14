#' Compile raw test data
#'
#' @title compileRawTestData
#' @aliases compileRawTestData
#'
#' @author T. Nauss
#'
#' @description Compile raw test data for this package
#'

compileRawTestData = function(){

  inpath = "F:/modis_carpathian_mountains/data/modis/tiles/c0001-0511_r0001-0522/"
  outpath = "F:/modis_carpathian_mountains/data_small/modis/tiles/c0001-0511_r0001-0522/"
  dirs = list.dirs(inpath)[-1]

  for(dir in dirs){
    files = list.files(dir, pattern = glob2rx("*.tif"), full.names = TRUE)
    lf = length(files)
    if(lf > 0){
      rstck = raster::stack(files)
      rstck = raster::crop(rstck, raster::extent(rstck, 1, 15, 1, 15))
      outfilepath = paste0(outpath, substr(dir, attr(regexpr(inpath, dir),"match.length")+1, nchar(dir)))

      if (!dir.exists(outfilepath))
        dir.create(outfilepath, recursive = TRUE)

      outfiles = paste0(outfilepath, "/", basename(files))
      raster::writeRaster(rstck, filename = outfiles, format = "GTiff", bylayer = TRUE, overwrite = TRUE)
    }
  }
}
