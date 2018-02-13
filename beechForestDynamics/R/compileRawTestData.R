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

raster::writeRaster(rstck, filename = outfiles,
                                      format = "GTiff", bylayer = TRUE, overwrite = TRUE)
inpath = "C:/Users/tnauss/permanent/edu/msc-phygeo-environmental-observations/data/modis"
outpath = paste0(dirname(inpath), "/testdata_bFD")
dirs = list.dirs(path)[-1]
dirs = dirs[-grep("mktrends", dirs)]

  for(dir in dirs){
      files = list.files(dir, pattern = glob2rx("*.tif"), full.names = TRUE)
      if(length(files) > 0){
          files = files[1:3]
          rstck = raster::stack(files)
          rstck = raster::crop(rstck, raster::extent(rstck, 1, 15, 1, 15))
          outfilepath = paste0(outpath,
                                                          substr(dir, attr(regexpr(inpath, dir),"match.length")+1, nchar(dir)))
          if (!dir.exists(outfilepath))
              dir.create(outfilepath, recursive = TRUE)
          outfiles = paste0(outfilepath, "/", basename(files))
          raster::writeRaster(rstck, filename = outfiles,
                                                format = "GTiff", bylayer = TRUE, overwrite = TRUE)
        }
    }
}
