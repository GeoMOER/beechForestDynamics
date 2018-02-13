#' Additional outlier check
#' @title Additional outlier check
#' @details this is an example function named 'Additional outlier check'
#' which checks for statistical outliers
#' @usage outlier_check(input, lq, uq)
#' @param input, path where input is
#' @param lq, Numeric, lower quantile limit
#' @param uq, Numeric, upper quantile limit
#' @param outfile vector with outfiles
#' @return geo-tiff data

library(doParallel)
#devtools::install_github("environmentalinformatics-marburg/GSODTools")
library(GSODTools)
rstack <- raster::stack(list.files("C:/Users/Marina/Documents/Uni/WiSe17-18/Umweltinformationssysteme/modis/modis_outliers_tiles/c0001-0511_r0001-0522/", pattern=glob2rx("*.tif"), full.names=TRUE))

outlier_check <- function(rstack, outfilepathes){

  #take the matix, go through all rows and calculate the outliers
  #based on the lower and upper quantile
  rstack_matrix = raster::as.matrix(rstack)
  rstack_matrix_sd =
    foreach(i = 1:nrow(rstack_matrix), .packages = "GSODTools",
            .export = ls(envir = globalenv())) %dopar% {
              val = rstack_matrix[i, ]
              if(length(which(!is.na(val))) > 2){
                id = GSODTools::tsOutliers(val,
                                           lower_quantile = lower_quantile,
                                           upper_quantile = upper_quantile,
                                           index = index)
                val[id] = NA
              }
              return(matrix(val, ncol = length(val), byrow = TRUE))
            }

  #bind rows of the outliers together
  rstack_matrix_sd = do.call("rbind", rstack_matrix_sd)
  gc()

  #assign outliers to layer
  for(l in seq(nlayers(rstack))){
    rstack[[l]] = raster::setValues(rstack[[l]], rstack_matrix[, l])

    WriteRaster(rstack, format="GTiff",
                filename=outfilepathes)
  }
}



outlier_check(rstack=rstack,
              outfilepathes = outfilepathes)
