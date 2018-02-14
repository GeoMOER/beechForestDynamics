#' @title Additional outlier check
#' @details this is an example function named 'Additional outlier check'
#' which checks for statistical outliers
#' @usage outlierCheck(input, lq, uq)
#' @param input, path where input is
#' @param lq, Numeric, lower quantile limit
#' @param uq, Numeric, upper quantile limit
#' @param outfile vector with outfiles
#' @param rstack Raster stack
#' @export outlierCheck
#' @return geo-tiff data


outlierCheck <- function(rstack, outfilepathes, lq, uq){

  #take the matix, go through all rows and calculate the outliers
  #based on the lower and upper quantile
  rstack_matrix = raster::as.matrix(rstack)
  rstack_matrix_sd =
    foreach(i = 1:nrow(rstack_matrix), .packages = "GSODTools",
            .export = ls(envir = globalenv())) %dopar% {
              val = rstack_matrix[i, ]
              if(length(which(!is.na(val))) > 2){
                id = GSODTools::tsOutliers(val,
                                           lower_quantile = lq,
                                           upper_quantile = uq,
                                           index = T)
                val[id] = NA
              }
              return(matrix(val, ncol = length(val), byrow = TRUE))
            }

  #bind rows of the outliers together
  rstack_matrix_sd = do.call("rbind", rstack_matrix_sd)
  gc()


  rstack_temp = rstack
  #assign outliers to layer
  for(l in seq(nlayers(rstack))){
    rstack[[l]] = raster::setValues(rstack[[l]], rstack_matrix_sd[, l])

    writeRaster(rstack[[l]], format="GTiff",
                filename= outfilepathes[l], overwrite=TRUE, bylayer=T)
  }
}
