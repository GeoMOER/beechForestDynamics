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


  rstack = foreach::foreach(i = raster::unstack(rstack),
                            j = as.list(outfilepathes),
                            k = lapply(seq(ncol(rstack_matrix_sd)), function(i){rstack_matrix_sd[,i]})) %dopar% {
                              i = raster::setValues(i, k)
                              raster::writeRaster(i,
                                                  filename = j,
                                                  format = "GTiff",
                                                  overwrite = TRUE)
                              return(i)
                            }

  rstack = stack(rstack)

  return(rstack)
}
