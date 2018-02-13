#' trend
#' @title trend
#' @details rank correlation coefficient Tau
#' need raster and gimms package
#' @usage trend(file_in, p, prewhitening, method, filename)
#' @param x rasterstack
#' @param p, numeric, defaults to 0.001. Significance level to be tested
#' @param prewhitening,logical, If TRUE (default), pre-whitening is applied prior to the Mann-Kendall trend test.
#' @param method The prewhitening method to apply ("yuepilon", "zhang")
#' @param filename writes output as filename in filename(dir)
#' @return rst_stau; geo-tiff data
#' @author HammerLe, Kleebaue

trend <- function(x, p, prewhitening, method, filename){

   #compute significantTau for input files
    rst_stau = gimms::significantTau(x = x,
                                     p = p,
                                     prewhitening = prewhitening,
                                     method = method,
                                     filename = filename)


    # store data in .tif-file
    raster::writeRaster(rst_stau, filename = filename,
                        format = "GTiff",
                        overwrite = TRUE)

   rst_stau <- raster::stack(rst_stau)
   return(rst_stau)
}
