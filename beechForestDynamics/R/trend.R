#' trend
#' @title trend
#' @details rank correlation coefficient Tau
#' @usage significantTau(x, p, prewhitening, method, filename)
#' @param file_in raster or rasterstack
#' @param p, numeric, defaults to 0.001. Significance level to be tested
#' @param prewhitening,logical, If TRUE (default), pre-whitening is applied prior to the Mann-Kendall trend test.
#' @param method The prewhitening method to apply ("yuepilon", "zhang")
#' @param filename output filename
#' @return rst_stau; geo-tiff data

trend <- function(file_in, p, prewhitening, method, filename){

   #compute significantTau for input files
    rst_stau = gimms::significantTau(x = file_in,
                                     p = p,
                                     prewhitening = prewhitening,
                                     method = method,
                                     filename = filename)

    return(rst_stau)
    }
