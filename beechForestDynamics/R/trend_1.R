#' trend
#' @title trend
#' @details rank correlation coefficient Tau
#' @usage significantTau(x, p, prewhitening, method, df, filename)

#' @param p, numeric, defaults to 0.001. Significance level to be tested
#' @param prewhitening, logical, If TRUE (default), pre-whitening is applied prior to the Mann-Kendall trend test.
#' @param method, character, The prewhitening method to apply,
#' @param df, logical, defaults to FALSE. If TRUE, a data.frame holding the value of Kendall's ?? and the referring significance level.
#' @param file_in list of all filenames
#' @param file_fls_stau blablabla
#' @return geo-tiff data

#######################################################################################################
##### trend #####
#install.packages("gimms")


file_in <- list.files("C:/Users/Marina/Documents/Uni/WiSe17-18/Umweltinformationssysteme/modis/modis_deseason_tiles/c0001-0511_r0001-0522/")
file_fls_stau <- ("C:/Users/Marina/Documents/Uni/WiSe17-18/Umweltinformationssysteme/modis/modis_deseason_tiles/c0001-0511_r0001-0522/test.tif")

trend <- function(p, prewhitening, method, df, file_in, file_fls_stau){

   #compute significantTau for significance levels
    rst_stau = gimms::significantTau(file_in=file_in,
                              p = p,
                              prewhitening = prewhitening,
                              method = method,
                              file_fls_stau = file_fls_stau)
}

trend(file_in=file_in,
      p=1.0,
      prewhitening = T,
      method = "yuepilon",
      file_fls_stau = file_fls_stau)

#Raster soll unter Pfad "file_fls_stau" gespeichert werden (writeRaster)



