<<<<<<< HEAD
#' trend
#' @title trend
#' @details rank correlation coefficient Tau
#' @usage significantTau(x, p, prewhitening, method, filename)
#'
#' @param file_in list of input filenames
#' @param p, numeric, defaults to 0.001. Significance level to be tested
#' @param prewhitening,logical, If TRUE (default), pre-whitening is applied prior to the Mann-Kendall trend test.
#' @param method, character, The prewhitening method to apply,
#' @param filename output filename
#' @return rst_stau, geo-tiff data

#######################################################################################################
##### trend #####


trend <- function(file_in, p, prewhitening, method, filename){

   #compute significantTau for input files
    rst_stau = gimms::significantTau(x = file_in,
                                     p = p,
                                     prewhitening = prewhitening,
                                     method = method,
                                     filename = filename)

    return(rst_stau)

    }

=======
#' trend
#' @title trend
#' @details rank correlation coefficient Tau
#' @usage significantTau(x, p, prewhitening, method, df, filename)
#' @param x, either a Raster object or a numeric vector
#' @param p, numeric, defaults to 0.001. Significance level to be tested
#' @param prewhitening,logical, If TRUE (default), pre-whitening is applied prior to the Mann-Kendall trend test.
#' @param method, character, The prewhitening method to apply,
#' @param df,logical, defaults to FALSE. If TRUE, a data.frame holding the value of Kendall's ?? and the referring significance level.
#' @param filename, character. Optional output filename.
#' @return geo-tiff data

#######################################################################################################
##### trend #####

trend <- function(x, p, prewhitening, method, df, filename){
   #set subpath
  subpath = paste0(path_modis_mktrends_tiles, basename(dir), "/")
      if (!dir.exists(subpath))
      dir.create(subpath, recursive = TRUE)

  #create files for significantTau from rasterstack deseason
    fls_stau = paste0(subpath, "MK_", substr(names(rst_dsn)[1], 1, 25),
                      substr(names(rst_dsn)[length(names(rst_dsn))],
                             18, length(names(rst_dsn))))

   #create vector for tau 1.000 and tau 0.010
     fls_stau = c(paste0(fls_stau, "_tau_1.000.tif"),
                 paste0(fls_stau, "_tau_0.010.tif"))
   #compute significantTau for significance level 1.0 and 0.01
    rst_stau_1000 = significantTau(rst_dsn, p = 1.0,
                                   prewhitening = TRUE, method = "yuepilon",
                                   filename = fls_stau[1])
    rst_stau_0010 = significantTau(rst_dsn, p = 0.01,
                                   prewhitening = TRUE, method = "yuepilon",
                                   filename = fls_stau[2])

    ## Combine trend tiles
    #list tau 1.000 files
    trends_stau_1000 = list.files(path_modis_mktrends_tiles, pattern = glob2rx("*tau_1.000.tif"),
                                  recursive = TRUE, full.names = TRUE)
    #merge tau 1.000 files
    mergeTiledRasters(rasterlist = trends_stau_1000,  overlap = 10, outpath = paste0(path_modis_results, "MK_DSN_FLD_A2003_2016_tau_1.000.tif"))

  }




>>>>>>> 950f29a7635c1ea1898060537737ebfc474c4270
