#' Write Raster
#' @title WriteRaster
#' @details This is an example function named 'WriteRaster'
#' which generates Rasters.
#' @usage WriteRaster (rst_scl, substr)
#' @param rst_scl Variable, die gestackte tiffs von Scaling, temporal aggregation, deseasoning and trend computation enthält
#' @param rst_fn rst_fn = stack(list.files(subpath, pattern = glob2rx("*.tif"), full.names = TRUE)) aus bug-Funktion
#' @param rst_fn_names enthält list von gestackten tiffs von 1 bis 12
#' 
#' @return geotiff-dateien von 1 bis 12 und von 20 bis zum Ende

#'# Erfasse Anzahl der Elemente aus der Liste mit den gestackten tiffs
#'substr(Datei die reingegeben wird, Startposition, Endposition) wählt tiffs von 1 bis 12 aus
#'nchar (Datei die reingegeben wird) gibt an wieviele files in rst_scl (gestackten tiffs) sind
#'gsub("was in file steht", " zu was es ersetzt werden soll", Datei um die es geht) 

substr1 <- 1 # Startposition des Einlesens aus der Liste der tiff-Dateien
substr2 <- 12 # Endposition des Einlesenes aus der Liste der tiff-Dateien
substr3 <- 20  # Startposition des Einlesens aus der Liste der tiff-Dateien

WriteRaster<- function(rst_scl, substr1, substr2, substr3){
  for (i in substr1){
    for (a in substr2){
      for (b in substr3){
      ##### rst_fn = stack(list.files(subpath, pattern = glob2rx("*.tif"), full.names = TRUE)) 
      subpath = paste0(path_modis_temp_agg_tiles, basename(dir), "/")
      if (!dir.exists(subpath)) # Wenn directory existiert, darein schreiben, sonst 
        dir.create(subpath, recursive = TRUE) # directory erstellen

#Erstellen von rst_fn_names: enthält rst_scl (gestackte Tiffs) von 1 bis 12
      rst_fn_names = paste0(substr(names(rst_scl),i, a), # nimmt tiffs von 1 bis 12  
                      names(rst_fn), substr(names(rst_scl),
                                            b, nchar(names(rst_scl)))) # startet bei substr3 und geht bis Ende der Liste der tiff-Dateien
      rst_fn_names = gsub("\\.\\.", ".", rst_fn_names) # gsub ersetzt ".." in filename zu "."
      rst_fn_names = paste0(subpath, rst_fn_names) # Pfad an dem Tiffs gespeichert werden sollen
      writeRaster(rst_fn, filename = rst_fn_names,
            format = "GTiff", bylayer = TRUE, overwrite = TRUE) # Tiffs abspeichern
      }
    }
  }
}


WriteRaster (rst_scl = rst_scl,
             substr1 = 1,
             substr2 = 12,
             substr3 = 20)
