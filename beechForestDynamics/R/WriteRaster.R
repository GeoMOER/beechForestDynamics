#' Write Raster
#' @title WriteRaster
#' @details This is an example function named 'WriteRaster'
#' which generates Rasters.
#' @usage WriteRaster (rst_scl, substr)
#' @param rst_scl Variable, die gestackte Tiffs von scaling, temporal aggregation, deseasoning and trend computation enthält
#' @param rst_fn rst_fn = stack(list.files(subpath, pattern = glob2rx("*.tif"), full.names = TRUE)) aus bug-Funktion
#' @param rst_fn_names enthält Liste von gestackten Tiffs von 1 bis 12
#'
#' @return GeoTiff-Dateien an 1. bis 12.  Position der Liste und von 20. Position bis zum Ende der Liste

#'# Erfasse Anzahl der Elemente aus der Liste mit den gestackten tiffs
#'substr(Datei die reingegeben wird, Startposition, Endposition) wählt Tiffs von 1. bis 12. Position in der Liste aus
#'nchar (Datei die reingegeben wird) gibt an wieviele Files in rst_scl (gestackten Tiffs) sind
#'gsub("was in File steht", " zu was es ersetzt werden soll", Datei um die es geht)

substr1 <- 1 # Startposition des Einlesens aus der Liste der Tiff-Dateien
substr2 <- 12 # Endposition des Einlesenes aus der Liste der Tiff-Dateien
substr3 <- 20  # Startposition des Einlesens aus der Liste der Tiff-Dateien

WriteRaster<- function(rst_scl, substr1, substr2, substr3){
  for (i in substr1){
    for (a in substr2){
      for (b in substr3){
      ##### rst_fn = stack(list.files(subpath, pattern = glob2rx("*.tif"), full.names = TRUE))
      subpath = paste0(path_modis_temp_agg_tiles, basename(dir), "/")
      if (!dir.exists(subpath)) # Wenn directory existiert, darein schreiben, ansonsten
        dir.create(subpath, recursive = TRUE) # directory erstellen

      #Erstellen von rst_fn_names: enthält rst_scl (gestackte Tiffs) von der 1. bis zur 12. Position der Liste
      rst_fn_names = paste0(substr(names(rst_scl),i, a), # Nimmt Tiffs von 1. bis zur 12. Position der Liste
                      names(rst_fn), substr(names(rst_scl),
                                            b, nchar(names(rst_scl)))) # startet bei "substr3" und geht bis Ende der Liste der Tiff-Dateien
      rst_fn_names = gsub("\\.\\.", ".", rst_fn_names) # gsub ersetzt ".." in filename zu "."
      rst_fn_names = paste0(subpath, rst_fn_names) # Pfad, an dem Tiffs gespeichert werden sollen
      writeRaster(rst_fn, filename = rst_fn_names,
            format = "GTiff", bylayer = TRUE, overwrite = TRUE) # Tiffs abspeichern
      }
    }
  }
}



##Funktionsaufruf
WriteRaster (rst_scl = rst_scl,
             substr1 = 1,
             substr2 = 12,
             substr3 = 20)
