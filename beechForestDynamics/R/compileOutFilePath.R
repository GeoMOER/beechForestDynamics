#' Write name function
#' @title writeName
#' @aliases writeName
#' @author Santowski, A. & C. Weber
#' @description Creates new folder structure and file-names for output files constructed in other functions.
#'
#' @param input_filepath List of input files
#' @param output_subdirectory output directory
#' @param prefix Set a new prefix to file-name (default = NA) - prefix or suffix must be set
#' @param suffix Set a new suffix to filename (default = NA) - prefix or suffix must be set
#'
#' @export compileOutFilePath

compileOutFilePath = function(input_filepath, output_subdirectory, prefix=NA, suffix=NA){

  outputpath = base::paste0(dirname(dirname(input_filepath[[1]])), "/", output_subdirectory, "/")
  if(!dir.exists(outputpath))
    dir.create(outputpath)

  if (!is.na(prefix)){
    prefix = paste0(prefix, "_")
  } else {
    prefix = ""
  }
  if (!is.na(suffix)){
    suffix = paste0("_", suffix)
  } else {
    suffix = ""
  }

    outfilepath = lapply(input_filepath, function(ifp){
      ifp = basename(ifp)
      ext_pos = base::regexpr("\\.[^\\.]*$", ifp)
      ext = base::substr(ifp, ext_pos, nchar(ifp))
      outfile = paste0(prefix, base::substr(ifp, 1, ext_pos-1), suffix, ext)
      outfilepath = paste0(outputpath, outfile)
      return(outfilepath)
    })

  return(outfilepath)
}

