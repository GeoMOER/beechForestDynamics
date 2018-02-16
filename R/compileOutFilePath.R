#' @title Compile output filepathes
#' @author A. Santowski & C. Weber
#'
#' @description Creates output subdirectory for generated data. Directory will be at the same level like input filepath.
#' It's possible to set different prefix or suffix to filename.
#'
#' @param input_filepathes List of input files
#' @param output_subdirectory output directory (name for output directory)
#' @param prefix Set a new prefix to file-name (default = NA)
#' @param suffix Set a new suffix to filename (default = NA)
#'
#' @export compileOutFilePath
#' @aliases compileOutFilePath
#'
#' @usage
#' my_outfilepathes = compileOutFilePath(input_filepathes, output_subdirectory, prefix=NA, suffix=NA)
#'
#' @examples
#' \dontrun{
#' compileOutFilePath(input_filepathes = "modis_filled_tiles",
#' output_subdiretory = "filled", prefix= "tiles", suffix = "filled")
#' }
#'

compileOutFilePath = function(input_filepathes, output_subdirectory, prefix=NA, suffix=NA){

  outputpath = base::paste0(dirname(dirname(input_filepathes[[1]])), "/", output_subdirectory, "/")
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

    outfilepath = lapply(input_filepathes, function(ifp){
      ifp = basename(ifp)
      ext_pos = base::regexpr("\\.[^\\.]*$", ifp)
      ext = base::substr(ifp, ext_pos, nchar(ifp))
      outfile = paste0(prefix, base::substr(ifp, 1, ext_pos-1), suffix, ext)
      outfilepath = paste0(outputpath, outfile)
      return(outfilepath)
    })

  outfilepath <- unlist(outfilepath)
  return(outfilepath)
}
