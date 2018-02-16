#' @title Check results
#'
#' @details Check results from functions against another dataset. It is assumed
#' that the path to the test data can be compiled by substituting a certain
#' part of the path to the result file. E.g. \code{.../data_test/fun/sample.file}
#' and \code{.../data/fun/sample.file}.
#'
#'
#' @param file Path to the result file which should be tested
#' @param subpath_file Subpath of the result file which should be replaced
#' @param subpath_test Subpath which should be inserted to reach the test file
#'
#' @return Information (TRUE/FALSE) if the data is identical.
#'
#' @export checkResults
#'
#' @examples
#' \dontrun{
#' checkResults("/data_test/fun/sample.file", "data_test", "data")
#' }

checkResults = function(file, subpath_file, subpath_test){
  testfile = raster(gsub(subpath_file, subpath_test, file))
  file = raster(file)
  results = getValues(file/testfile)
  if(all(results == 1)){
    return(TRUE)
  } else {
    return(list(summary(results), results))
  }
}
