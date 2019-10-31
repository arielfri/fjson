#' Fast JSON reader
#'
#' fjson reads a JSON file into a data table.
#'
#' @name fjson
#'
#' @usage fjson(path = NULL, text = NULL, nThread = NULL, verbose = TRUE)
#'
#' @param path path to a JSON file.
#' @param text input data itself as a character vector of one or more lines, for example as returned by readLines().
#' @param nThread number of threads to use. By default it is set to the number of cores minus one.
#' @param verbose logical. Should progress indicators be supressed?
#'
#' @return data.table
#' @author Ariel Fridman
#'
#' @details
#' fjson is a fast reader for JSON files. It is intnded for reading in JSON data that do not contain arrays, so it can be nicely organized into a table. The JSON file should have line breaks to separate each row of data.
#'
#' It offers several advantages over existing packages:
#' \enumerate{
#'   \item It is substantially faster, especially for large files. This is because the computationally intensive operations are parallelized using the parallel package, and are implemented in C++ using the Rcpp package.
#'   \item Data type conversions are automatically done based on the parse_guess() function from the readr package.
#'   \item It includes progress indicators.
#' }
#'
#' @examples
#' library(fjson)
#' df <- fjson(path = "file.json")
#' df <- fjson(text = c('{"time":"2019-01-01 00:00:00 UTC","character":"abc","integer_quoted":"30","integer_unquoted":30,"double_unquoted":30.333333}',
#' '{"character":"abcd","time":"2019-01-02 00:00:00 UTC","integer_quoted":"10","integer_unquoted":10,"double_unquoted":10.25}'))
#'
#' @useDynLib fjson
#' @import data.table parallel readr
#' @importFrom Rcpp sourceCpp
NULL
#' @export

fjson <- function(path = NULL, text = NULL, nThread = NULL, verbose = TRUE) {

  if (is.null(path) && is.null(text)) stop("No input data provided\n")
  if (is.null(path)==FALSE && is.null(text)==FALSE) stop("Either path or text should be specified, not both\n")

  if (is.null(nThread)) nThread <- parallel::detectCores()[1]-1

  if (is.null(path)==FALSE) {
    if (verbose) cat("Reading in data\n")
    out <- readLines(path)
  } else {
    if (is.character(text)==FALSE) stop("text must be a character vector\n")
    out <- text
  }

  if (nThread>1) {
    cl <- parallel::makeCluster(nThread)

    if (verbose) cat("Processing data\n")
    out <- parallel::parLapply(cl = cl,
                               X = out,
                               fun = fjson_parse_string)
    parallel::stopCluster(cl)
    out <- data.table::rbindlist(out, fill = TRUE)
  } else {
    if (verbose) cat("Processing data\n")
    out <- data.table::rbindlist(fjson_parse_vector(out), fill = TRUE)
  }
  cols <- colnames(out)
  return(out[, (cols):=lapply(.SD, readr::parse_guess, guess_integer = TRUE), .SDcols = cols][])
}
