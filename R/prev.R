

#' Unzip compressed files in tar or zip format
#'
#' Given a vector of compressed file names or the name of a folder containing
#' compressed files, unzip the files to the given output folder. If no output
#' folder is indicated, it is considered the same folder where the input files
#' are.
#'
#' We can indicate whether to include the file name (without the extension) as a
#' folder in the output folder.
#'
#' @param file A string or string vector.
#' @param out_dir A string or string vector, output folder.
#' @param include_filename A boolean, include file name as a folder in the output.
#'
#' @return A boolean.
#'
#' @family previous functions
#'
#' @examples
#'
#' # untarzip("data/usgs")
#'
#' file <- c("data/usgs/LC08_L1TP_200034_20230924_20230924_02_RT.tar",
#'           "data/esa/S2A_MSIL2A_20230905T105621_N0509_R094_T30SVF_20230905T170700.zip")
#' # untarzip(file)
#'
#' @export
untarzip <- function(file, out_dir = NULL, include_filename = TRUE) {
  if (length(file) == 1 & dir.exists(file)) {
    lf <- list.files(path = file, pattern = "*.tar|*.zip", ignore.case = TRUE)
    nexus <- get_nexus(file)
    file <- paste0(file, nexus, lf)
  }
  file_name <- basename(file)
  path_name <- dirname(file)
  n <- nchar(file_name)
  extension <- substr(file_name, n - 3, n)
  if (is.null(out_dir)) {
    out_dir <- path_name
  }
  # vectorial operation
  if (length(out_dir) < length(file)) {
    l <- length(out_dir)
    for (i in (l + 1):length(file)) {
      out_dir[i] <- out_dir[l]
    }
  }
  if (include_filename) {
    name <- substr(file_name, 1, n - 4)
  } else {
    name <- ""
  }
  for (i in 1:length(extension)) {
    nexus <- get_nexus(out_dir[i])
    if (extension[i] == ".tar" | extension[i] == ".TAR") {
      utils::untar(file[i], exdir = paste0(out_dir[i], nexus, name[i]))
    } else if (extension[i] == ".zip" | extension[i] == ".ZIP") {
      utils::unzip(file[i], exdir = paste0(out_dir[i], nexus, name[i]))
    } else {
      stop(sprintf("Unsupported file type: %s", extension[i]))
    }
  }
  TRUE
}


#' Get nexus
#'
#' Given a name, if it ends in "/" the nexus is the empty string, otherwise it
#' is "/".
#'
#' @param df A string.
#'
#' @return A string.
#'
#' @keywords internal
get_nexus <- function(name) {
  l <- nchar(name)
  c <- substr(name, start = l, stop = l)
  if (c == "/") {
    nexus <- ""
  } else {
    nexus <- "/"
  }
  nexus
}
