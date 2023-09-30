

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
#' @param only_show_files A boolean, only show the files that would be unzipped,
#' not unzip them.
#'
#' @return A vector of strings, name of the processed files.
#'
#' @family satellite previous functions
#' @seealso \code{\link{satres}}
#'
#' @examples
#'
#' # sat_untarzip("dat/usgs")
#'
#' # sat_untarzip("dat/esa")
#'
#' # file <- c("dat/usgs/LC08_L1TP_200034_20230924_20230924_02_RT.tar",
#' #           "dat/esa/S2A_MSIL2A_20230905T105621_N0509_R094_T30SVF_20230905T170700.zip")
#' # sat_untarzip(file)
#'
#' @export
sat_untarzip <- function(file, out_dir = NULL, include_filename = NULL, only_show_files = FALSE) {
  if (length(file) == 1) {
    if (dir.exists(file)) {
      file <-
        list.files(
          path = file,
          pattern = "*.tar|*.zip",
          ignore.case = TRUE,
          full.names = TRUE
        )
    }
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
  name <- rep("", length(file))
  if (!is.null(include_filename)) {
    if (include_filename) {
      name <- substr(file_name, 1, n - 4)
    }
  }
  res <- NULL
  for (i in 1:length(extension)) {
    nexus <- get_nexus(out_dir[i])
    if (extension[i] == ".tar" | extension[i] == ".TAR") {
      if (is.null(include_filename)) {
        name[i] <- substr(file_name[i], 1, n[i] - 4)
      }
      exdir <- paste0(out_dir[i], nexus, name[i])
      if (!only_show_files) {
        utils::untar(file[i], exdir = exdir)
      }
    } else if (extension[i] == ".zip" | extension[i] == ".ZIP") {
      exdir <- paste0(out_dir[i], nexus, name[i])
      if (!only_show_files) {
        utils::unzip(file[i], exdir = exdir)
      }
    } else {
      stop(sprintf("Unsupported file type: %s", extension[i]))
    }
    if (!only_show_files) {
      res <- c(res, file[i])
    } else {
      res <- c(res, sprintf("%s to %s", file[i], exdir))
    }
  }
  res
}


#' Get nexus
#'
#' Given a name, if it ends in "/" the nexus is the empty string, otherwise it
#' is "/".
#'
#' @param name A string.
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
