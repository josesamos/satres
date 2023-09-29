
include_filename = FALSE

file <- "datos/usgs/LC08_L1TP_200034_20230924_20230924_02_RT.tar"
file <- "LC08_L1TP_200034_20230924_20230924_02_RT.tar"

file <- "datos/esa/S2A_MSIL2A_20230905T105621_N0509_R094_T30SVF_20230905T170700.zip"

file <- c("datos/usgs/LC08_L1TP_200034_20230924_20230924_02_RT.tar",
          "datos/esa/S2A_MSIL2A_20230905T105621_N0509_R094_T30SVF_20230905T170700.zip")


untz_file <- function(file, out_dir = NULL, include_filename = TRUE) {

  # si el archivo es una carpeta, tratar todos los zip y tar de la misma

  file_name <- basename(file)
  path_name <- dirname(file)
  if (is.null(out_dir)) {
    out_dir <- path_name
  }
  if (length(out_dir) < length(file)) {
    l <- length(out_dir)
    for (i in (l + 1):length(file)) {
      out_dir[i] <- out_dir[l]
    }
  }
  n <- nchar(file_name)
  extension <- substr(file_name, n - 3, n)
  if (include_filename) {
    name <- substr(file_name, 1, n - 4)
  } else {
    name <- ""
  }
  for (i in 1:length(extension)) {
    if (extension[i] == ".tar" | extension[i] == ".TAR") {
      untar(file[i], exdir = paste0(out_dir[i], "/", name[i]))
    } else if (extension[i] == ".zip" | extension[i] == ".ZIP") {
      unzip(file[i], exdir = paste0(out_dir[i], "/", name[i]))
    }
  }
}
