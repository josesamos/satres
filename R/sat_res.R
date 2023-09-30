

sat_res <- function(dir, out_dir = NULL) {
  files <- NULL
  for (d in dir) {
    lf <- list.files(path = d, pattern = "*.TIF|.jp2", recursive = TRUE, full.names = TRUE)
    files <- c(files, lf)
  }
  if (is.null(out_dir)) {
    out_dir <- dir
  }
  # vectorial operation
  if (length(out_dir) < length(dir)) {
    l <- length(out_dir)
    for (i in (l + 1):length(dir)) {
      out_dir[i] <- out_dir[l]
    }
  }
  file_name <- basename(files)
  n <- nchar(file_name)
  extension <- substr(file_name, n - 3, n)
}
