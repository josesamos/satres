
#' Aggregate rasters in a folder
#'
#' Select the files that correspond to the band selection. Each one is assigned
#' the band identifier as a name.
#'
#' @param dir A string or string vector.
#' @param out_dir A string, output folder.
#' @param factor A integer.
#'
#' @return A string vector.
#'
#' @keywords internal
sat_aggregate <- function(dir, out_dir, factor = 100) {
  lf <-
    list.files(
      path = dir,
      pattern = "*.TIF|.jp2",
      recursive = TRUE,
      full.names = TRUE
    )
  for (f in lf) {
    file_name <- basename(f)
    n <- nchar(file_name)
    name <- substr(file_name, 1, n - 4)
    r1 <- terra::rast(f)
    r2 <- terra::aggregate(r1, fact = factor)
    terra::writeRaster(r2,
                       paste0(out_dir, file_name, '.TIF'),
                       filetype = "GTiff",
                       overwrite = TRUE)
  }
  lf
}

