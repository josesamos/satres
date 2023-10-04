
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
      full.names = TRUE,
      ignore.case = TRUE
    )
  nexus <- get_nexus(out_dir)
  for (f in lf) {
    file_name <- basename(f)
    n <- nchar(file_name)
    name <- substr(file_name, 1, n - 4)
    r1 <- terra::rast(f)
    r2 <- terra::aggregate(r1, fact = factor)
    terra::writeRaster(r2,
                       paste0(out_dir, nexus, file_name, '.TIF'),
                       filetype = "GTiff",
                       overwrite = TRUE)
  }
  lf
}


#' Get dimensions
#'
#' Get all dimensions in vector form to check them.
#'
#' @param sr A `satres` object.
#'
#' @return A string vector.
#'
#' @keywords internal
sat_dimensions <- function(sr) {
  res <- NULL
  for (n in names(sr$bands)) {
    res <- c(res,
             terra::ncol(sr$bands[[n]]),
             terra::nrow(sr$bands[[n]]),
             terra::nlyr(sr$bands[[n]]))
  }
  res
}


#' Get names
#'
#' Get all names in vector form to check them.
#'
#' @param sr A `satres` object.
#'
#' @return A string vector.
#'
#' @keywords internal
sat_names <- function(sr) {
  res <- NULL
  for (n in names(sr$bands)) {
    res <- c(res, n, names(sr$bands[[n]]))
  }
  res
}

#' Get minmax
#'
#' Get all minmax values in vector form to check them.
#'
#' @param sr A `satres` object.
#'
#' @return A string vector.
#'
#' @keywords internal
sat_minmax <- function(sr) {
  res <- NULL
  for (n in names(sr$bands)) {
    res <-
      c(res, n, as.vector(round(terra::minmax(sr$bands[[n]], compute = FALSE), 2)))
  }
  res
}



