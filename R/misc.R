
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
    res <- c(res, n,
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



