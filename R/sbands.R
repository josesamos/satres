# sat_band, sat_rest and sat_rest_msk functions

#' Select band files
#'
#' Select the files that correspond to the band selection. Each one is assigned
#' the band identifier as a name.
#'
#' @param files A string vector.
#'
#' @return A list of string vectors.
#'
#' @keywords internal
select_band_files <- function(files) {
  names(files) <- NA
  sel <- rep(FALSE, length(files))
  for (i in 1:length(sat_rest_msk)) {
    r <- grepl(sat_rest_msk[i], files, fixed = TRUE)
    if (sum(r) > 0) {
      sel <- sel | r
    }
  }
  band <- files[!sel]
  rest <- files[sel]
  band <- find_name_to_files(band, sat_band)
  rest <- find_name_to_files(rest, sat_rest)
  l <- list(band, rest)
  names(l) <- c('band', 'rest')
  l
}


#' Get satellite band names
#'
#' Get valid satellite band names.
#'
#' @return A string vector.
#'
#' @keywords internal
sat_band_names <- function() {
  unique(names(sat_band))
}
