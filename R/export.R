

#' As `terra` `SpatRaster` class
#'
#' Returns the multi-band raster of the indicated spatial resolution as an object
#' of class `SpatRaster` from package `terra`
#'
#' @param sr A `satres` object.
#' @param res A string, spatial resolution.
#'
#' @return A vector of strings.
#'
#' @family satellite exportation
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "satres")
#' sr <- satres(dir = esa,
#'              out_dir = tempdir())
#'
#' r <- sr |>
#'      as_SpatRaster("r1000m")
#'
#' @export
as_SpatRaster <- function(sr, res)
  UseMethod("as_SpatRaster")


#' @rdname as_SpatRaster
#' @export
as_SpatRaster.satres <- function(sr, res = NULL) {
  stopifnot("A spatial resolution must be indicated." = !is.null(res))
  stopifnot("The spatial resolution is not available." = res %in% names(sr$bands))
  sr$bands[[res]]
}



#' Save multi-band rasters according to their spatial resolution
#'
#' Saves multi-band raster files of the object according to its spatial
#' resolution. The file names correspond to the resolution of each one.
#'
#' They are stored in the folder that is indicated or, if none is indicated, in
#' the folder that was used to create the object.
#'
#' @param sr A `satres` object.
#' @param out_dir A string, output folder.
#' @param only_show_files A boolean, only show the files that would be created,
#' not create them.
#'
#' @return A vector of strings, name of the saved files.
#'
#' @family satellite exportation
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "satres")
#' sr <- satres(dir = esa)
#' f <- sr |>
#'      save_by_resolution(only_show_files = TRUE)
#'
#' @export
save_by_resolution <- function(sr, out_dir, only_show_files)
  UseMethod("save_by_resolution")


#' @rdname save_by_resolution
#' @export
save_by_resolution.satres <- function(sr, out_dir = NULL, only_show_files = FALSE) {
  if (is.null(out_dir)) {
    out_dir <- sr$out_dir
  }
  nexus <- get_nexus(out_dir)
  res <- NULL
  for (n in names(sr$bands)) {
    file <- paste0(out_dir, nexus, n, ".tif")
    res <- c(res, file)
    if (!only_show_files) {
      terra::writeRaster(sr$bands[[n]], file, filetype = "GTiff", overwrite = TRUE)
    }
  }
  res
}

