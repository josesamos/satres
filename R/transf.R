#' Clip all the bands based on a polygon
#'
#' Clips all bands of each spatial resolution according to the given polygon.
#'
#' It performs the operation independently of the CRS of the polygon and preserves
#' the CRS of the bands.
#'
#' @param sr A `satres` object.
#' @param polygon A `sf` polygon layer.
#'
#' @return A `satres` object.
#'
#' @family satellite transformation
#' @seealso \code{\link{satres}}
#'
#' @examples
#'
#' file <- system.file("extdata", "lanjaron.gpkg", package = "satres")
#' lanjaron <- sf::st_read(file, layer = "lanjaron_bbox", quiet = TRUE)
#'
#' esa <- system.file("extdata", "esa", package = "satres")
#' sr <- satres(dir = esa) |>
#'      clip_bands(polygon = lanjaron)
#'
#' @export
clip_bands <- function(sr, polygon)
  UseMethod("clip_bands")

#' @rdname clip_bands
#' @export
clip_bands.satres <- function(sr, polygon) {
  first <- TRUE
  for (b in names(sr$bands)) {
    if (first) {
      s <- sf::st_transform(polygon, terra::crs(sr$bands[[b]]))
      first <- FALSE
    }
    r <- terra::crop(sr$bands[[b]], s)
    sr$bands[[b]] <- terra::mask(r, s)
  }
  sr$out_dir <- NULL
  sr$virtual_files <- NULL
  sr
}


#' Select bands
#'
#' Select the bands of an object based on spatial resolution and band name.
#'
#' @param sr A `satres` object.
#' @param res A string, spatial resolution.
#' @param bands A string, band name.
#'
#' @return A `satres` object.
#'
#' @family satellite transformation
#' @seealso \code{\link{satres}}
#'
#' @examples
#'
#' file <- system.file("extdata", "lanjaron.gpkg", package = "satres")
#' lanjaron <- sf::st_read(file, layer = "lanjaron_bbox", quiet = TRUE)
#'
#' esa <- system.file("extdata", "esa", package = "satres")
#' sr <- satres(dir = esa) |>
#'      select_bands(res = c("r2000m", "r6000m"), bands = c("B02", "B03", "B04"))
#'
#' @export
select_bands <- function(sr, res, bands)
  UseMethod("select_bands")

#' @rdname select_bands
#' @export
select_bands.satres <- function(sr, res = NULL, bands = NULL) {
  if (!(is.null(res) & is.null(bands))) {
    res <- check_spatial_resolution(sr, res)
    bands <- check_bands(res, bands)
    for (r in names(sr$bands)) {
      if (!(r %in% res)) {
        sr$bands[[r]] <- NULL
      }
    }
    for (r in res) {
      b <- intersect(names(sr$bands[[r]]), bands)
      sr$bands[[r]] <- sr$bands[[r]][[b]]
    }
  }
  sr
}

