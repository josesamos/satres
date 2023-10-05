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


#' Select bands by spatial resolution and name
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
    bands <- check_bands(sr, res, bands)
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


#' Merge objects that are tiles
#'
#' Merge objects whose bands are tiles of a mosaic.
#'
#' The objects must have the same CRS, spatial resolution and bands.
#'
#' @param sr A `satres` object.
#' @param ... `satres` objects.
#'
#' @return A `satres` object.
#'
#' @family satellite transformation
#' @seealso \code{\link{satres}}
#'
#' @examples
#'
#' esa_f <- system.file("extdata", "esa/f", package = "satres")
#' esa_g <- system.file("extdata", "esa/g", package = "satres")
#' sr2 <- satres(dir = esa_f)
#' sr <- satres(dir = esa_g) |>
#'   merge_tiles(sr2)
#'
#' @export
merge_tiles <- function(sr, ...)
  UseMethod("merge_tiles")

#' @rdname merge_tiles
#' @export
merge_tiles.satres <- function(sr, ...) {
  x <- list(...)
  if (length(x) > 0) {
    res <- names(sr$bands)
    for (i in 1:length(x)) {
      sr2 <- x[[i]]
      if (length(setdiff(res, names(sr2$bands))) != 0) {
        stop("All objects must have the same spatial resolution.")
      }
      for (r in res) {
        if (length(setdiff(names(sr$bands[[r]]), names(sr2$bands[[r]]))) != 0) {
          stop("All objects must have the same bands.")
        }
        if (terra::crs(sr$bands[[r]]) != terra::crs(sr2$bands[[r]])) {
          stop("All objects must have the same CRS.")
        }
      }
    }
    result <- vector(mode = "list", length = length(res))
    names(result) <- res
    for (r in res) {
      res_bands <- vector(mode = "list", length = length(names(sr$bands[[r]])))
      names(res_bands) <- names(sr$bands[[r]])
      for (b in names(sr$bands[[r]])) {
        inst <- sprintf("terra::sprc(sr$bands[['%s']][['%s']]", r, b)
        for (i in 1:length(x)) {
          inst <-
            paste0(inst, sprintf(", x[[%d]]$bands[['%s']][['%s']]", i, r, b))
        }
        inst <- paste0(inst, ")")
        srcollection <- eval(parse(text = inst))
        res_bands[[b]] <- terra::merge(srcollection)
      }
      result[[r]] <- transform_to_multiband(res_bands)
    }
    structure(list(
      bands = result,
      out_dir = NULL,
      virtual_files = NULL
    ),
    class = "satres")
  } else {
    sr
  }
}

