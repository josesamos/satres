.onLoad <- function(libname, pkgname) {
  utils::data(
    "sat_rest",
    "sat_band",
    "sat_rest_msk",
    package = pkgname,
    envir = parent.env(environment())
  )
}

#' `satres` S3 class
#'
#' Creates a `satres` object from a set of raster files.
#'
#' Given a folder name or a vector of folder names, containing satellite band
#' raster files, creates an object containing all rasters grouped according to
#' their spatial resolution.
#'
#' If there are several rasters of the same area (tiles), it previously merges
#' them to form a single raster of the total area.
#'
#' A working folder where the virtual rasters are created can be indicated as a
#' parameter. Additionally, we indicate whether we wish to process only the
#' bands (B1 to B12) or all available files.
#'
#' @param dir A string or string vector, folder names.
#' @param out_dir A string, output folder.
#' @param only_bands A boolean, include only satellite bands.
#'
#' @return A `satres` object.
#'
#' @family satellite definition
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "satres")
#'
#' sr <- satres(dir = esa,
#'              out_dir = tempdir())
#'
#' sr <- satres(dir = esa,
#'              out_dir = tempdir(),
#'              only_bands = FALSE)
#'
#' @export
satres <- function(dir, out_dir = NULL, only_bands = TRUE) {
  files <- NULL
  for (d in dir) {
    lf <-
      list.files(
        path = d,
        pattern = "*.TIF|*.jp2",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    files <- c(files, lf)
  }
  if (is.null(out_dir)) {
    out_dir <- tempdir()
  }
  b_r <- select_band_files(files)
  if (only_bands) {
    files <- b_r[['band']]
  } else {
    files <- c(b_r[['band']], b_r[['rest']])
  }

  names <- sort(unique(names(files)))
  b <- vector("list", length = length(names))
  names(b) <- names
  nexus <- get_nexus(out_dir)
  vf <- NULL
  names_1layer <- NULL
  resolution <- NULL
  for (n in names) {
    f <- paste0(out_dir, nexus, n, ".vrt")
    vf <- c(vf, f)
    t <- terra::vrt(files[names(files) == n], f, overwrite = TRUE)
    # only tiles of the same raster
    if (terra::nlyr(t) == 1) {
      b[[n]] <- t
      names_1layer <- c(names_1layer, n)
      resolution <- c(resolution, terra::res(t)[1])
    }
  }
  b <- b[names_1layer]
  names <- names(b)
  names <- gsub("_10m", "", names)
  names <- gsub("_20m", "", names)
  names <- gsub("_60m", "", names)
  names(b) <- names
  r <- sort(unique(resolution))
  b2 <- vector("list", length = length(r))
  names(b2) <- paste0('r', r, 'm')
  b_resolution <- paste0('r', resolution, 'm')
  for (n in names(b2)) {
    b2[[n]] <- transform_to_multiband(bands = b[which(b_resolution == n)])
  }
  structure(list(
    bands = b2,
    out_dir = out_dir,
    virtual_files = vf
  ),
  class = "satres")
}


#' Get spatial resolutions
#'
#' Returns the spatial resolutions of the multi-band raster that make up the object.
#'
#' @param sr A `satres` object.
#'
#' @return A vector of strings.
#'
#' @family satellite definition
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#'
#' esa <- system.file("extdata", "esa", package = "satres")
#' sr <- satres(dir = esa,
#'              out_dir = tempdir())
#'
#' r <- sr |>
#'      get_spatial_resolution()
#'
#' @export
get_spatial_resolution <- function(sr)
  UseMethod("get_spatial_resolution")


#' @rdname get_spatial_resolution
#' @export
get_spatial_resolution.satres <- function(sr) {
  names(sr$bands)
}



#' Transforms a list of raster bands in a raster multi-band
#'
#' To generate a multi-band raster, the definition of the vector must be
#' executed in a single instruction, which is why it must be done through
#' meta programming.
#'
#' @param bands A list of raster bands.
#'
#' @return A raster band.
#'
#' @keywords internal
transform_to_multiband <- function(bands) {
  l <- length(bands)
  str <- "c(bands[[1]]"
  if (l > 1) {
    for (i in 2:l) {
      str <- paste0(str, sprintf(", bands[[%d]]", i))
    }
  }
  str <- paste0(str, ")")
  res <- eval(parse(text = str))
  names(res) <- names(bands)
  res
}


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


#' Find name to files
#'
#' Finds the name associated to a file name in a vector of named patterns.
#'
#' @param files A string vector.
#' @param patterns A string vector of values with names.
#'
#' @return A string vector.
#'
#' @keywords internal
find_name_to_files <- function(files, patterns) {
  names <- names(patterns)
  fn <- names(files)
  for (i in 1:length(patterns)) {
    r <- grepl(patterns[i], files, fixed = TRUE)
    if (sum(r) > 0) {
      fn[which(r)] <- names[i]
    }
  }
  names(files) <- fn
  files
}