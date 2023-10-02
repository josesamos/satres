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
#' @param dir A string or string vector.
#' @param out_dir A string, output folder.
#' @param only_bands A boolean, include only satellite bands.
#'
#' @return A vector of strings, name of the processed files.
#'
#' @family satellite functions
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
  file_name <- basename(files)
  n <- nchar(file_name)
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
#' @family satellite functions
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


#' Get spatial resolutions
#'
#' Returns the spatial resolutions of the multi-band raster that make up the object.
#'
#' @param sr A `satres` object.
#'
#' @return A vector of strings.
#'
#' @family satellite functions
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
#' @family satellite functions
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


#' find name to files
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
