.onLoad <- function(libname, pkgname) {
  utils::data("sat_rest",
       "sat_band",
       package = pkgname,
       envir = parent.env(environment()))
}

#' `satres` S3 class
#'
#' Creates a `satres` object from a set of raster files.
#'
#' Given a folder name or a vector of folder names, containing satellite band
#' raster files, creates an object containing all rasters grouped according to
#' their spatial resolution.
#'
#' If there are several rasters of the same area, it previously merges them to
#' form a single raster of the total area.
#'
#' A working folder where the virtual rasters are created is indicated as a
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
#' # s <- satres("dat/usgs")
#'
#' # s <- satres("dat/esa")
#'
#' # dir <- c("dat/usgs", "dat/esa")
#' # s <- satres(dir)
#'
#' @export
satres <- function(dir, out_dir = NULL, only_bands = TRUE) {
  files <- NULL
  for (d in dir) {
    lf <-
      list.files(
        path = d,
        pattern = "*.TIF|.jp2",
        recursive = TRUE,
        full.names = TRUE
      )
    files <- c(files, lf)
  }
  if (is.null(out_dir)) {
    out_dir <- dir[1]
  }
  file_name <- basename(files)
  n <- nchar(file_name)
  extension <- substr(file_name, n - 3, n)
  bands <- sat_band
  if (!only_bands) {
    bands <- c(bands, sat_rest)
  }
  files <- select_band_files(files, bands)

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
#'
#' @return A `satres` object.
#'
#' @family satellite functions
#' @seealso \code{\link{sat_untarzip}}
#'
#' @examples
#' # sr <- sr |>
#' #       save_by_resolution()
#'
#' @export
save_by_resolution <- function(sr, out_dir)
  UseMethod("save_by_resolution")


#' @rdname save_by_resolution
#' @export
save_by_resolution.satres <- function(sr, out_dir = NULL) {
  if (is.null(out_dir)) {
    out_dir <- sr$out_dir
  }
  nexus <- get_nexus(out_dir)
  for (n in names(sr$bands)) {
    terra::writeRaster(
      sr$bands[[n]],
      paste0(out_dir, nexus, n, ".tif"),
      filetype = "GTiff",
      overwrite = TRUE
    )
  }
  sr
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
#' # res <- sr |>
#' #        get_spatial_resolutions()
#'
#' @export
get_spatial_resolutions <- function(sr)
  UseMethod("get_spatial_resolutions")


#' @rdname get_spatial_resolutions
#' @export
get_spatial_resolutions.satres <- function(sr) {
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
#' @param bands A string vector.
#'
#' @return A string vector.
#'
#' @keywords internal
select_band_files <- function(files, bands) {
  names(files) <- NA
  fn <- names(files)
  bn <- names(bands)
  sel <- rep(FALSE, length(files))
  for (i in 1:length(bands)) {
    r <- grepl(bands[i], files, fixed = TRUE)
    if (sum(r) > 0) {
      sel <- sel | r
      fn[which(r)] <- bn[i]
    }
  }
  names(files) <- fn
  files[sel]
}