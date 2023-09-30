.onLoad <- function(libname, pkgname) {
  utils::data("sat_rest",
       "sat_band",
       package = pkgname,
       envir = parent.env(environment()))
}

#' Unzip compressed files in tar or zip format
#'
#' Given a vector of compressed file names or the name of a folder containing
#' compressed files, unzip the files to the given output folder. If no output
#' folder is indicated, it is considered the same folder where the input files
#' are.
#'
#' We can indicate whether to include the file name (without the extension) as a
#' folder in the output folder.
#'
#' @param dir A string or string vector.
#' @param out_dir A string, output folder.
#' @param only_bands A boolean, include only satellite bands.
#'
#' @return A vector of strings, name of the processed files.
#'
#' @family satellite functions
#'
#' @examples
#'
#' # sat_untarzip("dat/usgs")
#'
#' # sat_untarzip("dat/esa")
#'
#' # file <- c("dat/usgs/LC08_L1TP_200034_20230924_20230924_02_RT.tar",
#' #           "dat/esa/S2A_MSIL2A_20230905T105621_N0509_R094_T30SVF_20230905T170700.zip")
#' # sat_untarzip(file)
#'
#' @export
sat_res <- function(dir, out_dir = NULL, only_bands = TRUE) {
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
    b2[[n]] <- c(b[which(b_resolution == n)])
  }

}

eval(parse(text = "c(b2[['r60m']][[1]], b2[['r60m']][[2]])"))

terra::plot(b[["B2"]])
terra::nlyr(b[["MSK_QUALIT_B09"]])

tmp <- b[["B2"]]
terra::res(tmp)[1]

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
