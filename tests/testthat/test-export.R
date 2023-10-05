test_that("as_SpatRaster()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa)
    r <- sr |>
      as_SpatRaster("r1000m")
    c(names(r),
      terra::ncol(r),
      terra::nrow(r),
      terra::nlyr(r),
      as.vector(round(terra::minmax(r, compute = FALSE), 2)))
  },
  c(
    "B02",
    "B03",
    "B04",
    "B08",
    "110",
    "210",
    "4",
    "1053.38",
    "9115.54",
    "1062.36",
    "8836.69",
    "1050.82",
    "8734.01",
    "1051.04",
    "9087.86"
  ))
})


test_that("save_by_resolution()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa)
    r <- sr |>
      save_by_resolution(only_show_files = TRUE)
    basename(r)
  },
  c("r1000m.tif", "r2000m.tif", "r6000m.tif"))
})
