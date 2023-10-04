test_that("as_SpatRaster()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa,
                 out_dir = tempdir(),
                 only_bands = TRUE)
    r <- sr |>
      as_SpatRaster("r1000m")
    names(r)
  },
  c("B02", "B03", "B04", "B08"))
})


test_that("save_by_resolution()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa,
                 out_dir = tempdir(),
                 only_bands = TRUE)
    r <- sr |>
      save_by_resolution(only_show_files = TRUE)
    basename(r)
  },
  c("r1000m.tif", "r2000m.tif", "r6000m.tif"))
})
