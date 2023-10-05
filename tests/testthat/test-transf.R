test_that("clip_bands()", {
  expect_equal({
    file <- system.file("extdata", "lanjaron.gpkg", package = "satres")
    lanjaron <- sf::st_read(file, layer = "lanjaron_bbox", quiet = TRUE)

    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa) |>
      clip_bands(polygon = lanjaron)

    c(sat_dimensions(sr),
      sat_names(sr))
  },
  c(
    "r1000m",
    "12",
    "19",
    "4",
    "r2000m",
    "6",
    "9",
    "10",
    "r6000m",
    "2",
    "3",
    "11",
    "r1000m",
    "B02",
    "B03",
    "B04",
    "B08",
    "r2000m",
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B11",
    "B12",
    "B8A",
    "r6000m",
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B09",
    "B11",
    "B12",
    "B8A"
  ))
})


test_that("select_bands()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa) |>
      select_bands(res = c("r1000m", "r6000m"),
                   bands = c("B01", "B03", "B04"))
    c(sat_dimensions(sr),
      sat_names(sr))
  },
  c(
    "r1000m",
    "110",
    "210",
    "2",
    "r6000m",
    "19",
    "36",
    "3",
    "r1000m",
    "B03",
    "B04",
    "r6000m",
    "B01",
    "B03",
    "B04"
  ))
})

test_that("select_bands()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa) |>
      select_bands(res = c("r1000m", "r6000m"))
    c(sat_dimensions(sr),
      sat_names(sr))
  },
  c(
    "r1000m",
    "110",
    "210",
    "4",
    "r6000m",
    "19",
    "36",
    "11",
    "r1000m",
    "B02",
    "B03",
    "B04",
    "B08",
    "r6000m",
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B09",
    "B11",
    "B12",
    "B8A"
  ))
})

test_that("select_bands()", {
  expect_equal({
    esa <- system.file("extdata", "esa", package = "satres")
    sr <- satres(dir = esa) |>
      select_bands(bands = c("B01", "B03", "B04"))
    c(sat_dimensions(sr),
      sat_names(sr))
  },
  c(
    "r1000m",
    "110",
    "210",
    "2",
    "r2000m",
    "55",
    "105",
    "3",
    "r6000m",
    "19",
    "36",
    "3",
    "r1000m",
    "B03",
    "B04",
    "r2000m",
    "B01",
    "B03",
    "B04",
    "r6000m",
    "B01",
    "B03",
    "B04"
  ))
})

test_that("merge_tiles()", {
  expect_equal({
    esa_f <- system.file("extdata", "esa/f", package = "satres")
    esa_g <- system.file("extdata", "esa/g", package = "satres")
    sr2 <- satres(dir = esa_f)
    sr <- satres(dir = esa_g) |>
      merge_tiles(sr2)
    c(sat_dimensions(sr),
      sat_names(sr))
  },
  c(
    "r1000m",
    "110",
    "210",
    "4",
    "r2000m",
    "55",
    "105",
    "10",
    "r6000m",
    "19",
    "36",
    "11",
    "r1000m",
    "B02",
    "B03",
    "B04",
    "B08",
    "r2000m",
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B11",
    "B12",
    "B8A",
    "r6000m",
    "B01",
    "B02",
    "B03",
    "B04",
    "B05",
    "B06",
    "B07",
    "B09",
    "B11",
    "B12",
    "B8A"
  ))
})
