test_that("sat_untarzip()", {
  expect_equal({
    file <- c("a/b/f.tar",
              "c/d/g.zip")
    sat_untarzip(file, only_show_files = TRUE)
  },
  c("a/b/f.tar to a/b/f", "c/d/g.zip to c/d/"))
})

test_that("sat_untarzip()", {
  expect_equal({
    file <- c("a/b/f.tar",
              "c/d/g.zip")
    sat_untarzip(file, out_dir = 'e', only_show_files = TRUE)
  },
  c("a/b/f.tar to e/f", "c/d/g.zip to e/"))
})

test_that("sat_untarzip()", {
  expect_equal({
    file <- c("a/b/f.tar",
              "c/d/g.zip")
    sat_untarzip(file, include_filename = TRUE, only_show_files = TRUE)
  },
  c("a/b/f.tar to a/b/f", "c/d/g.zip to c/d/g"))
})

test_that("sat_untarzip()", {
  expect_equal({
    file <- c("a/b/f.tar",
              "c/d/g.zip")
    sat_untarzip(file, out_dir = 'e', include_filename = TRUE, only_show_files = TRUE)
  },
  c("a/b/f.tar to e/f", "c/d/g.zip to e/g"))
})

test_that("sat_untarzip()", {
  expect_equal({
    file <- c("a/b/f.tar",
              "c/d/g.zip")
    sat_untarzip(file, include_filename = FALSE, only_show_files = TRUE)
  },
  c("a/b/f.tar to a/b/", "c/d/g.zip to c/d/"))
})

test_that("sat_untarzip()", {
  expect_equal({
    file <- c("a/b/f.tar",
              "c/d/g.zip")
    sat_untarzip(file, out_dir = 'e', include_filename = FALSE, only_show_files = TRUE)
  },
  c("a/b/f.tar to e/", "c/d/g.zip to e/"))
})


test_that("sat_untarzip()", {
  file <- system.file("extdata", package = "satres")
  out_dir <- tempdir()
  res <- sat_untarzip(file, out_dir)

  expect_equal({
    all(c("satres", "satres.txt") %in% list.files(out_dir))
  },
  TRUE)

  expect_equal({
    basename(res)
  },
  c("satres.tar",
    "satres.zip"
  ))

})

