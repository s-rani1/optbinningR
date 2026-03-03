test_that("plot methods render for 1D and 2D models", {
  set.seed(1101)
  x <- rnorm(300)
  y <- rbinom(300, 1, plogis(0.8 * x))
  m <- fit(OptimalBinning("x"), x, y, algorithm = "optimal", max_n_bins = 6)

  png_file <- tempfile(fileext = ".png")
  grDevices::png(png_file, width = 900, height = 600)
  expect_no_error(plot(m, type = "binning"))
  expect_no_error(plot(m, type = "woe"))
  grDevices::dev.off()
  expect_true(file.exists(png_file))
  expect_gt(file.info(png_file)$size, 0)

  x2 <- data.frame(a = rnorm(400), b = runif(400, -2, 2))
  y2 <- rbinom(400, 1, plogis(0.7 * x2$a - 0.5 * x2$b))
  m2 <- fit(OptimalBinning2D("a", "b", "binary"), x2, y2, max_n_bins_x = 5, max_n_bins_y = 5)

  png_file2 <- tempfile(fileext = ".png")
  grDevices::png(png_file2, width = 900, height = 600)
  expect_no_error(plot(m2, type = "binning"))
  grDevices::dev.off()
  expect_true(file.exists(png_file2))
  expect_gt(file.info(png_file2)$size, 0)

  wm <- read.csv(.extdata_path("wine_multiclass.csv"), check.names = FALSE)
  mm <- fit(
    MulticlassOptimalBinning("ash"),
    wm[["ash"]],
    wm[["target"]],
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 6
  )
  png_file3 <- tempfile(fileext = ".png")
  grDevices::png(png_file3, width = 900, height = 600)
  expect_no_error(plot(mm, metric = "event_rate", style = "bin"))
  grDevices::dev.off()
  expect_true(file.exists(png_file3))
  expect_gt(file.info(png_file3)$size, 0)

  cd <- read.csv(.extdata_path("continuous_synth.csv"))
  mc <- fit(
    ContinuousOptimalBinning("x"),
    cd[["x"]],
    cd[["y"]],
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 10
  )
  png_file4 <- tempfile(fileext = ".png")
  grDevices::png(png_file4, width = 900, height = 600)
  expect_no_error(plot(mc, metric = "mean", style = "bin"))
  expect_no_error(plot(mc, metric = "iv", style = "actual"))
  grDevices::dev.off()
  expect_true(file.exists(png_file4))
  expect_gt(file.info(png_file4)$size, 0)
})
