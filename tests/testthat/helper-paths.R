.extdata_path <- function(file) {
  p <- system.file("extdata", file, package = "optbinningR")
  if (nzchar(p)) return(p)
  testthat::test_path("..", "..", "inst", "extdata", file)
}
