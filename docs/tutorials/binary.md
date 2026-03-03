# Binary Tutorial (Rendered)

This is the GitHub-rendered version of the binary tutorial.

Source R Markdown: [`inst/doc/tutorial-binary.Rmd`](../../inst/doc/tutorial-binary.Rmd)

## Example plot

![Binary plot](../images/tutorial-binary.png)

## Core flow

```r
library(optbinningR)
d <- read.csv(system.file("extdata", "breast_cancer_mean_radius.csv", package = "optbinningR"))

optb <- fit(
  OptimalBinning("mean radius"),
  x = d$x,
  y = d$y,
  algorithm = "optimal",
  prebinning_method = "cart",
  max_n_prebins = 20,
  monotonic_trend = "auto"
)

bt <- build(binning_table(optb))
analysis(optb)
plot(optb, type = "binning")
plot(optb, type = "woe")
```
