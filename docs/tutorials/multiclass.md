# Multiclass Tutorial (Rendered)

This is the GitHub-rendered version of the multiclass tutorial.

Source R Markdown: [`inst/doc/tutorial-multiclass.Rmd`](../../inst/doc/tutorial-multiclass.Rmd)

## Example plot

![Multiclass plot](../images/tutorial-multiclass-default.png)

## Core flow

```r
library(optbinningR)
wine <- read.csv(system.file("extdata", "wine_multiclass.csv", package = "optbinningR"), check.names = FALSE)

optb <- fit(
  MulticlassOptimalBinning("ash"),
  x = wine$ash,
  y = wine$target,
  algorithm = "optimal",
  solver = "cp",
  prebinning_method = "cart",
  max_n_bins = 6
)

bt <- build(binning_table(optb))
analysis(binning_table(optb))
plot(optb, metric = "event_rate", style = "bin")
```
