\name{binning_table}
\alias{binning_table}
\alias{build}
\title{Binning Table Handle and Build API}
\description{Instantiate a binning table handle and build Python-style binning tables.}
\usage{
binning_table(object)

build(object, ...)
}
\arguments{
  \item{object}{A fitted supported model object (for \code{binning_table}) or a \code{BinningTable} object (for \code{build}).}
  \item{...}{Reserved for compatibility.}
}
\value{
\code{binning_table()} returns a \code{BinningTable} object. \code{build()} returns a data frame with Python-style columns, row labels and totals row.
}
\examples{
x <- c(1, 2, 3, 4, 5, 6)
y <- c(0, 0, 0, 1, 1, 1)

m <- fit(OptimalBinning("x"), x, y, algorithm = "optimal", max_n_bins = 3)
bt <- binning_table(m)
build(bt)
}
