% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss-MVLN.R
\name{title}
\alias{title}
\alias{sshat}
\title{sshat}
\usage{
sshat(hat)
}
\description{
sshat
}
\examples{
library(ss3diags)

data(sma)
sscor(sma$CoVar)

hat(sma$derived_quants)

ssmvln(sma$CoVar,sma$derived_quants)
}
