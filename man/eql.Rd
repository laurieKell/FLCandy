% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility-funcs.R
\name{eql}
\alias{eql}
\alias{eql,FLStock-method}
\title{Equilibrium Model Fitting}
\usage{
eql(object, model)

\S4method{eql}{FLStock}(object, model = "bevholtSV")
}
\arguments{
\item{object}{An FLStock object}

\item{model}{Stock-recruitment model type (default="bevholtSV")}
}
\value{
An FLBRP object with additional attributes:
\itemize{
\item sr - Fitted stock-recruitment model
\item logLik - Model likelihood
\item prod - Production characteristics
\item tseries - Time series metrics
}
}
\description{
Fits stock-recruitment models and computes biological reference points using FLR framework.
}
\examples{
\dontrun{
data(ple4)
eql <- eql(ple4, model="rickerSV")
summary(eql)
}

}
