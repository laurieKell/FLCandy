% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selex.R
\name{selexpars}
\alias{selexpars}
\title{computes initial values for selex pars}
\usage{
selexpars(Sa, S50 = NULL, S95 = NULL, Smax = NULL, Dcv = NULL, Dmin = NULL)
}
\arguments{
\item{Sa}{selectivity at age Sa = selage(stk)}

\item{S50}{age-at-50\%-selectivty}

\item{S95}{age-at-95\%-selectivty}

\item{Smax}{age at peak of halfnormal or top of descending slop}

\item{Dcv}{CV of halfnormal determining the steepness of the descending slope}

\item{Dmin}{height of the descending slop at maximum age}
}
\value{
vector of selex pars
}
\description{
computes initial values for selex pars
}
\author{
Henning Winker (JRC)
}
