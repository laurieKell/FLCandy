% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLStock-indicatorsBiol.R
\name{pos}
\alias{pos}
\title{ASA: the Average Age of Spawners}
\usage{
pos(object, ...)
}
\arguments{
\item{object}{An FLQuant object with an ogive with the proportion of older spawners,
by default calculated by soawnOnce}
}
\value{
Returns the proportion of old spawners by biomass.
}
\description{
The Average Age of Spawners is the mean age of reproductive individuals.
The average age of spawners can indicate the age structure of a fish population
and its potential reproductive capacity. A decrease in the ASA could suggest
overfishing or other stressors affecting older age classes.
}
\examples{
\dontrun{
data(ple4)
asa(ple4)}
}
\seealso{
\code{\link{ssb}}, \code{\link{ssb.age}},
\code{\link{pos}}, \code{\link{spawnOnce}},
\code{\link{asa}}, \code{\link{awa}},
\code{\link{ssb}}, \code{\link{amat}}, \code{\link{wmat}},
\code{\link{fjuv}}, \code{\link{fapex}}
}
