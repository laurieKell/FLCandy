% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotFL-plotLengths.R
\name{plotLengths}
\alias{plotLengths}
\title{Length distributions}
\usage{
\S4method{plotLengths}{FLQuant}(
  x,
  direction = c("horizontal", "vertical"),
  block = c("lustrum", "decade"),
  palette = flpalette
)
}
\description{
Length distributions
}
\examples{
data(ple4)
iak <- invALK(FLPar(linf=42, k=2.1, t0=0.1), age=1:10)
les <- lenSamples(catch.n(ple4)[, 1:10], iak)
units(les) <- "cm"
plotLengths(les)
plotLengths(les) + geom_vline(aes(xintercept=mean), colour="red")
plotLengths(les) + geom_vline(aes(xintercept=median, colour="green"))
plotLengths(les) + geom_vline(aes(xintercept=mode), colour="black")
plotLengths(group(les, sum, year=year - year\%\%5))
plotLengths(group(les, mean, year=year - year\%\%5))
plotLengths(group(les, sum, year=year - year\%\%10))
plotLengths(les, block="decade")
plotLengths(les, direction="vertical")
plotLengths(group(les, mean, year=year - year\%\%5), direction="vertical")
plotLengths(les, direction="vertical", block="decade")
plotLengths(les) +
  geom_vline(data=as.data.frame(FLPar(L50=38)), aes(xintercept=data),
  linewidth=1)
plotLengths(les) +
  geom_vline(data=as.data.frame(FLPar(seq(38, 46, length=10), dimnames=list(params='L50', year=1957:1966, iter=1))), aes(xintercept=data),
  linewidth=1)
plotLengths(les, block="lustrum") +
  geom_vline(data=as.data.frame(FLPar(seq(38, 46, length=10), dimnames=list(params='L50', year=1957:1966, iter=1))), aes(xintercept=data),
  linewidth=1)
}
