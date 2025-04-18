% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R
\docType{methods}
\name{hcrICES}
\alias{hcrICES}
\alias{hcrICES-method}
\alias{hcrICES,FLStock,FLBRP-method}
\title{hcrICES}
\usage{
hcrICES(object,eql,sr_deviances,params, 
         start=max(dimnames(object)$year)-10, end=start+10, interval=1,
         err=NULL,bndTac=c(0,Inf),...)
}
\arguments{
\item{object}{an object of class \code{FLStock}}

\item{eql}{\code{FLBRP} with a stock recruitment relationship used for projection}

\item{...}{any additional arguments}

\item{sr_deviances}{\code{FLQuant} recuitment deviates on the log scale, i.e. multiplicative}

\item{params}{\code{FLPar} HCR parameters, specifying blim, btrig, bmin, ftar and fmin}

\item{start}{\code{numeric} first year for simulation}

\item{end}{\code{numeric}   last year for simulation}

\item{interval}{\code{numeric} time step, 1 year by default}

\item{err}{\code{FLQuant} assessment error on SSB for year used for HCR}

\item{bndTac}{\code{numeric} bounds on TAC, by default these are turned off, for 20 percent constraint set to c(0.8,1.2)}
}
\value{
returns a \code{list} with \code{FLStock} and \code{FLPar} objects for the stock and HCR
}
\description{
Calculates the global MSY, i.e. the largest catch that can be made with a knife edge selection pattern

Harvest Control Rule, calculates Total Allowable Catch (TAC) based on a hockey stock harvest control rule.
}
\examples{
data(ple4brp)
globalMsy(ple4brp)

hcrICES

\dontrun{
data(pl4)
}

}
\author{
Laurence Kell, Sea++
}
