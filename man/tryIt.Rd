% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tryIt.R
\name{tryIt}
\alias{tryIt}
\title{Safely Evaluate an Expression}
\usage{
tryIt(x, silent = TRUE)
}
\arguments{
\item{x}{An R expression to evaluate}

\item{silent}{Logical; if TRUE suppresses error messages (default FALSE)}
}
\value{
Returns the result of evaluating the expression if successful, NULL if it fails
}
\description{
Evaluates an expression and returns NULL if it fails, instead of throwing an error.
}
\details{
This function is useful for safely evaluating expressions that might fail,
particularly in loops or apply functions where you want to continue execution
even if some operations fail.
}
\examples{
# Successful evaluation
tryIt(1 + 1)

# Failed evaluation returns NULL
tryIt(log("a"))

# Suppress error messages
tryIt(log("a"), silent=TRUE)
}
