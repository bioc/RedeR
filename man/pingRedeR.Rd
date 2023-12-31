% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportedWrappers.R
\name{pingRedeR}
\alias{pingRedeR}
\title{Ping RedeR app}
\usage{
pingRedeR()
}
\value{
Ping test for RedeR app.
}
\description{
Test the R-to-Java interface of an active RedeR session.
}
\examples{
# Load RedeR
library(RedeR)

\donttest{
# Call 'start' and 'ping' methods
startRedeR()
pingRedeR()
}

}
\seealso{
\code{\link{startRedeR}}
}
\author{
Sysbiolab.
}
