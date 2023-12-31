% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportedWrappers.R
\name{exitRedeR}
\alias{exitRedeR}
\title{Exit the RedeR R-to-Java interface}
\usage{
exitRedeR()
}
\value{
Exit/close the RedeR application.
}
\description{
Close an active RedeR session.
}
\examples{
# Load RedeR
library(RedeR)

\donttest{
# Call 'start' and 'exit' methods
startRedeR()
exitRedeR()
}

}
\seealso{
\code{\link{startRedeR}}
}
\author{
Sysbiolab.
}
