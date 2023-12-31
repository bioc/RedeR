% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportedCallMethods.R
\docType{methods}
\name{version,RedPort-method}
\alias{version,RedPort-method}
\alias{version}
\title{Version}
\usage{
\S4method{version}{RedPort}(obj)
}
\arguments{
\item{obj}{A \code{RedPort}-class object.}
}
\value{
Version of the running app.
}
\description{
Returns the RedeR application version.
}
\examples{

rdp <- RedPort()

\donttest{
calld(rdp)
version(rdp)
}

}
\seealso{
\code{\link{pingRedeR}}
}
\author{
Sysbiolab.
}
