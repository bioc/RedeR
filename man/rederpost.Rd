\name{rederpost}
\alias{rederpost}
\alias{rederpost,RedPort-method}

\title{This function implements an RPC protocol for RedeR.}

\description{
Post to RedeR's handler at the Java side. 
}

\usage{
rederpost(obj, method, ..., gdata=list(...))
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{method}{A method listed in RedeR's handler at the Java side.}  
  \item{...}{Additional arguments passed to the Java side.}
  \item{gdata}{A list of graph options passed to RedeR's handler at the Java side. }   
}

\value{
Post to RedeR's handler at the Java side.
}

\author{Mauro Castro}

\note{
This is an internal function.
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\keyword{graph}
