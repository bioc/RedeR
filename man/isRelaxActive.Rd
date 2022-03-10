\name{isRelaxActive}
\alias{isRelaxActive}
\alias{isRelaxActive,RedPort-method}

\title{Inquires about RedeR current state. }

\description{
Inquires whether 'dynamics' algorithm is active in RedeR application.
}

\usage{
isRelaxActive(obj)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
}

\value{
Returns 1<integer> if true, 0<integer> otherwise.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
rdp <- RedPort() 

\dontrun{

  calld(rdp)
  isRelaxActive(rdp)
    # 1 or 0
}  
}

\keyword{graph}

