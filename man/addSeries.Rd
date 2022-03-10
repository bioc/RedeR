\name{addSeries}
\alias{addSeries}
\alias{addSeries,RedPort-method}

\title{Add series to RedeR application.}

\description{
Deprecated function...
}

\usage{
addSeries(obj, g, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{g}{ An igraph object. }  
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{setnodes}{Logical value, whether to update node attributes in the new item of the series (default = TRUE). } 
  \item{setedges}{Logical value, whether to add edges and update attributes in the new item of the series (default = TRUE). }
}
}

\value{
Submits series of R graphs to RedeR app. 
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
}

\examples{
# Deprecated function...
}

\keyword{graph}
