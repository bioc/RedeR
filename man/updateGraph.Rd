\name{updateGraph}
\alias{updateGraph}
\alias{updateGraph,RedPort-method}

\title{
Update RedeR graphs.
}

\description{
This function updates an active RedeR application session.
}

\usage{
updateGraph(obj, g=NULL)
}

\arguments{
  \item{obj}{ Object of RedPort Class.}
  \item{g}{ 
  An optional igraph object. If provided, 'g' will be updated with the graph
  layout displayed in the RedeR interface. Note: 'g' must be the same 
  igraph object sent to the RedeR interface by the 
  \code{\link[RedeR:addGraph]{addGraph}} function.
  }
}

\value{
An update igraph object.
}

\author{
Mauro Castro
}

\note{
Prior calling this method make sure that there is an active RedeR session.
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
# Initialize igraph
library(igraph)

edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

\dontrun{

  rdp <- RedPort() 
  calld(rdp)
  addEdges(rdp, edges)  
  updateGraph(rdp)
}  
}

\keyword{graph}
