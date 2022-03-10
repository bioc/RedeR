\name{updateCoordXY}
\alias{updateCoordXY}
\alias{updateCoordXY,RedPort-method}

\title{
Update node coordinates.
}

\description{
This function updates node coordinates of an igraph object with 
the node coordinates from the RedeR interface.
}

\usage{
updateCoordXY(obj, g, delNodes=FALSE, delEdges=FALSE)
}

\arguments{
  \item{obj}{ Object of RedPort Class.}
  \item{g}{ 
  An igraph object, which will be updated with the graph
  layout displayed in the RedeR interface. Note: 'g' must be the same 
  igraph object sent to the RedeR interface by the 
  \code{\link[RedeR:addGraph]{addGraph}} function.
  }
  \item{delNodes}{
  Option to delete nodes from 'g' when these nodes are not displayed in the RedeR interface.
  }
  \item{delEdges}{
  Option to delete edges from 'g' when these edges are not displayed in the RedeR interface.
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

g <- igraph::sample_pa(10,  directed=FALSE)
V(g)$name <- letters[1:10]

\dontrun{
  #--- send 'g' to RedeR
  rdp <- RedPort() 
  calld(rdp)
  addGraph(rdp, g)
  #--- apply RedeR's interactive layout
  relax(rdp, g, ps=TRUE)
  #--- update 'g' with changes introduced in the RedeR interface
  g <- updateCoordXY(rdp, g)
}  
}

\keyword{graph}
