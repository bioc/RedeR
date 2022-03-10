\name{mergeOutEdges}
\alias{mergeOutEdges}
\alias{mergeOutEdges,RedPort-method}

\title{Merge out-edges between connected containers and transfers edges from nodes to containers. }

\description{
Method to assign out-edges to containers in an active RedeR session. This method transfers edges from nodes to the respective containers.
}

\usage{
mergeOutEdges(obj,...)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{...}{ Additional arguments passed to RedeR application. } 
}

\details{       
Additional arguments: 
\describe{
   \item{nlevels}{Number of levels to be merged in the hierarchical network (default=2) <integer>. }
   \item{rescale}{Logical value. Whether to rescale out-edge width to not overextend container size; 
   if false, it will run a simple sum when combining out-edges between containers (default=TRUE). }
   \item{lb}{Custom lower bound to rescale edge width between containers (default=NA) <numeric>. }
   \item{ub}{Custom upper bound to rescale edge width between containers (default=NA) <numeric>. }
}
}

\value{
Add/change edge assigments.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via XML-RPC server (i.e. 'calld').
}

\seealso{
\code{\link[RedeR:RedPort]{RedPort}}
}

\examples{
# Initialize igraph
library(igraph)

el<-matrix(c("n1","n2","n1","n3","n1","n4","n2","n5","n2","n6","n2","n7"), ncol=2, byrow=TRUE)
g <- graph.edgelist(el)

\dontrun{

  rdp <- RedPort() 
  calld(rdp)
  addGraph( rdp, g, layout.kamada.kawai(g) )
  nestNodes( rdp, c("n1","n2") )
  mergeOutEdges(rdp)  
  updateGraph(rdp)
}  
}

\keyword{graph}
