\name{selectEdges}
\alias{selectEdges}
\alias{selectEdges,RedPort-method}

\title{selectEdges}

\description{
Select edges in an active RedeR application.  
}

\usage{
selectEdges(obj, edges)
}

\arguments{
  \item{obj}{Object of RedPort Class. }
  \item{edges}{A vertex sequence <vector of strings> or data frame of ncol=2. }  
}

\value{
Mark edges -- which can be handled by other methods.
}

\author{Mauro Castro}

\note{
Prior calling this method invoke RedeR application via 'calld' function.
}

\seealso{
  \code{\link[RedeR:RedPort]{RedPort}},
  \code{\link[RedeR:deleteSelectedEdges]{deleteSelectedEdges}}
}

\examples{
# Initialize igraph
library(igraph)

# edges as a vertex sequence
edges<-c("n1","n2","n1","n3","n1","n4","n1","n5","n1","n6","n1","n7")

# edges as a data.frame
edges <- data.frame(A=c("n1","n1","n1","n1","n1","n1"),
                    B=c("n2","n3","n4","n5","n6","n7"))

\dontrun{

  rdp <- RedPort() 
  calld(rdp)  
  addEdges(rdp, edges)
  updateGraph(rdp)
  selectEdges(rdp,c("n1","n3"))
  updateGraph(rdp)
}  
}

\keyword{graph}


