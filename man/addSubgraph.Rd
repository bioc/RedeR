\name{addSubgraph}
\alias{addSubgraph}
\alias{addSubgraph,RedPort-method}

\title{Add subgraphs to RedeR application.}

\description{
Deprecated function...
}

\usage{
addSubgraph(obj, g, nodes, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{g}{ An igraph object. }  
  \item{nodes}{Nodes of the subgraph <array of strings>}
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{gatt}{A list of graph attributes. See attribute syntax in \code{\link[RedeR:addGraph]{addGraph}} } 
  \item{gscale}{Expansion factor of the graph area related to the app panel (default = 75) <numerics>. } 
  \item{gcoord}{Sets the graph x,y center. Coords between 0 and 100 are set to the visible area of the app panel (default = c(75,75)) <numeric vector>. } 
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5'} 
}
}

\value{
Extracts subgraphs from 'igraph' objects and sends the result to the RedeR app. 
}


\author{Mauro Castro}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
\code{\link[RedeR:addSubgraph.list]{addSubgraph.list}}
}

\examples{
# Deprecated function...
}

\keyword{graph}
