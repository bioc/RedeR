\name{addSubgraph.list}
\alias{addSubgraph.list}
\alias{addSubgraph.list,RedPort-method}

\title{Add a list of subgraphs to RedeR application.}

\description{
Deprecated function...
}

\usage{
addSubgraph.list(obj, g, nodeList, ...)
}

\arguments{
  \item{obj}{ Object of RedPort Class. }
  \item{g}{ An igraph object. }  
  \item{nodeList}{List of nodes. Will be used to extra subgraphs from g. }
  \item{...}{ Additional arguments passed to RedeR application. }    
}

\details{       
Additional arguments: 
\describe{
  \item{gridRows}{Number of lines to layout the subgraph panel (default = 2) <integer>}
  \item{gridScale}{Expansion factor of the grid area in the app panel. Options: 0.0 to 100 (default = 50) <numeric>. } 
  \item{gscale}{Expansion factor each subgraph related to the app panel (default = 20) <numeric>. }
  \item{gatt}{Either a list or data frame with graph attributes (for data frames, attribute names on cols). See attribute syntax in \code{\link[RedeR:addGraph]{addGraph}} } 
  \item{update}{String argument: if 'all' it forces to update node/edge attributes of a graph already available in the app panel; if 'partial', only node attributes are updated (default = NULL). }
  \item{theme}{Some pre-defined nest attributes. Options: 'tm0','tm1','tm2','tm3','tm4','tm5','tm6'.}
}
}

\value{
Extracts subgraphs from 'igraph' objects and sends the result to the RedeR app. 
}

\author{Mauro Castro}

\examples{
# Deprecated function...
}

\keyword{graph}
