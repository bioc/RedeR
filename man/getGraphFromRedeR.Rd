% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportedWrappers.R
\name{getGraphFromRedeR}
\alias{getGraphFromRedeR}
\title{Get graphs from RedeR}
\usage{
getGraphFromRedeR(
  status = c("all", "selected", "notselected"),
  attribs = c("all", "minimal"),
  type = c("node", "container", "all"),
  unit = c("native", "point", "npc"),
  ...
)
}
\arguments{
\item{status}{A filter (string) indicating the status of the graph elements
that should be fetched from the RedeR app (default='all').}

\item{attribs}{A filter (string) indicating the graph attributes
that should be fetched from the RedeR app (default='all').}

\item{type}{A filter (string) indicating the graph element types that
should be fetched from the RedeR app (default='node').}

\item{unit}{A string specifying the unit for \emph{lengths}, \emph{widths},
and \emph{sizes} assigned to node and edge attributes. RedeR space 
coordinate system is native to Java Graphics2D, which uses 'points' by 
default (a point is 1/72 of an inch). Current options include 'native', 
'point', and 'npc'. The 'native' option will used definition from
\code{options('RedeR')}, which is set to 'point' by default. The 'npc'
option will return attribute values normalized to RedeR's viewport.}

\item{...}{Arguments passed to internal checks (ignore).}
}
\value{
igraph objects from RedeR.
}
\description{
Methods to wrap up RedeR graphs into igraph's R objects.
}
\examples{
# Load RedeR and igraph
library(RedeR)
library(igraph)

# Create an igraph
gtoy1 <- graph.lattice(c(3, 3, 3))

\donttest{
# Start the RedeR interface
startRedeR()

# Send graph to RedeR
addGraphToRedeR(g = gtoy1)

# Get graph from RedeR
gtoy2 <- getGraphFromRedeR()
}

}
\seealso{
\code{\link{startRedeR}}, \code{\link{addGraphToRedeR}}
}
\author{
Sysbiolab.
}
