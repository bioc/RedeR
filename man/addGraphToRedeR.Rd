% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportedWrappers.R
\name{addGraphToRedeR}
\alias{addGraphToRedeR}
\title{Adding 'igraph' objects to RedeR}
\usage{
addGraphToRedeR(
  g,
  layout = NULL,
  gscale = 75,
  zoom = 100,
  update.coord = TRUE,
  isNested = FALSE,
  unit = c("native", "point", "npc"),
  verbose = TRUE,
  ...
)
}
\arguments{
\item{g}{An \code{igraph} object. It must include coordinates and names
assigned to \code{x}, \code{y}, and \code{name} vertex attributes.}

\item{layout}{an optional numeric matrix with two columns for \code{x}
and \code{y} coordinates <numeric>.}

\item{gscale}{Expansion factor related to the app panel area <numeric>}

\item{zoom}{A zoom scale for the app panel (range: 0.0 to 100.0) <numeric>.}

\item{update.coord}{A logical value, whether to update \code{x} and \code{y}
coordinates in the app.}

\item{isNested}{A logical value, whether to nest all nodes into a new
container.}

\item{unit}{A string specifying the unit for \emph{lengths}, \emph{widths},
and \emph{sizes} assigned to node and edge attributes. RedeR space 
coordinate system is native to Java Graphics2D, which uses 'points' by 
default (a point is 1/72 of an inch). Current options include 'native', 
'point', and 'npc'. The 'native' option will used definition from
\code{options('RedeR')}, which is set to 'point' by default. The 'npc'
option will normalize attribute values to RedeR's viewport.}

\item{verbose}{A logical value specifying to display detailed messages
(when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).}

\item{...}{Additional arguments passed to the
\code{\link{nestNodes}} function (used when \code{isNested = TRUE}).}
}
\value{
Send igraph objects to RedeR.
}
\description{
Methods to display igraph objects in the RedeR application.
}
\examples{
# Load RedeR and igraph
library(RedeR)
library(igraph)

# Create an igraph
gtoy <- graph.lattice(c(5, 5, 5))

\donttest{
# Start the RedeR interface
startRedeR()

# Send graph to RedeR
addGraphToRedeR(g = gtoy, layout = layout_nicely(gtoy))
}

}
\seealso{
\code{\link{startRedeR}}, \code{\link{getGraphFromRedeR}}
}
\author{
Sysbiolab.
}
