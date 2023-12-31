% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportedAddGraphMethods.R
\docType{methods}
\name{addGraph,RedPort-method}
\alias{addGraph,RedPort-method}
\alias{addGraph}
\title{Adding 'igraph' objects to RedeR}
\usage{
\S4method{addGraph}{RedPort}(
  obj,
  g,
  layout = NULL,
  gscale = 75,
  zoom = 100,
  update.coord = TRUE,
  verbose = TRUE,
  isNested = FALSE,
  ...
)
}
\arguments{
\item{obj}{A \code{RedPort}-class object.}

\item{g}{An \code{igraph} object. It must include coordinates and names
assigned to \code{x}, \code{y}, and \code{name}  vertex attributes.}

\item{layout}{an optional numeric matrix with two columns for \code{x}
and \code{y} coordinates <numeric>.}

\item{gscale}{Expansion factor related to the app panel area <numeric>}

\item{zoom}{A zoom scale for the app panel (range: 0.0 to 100.0) <numeric>.}

\item{update.coord}{A logical value, whether to update \code{x} and \code{y}
coordinates in the app.}

\item{verbose}{A logical value specifying to display detailed messages
(when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).}

\item{isNested}{A logical value, whether to nest all nodes into a new
container.}

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
# Initialize RedeR and igraph
library(RedeR)
library(igraph)

gtoy <- graph.lattice(c(5, 5, 5))

rdp <- RedPort()

\dontrun{
calld(rdp)
addGraph(rdp, g = gtoy, layout = layout_nicely(gtoy))
}

}
\seealso{
\code{\link{addGraphToRedeR}}
}
\author{
Sysbiolab.
}
