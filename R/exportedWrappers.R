#-------------------------------------------------------------------------------
#' @title Adding 'igraph' objects to RedeR
#'
#' @description Methods to display igraph objects in the RedeR application.
#'
#' @param g An \code{igraph} object. It must include coordinates and names
#' assigned to \code{x}, \code{y}, and \code{name} vertex attributes.
#' @param layout an optional numeric matrix with two columns for \code{x}
#' and \code{y} coordinates <numeric>.
#' @param gscale Expansion factor related to the app panel area <numeric>
#' @param zoom A zoom scale for the app panel (range: 0.0 to 100.0) <numeric>.
#' @param update.coord A logical value, whether to update \code{x} and \code{y}
#' coordinates in the app.
#' @param isNested A logical value, whether to nest all nodes into a new
#' container.
#' @param unit A string specifying the unit for \emph{lengths}, \emph{widths},
#' and \emph{sizes} assigned to node and edge attributes. RedeR space 
#' coordinate system is native to Java Graphics2D, which uses 'points' by 
#' default (a point is 1/72 of an inch). Current options include 'native', 
#' 'point', and 'npc'. The 'native' option will used definition from
#' \code{options('RedeR')}, which is set to 'point' by default. The 'npc'
#' option will normalize attribute values to RedeR's viewport.
#' @param verbose A logical value specifying to display detailed messages
#' (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' @param ... Additional arguments passed to the
#' \code{\link{nestNodes}} function (used when \code{isNested = TRUE}).
#' @return Send igraph objects to RedeR.
#' @author Sysbiolab.
#' @seealso \code{\link{startRedeR}}, \code{\link{getGraphFromRedeR}}
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create an igraph
#' gtoy <- graph.lattice(c(5, 5, 5))
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Send graph to RedeR
#' addGraphToRedeR(g = gtoy, layout = layout_nicely(gtoy))
#' }
#'
#' @name addGraphToRedeR
#' @aliases addGraphToRedeR
#' @export
addGraphToRedeR <- function(g, layout = NULL, gscale = 75,
    zoom = 100, update.coord = TRUE, isNested = FALSE,
    unit = c("native", "point", "npc"),
    verbose = TRUE, ...) {

    unit <- match.arg(unit)
    g <- .convert.units(g = g, unit = unit, upload = TRUE)

    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    addGraph(obj = rdp, g = g, layout = layout, gscale = gscale,
        zoom = zoom, update.coord = update.coord, verbose = verbose,
        isNested = isNested, ... = ...)
}

#-------------------------------------------------------------------------------
#' @title Get graphs from RedeR
#'
#' @description Methods to wrap up RedeR graphs into igraph's R objects.
#'
#' @param status A filter (string) indicating the status of the graph elements
#' that should be fetched from the RedeR app (default='all').
#' @param attribs A filter (string) indicating the graph attributes
#' that should be fetched from the RedeR app (default='all').
#' @param type A filter (string) indicating the graph element types that
#' should be fetched from the RedeR app (default='node').
#' @param unit A string specifying the unit for \emph{lengths}, \emph{widths},
#' and \emph{sizes} assigned to node and edge attributes. RedeR space 
#' coordinate system is native to Java Graphics2D, which uses 'points' by 
#' default (a point is 1/72 of an inch). Current options include 'native', 
#' 'point', and 'npc'. The 'native' option will used definition from
#' \code{options('RedeR')}, which is set to 'point' by default. The 'npc'
#' option will return attribute values normalized to RedeR's viewport.
#' @param ... Arguments passed to internal checks (ignore).
#' @return igraph objects from RedeR.
#' @author Sysbiolab.
#' @seealso \code{\link{startRedeR}}, \code{\link{addGraphToRedeR}}
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create an igraph
#' gtoy1 <- graph.lattice(c(3, 3, 3))
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Send graph to RedeR
#' addGraphToRedeR(g = gtoy1)
#'
#' # Get graph from RedeR
#' gtoy2 <- getGraphFromRedeR()
#' }
#'
#' @name getGraphFromRedeR
#' @aliases getGraphFromRedeR
#' @export
getGraphFromRedeR <- function(status = c("all", "selected", "notselected"),
    attribs = c("all", "minimal"), type = c("node", "container", "all"),
    unit = c("native", "point", "npc"), ...) {

    status <- match.arg(status)
    attribs <- match.arg(attribs)
    type <- match.arg(type)
    unit <- match.arg(unit)

    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    g <- getGraph(obj = rdp, status = status, type = type, attribs = attribs)

    g <- .convert.units(g = g, unit = unit, upload = FALSE)

    return(g)
}

#-------------------------------------------------------------------------------
#' @title Update graph layout
#'
#' @description This function updates node coordinates of an igraph object
#' with the node coordinates from the RedeR interface.
#'
#' @param g An igraph object, which will be updated with the graph layout
#' displayed in the RedeR interface. Note: 'g' must be the same igraph object
#' sent to the RedeR interface by the\code{\link{addGraphToRedeR}} function.
#' @param delNodes Option to delete nodes from 'g' when these nodes are
#' not displayed in the RedeR interface.
#' @param delEdges Option to delete edges from 'g' when these edges are not
#' displayed in the RedeR interface.
#' @return An updated igraph object.
#' @author Sysbiolab.
#' @seealso \code{\link{startRedeR}}, \code{\link{addGraphToRedeR}}
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create an igraph
#' gtoy1 <- graph.lattice(c(3, 3, 3))
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Send graph to RedeR
#' addGraphToRedeR(g = gtoy1)
#'
#' # Update 'gtoy1' with changes introduced in the RedeR interface
#' gtoy2 <- updateLayoutFromRedeR(g = gtoy1)
#' }
#'
#' @name updateLayoutFromRedeR
#' @aliases updateLayoutFromRedeR
#' @export
updateLayoutFromRedeR <- function(g, delNodes = FALSE, delEdges = FALSE) {

    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    g <- .updateGraphLayout(obj = rdp, g = g, delNodes, delEdges)

    return(g)
}


#-------------------------------------------------------------------------------
#' @title Adding graph legends to the RedeR app
#'
#' @description Methods to display legends in the RedeR app.
#'
#' @param x A vector with legend values (see examples).
#' @param type A legend type. Options: 'nodecolor', 'edgecolor', 'nodesize',
#' 'edgewidth', 'nodeshape', 'edgetype'.
#' @param position Position of the legend in app panel. Options:
#' 'default', 'topleft', 'topright', 'bottomleft', 'bottomright', and 'remove'.
#' Use 'default' to place the legend on a predefined slot, or 'remove' to
#' delete the legend type.
#' @param orientation The orientation of the legend. Options: 'default',
#' 'vertical', 'horizontal'. Use 'default' to automatically set the
#' orientation for the legend type.
#' @param title A string for legend title.
#' @param font.size Font size (unit in points).
#' @param stretch A scaling factor to adjust the legend box (between 0 and 1).
#' @param ... Arguments passed to internal checks (ignore).
#' @return Send legend objects to RedeR app.
#' @author Sysbiolab.
#' @seealso \code{\link{startRedeR}}, \code{\link{addGraphToRedeR}}
#' @examples
#'
#' # Load RedeR
#' library(RedeR)
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Adding node and edge color legends
#' cols <- colorRampPalette(colors = c("red", "blue"))(14)
#' names(cols) <- 1:length(cols)
#' addLegendToRedeR(x = cols, type = "nodecolor")
#' addLegendToRedeR(x = cols, type = "edgecolor", stretch = 0.1)
#'
#' # Adding node size legend
#' nsize <- c(10, 20, 30, 40, 50)
#' addLegendToRedeR(x = nsize, type = "nodesize")
#'
#' # Adding edge width legend
#' esize <- c(1:10)
#' addLegendToRedeR(x = esize, type = "edgewidth")
#'
#' # Adding node shape legend
#' shape <- c("ELLIPSE","RECTANGLE","ROUNDED_RECTANGLE","TRIANGLE","DIAMOND")
#' names(shape) <- shape
#' addLegendToRedeR(x = shape, type = "nodeshape")
#'
#' # Adding edge linetype legend
#' ltype <- c("SOLID", "DOTTED", "DASHED", "LONG_DASH")
#' names(ltype) <- ltype
#' addLegendToRedeR(x = ltype, type = "edgetype")
#' }
#'
#' @name addLegendToRedeR
#' @aliases addLegendToRedeR
#' @export
addLegendToRedeR <- function(x, type = "nodecolor", position = "default",
    orientation = "default", title = type, font.size = 12,
    stretch = 0, ...) {

    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    .addLegend(rdp, x = x, type = type, position = position,
        orientation = orientation, title = title, font.size = font.size,
        stretch = stretch)
}

#-------------------------------------------------------------------------------
#' @title Start RedeR app from R
#'
#' @description Method to launch RedeR application from R.
#'
#' @param ... Arguments passed to the \code{\link{RedPort}} function.
#' @details
#' The \code{startRedeR()} is a wrapper function that launches the RedeR app
#' by calling \code{RedPort()} and \code{calld()} methods. Therefore, these
#' methods no longer needed to be called by the user from RedeR version >=3.
#' \itemize{
#' List of functions that uses \code{startRedeR()}:
#' \item{\link{addGraphToRedeR}}{ Methods to display igraph objects.}
#' \item{\link{getGraphFromRedeR}}{ Methods to wrap up RedeR graphs.}
#' \item{\link{addLegendToRedeR}}{ Methods to display legends.}
#' \item{\link{relaxRedeR}}{ Start RedeR's interactive layout.}
#' \item{\link{resetRedeR}}{ Reset an active RedeR session.}
#' \item{\link{exitRedeR}}{ Close an active RedeR session.}
#' \item{\link{pingRedeR}}{ Test the R-to-Java interface.}
#' \item{\link{addNodes}}{ Add nodes to an active RedeR application.}
#' \item{\link{addEdges}}{ Add edges to an active RedeR application.}
#' \item{\link{selectNodes}}{ Select nodes in an active RedeR application.}
#' \item{\link{selectEdges}}{ Select edges in an active RedeR application.}
#' \item{\link{deleteNodes}}{ Delete nodes from an active RedeR application.}
#' \item{\link{deleteEdges}}{ Delete edges from an active RedeR application.}
#' \item{\link{nestNodes}}{ Nest nodes into containers.}
#' \item{\link{mergeOutEdges}}{ Assign 'out-edges' to containers.}
#' }
#' @return System call to start the RedeR application.
#' @seealso \code{\link{addGraphToRedeR}}
#' @author Sysbiolab.
#' @examples
#' # Load RedeR
#' library(RedeR)
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#' }
#'
#' @name startRedeR
#' @export
startRedeR <- function(...) {

    rdp <- getOption("RedeR")$port
    if (is(rdp, "RedPort")) {
        if (ping(rdp) == 1) {
            return("RedeR interface is already in use!")
        }
    }

    rdp <- RedPort(... = ...)
    opt <- list()
    opt$port <- rdp
    opt$unit <- "point"
    options(RedeR = opt)
    calld(obj = rdp)

}

#-------------------------------------------------------------------------------
#' @title Reset RedeR app
#'
#' @description Reset an active RedeR session.
#'
#' @return Reset plotting panel.
#' @seealso \code{\link{startRedeR}}, \code{\link{addGraphToRedeR}}
#' @author Sysbiolab.
#' @examples
#' # Load RedeR
#' library(RedeR)
#'
#' \donttest{
#' # Call 'start' and 'reset' methods
#' startRedeR()
#' resetRedeR()
#' }
#'
#' @name resetRedeR
#' @export
resetRedeR <- function() {
    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    invisible(resetd(rdp))
}

#-------------------------------------------------------------------------------
#' @title Exit the RedeR R-to-Java interface
#'
#' @description Close an active RedeR session.
#'
#' @return Exit/close the RedeR application.
#' @seealso \code{\link{startRedeR}}
#' @author Sysbiolab.
#' @examples
#' # Load RedeR
#' library(RedeR)
#'
#' \donttest{
#' # Call 'start' and 'exit' methods
#' startRedeR()
#' exitRedeR()
#' }
#'
#' @name exitRedeR
#' @export
exitRedeR <- function() {
    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    exitd(rdp)
}

#-------------------------------------------------------------------------------
#' @title Ping RedeR app
#'
#' @description Test the R-to-Java interface of an active RedeR session.
#'
#' @return Ping test for RedeR app.
#' @seealso \code{\link{startRedeR}}
#' @author Sysbiolab.
#' @examples
#' # Load RedeR
#' library(RedeR)
#'
#' \donttest{
#' # Call 'start' and 'ping' methods
#' startRedeR()
#' pingRedeR()
#' }
#'
#' @name pingRedeR
#' @export
pingRedeR <- function() {
    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    if (ping(rdp) == 0) {
        msg <- "Unable to access RedeR."
    } else {
        msg <- version(rdp)
    }

    return(msg)
}

#-------------------------------------------------------------------------------
#' @title Relax
#'
#' @description RedeR's hierarchical force-directed interactive layout.
#'
#' @param p1 Edge target length (unit in points; >= 1 ) <numeric>.
#' @param p2 Edge stiffness (arbitrary unit; >= 0 ) <numeric>.
#' @param p3 Node repulsion factor (arbitrary unit; >= 0 ) <numeric>.
#' @param p4 Node perimeter effect (unit in points; >= 0 ) <numeric>.
#' @param p5 Node speed limit (arbitrary unit; >= 0 ) <numeric>.
#' @param p6 Repulsion radius, i.e., this parameter limits the repulsion
#' factor range (unit as in 'p1'; >= 0 ) <numeric>.
#' @param p7 Central pull (arbitrary unit; >= 0 ) <numeric>.
#' @param p8 Nest-nest edge target length, i.e., edge target between
#' linked containers (unit in points; >= 1 ) <numeric>.
#' @param p9 Nest-node repulsion factor, i.e., repulsion among containers and
#' out-nodes (arbitrary unit; >= 0 ) <numeric>.

#' @details RedeR's interactive layout uses a force-directed algorithm
#' described elsewhere (Brandes 2001; Fruchterman and Reingold 1991). Here
#' we adapted the method to deal with nested networks. In force-directed
#' graphs, each edge can be regarded as a spring - with a given target
#' length - and can either exert a repulsive or attractive force on the
#' connected nodes, while nodes are analogous to mutually repulsive charged
#' particles that move according to the applied forces. In RedeR, the
#' simulation is additionally constrained by the hierarchical structure
#' of the network. For example, a nested node is constrained to its
#' parent-node by opposing forces applied by the nest, which is regarded
#' as a special node whose nested objects can reach a local equilibrium
#' independently from other network levels. The simulation is adjusted by
#' global options and evolves until the system reaches the equilibrium state.
#' The default values are set to layout sparse networks with few nodes
#' (e.g. 10-100 nodes). For large and dense networks better results can
#' be achieved interactively by tuning one or more parameters.
#'
#' @references
#'
#' Brandes U. Drawing graphs: methods and models. In: Lecture notes in computer
#' science. Kaufmann M. and Wagner D. (Ed), vol. 2025. Heidelberg:
#' Springer; 2001: 71-86.
#'
#' Fruchterman TMJ, Reingold EM. Graph drawing by force-directed placement.
#' Software: Practice and Experience 1991, 21(11):1129-1164.
#'
#' @return Layout a graph in the app panel.
#' @seealso \code{\link{startRedeR}}, \code{\link{addGraphToRedeR}}
#' @author Sysbiolab.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create an igraph
#' gtoy <- graph.lattice(c(5, 5, 5))
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Send the igraph to RedeR
#' addGraphToRedeR(g = gtoy)
#'
#' # Start interactive layout
#' relaxRedeR()
#' }
#'
#' @name relaxRedeR
#' @aliases relaxRedeR
#' @export
relaxRedeR <- function(p1 = 100, p2 = 100, p3 = 100, p4 = 100,
    p5 = 100, p6 = 10, p7 = 10, p8 = 100, p9 = 10) {

    rdp <- getOption("RedeR")$port
    if (!is(rdp, "RedPort")) rdp <- RedPort()

    relax(obj = rdp, p1, p2, p3, p4, p5, p6, p7, p8, p9)

    invisible()
}
