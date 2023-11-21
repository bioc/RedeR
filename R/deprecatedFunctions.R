################################################################################
### Deprecated
################################################################################

#-------------------------------------------------------------------------------
#' @title Deprecated functions
#'
#' @description List of deprecated functions in RedeR V3.
#'
#' @param x Deprecated arg.
#' @param ... Additional deprecated args.
#' @return ---
#' @seealso \code{\link{addGraph}}, \code{\link{getGraph}}
#' @examples
#' # List of deprecated functions in RedeR V3:
#' # -gtoy.rm (no replacement)
#' # -cea (no replacement)
#' # -mergeNodes (set in app only)
#' # -setArrowDirection (set via addGraph function)
#' # -addEdgeBetweenContainers (set in app only)
#' # -nesthc (no replacement)
#' # -getSourceEdgeIDs (replaced by getGraph)
#' # -getTargetEdgeIDs (replaced by getGraph)
#' # -getEdgeIDs (replaced by getGraph)
#' # -selectAllNodes (replaced by selectNodes)
#' # -selectAllEdges (replaced by selectEdges)
#' # -getNodeIDs (replaced by getGraph)
#' # -addLegend.color (replaced by addLegend)
#' # -addLegend.shape (replaced by addLegend)
#' # -addLegend.size (replaced by addLegend)
#' # -getContainerComponets (replaced by getGraph)
#' # -updateContainerSize (set in app only)
#' # -deleteSelectedEdges (replaced by deleteEdges)
#' # -deleteSelectedNodes (replaced by deleteNodes)
#'
#' @name Deprecated
#' @aliases gtoy.rm
#' @aliases cea
#' @export
gtoy.rm <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function.",
        old = "gtoy.rm")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
deleteSelectedEdges <- function() {
    .Deprecated(new = "deleteEdges",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "deleteSelectedEdges")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
att.mape <- function() {
    .Deprecated(new = "att.sete",
        msg = "Deprecated function: see updated 'att.sete' function.",
        old = "att.mape")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
deleteSelectedNodes <- function() {
    .Deprecated(new = "deleteNodes",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "deleteSelectedNodes")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getContainerComponets <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getContainerComponets")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
updateContainerSize <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "updateContainerSize")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
updateGraph <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "updateGraph")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
updateCoordXY <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "updateCoordXY")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
deSelectGraph <- function(x, ...) {
    .Deprecated(new = "deleteNodes",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "deSelectGraph")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
deSelectNodes <- function(x, ...) {
    .Deprecated(new = "selectNodes",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "deSelectNodes")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
deSelectEdges <- function(x, ...) {
    .Deprecated(new = "selectNodes",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "deSelectEdges")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
selectGraph <- function(x, ...) {
    .Deprecated(new = "selectNodes",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "selectGraph")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getNodes <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getNodes")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getEdges <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getEdges")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
cea <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function.",
        old = "cea")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
mergeNodes <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "mergeNodes")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
setArrowDirection <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "setArrowDirection")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
addEdgeBetweenContainers <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "addEdgeBetweenContainers")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
nesthc <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "nesthc")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getSourceEdgeIDs <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getSourceEdgeIDs")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getTargetEdgeIDs <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getTargetEdgeIDs")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getEdgeIDs <- function(x, ...) {
    .Deprecated(new = "getGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getEdgeIDs")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
selectAllNodes <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: use 'selectNodes' function.",
        old = "selectAllNodes")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
selectAllEdges <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: use 'selectEdges' function.",
        old = "selectAllEdges")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
getNodeIDs <- function(x, ...) {
    .Deprecated(new = "addGraph",
        msg = "Deprecated function: see new 'startRedeR()' related methods.",
        old = "getNodeIDs")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
addLegend.color <- function(x, ...) {
    .Deprecated(new = "addLegend",
        msg = "Deprecated function: see new 'addLegendToRedeR' function.",
        old = "addLegend.color")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
addLegend.shape <- function(x, ...) {
    .Deprecated(new = "addLegend",
        msg = "Deprecated function: see new 'addLegendToRedeR' function.",
        old = "addLegend.shape")
}

#-------------------------------------------------------------------------------
#' @name Deprecated
#' @export
addLegend.size <- function(x, ...) {
    .Deprecated(new = "addLegend",
        msg = "Deprecated function: see new 'addLegendToRedeR' function.",
        old = "addLegend.size")
}
