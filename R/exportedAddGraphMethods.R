#-------------------------------------------------------------------------------
#' @title Adding 'igraph' objects to RedeR
#'
#' @description Methods to display igraph objects in the RedeR application.
#'
#' @param obj A \code{RedPort}-class object.
#' @param g An \code{igraph} object. It must include coordinates and names
#' assigned to \code{x}, \code{y}, and \code{name}  vertex attributes.
#' @param layout an optional numeric matrix with two columns for \code{x}
#' and \code{y} coordinates <numeric>.
#' @param gscale Expansion factor related to the app panel area <numeric>
#' @param zoom A zoom scale for the app panel (range: 0.0 to 100.0) <numeric>.
#' @param update.coord A logical value, whether to update \code{x} and \code{y}
#' coordinates in the app.
#' @param verbose A logical value specifying to display detailed messages
#' (when \code{verbose=TRUE}) or not (when \code{verbose=FALSE}).
#' @param isNested A logical value, whether to nest all nodes into a new
#' container.
#' @param ... Additional arguments passed to the
#' \code{\link{nestNodes}} function (used when \code{isNested = TRUE}).
#' @return Send igraph objects to RedeR.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}
#' @examples
#' # Initialize RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' gtoy <- graph.lattice(c(5, 5, 5))
#'
#' rdp <- RedPort()
#'
#' \dontrun{
#' calld(rdp)
#' addGraph(rdp, g = gtoy, layout = layout_nicely(gtoy))
#' }
#'
#' @import methods
#' @importFrom igraph degree vcount ecount which_mutual
#' @importFrom igraph as_edgelist as_adjacency_matrix is_simple
#' @importFrom igraph simplify V E 'V<-' 'E<-' is_directed vertex_attr
#' @importFrom igraph layout_nicely as.undirected delete_edge_attr
#' @importFrom igraph vertex_attr_names edge_attr edge_attr_names
#' @importFrom igraph delete_vertex_attr 'edge_attr<-' any_loop
#' @importFrom igraph delete_edges delete_vertices any_multiple
#' @importFrom igraph graph_attr_names graph_attr add_vertices norm_coords
#' @importFrom igraph ends 'graph_attr<-' 'vertex_attr<-'
#' @docType methods
#' @rdname addGraph-methods
#' @aliases addGraph
#' @export
setMethod(
    "addGraph", "RedPort",
    function(obj, g, layout = NULL, gscale = 75, zoom = 100,
             update.coord = TRUE, verbose = TRUE, isNested = FALSE, ...) {

        # ping reder
        if (ping(obj) == 0) {
            return(invisible())
        }

        # check igraph object
        if (!is_igraph(g)) {
            stop("'g' should be an igraph object.", call. = FALSE)
        } else {
            g <- .update_reder_att(g)
        }
        .validate.args("singleNumber", "gscale", gscale)
        .validate.args("singleNumber", "zoom", zoom)
        .validate.args("singleLogical", "update.coord", update.coord)
        .validate.args("singleLogical", "verbose", verbose)
        .validate.args("singleLogical", "isNested", isNested)
        if (!is.null(layout)) {
            if (vcount(g) <= 1) {
                layout <- NULL
            } else {
                .validate.args("numeric_mtx", "layout", layout)
                if (ncol(layout) < 2) {
                    stop("'layout' should be a two-column numeric matrix.")
                }
                layout <- layout[, c(1, 2), drop = FALSE]
            }
        }

        # return if graph is empty
        if (vcount(g) == 0) {
            return(invisible())
        }

        # validate igraph
        g <- .validate.igraph(g, layout, verbose)
        if (isNested) gatt <- .get.nest.att(g)

        # Add extra 'mock' node if vcount == 1
        # ..to keep data type as vector during server connection
        if (vcount(g) == 1) {
            g <- .add.extra.vertex(g)
        }

        # Check/get graph attribs.
        if (is.numeric(.G(g, "gscale"))) gscale <- .G(g, "gscale")[1]
        if (is.numeric(.G(g, "zoom"))) zoom <- .G(g, "zoom")[1]
        if (is.character(.G(g, "bgcolor"))) {
            bgcolor <- .G(g, "bgcolor")[1]
        } else {
            bgcolor <- NA
        }

        # Set zoom if available
        if (.is_singleNumber(zoom)) {
            if (verbose) message("** Setting graph 'zoom'...")
            invisible(.rederexpresspost(obj, "RedHandler.setZoom", zoom))
        } else {
            zoom <- 100
        }
        g <- .set.gscale(obj, g, gscale, zoom)

        if (.is_singleColor(bgcolor)) {
            bgcolor <- grDevices::colorRampPalette(bgcolor, 
              alpha = TRUE)(length(bgcolor))
            message("** Setting graph background 'color'...")
            invisible(
              .rederexpresspost(obj, "RedHandler.setBackground", bgcolor)
              )
        }

        if (verbose) message("** Uploading graph to RedeR...")

        # Set/get nodes and edges to submit to the app
        nodes <- V(g)$name
        edges <- as_edgelist(g, names = TRUE)
        edges <- cbind(as.character(edges[, 1]), as.character(edges[, 2]))

        # Add nodes, edges and set attributes (if available)
        if (verbose && vcount(g) > 0) message("...nodes!")
        if (verbose && ecount(g) > 0) message("...edges!")
        if (length(vertex_attr_names(g)) > 3) {
            if (verbose) message("** Uploading node attributes...")
        }
        nodeLabel <- V(g)$nodeLabel
        coordX <- V(g)$x
        coordY <- -V(g)$y
        nodeSize <- V(g)$nodeSize
        nodeShape <- V(g)$nodeShape
        nodeColor <- V(g)$nodeColor
        nodeBend <- V(g)$nodeBend
        nodeLineWidth <- V(g)$nodeLineWidth
        nodeLineColor <- V(g)$nodeLineColor
        nodeLabelSize <- V(g)$nodeLabelSize
        nodeLabelColor <- V(g)$nodeLabelColor
        nodeWeight <- V(g)$nodeWeight
        # nodeLabel
        if (anyNA(nodeLabel)) {
            nodeLabel <- character(2)
        } else {
            if (verbose) message("...node 'label'")
        }
        # Node coords.
        c1 <- !is.null(coordX) && !is.null(coordY)
        c2 <- length(coordX) > 1 && length(coordY) > 1
        if (update.coord && (c1 && c2)) {
            if (verbose) message("...node 'coords'")
        } else {
            coordX <- as.numeric(c(0, 0))
            coordY <- as.numeric(c(0, 0, 0))
        }
        # nodeBend
        if (anyNA(nodeBend)) {
            nodeBend <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...node 'bend'")
        }
        # nodeSize
        if (anyNA(nodeSize)) {
            nodeSize <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...node 'size'")
        }
        # nodeShape
        if (anyNA(nodeShape)) {
            nodeShape <- character(2)
        } else {
            if (verbose) message("...node 'shape'")
        }
        # nodeColor
        if (anyNA(nodeColor)) {
            nodeColor <- character(2)
        } else {
            nodeColor <- grDevices::colorRampPalette(nodeColor,
                alpha = TRUE)(length(nodeColor))
            if (verbose) message("...node 'color'")
        }
        # nodeWeight
        if (anyNA(nodeWeight)) {
            nodeWeight <- as.numeric(c(0, 0))
        } else {
            if (verbose) message("...node 'weight'")
        }
        # nodeLineWidth
        if (anyNA(nodeLineWidth)) {
            nodeLineWidth <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...node 'line width'")
        }
        # nodeLineColor
        if (anyNA(nodeLineColor)) {
            nodeLineColor <- character(2)
        } else {
            nodeLineColor <- grDevices::colorRampPalette(nodeLineColor,
                alpha = TRUE)(length(nodeLineColor))
            if (verbose) message("...node 'line color'")
        }
        # nodeLabelSize
        if (anyNA(nodeLabelSize)) {
            nodeLabelSize <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...node 'label size'")
        }
        # nodeLabelColor
        if (anyNA(nodeLabelColor)) {
            nodeLabelColor <- character(2)
        } else {
            nodeLabelColor <- grDevices::colorRampPalette(nodeLabelColor,
                alpha = TRUE)(length(nodeLabelColor))
            if (verbose) message("...node 'label color'")
        }

        # Get/set edges attributes (if available)
        edgeWeight <- E(g)$edgeWeight
        edgeLineWidth <- E(g)$edgeLineWidth
        edgeLineColor <- E(g)$edgeLineColor
        edgeLineType <- E(g)$edgeLineType
        arrowType <- E(g)$arrowType
        arrowLength <- E(g)$arrowLength
        arrowAngle <- E(g)$arrowAngle

        if (length(edge_attr_names(g)) > 0 && ecount(g) > 0) {
            if (verbose) message("** Uploading edge attributes...")
        }

        # correct vectors case only one attr/edge is provided (use dummy)
        if (nrow(edges) == 1) {
            edges <- rbind(edges, edges)
            if (!is.null(edgeLineWidth)) edgeLineWidth <- c(edgeLineWidth, -1)
            if (!is.null(edgeLineColor)) edgeLineColor <- c(edgeLineColor, "")
            if (!is.null(edgeLineType)) edgeLineType <- c(edgeLineType, "")
            if (!is.null(arrowType)) arrowType <- c(arrowType, -10)
            if (!is.null(arrowLength)) arrowLength <- c(arrowLength, -1)
            if (!is.null(arrowAngle)) arrowAngle <- c(arrowAngle, -1)
            if (!is.null(edgeWeight)) edgeWeight <- c(edgeWeight, 0)
        }

        # edgeLineWidth
        if (anyNA(edgeLineWidth)) {
            edgeLineWidth <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...edge 'line width'")
        }
        # edgeLineColor
        if (anyNA(edgeLineColor)) {
            edgeLineColor <- character(2)
        } else {
            edgeLineColor <- grDevices::colorRampPalette(edgeLineColor,
                alpha = TRUE)(length(edgeLineColor))
            if (verbose) message("...edge 'line color'")
        }
        # edgeLineType
        if (anyNA(edgeLineType)) {
            edgeLineType <- character(2)
        } else {
            if (verbose) message("...edge 'line type'")
        }

        # arrowType
        if (length(arrowType) > 1) {
            if (verbose) message("...arrow 'type/direction'")
        } else {
            arrowType <- as.numeric(c(-10, -10))
        }
        # arrowLength
        if (anyNA(arrowLength)) {
            arrowLength <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...arrow 'length'")
        }
        # arrowAngle
        if (anyNA(arrowAngle)) {
            arrowAngle <- as.numeric(c(-1, -1))
        } else {
            if (verbose) message("...arrow 'angle'")
        }

        # edgeWeight
        if (anyNA(edgeWeight)) {
            edgeWeight <- as.numeric(c(0, 0))
        } else {
            if (verbose) message("...edge 'weight'")
        }

        # Loading graph...
        gcoord <- c(50, 50)
        charsuppl <- c("default", ifelse(update.coord, "true", "false"))
        if (ecount(g) > 0) {
            refid <- .rederexpresspost(obj, "RedHandler.updateGraphMap",
                edges[, 1], edges[, 2], edgeLineWidth, edgeLineColor,
                edgeLineType, edgeWeight, arrowType, arrowLength, arrowAngle,
                nodes, coordX, coordY, nodeBend, nodeSize, nodeShape,
                nodeColor, nodeWeight, nodeLineWidth, nodeLineColor,
                nodeLabelSize, nodeLabelColor, nodeLabel, gcoord,
                charsuppl)
        } else {
            refid <- .rederexpresspost(obj, "RedHandler.updateNodeMap",
                nodes, coordX, coordY, nodeBend, nodeSize, nodeShape,
                nodeColor, nodeWeight, nodeLineWidth, nodeLineColor,
                nodeLabelSize, nodeLabelColor, nodeLabel, gcoord,
                charsuppl)
        }

        if (isNested) {
            refid <- nestNodes(nodes, gscale = gscale, gatt = gatt,
                verbose = verbose, rdp = obj, ... = ...)
        }

        invisible(.updateGraph(obj))

        .addGraph.deprecated_args(... = ...)

        # Check nesting ref
        if (isNested) {
            if (!is.null(refid)) {
                return(refid)
            }
        }

    }
)

#-------------------------------------------------------------------------------
.addGraph.deprecated_args <- function(nodes = NA, isAssigned = NA,
    isAnchored = NA, gcoord = NA, status = NA, theme = NA,
    gatt = NA, parent = NA, nestImage = NA, getpack = NA, .zoom = NA,
    gzoom, loadEdges, ntransform) {
    args <- rep(0, 3)
    names(args) <- c("'gzoom'", "'loadEdges'", "'ntransform'")
    if (missing(gzoom)) args["'gzoom'"] <- NA
    if (missing(loadEdges)) args["'loadEdges'"] <- NA
    if (missing(ntransform)) args["'ntransform'"] <- NA
    args <- args[!is.na(args)]
    if (length(args) > 0) {
        args <- paste0(names(args), collapse = ", ")
        msg <- "Deprecated 'addGraph' args: "
        warning(msg, args, call. = FALSE)
    }
}

#-------------------------------------------------------------------------------
# Update RedeR attributes (lift RedeR V2.X to V3.X)
.update_reder_att <- function(g) {
    flagOld <- FALSE
    #--- update graph attrs
    gnames <- graph_attr_names(g)
    old_new <- c("bgColor", "nestAlias", "nestFontSize", "nestFontColor")
    names(old_new) <- c("bgcolor", "nestLabel", "nestLabelSize", 
      "nestLabelColor")
    old_new <- old_new[old_new %in% gnames]
    old_new <- old_new[!(names(old_new) %in% gnames)]
    if (length(old_new) > 0) {
        flagOld <- TRUE
        for (i in seq_along(old_new)) {
            at <- old_new[i]
            graph_attr(g, name = names(at)) <- graph_attr(g, name = at)
        }
    }
    #--- update vertex attrs
    vnames <- vertex_attr_names(g)
    old_new <- c("nodeAlias", "nodeFontSize", "nodeFontColor")
    names(old_new) <- c("nodeLabel", "nodeLabelSize", "nodeLabelColor")
    old_new <- old_new[old_new %in% vnames]
    old_new <- old_new[!(names(old_new) %in% vnames)]
    if (length(old_new) > 0) {
        flagOld <- TRUE
        for (i in seq_along(old_new)) {
            at <- old_new[i]
            vertex_attr(g, name = names(at)) <- vertex_attr(g, name = at)
        }
    }
    if (!"x" %in% vnames && "coordX" %in% vnames) {
        flagOld <- TRUE
        vertex_attr(g, name = "x") <- V(g)$coordX
    }
    if (!"y" %in% vnames && "coordY" %in% vnames) {
        flagOld <- TRUE
        vertex_attr(g, name = "y") <- -V(g)$coordY
    }
    #--- update edge attrs
    enames <- edge_attr_names(g)
    if ("arrowDirection" %in% enames) {
        flagOld <- TRUE
        edge_attr(g, name = "arrowType") <- E(g)$arrowDirection
    }
    if (flagOld) {
        # g <- .rescale.old.nodesize.v2(g)
        msg1 <- "The input 'g' object seems to use old RedeR attribute names;\n"
        msg2 <- "...on this time 'g' will be updated on the fly!\n"
        msg3 <- "...but this may lead to incorrect formatting.\n"
        msg4 <- "...please, see RedeR's vignette for updated attribute types."
        warning(msg1, msg2, msg3, msg4, call. = FALSE)
    }
    return(g)
}

#-------------------------------------------------------------------------------
.get.nest.att <- function(g) {
    gatt <- .get.default.gatt()
    attnames <- graph_attr_names(g)
    attnames <- attnames[attnames %in% names(gatt)]
    if (length(attnames) > 0) {
        for (at in attnames) {
            tp <- graph_attr(g, at)
            if (is.vector(tp) && !is.list(tp)) {
                gatt[[at]] <- tp
            }
        }
    }
    gatt <- gatt[!unlist(lapply(gatt, anyNA))]
    return(gatt)
}

#-------------------------------------------------------------------------------
.set.gscale <- function(obj, g, gscale, zoom) {
    coordX <- V(g)$x
    coordY <- V(g)$y
    pScale <- .rederpost(obj, "RedHandler.getPanelScale")
    pScale <- as.numeric(pScale)[1]
    pScale <- pScale * (gscale / 100)
    layout <- .normalize.xy(xy = cbind(coordX, coordY), to = c(0, pScale))
    V(g)$x <- layout[, 1]
    V(g)$y <- layout[, 2]
    return(g)
}

#-------------------------------------------------------------------------------
.normalize.g <- function(g, to = c(0, 100)) {
    if (vcount(g) > 0) {
        x <- V(g)$x
        y <- V(g)$y
        x <- x - mean(range(x))
        y <- y - mean(range(y))
        from <- range(c(x, y))
        x <- rescale(x, from = from, to = to)
        y <- rescale(y, from = from, to = to)
        x <- x - min(x)
        y <- y - min(y)
        V(g)$x <- x
        V(g)$y <- y
    }
    return(g)
}
.normalize.xy <- function(xy, to = c(0, 100)) {
    if (nrow(xy) > 0) {
        x <- xy[, 1]
        y <- xy[, 2]
        x <- x - mean(range(x))
        y <- y - mean(range(y))
        from <- range(c(x, y))
        x <- rescale(x, from = from, to = to)
        y <- rescale(y, from = from, to = to)
        x <- x - min(x)
        y <- y - min(y)
        xy[, 1] <- x
        xy[, 2] <- y
    }
    return(xy)
}

#-------------------------------------------------------------------------------
.add.extra.vertex <- function(g) {
    g <- add_vertices(g, 1)
    V(g)$name[2] <- "MM0<$$>"
    if (!is.null(V(g)$nodeLabel)) {
        V(g)$nodeLabel[2] <- V(g)$nodeLabel[1]
    }
    if (!is.null(V(g)$coordX)) {
        V(g)$coordX[2] <- V(g)$coordX[1]
    }
    if (!is.null(V(g)$coordY)) {
        V(g)$coordY[2] <- V(g)$coordY[1]
    }
    if (!is.null(V(g)$nodeBend)) {
        V(g)$nodeBend[2] <- V(g)$nodeBend[1]
    }
    if (!is.null(V(g)$nodeSize)) {
        V(g)$nodeSize[2] <- V(g)$nodeSize[1]
    }
    if (!is.null(V(g)$nodeShape)) {
        V(g)$nodeShape[2] <- V(g)$nodeShape[1]
    }
    if (!is.null(V(g)$nodeColor)) {
        V(g)$nodeColor[2] <- V(g)$nodeColor[1]
    }
    if (!is.null(V(g)$nodeWeight)) {
        V(g)$nodeWeight[2] <- V(g)$nodeWeight[1]
    }
    if (!is.null(V(g)$nodeLineWidth)) {
        V(g)$nodeLineWidth[2] <- V(g)$nodeLineWidth[1]
    }
    if (!is.null(V(g)$nodeLineColor)) {
        V(g)$nodeLineColor[2] <- V(g)$nodeLineColor[1]
    }
    if (!is.null(V(g)$nodeLabelSize)) {
        V(g)$nodeLabelSize[2] <- V(g)$nodeLabelSize[1]
    }
    if (!is.null(V(g)$nodeLabelColor)) {
        V(g)$nodeLabelColor[2] <- V(g)$nodeLabelColor[1]
    }
    return(g)
}
