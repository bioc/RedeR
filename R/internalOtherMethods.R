################################################################################
### Internal Functions (most from no longer exported -deprecated- methods)
################################################################################

#-------------------------------------------------------------------------------
.getContainerComponets <- function(obj, container) {
    container <- as.character(container)
    .rederexpresspost(obj, "RedHandler.getContainerComponets", container)
}
#-------------------------------------------------------------------------------
.updateContainerSize <- function(obj) {
    return(.rederpost(obj, "RedHandler.updateContainerSize"))
}
#-------------------------------------------------------------------------------
.G <- function(g, att) {
    graph_attr(g, att)
}
#-------------------------------------------------------------------------------
.updateGraph <- function(obj) {
    invisible(.rederpost(obj, "RedHandler.updateGraph"))
}
#-------------------------------------------------------------------------------
.getNodeLabel <- function(obj, status = "all", type = "node") {
    return(.rederpost(obj, "RedHandler.getNodeAliases", type, status))
}
#-------------------------------------------------------------------------------
.getNodeX <- function(obj, status = "all", type = "node") {
    return(.rederpost(obj, "RedHandler.getNodeX", type, status))
}
#-------------------------------------------------------------------------------
.getNodeY <- function(obj, status = "all", type = "node") {
    NodeY <- .rederpost(obj, "RedHandler.getNodeY", type, status)
    NodeY <- rescale(-NodeY, to = range(NodeY))
    return(NodeY)
}
#-------------------------------------------------------------------------------
.getNodeBend <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeBend", type, status)
}
#-------------------------------------------------------------------------------
.getNodeSize <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeSize", type, status)
}
#-------------------------------------------------------------------------------
.getNodeShape <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeShape", type, status)
}
#-------------------------------------------------------------------------------
.getNodeColor <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeColor", type, status)
}
#-------------------------------------------------------------------------------
.getNodeLineWidth <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeLineWidth", type, status)
}
#-------------------------------------------------------------------------------
.getNodeLineColor <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeLineColor", type, status)
}
#-------------------------------------------------------------------------------
.getNodeLabelSize <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeFontSize", type, status)
}
#-------------------------------------------------------------------------------
.getNodeLabelColor <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeFontColor", type, status)
}
#-------------------------------------------------------------------------------
.getNodeWeight <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getNodeWeight", type, status)
}
#-------------------------------------------------------------------------------
.getArrowDirection <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getArrowDirection", type, status)
}
#-------------------------------------------------------------------------------
.getArrowLength <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getArrowLength", type, status)
}
#-------------------------------------------------------------------------------
.getArrowAngle <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getArrowAngle", type, status)
}
#-------------------------------------------------------------------------------
.getEdgeLineWidth <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getEdgeWidth", type, status)
}
#-------------------------------------------------------------------------------
.getEdgeLineColor <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getEdgeColor", type, status)
}
#-------------------------------------------------------------------------------
.getEdgeLineType <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getEdgeType", type, status)
}
#-------------------------------------------------------------------------------
.getEdgeWeight <- function(obj, status = "all", type = "node") {
    .rederpost(obj, "RedHandler.getEdgeWeight", type, status)
}
#-------------------------------------------------------------------------------
.selectGraph <- function(obj) {
    .rederpost(obj, "RedHandler.selectGraph")
}
#-------------------------------------------------------------------------------
.deSelectEdges <- function(obj) {
    .rederpost(obj, "RedHandler.deSelectEdges")
}
#-------------------------------------------------------------------------------
.deSelectNodes <- function(obj) {
    .rederpost(obj, "RedHandler.deSelectNodes")
}
#-------------------------------------------------------------------------------
.deSelectGraph <- function(obj) {
    .rederpost(obj, "RedHandler.deSelectGraph")
}

#-------------------------------------------------------------------------------
.getNodes <- function(obj, status = "selected", type = "node") {
    .rederpost(obj, "RedHandler.getNodes", type, status)
}

#-------------------------------------------------------------------------------
.getEdges <- function(obj, status = "selected", type = "node") {
    .rederpost(obj, "RedHandler.getEdges", type, status)
}

#-------------------------------------------------------------------------------
.updateGraphLayout <- function(obj, g, delNodes = FALSE, delEdges = FALSE) {
    if (!is_igraph(g)) {
        stop("'g' object should be an igraph object!")
    }
    # g <- .update_reder_att(g)
    # g <- .validate.igraph(g, layout=NULL, verbose=FALSE)
    .validate.args("singleLogical", "delNodes", delNodes)
    .validate.args("singleLogical", "delEdges", delEdges)
    if (is.null(V(g)$x)) V(g)$x <- 0
    if (is.null(V(g)$y)) V(g)$y <- 0
    gcls <- class(g)
    invisible(.rederpost(obj, "RedHandler.stopRelax"))
    invisible(.rederpost(obj, "RedHandler.lockDragAndZoom"))
    gg <- getGraph(obj, attribs = "minimal")
    invisible(.rederpost(obj, "RedHandler.unLockDragAndZoom"))
    if (vcount(gg) == 0) {
        tp1 <- "No vertex appears to be available in the ReadeR interface.\n"
        tp2 <- "The 'updateLayoutFromRedeR' function aims to "
        tp3 <- "update node coordinates in the 'g' object "
        tp4 <- "using graph coordinates from the RedeR interface!"
        stop(tp1, tp2, tp3, tp4)
    }
    if (is.null(V(g)$name)) {
        if (vcount(g) != vcount(gg)) {
            tp1 <- "Number of vertices in 'g' and "
            tp2 <- "in the RedeR interface is different,\n"
            tp3 <- "and vertices in the 'g' object are not named.\n"
            tp4 <- "Not possible to match these graphs."
            stop(tp1, tp2, tp3, tp4)
        }
        V(g)$name <- as.character(seq_len(vcount(g)))
        V(gg)$name <- as.character(seq_len(vcount(gg)))
        tp1 <- "vertices in 'g' are not named; "
        tp2 <- "it will be used vertex numbers as IDs."
        warning(tp1, tp2, call. = FALSE)
    }
    if (sum(V(g)$name %in% V(gg)$name) == 0) {
        tp1 <- "No vertices in 'g' seem to match "
        tp2 <- "vertices in the ReadeR interface."
        warning(tp1, tp2, call. = FALSE)
    }
    #--- intersect vertices
    nodes <- intersect(V(g)$name, V(gg)$name)
    idx <- which(!V(g)$name %in% nodes)
    if (delNodes && length(idx) > 0) {
        message("Deleting ", length(idx), " vertices from 'g'!")
        g <- delete_vertices(g, v = idx)
    }
    idx <- which(!V(gg)$name %in% nodes)
    if (length(idx) > 0) {
        gg <- delete_vertices(gg, v = idx)
    }
    #--- normalize coordinates in both graphs
    g <- .normalize.g(g)
    gg <- .normalize.g(gg)
    #--- update coords
    message("Updating 'x' and 'y' coordinates...")
    idx <- match(V(gg)$name, V(g)$name)
    V(g)$x[idx] <- V(gg)$x
    V(g)$y[idx] <- V(gg)$y
    #--- delete edges
    if (delEdges) {
        eg <- .get_elist(g)
        egg <- .get_elist(gg)
        if (nrow(eg) > 0 && nrow(egg) > 0) {
            idx <- eg$ID12 %in% egg$ID12 | eg$ID12 %in% egg$ID21
            idx <- which(!idx)
            if (length(idx) > 0) {
                message("Deleting ", length(idx), " edges from 'g'!")
                g <- delete_edges(g, idx)
            }
        }
    }
    #--- update zoom and class
    g$zoom <- .rederpost(obj, "RedHandler.getZoom")
    class(g) <- gcls
    return(g)
}
.get_elist <- function(g) {
    el <- as_edgelist(g)
    if (nrow(el) > 0) {
        el <- data.frame(as_edgelist(g), stringsAsFactors = FALSE)
        colnames(el) <- c("Node1", "Node2")
        el$ID12 <- paste0(el$Node1, "|", el$Node2)
        el$ID21 <- paste0(el$Node2, "|", el$Node1)
    }
    return(el)
}
