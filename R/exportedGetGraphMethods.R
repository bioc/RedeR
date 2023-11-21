#-------------------------------------------------------------------------------
#' @title Get graphs from RedeR
#'
#' @description Methods to wrap up RedeR graphs into igraph's R objects.
#'
#' @param obj A \code{RedPort}-class object.
#' @param status A filter (string) indicating the status of the graph elements
#' that should be fetched from the RedeR app (default='all').
#' @param attribs A filter (string) indicating the graph attributes
#' that should be fetched from the RedeR app (default='all').
#' @param type A filter (string) indicating the graph element types that
#' should be fetched from the RedeR app (default='node').
#' @return igraph objects from RedeR.
#' @author Sysbiolab.
#' @seealso \code{\link{getGraphFromRedeR}}
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' g <- graph.lattice(c(5, 5, 5))
#'
#' rdp <- RedPort()
#'
#' \donttest{
#' calld(rdp)
#' addGraph(rdp, g, layout_nicely(g))
#' g <- getGraph(rdp)
#' }
#'
#' @import methods
#' @importFrom igraph graph_from_data_frame is_igraph
#' @importFrom igraph make_empty_graph set_vertex_attr
#' @importFrom igraph set_graph_attr add_edges delete_graph_attr
#' @importFrom scales rescale
#' @docType methods
#' @rdname getGraph-methods
#' @aliases getGraph
#' @export
setMethod(
    "getGraph", "RedPort",
    function(obj, status = c("all", "selected", "notselected"),
        attribs = c("all", "minimal"),
        type = c("node", "container", "all")) {
        if (ping(obj) == 0) {
            return(make_empty_graph(n = 0, directed = FALSE))
        }
        status <- match.arg(status)
        type <- match.arg(type)
        attribs <- match.arg(attribs)
        # Get graph objects from RedeR app
        nodes <- .getNodes(obj, status, type)
        edges <- .getEdges(obj, status, type)
        # Build igraph object
        if (length(nodes) == 1 && nodes == "") {
            g <- make_empty_graph(n = 0, directed = FALSE)
            return(g)
        } else if (length(edges) == 1 && edges == "") {
            edges <- NULL
        } else {
            edges <- matrix(edges, ncol = 2, byrow = TRUE)
            colnames(edges) <- c("NodeA", "NodeB")
            edges <- data.frame(edges, stringsAsFactors = FALSE)
        }
        nodes <- data.frame(name = nodes, stringsAsFactors = FALSE)
        nodes$x <- .getNodeX(obj, status, type)
        nodes$y <- .getNodeY(obj, status, type)
        # Add required attributes
        if (attribs == "minimal") {
            if (is.null(edges)) {
                g <- make_empty_graph(n = nrow(nodes), directed = FALSE)
                g <- set_vertex_attr(g, "name", value = nodes$name)
                g <- set_vertex_attr(g, "x", value = nodes$x)
                g <- set_vertex_attr(g, "y", value = nodes$y)
            } else {
                idx <- (edges$NodeA %in% nodes$name) & 
                  (edges$NodeB %in% nodes$name)
                edges <- edges[idx, ]
                g <- graph_from_data_frame(edges, directed = FALSE,
                    vertices = nodes)
            }
        } else {
            nodes$nodeLabel <- .getNodeLabel(obj, status, type)
            nodes$nodeLabelSize <- .getNodeLabelSize(obj, status, type)
            nodes$nodeLabelColor <- .getNodeLabelColor(obj, status, type)
            nodes$nodeSize <- .getNodeSize(obj, status, type)
            nodes$nodeShape <- .getNodeShape(obj, status, type)
            nodes$nodeBend <- .getNodeBend(obj, status, type)
            nodes$nodeColor <- .getNodeColor(obj, status, type)
            nodes$nodeLineWidth <- .getNodeLineWidth(obj, status, type)
            nodes$nodeLineColor <- .getNodeLineColor(obj, status, type)
            nodes$nodeWeight <- .getNodeWeight(obj, status, type)
            if (is.null(edges)) {
                g <- make_empty_graph(n = nrow(nodes), directed = FALSE)
                g <- set_vertex_attr(g, "name", value = nodes$name)
                g <- set_vertex_attr(g, "x", value = nodes$x)
                g <- set_vertex_attr(g, "y", value = nodes$y)
                g <- set_vertex_attr(g, "nodeLabel", value = nodes$nodeLabel)
                g <- set_vertex_attr(g, "nodeLabelSize", 
                  value = nodes$nodeLabelSize)
                g <- set_vertex_attr(g, "nodeLabelColor", 
                  value = nodes$nodeLabelColor)
                g <- set_vertex_attr(g, "nodeSize", value = nodes$nodeSize)
                g <- set_vertex_attr(g, "nodeShape", value = nodes$nodeShape)
                g <- set_vertex_attr(g, "nodeBend", value = nodes$nodeBend)
                g <- set_vertex_attr(g, "nodeColor", value = nodes$nodeColor)
                g <- set_vertex_attr(g, "nodeLineWidth", 
                  value = nodes$nodeLineWidth)
                g <- set_vertex_attr(g, "nodeLineColor", 
                  value = nodes$nodeLineColor)
                g <- set_vertex_attr(g, "nodeWeight", value = nodes$nodeWeight)
            } else {
                edges$edgeLineType <- .getEdgeLineType(obj, status, type)
                edges$edgeLineWidth <- .getEdgeLineWidth(obj, status, type)
                edges$edgeLineColor <- .getEdgeLineColor(obj, status, type)
                edges$arrowType <- .getArrowDirection(obj, status, type)
                edges$arrowLength <- .getArrowLength(obj, status, type)
                edges$arrowAngle <- .getArrowAngle(obj, status, type)
                edges$edgeWeight <- .getEdgeWeight(obj, status, type)
                idx <- (edges$NodeA %in% nodes$name) & 
                  (edges$NodeB %in% nodes$name)
                edges <- edges[idx, ]
                g <- graph_from_data_frame(edges, directed = FALSE,
                    vertices = nodes)
            }
        }
        g <- .normalize.g(g)
        g$zoom <- .rederpost(obj, "RedHandler.getZoom")
        return(g)
    }
)
