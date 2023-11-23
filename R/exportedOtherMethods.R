#-------------------------------------------------------------------------------
#' @title addNodes
#'
#' @description Add nodes to an active RedeR application.
#'
#' @param nodes A vector with node names.
#' @param ... Arguments passed to internal checks (ignore).
#' @return Add nodes to an active RedeR session.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create a vector with node names
#' nodes <- c("n1", "n2", "n3", "n4", "n5")
#'
#' \donttest{
#' # Start the RedeR interface and add nodes
#' startRedeR()
#' addNodes(nodes)
#' }
#'
#' @import methods
#' @docType methods
#' @rdname addNodes-methods
#' @aliases addNodes
#' @export
setMethod(
    "addNodes", "character",
    function(nodes, ...) {

        .check.port.opt("addNodes", as.list(environment()), 
            list(...=...))
        
        .validate.args("allCharacter", "nodes", nodes)

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 1) {
            .rederexpresspost(rdp, "RedHandler.addNodes", nodes)
        } else {
            return(invisible())
        }
    }
)

#-------------------------------------------------------------------------------
#' @title addEdges
#'
#' @description Add edges to an active RedeR application.
#'
#' @param edges A vertex sequence <vector of strings> or data frame of ncol=2.
#' @param ... Arguments passed to internal checks (ignore).
#' @return Add edges to an active RedeR session.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create some edges as a vertex sequence
#' edges <- c("n1", "n2", "n1", "n3", "n1", "n4", "n1", "n5")
#'
#' # ...or as a data.frame
#' edges <- data.frame(
#'     A = c("n1", "n1", "n1", "n1"),
#'     B = c("n2", "n3", "n4", "n5")
#' )
#'
#' \donttest{
#' # Start the RedeR interface and add edges
#' startRedeR()
#' addEdges(edges)
#' }
#'
#' @import methods
#' @docType methods
#' @rdname addEdges-methods
#' @aliases addEdges
#' @export
setMethod(
    "addEdges", "character",
    function(edges, ...) {

        .check.port.opt("addEdges", as.list(environment()), 
            list(...=...))
        
        .validate.args("allCharacter", "edges", edges)

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 1) {
            .rederexpresspost(rdp, "RedHandler.addEdges", edges)
        } else {
            return(invisible())
        }
    }
)
#' @import methods
#' @docType methods
#' @rdname addEdges-methods
#' @aliases addEdges
setMethod(
    "addEdges", "data.frame",
    function(edges, ...) {

        .check.port.opt("addEdges", as.list(environment()), 
            list(...=...))
        
        if (ncol(edges) != 2) {
            stop("Edges must be provided as a 2-column 'data.frame'")
        }
        edges[, 1] <- as.character(edges[, 1])
        edges[, 2] <- as.character(edges[, 2])
        edges <- as.character(t(edges))

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 1) {
            .rederexpresspost(rdp, "RedHandler.addEdges", edges)
        } else {
            return(invisible())
        }
    }
)

#-------------------------------------------------------------------------------
#' @title deleteNodes
#'
#' @description Delete nodes from an active RedeR application.
#'
#' @param nodes A vector with node names.
#' @param ... Arguments passed to internal checks (ignore).
#' @return Remove graph objects from RedeR app.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create a vector with node names
#' nodes <- c("n1", "n2", "n3", "n4", "n5")
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Add and delete nodes
#' addNodes(nodes)
#' deleteNodes(c("n1", "n3"))
#' }
#'
#' @import methods
#' @docType methods
#' @rdname deleteNodes-methods
#' @aliases deleteNodes
#' @export
setMethod(
    "deleteNodes", "character",
    function(nodes, ...) {

        .check.port.opt("deleteNodes", as.list(environment()), 
            list(...=...))
        
        .validate.args("allCharacter", "nodes", nodes)

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 1) {
            .rederexpresspost(rdp, "RedHandler.deleteNodes", nodes)
        } else {
            return(invisible())
        }
    }
)

#-------------------------------------------------------------------------------
#' @title deleteEdges
#'
#' @description Delete edges from an active RedeR application.
#'
#' @param edges A vertex sequence <vector of strings> or data frame of ncol=2.
#' @param ... Arguments passed to internal checks (ignore).
#' @return Remove graph objects from RedeR app.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create some edges as a data.frame
#' edges <- data.frame(
#'     A = c("n1", "n1", "n1", "n1"),
#'     B = c("n2", "n3", "n4", "n5")
#' )
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#' 
#' # Add and delete edges
#' addEdges(edges)
#' deleteEdges(c("n1", "n3", "n1", "n6"))
#' }
#'
#' @import methods
#' @docType methods
#' @rdname deleteEdges-methods
#' @aliases deleteEdges
setMethod(
    "deleteEdges", "character",
    function(edges, ...) {

        .check.port.opt("deleteEdges", as.list(environment()), 
            list(...=...))
        
        .validate.args("allCharacter", "edges", edges)

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 1) {
            .rederexpresspost(rdp, "RedHandler.deleteEdges", edges)
        } else {
            return(invisible())
        }
    }
)
#' @import methods
#' @docType methods
#' @rdname deleteEdges-methods
#' @aliases deleteEdges
#' @export
setMethod(
    "deleteEdges", "data.frame",
    function(edges, ...) {

        .check.port.opt("deleteEdges", as.list(environment()), 
            list(...=...))
        
        if (ncol(edges) != 2) {
            stop("Edges must a 'vector' or a 2-column 'data.frame'")
        }
        edges[, 1] <- as.character(edges[, 1])
        edges[, 2] <- as.character(edges[, 2])
        edges <- as.character(t(edges))

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 1) {
            .rederexpresspost(rdp, "RedHandler.deleteEdges", edges)
        } else {
            return(invisible())
        }
    }
)

#-------------------------------------------------------------------------------
#' @title selectNodes
#'
#' @description Select nodes in an active RedeR application.
#'
#' @param nodes A string or array of strings with node names.
#' @param anchor A logical value, whether to anchor nodes, which will prevent
#' the \code{\link{relax}} function from applying the relaxing algorithm
#' on the selected nodes.
#' @param nid A nest ID. This will restrict searching to a specific container.
#' @param ... Arguments passed to internal checks (ignore).
#' @return Mark nodes -- which can be handled by other methods.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create some edges as a data.frame
#' edges <- data.frame(
#'     A = c("n1", "n1", "n1", "n1"),
#'     B = c("n2", "n3", "n4", "n5")
#' )
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#' 
#' # Add edges and select nodes
#' addEdges(edges)
#' selectNodes(c("n1", "n3"))
#' }
#'
#' @import methods
#' @docType methods
#' @rdname selectNodes-methods
#' @aliases selectNodes
#' @export
setMethod(
    "selectNodes", "character",
    function(nodes, anchor = FALSE, nid = NULL, ...) {

        .check.port.opt("selectNodes", as.list(environment()), 
            list(...=...))

        .validate.args("allCharacter", "nodes", nodes)
        .validate.args("singleLogical", "anchor", anchor)
        nid <- ifelse(is.null(nid), "", nid)
        .validate.args("singleString", "nid", nid)
        anchor <- ifelse(anchor, "true", "false")

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 0) {
            return(invisible())
        }

        .deSelectNodes(rdp)
        if (length(nodes) > 1) {
            invisible(.rederexpresspost(
                rdp, "RedHandler.selectNodeList", nodes, anchor, nid
            ))
        } else {
            invisible(.rederexpresspost(
                rdp, "RedHandler.selectNodeString", nodes, anchor, nid
            ))
        }
    }
)

#-------------------------------------------------------------------------------
#' @title selectEdges
#'
#' @description Select edges in an active RedeR application.
#'
#' @param edges A vertex sequence <vector of strings> or data frame of ncol=2.
#' @param ... Arguments passed to internal checks (ignore).
#' @return Mark edges -- which can be handled by other methods.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # Create some edges as a data.frame
#' edges <- data.frame(
#'     A = c("n1", "n1", "n1", "n1"),
#'     B = c("n2", "n3", "n4", "n5")
#' )
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Add and select edges
#' addEdges(edges)
#' selectEdges(c("n1", "n3"))
#' }
#'
#' @import methods
#' @docType methods
#' @rdname selectEdges-methods
#' @aliases selectEdges
#' @export
setMethod(
    "selectEdges", "character",
    function(edges, ...) {

        .check.port.opt("selectEdges", as.list(environment()), 
            list(...=...))
        
        .validate.args("allCharacter", "edges", edges)

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 0) {
            return(invisible())
        }

        .deSelectEdges(rdp)
        invisible(.rederexpresspost(rdp, "RedHandler.selectEdges", edges))
    }
)
#' @import methods
#' @docType methods
#' @rdname selectEdges-methods
#' @aliases selectEdges
#' @export
setMethod(
    "selectEdges", "data.frame",
    function(edges, ...) {

        .check.port.opt("selectEdges", as.list(environment()), 
            list(...=...))
        
        if (ncol(edges) != 2) {
            stop("Edges must be provided as a 2-column 'data.frame'")
        }
        edges[, 1] <- as.character(edges[, 1])
        edges[, 2] <- as.character(edges[, 2])
        edges <- as.character(t(edges))

        rdp <- getOption("RedeR")$port
        if (!is(rdp, "RedPort")) rdp <- RedPort()
        if (ping(rdp) == 0) {
            return(invisible())
        }

        .deSelectEdges(rdp)
        invisible(.rederexpresspost(rdp, "RedHandler.selectEdges", edges))
    }
)

#-------------------------------------------------------------------------------
.check.port.opt <- function(method, env, dots) {
    lst <- c(env, dots)
    if (length(lst) > 0) {
        lst <- lapply(lst, function(lt) {
            is(lt, "RedPort")
        })
        if (any(unlist(lst))) {
            msg <- paste0("'", method,
                "' no longer requires a RedPort-class object")
            stop(msg, call. = FALSE)
        }
    }
}
