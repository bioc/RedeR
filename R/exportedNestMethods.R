#-------------------------------------------------------------------------------
#' @title nestNodes
#'
#' @description Nest nodes into containers.
#'
#' @param nodes A vector with node names available in the RedeR app.
#' @param isAssigned Logical value, whether to assign the container name to
#' the nested nodes
#' @param isAnchored Logical value, whether to anchor the container in
#' dynamic layout sessions.
#' @param gscale Expansion factor of the nest area related to a parent nest,
#' or related to the app panel.
#' @param gcoord A numeric vector with 'x' and 'y' coordinates for the center
#' of nest related to the app panel or a parent container. Coordinates
#' between 0 and 100 are set to visible areas of the app panel.
#' @param status Status of the container on the screen: 'plain',
#' 'transparent', or 'hide'.
#' @param theme Some pre-defined graph attributes. Options: 'th0', 'th1',
#' 'th2', and 'th3'.
#' @param gatt
#' \itemize{
#' A list with container attributes (see details).
#' \item{\code{nestShape}}{ A single string.}
#' \item{\code{nestSize}}{ A single number >=0.}
#' \item{\code{nestColor}}{ A single color name or hexadecimal code.}
#' \item{\code{nestLabel}}{ A single string.}
#' \item{\code{nestLabelSize}}{ A single number >=0.}
#' \item{\code{nestLabelColor}}{ A single color name or hexadecimal code.}
#' \item{\code{nestLabelCoords}}{ A numeric vector with two numbers
#' (e.g. c(x=0, y=0)).}
#' \item{\code{nestLineType}}{ A single string.}
#' \item{\code{nestLineWidth}}{ A single number >=0.}
#' \item{\code{nestLineColor}}{ A single color name or hexadecimal code.}
#' }
#' @param parent Optional argument, a nest ID of a parent nest.
#' It must be used with 'isAssign=TRUE'.
#' @param verbose A logical value specifying to display detailed messages
#' (when verbose=TRUE) or not (when verbose=FALSE).
#' @param rdp A \code{RedPort}-class object used by internal calls (ignore).
#' @return Add/change graph objects.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @details
#' The \code{gatt} argument can be used to pass detailed attributes to
#' containers, for example, \code{gatt = list(nestLabel="Nest1")}.
#' \itemize{
#' \item{Options for \code{nestShape}: "ELLIPSE", "RECTANGLE",
#' "ROUNDED_RECTANGLE", "TRIANGLE", and "DIAMOND"}
#' \item{Options for \code{nestLineType}: "SOLID", "DOTTED",
#' "DASHED", "LONG_DASH".}
#' \item{When \code{nestLabelCoords = c(x=0, y=0)} then the label will be
#' centered and placed at the top of the container.}
#' }
#'
#' @examples
#' # Initialize RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # create a graph from an edge list
#' el <- matrix(c("n1", "n2", "n3", "n4"), ncol = 2, byrow = TRUE)
#' g <- graph.edgelist(el)
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#'
#' # Add 'g' to the interface
#' addGraphToRedeR(g, layout.kamada.kawai(g))
#'
#' # Nest nodes in the interface
#' nestNodes(c("n1", "n2"), gcoord = c(30, 30))
#' nestNodes(c("n3", "n4"), gcoord = c(70, 70))
#' }
#'
#' @import methods
#' @docType methods
#' @rdname nestNodes-methods
#' @aliases nestNodes
#' @export
setMethod(
    "nestNodes", "character",
    function(nodes, isAssigned = TRUE, isAnchored = TRUE, gscale = 40,
        gcoord = c(50, 50), status = c("plain", "hide", "transparent"),
        theme = c("th0", "th1", "th2", "th3"), gatt = list(),
        parent = NULL, verbose = TRUE, rdp = NA) {

        if (!is(rdp, "RedPort")){
            rdp <- getOption("RedeR")$port
            if (!is(rdp, "RedPort")) rdp <- RedPort()
        }
        if (ping(rdp) == 0) {
            return(invisible())
        }

        .validate.args("allCharacter", "nodes", nodes)
        .validate.args("singleLogical", "isAssigned", isAssigned)
        .validate.args("singleLogical", "isAnchored", isAnchored)
        .validate.args("singleNumber", "gscale", gscale)
        .validate.args("numeric_vec", "gcoord", gcoord)
        status <- match.arg(status)
        theme <- .validate.old.args("theme", theme)
        theme <- match.arg(theme)
        theme <- switch(theme, th1 = 1, th2 = 2, th3 = 3, 0)
        if (!is.list(gatt)) {
            msg <- paste0(
                "'gatt' must be a list of valid nest attributes ",
                "(e.g. gatt$nestColor, gatt$gscale...)"
            )
            stop(msg, call. = FALSE)
        } else {
            .validate.gatt(gatt)
            if (!is.null(gatt$nestShape)) {
                gatt$nestShape <- .validate.shapes(gatt$nestShape)
            }
            if (!is.null(gatt$nestLineType)) {
                gatt$nestLineType <- .validate.linetypes(gatt$nestLineType)
            }
        }
        if (!is.null(parent)) {
            .validate.args("singleString", "parent", parent)
        }

        # get app zoom for themes
        .zoom <- .rederpost(rdp, "RedHandler.getZoom")
        .zoom <- as.numeric(.zoom)
        if (is.nan(.zoom)) {
            .zoom <- 100
        } else if (.zoom > 100) {
            .zoom <- 100
        }
        if (theme == 1) {
            if (is.null(gatt$nestShape)) 
              gatt$nestShape <- "ROUNDED_RECTANGLE"
            if (is.null(gatt$nestColor)) 
              gatt$nestColor <- "#ffffff"
            if (is.null(gatt$nestLineWidth)) 
              gatt$nestLineWidth <- 4 * (100 / .zoom)
            if (is.null(gatt$nestLabelSize)) 
              gatt$nestLabelSize <- 24 * (100 / .zoom)
            if (is.null(gatt$nestLabelCoords)) 
              gatt$nestLabelCoords <- c(5, 10.8)
        } else if (theme == 2) {
            if (is.null(gatt$nestShape)) 
              gatt$nestShape <- "ROUNDED_RECTANGLE"
            if (is.null(gatt$nestColor)) 
              gatt$nestColor <- "#ffffff"
            if (is.null(gatt$nestLineWidth)) 
              gatt$nestLineWidth <- 2 * (100 / .zoom)
            if (is.null(gatt$nestLineColor)) 
              gatt$nestLineColor <- "#000000"
            if (is.null(gatt$nestLineType)) 
              gatt$nestLineType <- "DOTTED"
        } else if (theme == 3) {
            if (is.null(gatt$nestShape)) 
              gatt$nestShape <- "ROUNDED_RECTANGLE"
            if (is.null(gatt$nestColor)) 
              gatt$nestColor <- "#ffffff"
            if (is.null(gatt$nestLineWidth)) 
              gatt$nestLineWidth <- 4 * (100 / .zoom)
            if (is.null(gatt$nestLabelSize)) 
              gatt$nestLabelSize <- 24 * (100 / .zoom)
            if (is.null(gatt$nestLineColor)) 
              gatt$nestLineColor <- "#000000"
            if (is.null(gatt$nestLineType)) 
              gatt$nestLineType <- "DOTTED"
            if (is.null(gatt$nestLabelCoords)) 
              gatt$nestLabelCoords <- c(5, 10.8)
        }
        status1 <- "plain"
        if (status %in% c("plain", "hide", "transparent")) {
            status1 <- status
        }
        status2 <- "default"
        if (is.logical(gatt$isAnchored)) {
            if (gatt$isAnchored) status2 <- "anchor"
        } else {
            if (isAnchored) status2 <- "anchor"
        }
        status3 <- "default"
        if (is.logical(gatt$isAssigned)) {
            if (gatt$isAssigned) {
                isAssigned <- TRUE
                status3 <- "assign"
                if (verbose) 
                  message("** Assigning 'parent' ID to node names...")
            }
        } else {
            if (isAssigned) {
                status3 <- "assign"
                if (verbose) 
                  message("** Assigning 'parent' ID to node names...")
            }
        }
        if (!is.null(parent) && isAssigned) {
            nodes <- paste(nodes, ".$", parent, sep = "")
        }

        # Get string attributes
        charAtt <- rep("<$$>", 6)
        if (!is.null(gatt)) {
            if (verbose) message("** Uploading nest attributes ...")
        }
        # Nest aliases
        if (!is.null(gatt$nestLabel)) {
            if (verbose) message("...nest 'label'")
            charAtt[1] <- gatt$nestLabel[1]
        }
        # Nest shape
        if (!is.null(gatt$nestShape)) {
            charAtt[2] <- gatt$nestShape[1]
            if (verbose) message("...nest 'shape'")
        }
        # Nest line type
        if (!is.null(gatt$nestLineType)) {
            charAtt[3] <- gatt$nestLineType[1]
            if (verbose) message("...nest 'line type'")
        }
        # Nest color
        if (!is.null(gatt$nestColor)) {
            charAtt[4] <- colorRampPalette(gatt$nestColor[1], alpha = TRUE)(1)
            if (verbose) message("...nest 'color'")
        }
        # Nest line color
        if (!is.null(gatt$nestLineColor)) {
            charAtt[5] <- colorRampPalette(gatt$nestLineColor[1], 
              alpha = TRUE)(1)
            if (verbose) message("...nest 'line color'")
        }
        # Nest font color
        if (!is.null(gatt$nestLabelColor)) {
            charAtt[6] <- colorRampPalette(gatt$nestLabelColor[1], 
              alpha = TRUE)(1)
            if (verbose) message("...nest 'label color'")
        }

        # Get numerics attributes
        numericAtt <- c(-8, -8, -1, -1, -1, -1, 909192, 909192)
        # Nest font coords
        if (is.numeric(gatt$nestLabelCoords)) {
            numericAtt[1] <- gatt$nestLabelCoords[1]
            numericAtt[2] <- gatt$nestLabelCoords[2]
            if (verbose) message("...nest label 'coords'")
        }
        # Nest font size
        if (!is.null(gatt$nestLabelSize)) {
            numericAtt[3] <- gatt$nestLabelSize[1]
            if (verbose) message("...nest font 'size'")
        }
        # Nest line width
        if (!is.null(gatt$nestLineWidth)) {
            if (verbose) message("...nest 'line width'")
            numericAtt[4] <- gatt$nestLineWidth[1]
        }
        # Nest size
        if (!is.null(gatt$nestSize)) {
            if (verbose) message("...nest 'size'")
            numericAtt[5] <- gatt$nestSize[1]
        }
        # Nest gscale
        if (is.null(gatt$nestSize)) {
            if (!is.null(gatt$gscale)) {
                numericAtt[6] <- gatt$gscale[1]
            } else {
                numericAtt[6] <- gscale
            }
            if (verbose) message("...nest 'gscale'")
        }
        # Nest gcoord
        if (!is.null(gatt$gcoord)) {
            if (verbose) message("...nest 'gcoord'")
            numericAtt[7] <- gatt$gcoord[1]
            numericAtt[8] <- gatt$gcoord[2]
        } else {
            if (length(gcoord) == 2) {
                if (verbose) message("...nest 'gcoord'")
                numericAtt[7] <- gcoord[1]
                numericAtt[8] <- gcoord[2]
            }
        }

        ref <- .rederexpresspost(
            rdp, "RedHandler.nestexpress", nodes,
            c(status1, status2, status3), charAtt, numericAtt
        )

        invisible(.updateGraph(rdp))

        return(ref)
    }
)

#-------------------------------------------------------------------------------
#' @title mergeOutEdges
#'
#' @description Method to assign out-edges to containers in an active
#' RedeR session. This method transfers edges from nodes to the respective
#' containers.
#'
#' @param nlevels Number of levels (>=1) to be merged in the nested network.
#' @param rescale Logical value, whether to rescale out-edge width to not
#' overextend the container size; if 'FALSE', it will run a simple sum when
#' combining the out-edges.
#' @param lb Custom lower bound to rescale edge width between containers.
#' @param ub Custom upper bound to rescale edge width between containers.
#' @param rdp A \code{RedPort}-class object used by internal calls (ignore).
#' @return Add/change edge assigments.
#' @author Sysbiolab.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}.
#' @examples
#' # Load RedeR and igraph
#' library(RedeR)
#' library(igraph)
#'
#' # create a fully connected graph
#' g <- igraph::make_full_graph(5)
#' V(g)$name <- paste0("n", 1:5)
#'
#' \donttest{
#' # Start the RedeR interface
#' startRedeR()
#' 
#' # Add 'g' to the interface
#' addGraphToRedeR(g, layout.kamada.kawai(g))
#'
#' # Nest nodes in the interface
#' nestNodes(c("n1", "n2", "n3"), gcoord = c(30, 30), gscale = 30)
#' nestNodes(c("n4", "n5"), gcoord = c(70, 70), gscale = 20)
#'
#' # Merge nodes between containers
#' mergeOutEdges()
#' }
#' 
#' @docType methods
#' @rdname mergeOutEdges-methods
#' @aliases mergeOutEdges
#' @export
#'
setMethod("mergeOutEdges", c("numeric_Or_missing"), 
    function(nlevels = 2, rescale = TRUE, lb = NA, 
        ub = NA, rdp = NA) {
        
        if (!is(rdp, "RedPort")){
            rdp <- getOption("RedeR")$port
            if (!is(rdp, "RedPort")) rdp <- RedPort()
        }
        if (ping(rdp) == 0) {
            return(invisible())
        }
        
        .validate.args("singleNumber", "nlevels", nlevels)
        .validate.args("singleLogical", "rescale", rescale)
        if (!is.na(lb)) .validate.args("singleNumber", "lb", lb)
        if (!is.na(ub)) .validate.args("singleNumber", "ub", ub)
        if (nlevels < 1) nlevels <- 1
        if (is.na(lb) || is.na(ub) || !rescale) {
            lb <- 0; ub <- 0
        } else {
            lb <- max(lb, 0)
            ub <- max(ub, 0)
        }
        rescale <- ifelse(rescale, "true", "false")
        for (i in seq_len(nlevels)) {
            res <- .rederexpresspost(obj = rdp,
                "RedHandler.mergeContainerOutEdges", rescale, lb, ub)
        }
        
    }
)
