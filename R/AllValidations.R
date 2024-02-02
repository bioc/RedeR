################################################################################
### Default attributes
################################################################################
.get.required.vatt <- function() {
    atts <- list("x" = 0, "y" = 0, "name" = NA)
    return(atts)
}
.get.default.vatt <- function() {
    atts <- list("nodeLabel" = NA, "nodeLabelSize" = NA,
        "nodeLabelColor" = NA, "nodeSize" = NA,
        "nodeShape" = NA, "nodeColor" = NA,
        "nodeLineWidth" = NA, "nodeLineColor" = NA,
        "nodeBend" = NA, "nodeWeight" = NA)
    return(atts)
}
.get.default.eatt <- function(is.directed = FALSE) {
    atts <- list("edgeLineWidth" = NA, "edgeLineColor" = NA,
        "edgeLineType" = NA, "edgeWeight" = NA,
        "arrowLength" = NA, "arrowAngle" = NA)
    if (is.directed) {
        atts$arrowType <- 1
    } else {
        atts$arrowType <- 0
    }
    return(atts)
}
.get.default.gatt <- function() {
    atts <- list("nestLabel" = NA, "nestShape" = NA, "nestLineType" = NA,
        "nestColor" = NA, "nestLineColor" = NA, "nestLabelColor" = NA,
        "nestLabelCoords" = NA, "nestLabelSize" = NA, "nestLineWidth" = NA,
        "nestSize" = NA, "isAnchored" = NA, "isAssigned" = NA, "gscale" = NA,
        "gcoord" = NA, "bgcolor" = NA)
    return(atts)
}

################################################################################
### Validate igraph for GraphSpace
################################################################################
.validate.igraph <- function(g, layout, verbose = TRUE) {
    if (verbose) message("** Validating the 'igraph' object...")
    if (!is(g, "igraph")) {
        stop("'g' should be an 'igraph' object.", call. = FALSE)
    }
    if (!is.null(layout)) {
        if (nrow(layout) != vcount(g)) {
            msg <- paste("'layout' must have xy-coordinates",
                "for the exact number of nodes in 'g'")
            stop(msg, call. = FALSE)
        } else {
            V(g)$x <- layout[, 1]
            V(g)$y <- layout[, 2]
        }
    } else if (is.null(V(g)$x) || is.null(V(g)$y)) {
        layout <- layout_nicely(g)
        V(g)$x <- layout[, 1]
        V(g)$y <- layout[, 2]
        msg <- paste0("** Using a layout algorithm automatically...")
        if (verbose) message(msg)
    }
    if (is.null(V(g)$name)) {
        msg <- paste0("** Vertex names not available; 'name' attribute ",
            "will be assigned automatically.")
        if (verbose) message(msg)
        V(g)$name <- paste0("n", seq_len(vcount(g)))
    } else if (anyDuplicated(V(g)$name) > 0) {
        msg <- paste0("'name' vertex attribute should not contain ",
            "duplicated names.")
        stop(msg, call. = FALSE)
    }
    if (!is_simple(g)) {
        if (verbose && any_loop(g)) message("** Removing loops...")
        if (verbose && any_multiple(g))
            message("** Merging duplicated edges...")
        g <- simplify(g, edge.attr.comb = list(weight = "max", "first"))
    }
    g <- .validate.nodes(g)
    g <- .validate.edges(g)
    g <- .validate.graph(g)
    g <- .validate.others(g)
    return(g)
}

#-------------------------------------------------------------------------------
.validate.nodes <- function(g) {
    atts <- c(.get.required.vatt(), .get.default.vatt())
    a_names <- names(atts)
    b_names <- a_names[a_names %in% vertex_attr_names(g)]
    if (vcount(g) > 0) {
        .validate.vatt(vertex_attr(g)[b_names])
    }
    c_names <- a_names[!a_names %in% b_names]
    if (length(c_names) > 0) {
        for (at in c_names) {
            vertex_attr(g, name = at) <- atts[[at]]
        }
    }
    d_names <- vertex_attr_names(g)
    d_names <- d_names[!c_names %in% a_names]
    if (length(d_names) > 0) {
        for (at in d_names) {
            g <- delete_vertex_attr(g, name = at)
        }
    }
    return(g)
}

#-------------------------------------------------------------------------------
.validate.edges <- function(g) {
    atts <- .get.default.eatt(is_directed(g))
    a_names <- names(atts)
    b_names <- a_names[a_names %in% edge_attr_names(g)]
    if (ecount(g) > 0) {
        .validate.eatt(edge_attr(g)[b_names])
    }
    c_names <- a_names[!a_names %in% b_names]
    if (length(c_names) > 0) {
        for (at in c_names) {
            edge_attr(g, name = at) <- atts[[at]]
        }
    }
    d_names <- edge_attr_names(g)
    d_names <- d_names[!c_names %in% a_names]
    if (length(d_names) > 0) {
        for (at in d_names) {
            g <- delete_edge_attr(g, name = at)
        }
    }
    g <- .transform.arrowtype(g)
    g <- .transform.linetype(g)
    return(g)
}

#-------------------------------------------------------------------------------
.validate.graph <- function(g) {
    gatt <- .get.default.gatt()
    a_names <- names(gatt)
    b_names <- a_names[a_names %in% graph_attr_names(g)]
    if (ecount(g) > 0) {
        .validate.gatt(graph_attr(g)[b_names])
    }
    c_names <- graph_attr_names(g)
    c_names <- c_names[!c_names %in% a_names]
    if (length(c_names) > 0) {
        for (at in c_names) {
            g <- delete_graph_attr(g, name = at)
        }
    }
    if (is_directed(g)) {
        g <- .transform.directed.graph(g)
    }
    return(g)
}

#-------------------------------------------------------------------------------
.validate.others <- function(g) {
    if (!is.null(V(g)$nodeLabel)) {
        nodeLabel <- V(g)$nodeLabel
        if (sum(is.na(nodeLabel)) < length(nodeLabel)) {
            nodeLabel[is.na(nodeLabel)] <- ""
            V(g)$nodeLabel <- nodeLabel
        }
    }
    if (!is.null(E(g)$edgeLineType)) {
        edgeLineType <- E(g)$edgeLineType
        E(g)$edgeLineType <- .validate.linetypes(edgeLineType)
    }
    if (!is.null(V(g)$nodeShape)) {
        nodeShape <- V(g)$nodeShape
        V(g)$nodeShape <- .validate.shapes(nodeShape)
    }
    if (!is.null(g$nestShape)) {
        g$nestShape <- .validate.shapes(g$nestShape)
    }
    if (!is.null(g$nestLineType)) {
        g$nestLineType <- .validate.linetypes(g$nestLineType)
    }
    return(g)
}

#-------------------------------------------------------------------------------
.validate.shapes <- function(vshapes) {
    if (!anyNA(vshapes)) {
        if (is.numeric(vshapes)) {
            vshapes[vshapes > 25] <- 21
            vshapes[vshapes < 0] <- 1
            shapes <- .point2shapes()
            vshapes <- names(shapes)[match(vshapes, shapes)]
        } else {
            shapes <- c("ELLIPSE", "RECTANGLE", "ROUNDED_RECTANGLE",
                "TRIANGLE", "DIAMOND")
            vshapes <- toupper(vshapes)
            vshapes[grep("CIRCLE", vshapes)] <- "ELLIPSE"
            vshapes[grep("SQUARE", vshapes)] <- "RECTANGLE"
            vshapes[grep("TRIANGLE", vshapes)] <- "TRIANGLE"
            vshapes[grep("DIAMOND", vshapes)] <- "DIAMOND"
            vshapes[!vshapes %in% shapes] <- "ELLIPSE"
        }
    }
    return(vshapes)
}
.validate.linetypes <- function(linetypes) {
    if (!anyNA(linetypes)) {
        if (is.numeric(linetypes)) {
            linetypes[linetypes > 6] <- 1
            linetypes[linetypes < 0] <- 1
            ltypes <- .lty2linetypes()
            linetypes <- names(ltypes)[match(linetypes, ltypes)]
        } else {
            ltypes <- c("SOLID", "DOTTED", "DASHED", "LONG_DASH")
            linetypes <- toupper(linetypes)
            linetypes[grep("SOLID", linetypes)] <- "SOLID"
            linetypes[grep("DOT|13", linetypes)] <- "DOTTED"
            linetypes[grep("DASHED|44", linetypes)] <- "DASHED"
            linetypes[grep("LONGDASH|73", linetypes)] <- "LONG_DASH"
            linetypes[grep("TWODASH|2262", linetypes)] <- "DASHED"
            linetypes[grep("2121", linetypes)] <- "DASHED"
            linetypes[!linetypes %in% ltypes] <- "SOLID"
        }
    }
    return(linetypes)
}

#-------------------------------------------------------------------------------
.lty2linetypes <- function() {
    lty <- c(0:6)
    names(lty) <- c("SOLID", "SOLID", "DASHED", "DOTTED", "DASHED",
        "LONG_DASH", "DASHED")
    return(lty)
}
.point2shapes <- function() {
    shapes <- c(
        0,
        1, 2, 3, 4, 5,
        6, 7, 8, 9, 10,
        11, 12, 13, 14, 15,
        16, 17, 18, 19, 20,
        21, 22, 23, 24, 25)
    names(shapes) <- c("RECTANGLE",
        "ELLIPSE", "TRIANGLE", "RECTANGLE", "RECTANGLE", "DIAMOND",
        "TRIANGLE", "RECTANGLE", "ELLIPSE", "DIAMOND", "ELLIPSE",
        "ELLIPSE", "RECTANGLE", "RECTANGLE", "RECTANGLE", "RECTANGLE",
        "ELLIPSE", "TRIANGLE", "DIAMOND", "ELLIPSE", "ELLIPSE",
        "ELLIPSE", "RECTANGLE", "DIAMOND", "TRIANGLE", "TRIANGLE"
    )
    return(shapes)
}

################################################################################
### Validate attribute values
################################################################################
.validate.vatt <- function(atts) {
    if (!is.null(atts[["x"]])) {
        .validate.args("numeric_vec", "x", atts[["x"]])
    }
    if (!is.null(atts[["y"]])) {
        .validate.args("numeric_vec", "y", atts[["y"]])
    }
    if (!is.null(atts[["name"]])) {
        .validate.args("allCharacter", "name", atts[["name"]])
    }
    if (!is.null(atts[["nodeLabel"]])) {
        .validate.args("allCharacterOrNa", "nodeLabel", atts[["nodeLabel"]])
    }
    if (!is.null(atts[["nodeLabelSize"]])) {
        .validate.args("numeric_vec", "nodeLabelSize", atts[["nodeLabelSize"]])
        if (min(atts[["nodeLabelSize"]]) < 0) {
            stop("'nodeLabelSize' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["nodeLabelColor"]])) {
        .validate.colors("allColors", "nodeLabelColor", 
          atts[["nodeLabelColor"]])
    }
    if (!is.null(atts[["nodeSize"]])) {
        .validate.args("numeric_vec", "nodeSize", atts[["nodeSize"]])
        if (min(atts[["nodeSize"]]) < 0) {
            stop("'nodeSize' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["nodeShape"]])) {
        .validate.args("allCharacterOrInteger", "nodeShape", 
          atts[["nodeShape"]])
    }
    if (!is.null(atts[["nodeColor"]])) {
        .validate.colors("allColors", "nodeColor", atts[["nodeColor"]])
    }
    if (!is.null(atts[["nodeLineWidth"]])) {
        .validate.args("numeric_vec", "nodeLineWidth", atts[["nodeLineWidth"]])
        if (min(atts[["nodeLineWidth"]]) < 0) {
            stop("'nodeLineWidth' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["nodeLineColor"]])) {
        .validate.colors("allColors", "nodeLineColor", atts[["nodeLineColor"]])
    }
    if (!is.null(atts[["nodeBend"]])) {
        .validate.args("numeric_vec", "nodeBend", atts[["nodeBend"]])
        if (max(atts[["nodeBend"]]) > 100 || min(atts[["nodeBend"]]) < 0) {
            stop("'nodeBend' should be a vector of numeric values in [0, 100]",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["nodeWeight"]])) {
        .validate.args("numeric_vec", "nodeWeight", atts[["nodeWeight"]])
        if (min(atts[["nodeWeight"]]) < 0) {
            stop("'nodeWeight' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
}
#-------------------------------------------------------------------------------
.validate.eatt <- function(atts) {
    if (!is.null(atts[["edgeLineWidth"]])) {
        .validate.args("numeric_vec", "edgeLineWidth", atts[["edgeLineWidth"]])
        if (min(atts[["edgeLineWidth"]]) < 0) {
            stop("'edgeLineWidth' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["edgeLineColor"]])) {
        .validate.colors("allColors", "edgeLineColor", atts[["edgeLineColor"]])
    }
    if (!is.null(atts[["edgeLineType"]])) {
        .validate.args("allCharacterOrInteger", "edgeLineType",
            atts[["edgeLineType"]])
    }
    if (!is.null(atts[["arrowLength"]])) {
        .validate.args("numeric_vec", "arrowLength", atts[["arrowLength"]])
        if (min(atts[["arrowLength"]]) < 0) {
            stop("'arrowLength' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["arrowType"]])) {
        .validate.args("allCharacterOrInteger", "arrowType", 
          atts[["arrowType"]])
    }
    if (!is.null(atts[["edgeWeight"]])) {
        .validate.args("numeric_vec", "edgeWeight", atts[["edgeWeight"]])
        if (min(atts[["edgeWeight"]]) < 0) {
            stop("'edgeWeight' should be a vector of numeric values >=0",
                call. = FALSE)
        }
    }
    if (!is.null(atts[["arrowAngle"]])) {
        .validate.args("numeric_vec", "arrowAngle", atts[["arrowAngle"]])
        if (max(atts[["arrowAngle"]]) > 90 || min(atts[["arrowAngle"]]) < 10) {
            msg <- "'arrowAngle' should be a numeric vector in [10, 90]"
            stop(msg, call. = FALSE)
        }
    }
}
.validate.gatt <- function(gatt) {
    if (!is.null(gatt[["nestLabel"]])) {
        .validate.args("singleString", "nestLabel", gatt[["nestLabel"]])
    }
    if (!is.null(gatt[["nestShape"]])) {
        .validate.args("singleStringOrNumber", "nestShape", 
          gatt[["nestShape"]])
    }
    if (!is.null(gatt[["nestLineType"]])) {
        .validate.args("singleString", "nestLineType", gatt[["nestLineType"]])
    }
    if (!is.null(gatt[["nestColor"]])) {
        .validate.colors("singleColor", "nestColor", gatt[["nestColor"]])
    }
    if (!is.null(gatt[["nestLineColor"]])) {
        .validate.colors("singleColor", "nestLineColor",
          gatt[["nestLineColor"]])
    }
    if (!is.null(gatt[["nestLabelColor"]])) {
        .validate.colors("singleColor", "nestLabelColor", 
          gatt[["nestLabelColor"]])
    }
    if (!is.null(gatt[["nestLabelCoords"]])) {
        .validate.args("numeric_vec", "nestLabelCoords",
          gatt[["nestLabelCoords"]])
        if (length(gatt[["nestLabelCoords"]]) != 2) {
            stop("'nestLabelCoords' should be a numeric vector with two values",
                call. = FALSE)
        }
    }
    if (!is.null(gatt[["nestLabelSize"]])) {
        .validate.args("singleNumber", "nestLabelSize", gatt[["nestLabelSize"]])
        if (min(gatt[["nestLabelSize"]]) < 0) {
            stop("'nestLabelSize' should be a single number >=0.", 
              call. = FALSE)
        }
    }
    if (!is.null(gatt[["nestLineWidth"]])) {
        .validate.args("singleNumber", "nestLineWidth", gatt[["nestLineWidth"]])
        if (gatt[["nestLineWidth"]] < 0) {
            stop("'nestLineWidth' should be a single number >=0.", 
              call. = FALSE)
        }
    }
    if (!is.null(gatt[["nestSize"]])) {
        .validate.args("singleNumber", "nestSize", gatt[["nestSize"]])
        if (min(gatt[["nestSize"]]) < 0) {
            stop("'nestSize' should be a single number >0.", call. = FALSE)
        }
    }
    if (!is.null(gatt[["isAnchored"]])) {
        .validate.args("singleLogical", "isAnchored", gatt[["isAnchored"]])
    }
    if (!is.null(gatt[["isAssigned"]])) {
        .validate.args("singleLogical", "isAssigned", gatt[["isAssigned"]])
    }
    if (!is.null(gatt[["gscale"]])) {
        .validate.args("singleNumber", "gscale", gatt[["gscale"]])
        if (min(gatt[["gscale"]]) < 0) {
            stop("'gscale' should be a single number >0.", call. = FALSE)
        }
    }
    if (!is.null(gatt[["gcoord"]])) {
        .validate.args("numeric_vec", "gcoord", gatt[["gcoord"]])
        if (length(gatt[["gcoord"]]) != 2) {
            stop("'gcoord' should be a vector with two numeric values.",
                call. = FALSE)
        }
    }
    if (!is.null(gatt[["bgcolor"]])) {
        .validate.colors("singleColor", "bgcolor", gatt[["bgcolor"]])
    }
}

################################################################################
### Transform attribute types
################################################################################
.transform.arrowtype <- function(g) {
    if (ecount(g) > 0 && "arrowType" %in% names(edge_attr(g))) {
        eatt <- E(g)$arrowType
        gtype <- is_directed(g)
        aty <- .get.arrowtypes(gtype)
        if (.all_integerValues(eatt)) {
            idx <- !eatt %in% aty
            if (any(idx)) {
                eatt[idx] <- 1
                .get.arrowtypes(gtype, warning = TRUE)
            }
        } else {
            idx <- eatt %in% as.character(aty)
            if (any(idx)) {
                eatt[idx] <- names(aty)[match(eatt[idx], as.character(aty))]
            }
            idx <- !eatt %in% names(aty)
            if (any(idx)) {
                eatt[idx] <- "-->"
                .get.arrowtypes(gtype, warning = TRUE)
            }
            eatt <- aty[eatt]
        }
        E(g)$arrowType <- eatt
    }
    return(g)
}
.get.arrowtypes <- function(is.dir = FALSE, warning = FALSE) {
    atp1 <- c(
        "---" = 0, "--" = 0, "-" = 0,
        "-->" = 1, "->" = 1, ">" = 1,
        "<--" = 2, "<-" = 2, "<" = 2,
        "<->" = 3, "<>" = 3,
        "|->" = 4, "|>" = 4)
    atp2 <- c(
        "--|" = -1, "-|" = -1, "|" = -1,
        "|--" = -2, "|-" = -2, "|" = -2,
        "|-|" = -3, "||" = -3,
        "<-|" = -4, "<|" = -4)
    atypes <- c(atp1, atp2)
    if (is.dir) {
        atypes <- atypes[atypes %in% c(-1, 0, 1)]
    }
    if (warning) {
        gtype <- ifelse(is.dir, "directed", "undirected")
        msg1 <- paste("'arrowType' integer or character code in",
            gtype, "graphs should be in:\n")
        if (is.dir) {
            msg2 <- atypes[match(unique(atypes), atypes)]
            msg2 <- paste(msg2, paste0("'", names(msg2), "'"), sep = " = ")
            msg2 <- paste(msg2, collapse = ", ")
        } else {
            atp1 <- atp1[match(unique(atp1), atp1)]
            atp2 <- atp2[match(unique(atp2), atp2)]
            atp1 <- paste(atp1, paste0("'", names(atp1), "'"), sep = " = ")
            atp2 <- paste(atp2, paste0("'", names(atp2), "'"), sep = " = ")
            atp1 <- paste0(paste(atp1, collapse = ", "), "\n")
            atp2 <- paste(atp2, collapse = ", ")
            msg2 <- paste0(atp1, atp2)
        }
        warning(msg1, msg2, call. = FALSE)
    } else {
        return(atypes)
    }
}
#-------------------------------------------------------------------------------
.transform.linetype <- function(g) {
    if (ecount(g) > 0 && "edgeLineType" %in% names(edge_attr(g))) {
        if (.all_integerValues(E(g)$edgeLineType)) {
            lty <- E(g)$edgeLineType
            ltypes <- .get.linetype()
            lty[!lty %in% ltypes] <- 1
            lty <- ltypes[match(lty, ltypes)]
            E(g)$edgeLineType <- names(lty)
        }
    }
    return(g)
}
.get.linetype <- function() {
    atp <- c('blank' = 0, 'solid' = 1, 'dashed' = 2, 'dotted' = 3,
        'dotdash' = 4, 'longdash' = 5, 'twodash' = 6)
}

################################################################################
### Transform directed into undirected graph, preserving arrowtype information
################################################################################
.transform.directed.graph <- function(g) {
    if (ecount(g) > 0) {
        E(g)$emode <- 1
        E(g)$emode[which_mutual(g)] <- 3
        # e <- emode <- as_adjacency_matrix(g, sparse=FALSE, attr="emode")
        e <- emode <- .adjacency(g, attr = "emode")
        g <- delete_edge_attr(g, "emode")
        bl <- lower.tri(emode) & emode == 3
        emode[bl] <- 0
        edges <- arrayInd(seq_len(prod(dim(emode))), dim(emode),
            useNames = TRUE)
        edges <- as.data.frame(edges)
        colnames(edges) <- c("vertex1", "vertex2")
        edges$emode <- as.numeric(emode)
        edges$e <- as.numeric(e > 0)
        eid <- e; eid[, ] <- 0
        ut <- upper.tri(eid)
        eid[ut] <- seq_len(sum(ut))
        eid <- t(eid)
        eid[ut] <- seq_len(sum(ut))
        edges$eid <- as.numeric(eid)
        edges$ut <- as.numeric(upper.tri(e))
        edges$lt <- as.numeric(lower.tri(e))
        atts <- .extract.directed.att(g)
        if (!all(atts[, c(1, 2)] == edges[, c(1, 2)])) {
            stop("unexpected indexing during edge attribute combination.",
                call. = FALSE)
        }
        edges <- cbind(edges, atts[, -c(1, 2)])
        eid <- unique(edges$eid[edges$e > 0])
        edges <- edges[edges$eid %in% eid, ]
        edges <- edges[order(edges$eid), ]
        rownames(edges) <- NULL
        edges <- .set.arrowtype.dir(edges)
        g <- .set.undirected(g, edges)
    } else {
        g <- as.undirected(g,
            edge.attr.comb = list(weight = "max", "first"))
    }
    return(g)
}
.set.undirected <- function(g, edges) {
    g <- as.undirected(g, edge.attr.comb = "ignore")
    g <- delete_edges(g, seq_len(ecount(g)))
    e <- rbind(edges[, 1], edges[, 2])
    attr <- as.list(edges[, -c(1, 2)])
    g <- add_edges(g, e, attr = attr)
    return(g)
}
.set.arrowtype.dir <- function(edges, a_name = "arrowType") {
    # Flip ut/lt from single-edge arrows; this
    # for collecting arrows from the same mtx side
    idx <- which(edges$emode == 1 & edges$lt == 1)
    if (length(idx) > 0) {
        for (i in idx) {
            ii <- which(edges$eid == edges$eid[i])
            edges[ii, c("ut", "lt")] <- edges[ii, c("lt", "ut")]
        }
    }
    # collect left-side arrows
    arrow1 <- edges[edges$lt == 1, a_name]
    arrow1[is.na(arrow1)] <- 0
    # collect right-side arrows
    arrow2 <- edges[edges$ut == 1, a_name]
    arrow2[is.na(arrow2)] <- 0
    # get single-edge assigments
    edges <- edges[, -which(colnames(edges) %in% a_name)]
    edges <- edges[edges$e == 1, ]
    eid <- sort(unique(edges$eid))
    edges <- edges[order(-edges$ut, edges$eid), ]
    edges <- edges[match(eid, edges$eid), ]
    # add arrows and remove intermediate columns
    edges <- .merge.arrowtypes.dir(edges, arrow1, arrow2)
    edges <- edges[, -which(colnames(edges) %in%
        c("emode", "e", "eid", "ut", "lt"))]
    return(edges)
}
.merge.arrowtypes.dir <- function(edges, arrow1, arrow2) {
    ##  0 = "---", 1 = "-->",  2 = "<--",  3 = "<->",  4 = "|->",
    ## -1 = "--|", -2 = "|--", -3 = "|-|", -4 = "<-|",
    atypes <- c(0, 1, 2, 3, 4, -1, -2, -3, -4)
    names(atypes) <- c("00", "01", "10", "11", "-11", "0-1", "-10",
      "-1-1", "1-1")
    arrowType <- paste0(format(arrow1, digits = 1, trim = TRUE),
        format(arrow2, digits = 1, trim = TRUE))
    edges$arrowType <- as.numeric(atypes[arrowType])
    return(edges)
}
.extract.directed.att <- function(g) {
    # e <- as_adjacency_matrix(g, sparse = FALSE)
    e <- .adjacency(g)
    atts <- arrayInd(seq_len(prod(dim(e))), dim(e), useNames = TRUE)
    atts <- as.data.frame(atts)
    colnames(atts) <- c("vertex1", "vertex2")
    atts$e <- as.numeric(e)
    a_names <- edge_attr_names(g)
    ne <- e == 0
    for (at in a_names) {
        # x <- as_adjacency_matrix(g, sparse = FALSE, attr = at)
        x <- .adjacency(g, attr = at)
        x[ne] <- NA
        if (is.numeric(x)) {
            atts[[at]] <- as.numeric(x)
        } else {
            atts[[at]] <- as.character(x)
        }
    }
    rownames(atts) <- NULL
    atts <- atts[, c("vertex1", "vertex2", a_names)]
    return(atts)
}
# ..this is a fix for 'as_adjacency_matrix', when 'attr' is character
.adjacency <- function(graph, attr = NULL) {
  if(is.null(attr)){
    exattr <- rep(1, ecount(graph))
  } else {
    exattr <- edge_attr(graph, as.character(attr))
  }
  if (is.logical(exattr)) {
    res <- matrix(FALSE, nrow = vcount(graph), ncol = vcount(graph))
  } else if (is.numeric(exattr)) {
    res <- matrix(0, nrow = vcount(graph), ncol = vcount(graph))
  } else {
    res <- matrix(NA, nrow = vcount(graph), ncol = vcount(graph))
  }
  e <- igraph::ends(graph, seq_len(ecount(graph)), names = FALSE)
  res[e] <- exattr
  if (!is_directed(graph)) {
    res[e[,c(2,1)]] <- exattr
  }
  colnames(res) <- rownames(res) <- V(graph)$name
  return(res)
}

################################################################################
### Convert 'point' (default) to 'npc'
################################################################################
.convert.units <- function(g, unit, upload) {
    if (!is_igraph(g)) {
        stop("'g' should be an igraph object.", call. = FALSE)
    }
    uc <- .unit.convert(unit, upload)
    if ("nodeSize" %in% vertex_attr_names(g)) {
        vertex_attr(g, name = "nodeSize") <- V(g)$nodeSize * uc
        if (!is.null(g$legNodeSize$scale)) {
            g$legNodeSize$scale <- g$legNodeSize$scale * uc
        }
    }
    if ("nestSize" %in% graph_attr_names(g)) {
        graph_attr(g, name = "nestSize") <- g$nestSize * uc
    }
    if ("edgeLineWidth" %in% edge_attr_names(g)) {
        edge_attr(g, name = "edgeLineWidth") <- E(g)$edgeLineWidth * uc
    }
    if ("arrowLength" %in% edge_attr_names(g)) {
        edge_attr(g, name = "arrowLength") <- E(g)$arrowLength * uc
    }
    return(g)
}
.unit.convert <- function(unit, upload = TRUE) {
    opts <- c("native", "point", "npc")
    if (unit == "native") {
        opt <- getOption("RedeR")$unit
        if (.is_singleString(opt)) {
            unit <- ifelse(opt %in% opts, opt, opt[1])
        } else {
            unit <- opt[1]
        }
    }
    if (unit == "point") {
        uc <- 1
    } else if (unit == "npc") {
        uc <- ifelse(upload, 5, 1 / 5)
    } else {
        uc <- 1
    }
    return(uc)
}
