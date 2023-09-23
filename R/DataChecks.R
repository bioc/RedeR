## -----------------------------------------------------------------------------
# check available default igraph attrbs
check.igraph.format <- function(g) {
  #---remove vertex attributes set as lists
  vnames <- vertex_attr_names(g)
  if (length(vnames) > 0) {
    for (i in 1:length(vnames)) {
      if (is.list(vertex_attr(g, vnames[i]))) {
        g <- delete_vertex_attr(g, vnames[i])
      } else {
        vnames[i] <- NA
      }
    }
    if (any(!is.na(vnames))) {
      tp1 <- paste0("The following vertex attributes are ",
        "set as lists and will be removed:\n")
      warning(tp1, paste(vnames[!is.na(vnames)], collapse = ", "),
        call. = FALSE)
    }
  }
  #---remove edge attributes set as lists
  enames <- edge_attr_names(g)
  if (length(enames) > 0) {
    for (i in 1:length(enames)) {
      if (is.list(edge_attr(g, enames[i]))) {
        g <- delete_edge_attr(g, enames[i])
      } else {
        enames[i] <- NA
      }
    }
    if (any(!is.na(enames))) {
      tp1 <- paste0("The following edge attributes are ",
        "set as lists and will be removed:\n")
      warning(tp1, paste(enames[!is.na(enames)], collapse = ", "),
        call. = FALSE)
    }
  }
  #--- Set igraph coordinates
  # Note1: y-coordinate is flipped at the Java side
  # Note2: coordX/coordY attrbs are retained for compatibility
  if (is.null(V(g)$coordX)) {
    if (!is.null(V(g)$x)) {
      V(g)$coordX <- V(g)$x
    }
  }
  if (is.null(V(g)$coordY)) {
    if (!is.null(V(g)$y)) {
      V(g)$coordY <- -V(g)$y
    }
  }
  #--- Set igraph attributes
  if (!is.null(V(g)$color) && is.null(V(g)$nodeColor))
    V(g)$nodeColor <- V(g)$color
  if (!is.null(V(g)$color) && is.null(V(g)$nodeColor))
    V(g)$nodeColor <- V(g)$color
  if (!is.null(V(g)$frame.color) && is.null(V(g)$nodeLineColor))
    V(g)$nodeLineColor <- V(g)$frame.color
  if (!is.null(V(g)$size) && is.null(V(g)$nodeSize))
    V(g)$nodeSize <- V(g)$size * 2
  if (!is.null(V(g)$label) && is.null(V(g)$nodeAlias))
    V(g)$nodeAlias <- V(g)$label
  if (!is.null(V(g)$label.cex) && is.null(V(g)$nodeFontSize))
    V(g)$nodeFontSize <- V(g)$label.cex * 20
  if (!is.null(V(g)$label.color) && is.null(V(g)$nodeFontColor))
    V(g)$nodeFontColor <- V(g)$label.color
  if (!is.null(V(g)$shape) && is.null(V(g)$nodeShape)) {
    shapes <- V(g)$shape
    shapes[shapes == "circle"] = "ELLIPSE"
    shapes[shapes != "circle"] = "RECTANGLE"
    V(g)$nodeShape <- shapes
  }
  if (!is.null(E(g)$width) && is.null(E(g)$edgeWidth))
    E(g)$edgeWidth <- E(g)$width
  if (!is.null(E(g)$color) && is.null(E(g)$edgeColor))
    E(g)$edgeColor <- E(g)$color
  if (!is.null(E(g)$weight) && is.null(E(g)$edgeWeight))
    E(g)$edgeWeight <- E(g)$weight
  if (!is.null(E(g)$lty) && is.null(E(g)$edgeType)) {
    edgeType <- E(g)$lty
    idx <- c(0:6)
    names(idx) <- c("blank", "solid", "dashed", "dotted", "dotdash",
      "longdash", "twodash")
    if (!is.numeric(edgeType)) {
      edgeType[!edgeType %in% names(idx)] <- "solid"
      edgeType <- idx[edgeType]
    }
    idx <- idx[edgeType + 1]
    edgeType[idx == 0] <- "SOLID"
    edgeType[idx == 1] <- "SOLID"
    edgeType[idx == 2] <- "DASHED"
    edgeType[idx == 3] <- "DOTTED"
    edgeType[idx == 4] <- "DOTTED"
    edgeType[idx == 5] <- "LONG_DASH"
    edgeType[idx == 6] <- "DASHED"
    E(g)$edgeType <- edgeType
  }
  if (is.null(V(g)$nodeLineColor) && !is.null(V(g)$color))
    V(g)$nodeLineColor = "black"
  return(g)
}

## -----------------------------------------------------------------------------
check.igraph.direction <- function(g) {
  # Check direction and type
  arrowType = E(g)$arrowType
  if (!is.null(arrowType) && length(arrowType) > 0) {
    c1 <- !all.integerValues(arrowType)
    c2 <- (sum(arrowType < -1) > 0 || sum(arrowType > 1) > 0)
    if (c1) {
      stop("'arrow type' must be integer values (-1, 0, or 1), without NAs.")
    } else if (c2) {
      stop("invalid 'arrow type' input (options: -1, 0, or 1)")
    }
    g <- .arrowtype4reder(g)
    E(g)$arrowDirection <- E(g)$arrowType
  } else {
    # set direction to edge attributes
    idxmutual <- igraph::which_mutual(g)
    E(g)$arrowDirection <- 1
    E(g)$arrowDirection[idxmutual] <- 3
    # essa funcao nao retorno ordem correta!!! controlado agora em J!
    # g=igraph::as.undirected(g, mode="collapse")
    c1 <- length(igraph::edge_attr_names(g)) > 0
    c2 <- sum(idxmutual) > 0
    if (c1 && c2) {
      warning("mutual edges were collapsed to unique edges.")
      # isso sera feito no lado Java, ultimo link define valor final!!!
    }
    # Remove multiple edges and loops
    if (!igraph::is_simple(g)) {
      g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
      warning("loops and/or multiple edges were removed.")
    }
  }
  return(g)
}
.arrowtype4reder <- function(g) {
  #---get edges and mode
  edgeMtx <- igraph::as_adjacency_matrix(g,
    sparse = FALSE, attr = NULL, names = FALSE)
  modeMtx <- igraph::as_adjacency_matrix(g,
    sparse = FALSE, attr = "arrowType", names = FALSE)
  #---differentiate upper.tri/lower.tri
  modeMtx[upper.tri(modeMtx)] <- modeMtx[upper.tri(modeMtx)] * 10
  edgeMtx[modeMtx != 0] <- modeMtx[modeMtx != 0] * 10
  #---set arrowType
  edl <- igraph::as_edgelist(g, names = FALSE)
  arrowType <- sapply(1:nrow(edl), function(i) {
    tp <- edgeMtx[edl[i, 1], edl[i, 2]] +
      edgeMtx[edl[i, 2], edl[i, 1]]
    return(tp)
  })
  arrowType[arrowType == 1 | arrowType == 2] <- 0
  #---set mode---#
  # arrow key, related to 'A->B' orientation
  #  0 = undirected:  0 (A-B or B-A)
  # +1 = simple: +10 (A->B)
  # -1 = simple: -10 (A-/B)
  # +2 = simple: +100 (B->A) -- will be +1 in reverse edges
  # -2 = simple: -100 (B-/A) -- will be -1 in reverse edges
  # +3 = double: +110 (same mode, A->B and B->A)
  # -3 = double: -110 (same mode, A-\B and B-\A)
  # +4 = double: +90 (inverse mode, A->B and B-\A)
  # -4 = double: -90 (inverse mode, A-\B and B->A)
  key <- c(0, 10, -10, 100, -100, 110, -110, 90, -90)
  lab <- c(0, 1, -1, 1, -1, 3, -3, 4, -4)
  arrowType <- sapply(1:length(arrowType), function(i) {
    tp <- arrowType[i]
    lab[which(key == tp)]
  })
  E(g)$arrowType <- arrowType
  #---remove duplicates
  idx <- which_mutual(g) & edl[, 1] > edl[, 2]
  g <- delete_edges(g, edges = which(idx))
  return(g)
}

## -----------------------------------------------------------------------------
.igraph2reder <- function(g) {
  g <- .igraph2rederCoords(g)
  g <- .igraph2rederAttribs(g)
  return(g)
}
.igraph2rederCoords <- function(g) {
  if (!is.null(V(g)$x)) {
    V(g)$coordX <- V(g)$x
    V(g)$coordX <- V(g)$coordX - mean(V(g)$coordX, na.rm = TRUE)
  }
  if (!is.null(V(g)$y)) {
    V(g)$coordY <- -V(g)$y
    V(g)$coordY <- V(g)$coordY - mean(V(g)$coordY, na.rm = TRUE)
  }
  return(g)
}
.igraph2rederAttribs <- function(g) {
  #---remove vertex attributes set as lists
  vnames <- vertex_attr_names(g)
  if (length(vnames) > 0) {
    for (i in 1:length(vnames)) {
      if (is.list(vertex_attr(g, vnames[i]))) {
        g <- delete_vertex_attr(g, vnames[i])
      } else {
        vnames[i] <- NA
      }
    }
    if (any(!is.na(vnames))) {
      tp1 <- "The following vertex attributes are lists and will be removed:\n"
      warning(tp1, paste(vnames[!is.na(vnames)],
        collapse = ", "), call. = FALSE)
    }
  }
  #---remove edge attributes set as lists
  enames <- edge_attr_names(g)
  if (length(enames) > 0) {
    for (i in 1:length(enames)) {
      if (is.list(edge_attr(g, enames[i]))) {
        g <- delete_edge_attr(g, enames[i])
      } else {
        enames[i] <- NA
      }
    }
    if (any(!is.na(enames))) {
      tp1 <- "The following edge attributes are lists and will be removed:\n"
      warning(tp1, paste(enames[!is.na(enames)], collapse = ", "),
        call. = FALSE)
    }
  }
  #--- Set igraph attributes
  if (!is.null(V(g)$color)) V(g)$nodeColor <- V(g)$color
  if (!is.null(V(g)$frame.color)) V(g)$nodeLineColor <- V(g)$frame.color
  if (!is.null(V(g)$size)) V(g)$nodeSize <- V(g)$size * 2
  if (!is.null(V(g)$label)) V(g)$nodeAlias <- V(g)$label
  if (!is.null(V(g)$label.cex)) V(g)$nodeFontSize <- V(g)$label.cex * 20
  if (!is.null(V(g)$label.color)) V(g)$nodeFontColor <- V(g)$label.color
  if (!is.null(V(g)$shape)) {
    shapes <- V(g)$shape
    shapes[shapes == "circle"] = "ELLIPSE"
    shapes[shapes != "circle"] = "RECTANGLE"
    V(g)$nodeShape <- shapes
  }
  if (!is.null(E(g)$width)) E(g)$edgeWidth <- E(g)$width
  if (!is.null(E(g)$color)) E(g)$edgeColor <- E(g)$color
  if (!is.null(E(g)$weight)) E(g)$edgeWeight <- E(g)$weight
  if (!is.null(E(g)$lty)) {
    edgeType <- E(g)$lty
    idx <- c(0:6)
    names(idx) <- c("blank", "solid", "dashed", "dotted", "dotdash",
      "longdash", "twodash")
    if (!is.numeric(edgeType)) {
      edgeType[!edgeType %in% names(idx)] <- "solid"
      edgeType <- idx[edgeType]
    }
    idx <- idx[edgeType + 1]
    edgeType[idx == 0] <- "SOLID"
    edgeType[idx == 1] <- "SOLID"
    edgeType[idx == 2] <- "DASHED"
    edgeType[idx == 3] <- "DOTTED"
    edgeType[idx == 4] <- "DOTTED"
    edgeType[idx == 5] <- "LONG_DASH"
    edgeType[idx == 6] <- "DASHED"
    E(g)$edgeType <- edgeType
  }
  if (is.null(V(g)$nodeLineColor)) V(g)$nodeLineColor = "black"
  return(g)
}

## -----------------------------------------------------------------------------
.reder2igraph <- function(g) {
  g <- .reder2igraphCoords(g)
  # g <- .reder2igraphDirection(g)
  g <- .reder2igraphAttribs(g)
  return(g)
}
.reder2igraphCoords <- function(g) {
  if (!is.null(V(g)$coordX)) {
    V(g)$x <- V(g)$coordX
    V(g)$x <- V(g)$x - mean(V(g)$x, na.rm = TRUE)
  }
  if (!is.null(V(g)$coordY)) {
    V(g)$y <- -V(g)$coordY
    V(g)$y <- V(g)$y - mean(V(g)$y, na.rm = TRUE)
  }
  return(g)
}
# 'igraph' does not support '-/+' arrow types.
.reder2igraphDirection <- function(g) {
  if (!is.null(E(g)$arrowDirection)) {
    types <- c(0, 1, -1, 2, -2, 3, -3, 4, -4)
    # Not supported in the plot.igraph():
    # c("--", "->", "-|", "<-", "|-", "<->", "|-|", "|->", "<-|")
    names(types) <- c("--", "->", "->", "<-", "<-", "<->", "<->", "<->", "<->")
    idx <- match(E(g)$arrowDirection, types)
    E(g)$arrow.mode <- names(types)[idx]
  }
  return(g)
}
.reder2igraphAttribs <- function(g) {
  #---remove vertex attributes set as lists
  vnames <- vertex_attr_names(g)
  if (length(vnames) > 0) {
    for (i in 1:length(vnames)) {
      if (is.list(vertex_attr(g, vnames[i]))) {
        g <- delete_vertex_attr(g, vnames[i])
      } else {
        vnames[i] <- NA
      }
    }
  }
  #---remove edge attributes set as lists
  enames <- edge_attr_names(g)
  if (length(enames) > 0) {
    for (i in 1:length(enames)) {
      if (is.list(edge_attr(g, enames[i]))) {
        g <- delete_edge_attr(g, enames[i])
      } else {
        enames[i] <- NA
      }
    }
  }
  #--- Set igraph attributes
  if (!is.null(V(g)$nodeColor)) V(g)$color <- V(g)$nodeColor
  if (!is.null(V(g)$nodeLineColor)) V(g)$frame.color <- V(g)$nodeLineColor
  if (!is.null(V(g)$nodeSize)) V(g)$size <- V(g)$nodeSize
  if (!is.null(V(g)$nodeAlias)) V(g)$label <- V(g)$nodeAlias
  if (!is.null(V(g)$nodeFontSize)) V(g)$label.cex <- V(g)$nodeFontSize / 20
  if (!is.null(V(g)$nodeFontColor)) V(g)$label.color <- V(g)$nodeFontColor
  if (!is.null(V(g)$nodeShape)) {
    shapes <- V(g)$nodeShape
    shapes[shapes == "ELLIPSE"] = "circle"
    shapes[shapes == "RECTANGLE"] = "rectangle"
    shapes[shapes == "ROUNDED_RECTANGLE"] = "crectangle"
    shapes[shapes == "TRIANGLE"] = "vrectangle"
    shapes[shapes == "DIAMOND"] = "pie"
    V(g)$shape <- shapes
  }
  if (!is.null(E(g)$edgeWidth)) E(g)$width <- E(g)$edgeWidth
  if (!is.null(E(g)$edgeColor)) E(g)$color <- E(g)$edgeColor
  if (!is.null(E(g)$edgeWeight)) E(g)$weight <- E(g)$edgeWeight
  if (!is.null(E(g)$edgeType)) {
    edgeType <- E(g)$edgeType
    lty <- rep(1, length(edgeType))
    lty[edgeType == "SOLID"] <- 1
    lty[edgeType == "DASHED"] <- 2
    lty[edgeType == "DOTTED"] <- 3
    lty[edgeType == "LONG_DASH"] <- 5
    E(g)$lty <- lty
  }
  if (is.null(V(g)$frame.color)) V(g)$frame.color = "black"
  return(g)
}
