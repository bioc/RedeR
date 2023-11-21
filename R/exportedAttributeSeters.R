#-------------------------------------------------------------------------------
#' @title Transforming edge and vertex attributes
#'
#' @description
#'
#' Given an 'igraph' object, 'att.addv' adds a new attribute with a fixed
#' 'value' to all nodes or selected nodes, while 'att.adde' adds a new
#' attribute with a fixed 'value' to all edges.
#'
#' The 'att.mapv' and 'att.mape' functions map data frames to an
#' 'igraph' object.
#'
#' The 'att.setv' and 'att.sete' functions rename attributes available in
#' the an 'igraph' object, transforming them into new attribute classes
#' (for example, numeric values into colors or sizes).
#'
#' @param g An 'igraph' object.
#' @param from An attribute name available in 'g'.
#' @param to A valid RedeR attribute name (see \code{\link{addGraph}} or
#' type 'att.setv()' and 'att.sete()' for a quick list).
#' @param value A single value for an edge or vertex attribute.
#' @param filter A named list of length = 1, used to filter which nodes will
#' receive the attribute. The attribute 'to' will be added to nodes which
#' have the attribute.
#' @param index An optional index to set an attribute to a subset of vertices
#' or edges.
#' @param breaks A numeric vector of two or more breakpoints to be applied
#' to the attribute values.
#' @param pal Color palette option (1 or 2); 'pal=1' will use a single color
#' palette, while 'pal=2' will split 'breaks' at the center, generating two
#' color palettes. The 'pal=2' option may be useful to build separated color
#' palettes, for example, negative and positive values.
#' @param xlim A numeric vector with three boundaries:
#' c(<lower>, <upper>, <NA>). It corresponds to boundary values to be apply
#' to numeric attributes (e.g. nodeSize). Default: c(20, 100, 1).
#' @param cols Vector of colors (either hexadecimals or valid color names).
#' @param na.col A single color for NAs.
#' @param nquant Number of breakpoints to split attribute values by quantiles.
#' @param dat A data frame with the attributes to be mapped to 'g'.
#' @param refcol A reference column in the 'dat' object used to map 'dat'
#' to 'g'. For 'att.mapv', 'refcol' is a single integer value indicating a
#' column with node ids. For 'att.mape', 'refcol' is a vector with two integers
#' indicating columns with edge ids. Also, for 'att.mapv', when 'refcol = 0'
#' rownames will be used to map 'dat' to 'g'.
#' @param digits Integer indicating the number of decimal places in the
#' legend of numerical attributes.
#' @param title A legend title.
#' @param isrev A logical value, whether to verse attribute values.
#' @return Add, map, and set igraph attributes to the RedeR application.
#' @seealso \code{\link{addGraphToRedeR}}, \code{\link{getGraphFromRedeR}}
#' @examples
#' library(igraph)
#' 
#' # Generate a 'toy' graph with vertex names
#' gtoy <- sample_pa(10, directed=FALSE)
#' V(gtoy)$name <- paste0("V",1:vcount(gtoy))
#' 
#' # Create data frame with IDs compatible to vertex names
#' df <- data.frame(ID=sample(V(gtoy)$name))
#' 
#' # Add two variables to 'df' for demonstration
#' df$var_mumbers <- rnorm(nrow(df))
#' df$var_letters <- letters[1:nrow(df)]
#' 
#' ### Using the 'att.set' functions to transform edge and vertex attributes
#'
#' # Map 'df' to vertex attributes
#' gtoy <- att.mapv(g = gtoy, dat = df, refcol = 1)
#'
#' # Set a new vertex attribute, creating 'nodeLabel' from 'var_letters'
#' gtoy <- att.setv(gtoy, from = "var_letters", to = "nodeLabel")
#'
#' # Set a new vertex attribute, creating 'nodeColor' from 'var_mumbers'
#' gtoy <- att.setv(gtoy,
#'     from = "var_mumbers", to = "nodeColor",
#'     breaks = seq(-1, 1, 0.2), pal = 2
#' )
#'
#' # Set a new vertex attribute, creating 'nodeSize' from 'var_mumbers'
#' gtoy <- att.setv(gtoy,
#'     from = "var_mumbers", to = "nodeSize", nquant = 10,
#'     isrev = TRUE, xlim = c(5, 40, 1)
#' )
#' 
#' ### Using the 'att.add' functions to add fixed values
#' 
#' # Add a new vertex attribute, creating 'nodeFontSize' from a fixed value
#' gtoy <- att.addv(gtoy, to = "nodeFontSize", value = 10)
#' 
#' # ...as above, but applied only to three nodes
#' gtoy <- att.addv(gtoy,
#'     to = "nodeFontSize", value = 100,
#'     filter = list("name" = V(gtoy)$name[1:3])
#' )
#'
#' @name transform.attributes
#' @importFrom igraph set_edge_attr
#' @importFrom grDevices grey
#' @importFrom stats quantile
#' @aliases att.addv
#' @export
att.addv <- function(g, to, value, index = V(g), filter = NULL) {
    if (!is.null(filter)) {
        index <- vertex_attr(g, names(filter)) %in% unlist(filter)
        g <- set_vertex_attr(g, to, value = value, index = index)
    } else {
        g <- set_vertex_attr(g, to, value = value, index = index)
    }
    return(g)
}

#-------------------------------------------------------------------------------
#' @name transform.attributes
#' @aliases att.adde
#' @export
att.adde <- function(g, to, value, index = E(g)) {
    set_edge_attr(g, to, value = value, index = index)
}

#-------------------------------------------------------------------------------
# Set RedeR atts to igraph vertices
# ..todo: patela 2 somente aceita vetor de cores de numero par!!!

#' @name transform.attributes
#' @aliases att.setv
#' @export
att.setv <- function(g, from = "name", to = "nodeColor", pal = 1,
    cols = NULL, na.col = "grey70", xlim = c(20, 100, 1), breaks = NULL,
    nquant = NULL, digits = 1, title = from, isrev = FALSE) {
    .validate.args("singleString", "from", from)
    .validate.args("singleString", "to", to)
    .validate.args("singleString", "title", title)
    .validate.colors("allColors", "cols", cols)
    .validate.colors("singleColor", "na.col", na.col)
    .validate.args("numeric_vec", "xlim", xlim)
    .validate.args("singleNumber", "digits", digits)
    .validate.args("singleNumber", "pal", pal)
    if (!is.null(breaks)) .validate.args("numeric_vec", "breaks", breaks)
    if (!is.null(nquant)) .validate.args("numeric_vec", "nquant", nquant)
    .validate.args("singleLogical", "isrev", isrev)
    to <- .validate.old.args("att.setv", to)
    if (!is_igraph(g)) stop("Not an igraph object!")
    fromatt <- vertex_attr(g, from)
    if (is.null(fromatt)) {
        msg <- paste("graph attribute '", from, "' is absent.", sep = "")
        stop(msg)
    }
    coltype <- c("nodeColor", "nodeLineColor", "nodeLabelColor")
    numtype <- c("nodeSize", "nodeLineWidth", "nodeLabelSize", "nodeBend")
    defaultatt <- c(coltype, numtype, "nodeLabel", "nodeShape")
    if (!to %in% defaultatt) {
        .print.values.setv()
        msg <- paste("arg 'to=", to,
            "'is not consistent with 'att.setv' options",
            "(see above).")
        warning(msg)
    }
    if (!is.numeric(fromatt) && !is.integer(fromatt) &&
        !is.character(fromatt)) {
        ms <- paste("graph attribute '", from,
            "' is not consistent with supported data types:\n",
            "character, numeric, or integer.",
            sep = "")
        stop(ms)
    }
    if (!pal %in% c(1, 2)) {
        stop("not a valid palette! 'pal' options: 1 or 2")
    }
    if (length(xlim) < 3) {
        msg <- paste("'xlim' vector should provide three numbers.", sep = "")
        stop(msg)
    }
    att <- NULL
    if (to %in% coltype) {
        if (is.null(breaks) && is.null(nquant)) {
            att <- .colorcategory(fromatt, cols, na.col, isrev)
        } else {
            if (pal == 1) {
                att <- .colorscale1(fromatt, breaks, cols, na.col,
                    isrev, nquant, digits)
            } else {
                att <- .colorscale2(fromatt, breaks, cols, na.col,
                    isrev, nquant, digits)
            }
        }
    } else if (to %in% numtype) {
        szs <- .get.node.sizes(xlim, to)
        if (is.null(breaks) && is.null(nquant)) {
            att <- .xcategory(fromatt, szs, isrev)
        } else {
            att <- .xscale(fromatt, breaks, szs, isrev, nquant, digits)
        }
    } else if (to == "nodeShape") {
        att <- list()
        att$res <- as.character(fromatt)
    } else if (to == "nodeLabel") {
        att <- list()
        att$res <- as.character(fromatt)
    }
    if (!is.null(att)) {
        g <- set_vertex_attr(graph = g, name = to, value = att$res)
        if (!is.null(att$leg)) {
            to <- gsub("\\b(\\w)", "\\U\\1", to, perl = TRUE)
            leg <- paste("leg", to, sep = "")
            att$leg$title <- title
            g <- set_graph_attr(graph = g, name = leg, value = att$leg)
        }
    } else {
        message("...unable to conclude the request.")
    }
    return(g)
}

#-------------------------------------------------------------------------------
.get.node.sizes <- function(xlim, to) {
    szmin <- xlim[1]
    szmax <- xlim[2]
    na.sz <- xlim[3]
    xlim <- xlim[-3]
    szmin <- max(0, szmin)
    szmax <- max(0, szmax)
    na.sz <- max(0, na.sz)
    if (to == "nodeBend") {
        szmin <- min(100, szmin)
        szmax <- min(100, szmax)
        na.sz <- min(100, na.sz)
    }
    szs <- list(szmin = szmin, szmax = szmax, na.sz = na.sz, xlim)
    return(szs)
}

#-------------------------------------------------------------------------------
.get.edge.sizes <- function(xlim, to) {
    szmin <- xlim[1]
    szmax <- xlim[2]
    na.sz <- xlim[3]
    xlim <- xlim[-3]
    szmin <- max(0, szmin)
    szmax <- max(0, szmax)
    na.sz <- max(0, na.sz)
    szs <- list(szmin = szmin, szmax = szmax, na.sz = na.sz, xlim)
    return(szs)
}

#-------------------------------------------------------------------------------
.print.values.setv <- function() {
    coltype <- c("nodeColor", "nodeLineColor", "nodeLabelColor")
    numtype <- c("nodeSize", "nodeLineWidth", "nodeLabelSize", "nodeBend")
    defaultatt <- c(coltype, numtype, "nodeLabel", "nodeShape")
    d1 <- "Hexadecimal or color name "
    d2 <- "Numeric (>=0)"
    d3 <- "Numeric (0-100)"
    d4 <- "String (a label)"
    d5 <- "String (ELLIPSE, RECTANGLE, ROUNDED_RECTANGLE, TRIANGLE, DIAMOND)"
    description <- c(d1, d1, d1, d2, d2, d2, d3, d4, d5)
    msg <- "*List of attributes handled by 'att.setv' function:"
    print(msg)
    print(cbind(Attribute = defaultatt, Value = description), quote = FALSE)
}
.print.values.sete <- function() {
    coltype <- c("edgeLineColor")
    numtype <- c("edgeLineWidth", "edgeWeight", "arrowLength", "arrowAngle")
    defaultatt <- c(coltype, numtype, "arrowType", "edgeLineType")
    d1 <- "Hexadecimal or color name"
    d2 <- "Numeric (>=0)"
    d3 <- "Numeric (10-90)"
    d4 <- "Integer (0, 1, 2, 3, 4, -1, -2, -3, -4)"
    d5 <- "String (SOLID, DOTTED, DASHED, LONG_DASH)"
    description <- c(d1, d2, d2, d2, d3, d4, d5)
    msg <- "*List of attributes handled by the 'att.sete' function:"
    print(msg)
    print(cbind(Attribute = defaultatt, Value = description), quote = FALSE)
}

#-------------------------------------------------------------------------------
# color palette for 'category' or 'enumerated' data type
.colorcategory <- function(x, cols, na.col, isrev) {
    if (is.null(cols)) {
        cols <- c("darkblue", "blue", "orange", "cyan", "red", "darkred")
    }
    if (is.null(na.col)) {
        na.col <- grey(0.7)
    } else {
        na.col <- na.col[1]
    }
    if (isrev) cols <- rev(cols)
    # compute mapping
    x <- as.factor(x)
    cols <- colorRampPalette(colors = cols)(nlevels(x))
    x.col <- cols[x]
    x.col[is.na(x.col)] <- colorRampPalette(colors = c(na.col, na.col))(1)
    leg <- list(scale = cols, legend = levels(x))
    res <- list(res = x.col, leg = leg)
    return(res)
}
#--color scale palette with breaks
.colorscale1 <- function(
    x, breaks, cols, na.col, isrev,
    nquant, digits) {
    # check arg
    if (!is.null(nquant)) {
        if (nquant < 2) stop("require at least two quantiles!")
        breaks <- quantile(x,
            probs = seq(0, 1, length.out = nquant),
            na.rm = TRUE, names = FALSE
        )
        breaks <- unique(breaks)
        nquant <- length(breaks)
        if (length(breaks) < 3) stop("not enough intervals for 'nquant'!")
    }
    if (is.null(cols)) {
        cols <- c("darkblue", "blue", "orange", "cyan", "red", "darkred")
    }
    if (is.null(na.col)) {
        na.col <- grey(0.7)
    } else {
        na.col <- na.col[1]
    }
    if (is.character(x)) {
        stop("'breaks' arg. can not be applyed to characters!")
    }
    if (length(breaks) < 3) {
        stop("require at least three breaks!")
    }
    if (isrev) cols <- rev(cols)
    # adjust breaks and get palette
    bkcenter <- (breaks[-length(breaks)] + breaks[-1]) / 2
    bkcenter <- c(-Inf, bkcenter, +Inf)
    cols <- colorRampPalette(colors = cols)(length(bkcenter) - 1)
    # set colors to x
    x.col <- rep(NA, length(x))
    cuts <- cut(x[!is.na(x)], breaks = bkcenter, include.lowest = TRUE)
    x.col[!is.na(x)] <- cols[as.integer(cuts)]
    x.col[is.na(x.col)] <- colorRampPalette(colors = c(na.col, na.col))(1)
    # get intervals
    if (is.null(nquant)) {
        interv <- levels(cuts)
    } else {
        interv <- seq(0, 1, length.out = nquant + 1)[-1]
        interv <- paste(interv * 100, "%", sep = "")
    }
    breaks <- format(breaks, digits = digits, nsmall = digits)
    leg <- list(scale = cols, legend = breaks, interval = interv)
    res <- list(res = x.col, leg = leg)
    return(res)
}
#--neg/pos color scale palette with breaks (left/right)
.colorscale2 <- function(x, breaks, cols, na.col, isrev, nquant, digits) {
    # check args
    if (!is.null(nquant)) {
        if (nquant < 2) stop("require at least two quantiles!")
        breaks <- quantile(x,
            probs = seq(0, 1, length.out = nquant),
            na.rm = TRUE, names = FALSE
        )
        breaks <- unique(breaks)
        nquant <- length(breaks)
        if (length(breaks) < 3) stop("not enough intervals for 'nquant'!")
    }
    if (is.null(cols)) cols <- c("darkblue", "white", "darkred")
    if (is.null(na.col)) {
        na.col <- grey(0.7)
    } else {
        na.col <- na.col[1]
    }
    if (is.character(x)) {
        stop("'breaks' arg. can not be applyed to characters!")
    }
    if (length(breaks) < 3) {
        stop("require at least three breaks!")
    }
    if (isrev) cols <- rev(cols)
    bkcenter <- (breaks[-length(breaks)] + breaks[-1]) / 2
    # check color vec
    lt <- length(cols)
    if (lt / 2 == as.integer(lt / 2)) lt <- lt + 1
    cols <- colorRampPalette(colors = cols)(lt)
    lfrt <- as.integer(lt / 2) + 1
    # get neg/pos colors
    negCols <- cols[seq_len(lfrt)]
    posCols <- cols[c(lfrt:lt)]
    ct.col <- cols[lfrt]
    # check and adjust breaks
    lt <- length(bkcenter)
    if (lt / 2 == as.integer(lt / 2)) {
        lf <- lt / 2
        rt <- (lt / 2) + 1
        center <- (bkcenter[lf] + bkcenter[rt]) / 2
        negBreaks <- c(-Inf, bkcenter[seq_len(lf)], center)
        posBreaks <- c(center, bkcenter[c(rt:lt)], +Inf)
    } else {
        lfrt <- as.integer(lt / 2) + 1
        center <- bkcenter[lfrt]
        negBreaks <- c(-Inf, bkcenter[seq_len(lfrt)])
        posBreaks <- c(bkcenter[c(lfrt:lt)], +Inf)
    }
    # set main palettes
    negCols <- colorRampPalette(colors = negCols)(
        length(negBreaks))[-length(negBreaks)]
    posCols <- colorRampPalette(colors = posCols)(length(posBreaks))[-1]
    # set minor palettesscale
    na.col <- colorRampPalette(colors = c(na.col, na.col))(1)
    ct.col <- colorRampPalette(colors = c(ct.col, ct.col))(1)
    # set colors to x
    x.col <- rep(NA, length(x))
    idx <- x < center & !is.na(x)
    negcuts <- cut(x[idx], breaks = negBreaks, include.lowest = TRUE)
    x.col[idx] <- negCols[as.integer(negcuts)]
    idx <- x > center & !is.na(x)
    poscuts <- cut(x[idx], breaks = posBreaks)
    x.col[idx] <- posCols[as.integer(poscuts)]
    x.col[x == center] <- ct.col
    x.col[is.na(x.col)] <- na.col
    # get intervals
    if (is.null(nquant)) {
        interv <- c(levels(negcuts), levels(poscuts))
    } else {
        interv <- seq(0, 1, length.out = nquant + 1)[-1]
        interv <- paste(interv * 100, "%", sep = "")
    }
    testlen <- length(breaks) / 2
    if (as.integer(testlen) < testlen) {
        idx <- as.integer(testlen) + 1
        breaks <- breaks[c(seq_len(idx), c(idx:length(breaks)))]
    }
    breaks <- format(breaks, digits = digits, nsmall = digits)
    leg <- list(
        scale = c(negCols, posCols), legend = breaks,
        interval = interv
    )
    res <- list(res = x.col, leg = leg)
    return(res)
}
#--size scale for 'category' or 'enumerated' data type
.xcategory <- function(x, szs, isrev) {
    # set sz and return vec
    x <- as.factor(x)
    szlevs <- seq(szs$szmin, szs$szmax, length.out = nlevels(x))
    if (isrev) szlevs <- rev(szlevs)
    x.sz <- szlevs[x]
    x.sz[is.na(x.sz)] <- szs$na.sz
    leg <- list(scale = szlevs, legend = levels(x))
    res <- list(res = x.sz, leg = leg)
    return(res)
}
#--size scale with breaks
.xscale <- function(x, breaks, szs, isrev, nquant, digits) {
    # check arg
    if (!is.null(nquant)) {
        if (nquant < 2) {
            stop("require at least two quantiles!")
        }
        breaks <- quantile(x,
            probs = seq(0, 1, length.out = nquant),
            na.rm = TRUE, names = FALSE
        )
        breaks <- unique(breaks)
        nquant <- length(breaks)
        if (length(breaks) < 3) {
            stop("not enough intervals for 'nquant'!")
        }
    }
    if (is.character(x)) {
        stop("'breaks' arg. can not be applyed to characters!")
    }
    if (length(breaks) < 3) {
        stop("require at least three breaks!")
    }
    bkcenter <- (breaks[-length(breaks)] + breaks[-1]) / 2
    # adjust breaks
    bkcenter <- (breaks[-length(breaks)] + breaks[-1]) / 2
    bkcenter <- c(-Inf, bkcenter, +Inf)
    # get sz levels
    szlevs <- seq(szs$szmin, szs$szmax, length.out = length(bkcenter) - 1)
    if (length(szs$xlim) == length(szlevs)) szlevs <- szs$xlim
    if (isrev) szlevs <- rev(szlevs)
    # set sz to x
    x.sz <- rep(NA, length(x))
    cuts <- cut(x[!is.na(x)], breaks = bkcenter, include.lowest = TRUE)
    x.sz[!is.na(x)] <- szlevs[as.integer(cuts)]
    x.sz[is.na(x.sz)] <- szs$na.sz
    # get interval
    if (is.null(nquant)) {
        interv <- levels(cuts)
    } else {
        interv <- seq(0, 1, length.out = nquant + 1)[-1]
        interv <- paste(interv * 100, "%", sep = "")
    }
    breaks <- format(breaks, digits = digits, nsmall = digits)
    leg <- list(scale = szlevs, legend = breaks, interval = interv)
    res <- list(res = x.sz, leg = leg)
    return(res)
}

#-------------------------------------------------------------------------------
# Set RedeR atts to igraph edges

#' @name transform.attributes
#' @aliases att.sete
#' @export
att.sete <- function(
    g, from = "name", to = "edgeColor", pal = 1,
    cols = NULL, na.col = "grey70", xlim = c(20, 100, 1), breaks = NULL,
    nquant = NULL, title = from, digits = 1, isrev = FALSE) {
    .validate.args("singleString", "from", from)
    .validate.args("singleString", "to", to)
    .validate.args("singleString", "title", title)
    .validate.colors("allColors", "cols", cols)
    .validate.colors("singleColor", "na.col", na.col)
    .validate.args("numeric_vec", "xlim", xlim)
    .validate.args("singleNumber", "digits", digits)
    .validate.args("singleNumber", "pal", pal)
    to <- .validate.old.args("att.sete", to)
    if (!is.null(breaks)) .validate.args("numeric_vec", "breaks", breaks)
    if (!is.null(nquant)) .validate.args("numeric_vec", "nquant", nquant)
    .validate.args("singleLogical", "isrev", isrev)
    if (!is_igraph(g)) stop("Not an igraph object!")
    fromatt <- edge_attr(g, from)
    if (is.null(fromatt)) {
        msg <- paste("graph attribute '", from, "' is absent.", sep = "")
        stop(msg)
    }
    # set att
    coltype <- c("edgeLineColor")
    numtype <- c("edgeLineWidth", "edgeWeight", "arrowLength", "arrowAngle")
    defaultatt <- c(coltype, numtype, "arrowType", "edgeLineType")
    if (!to %in% defaultatt) {
        .print.values.sete()
        msg <- paste("arg 'to=", to,
            "'is not consistent with 'att.sete' options",
            "(see above).")
        warning(msg)
    }
    if (!is.numeric(fromatt) && !is.integer(fromatt) &&
        !is.character(fromatt)) {
        ms <- paste("graph attribute '", from,
            "' is not consistent with supported data types:
        character, numeric, or integer.", sep = "")
        stop(ms)
    }
    if (!pal %in% c(1, 2)) {
        stop("not a valid palette! 'pal' options: 1 or 2")
    }
    if (length(xlim) < 3) {
        msg <- paste("'xlim' vector should provide three numbers.", sep = "")
        stop(msg)
    }
    att <- NULL
    if (to %in% coltype) {
        if (is.null(breaks) && is.null(nquant)) {
            att <- .colorcategory(fromatt, cols, na.col, isrev)
        } else {
            if (pal == 1) {
                att <- .colorscale1(
                    fromatt, breaks, cols, na.col, isrev,
                    nquant, digits
                )
            } else {
                att <- .colorscale2(
                    fromatt, breaks, cols, na.col, isrev,
                    nquant, digits
                )
            }
        }
    } else if (to %in% numtype) {
        szs <- .get.edge.sizes(xlim, to)
        if (is.null(breaks) && is.null(nquant)) {
            att <- .xcategory(fromatt, szs, isrev)
        } else {
            att <- .xscale(fromatt, breaks, szs, isrev, nquant, digits)
        }
    } else if (to == "arrowType") {
        att <- list()
        att$res <- fromatt
    } else if (to == "edgeLineType") {
        att <- list()
        att$res <- as.character(fromatt)
    }
    if (!is.null(att)) {
        g <- set_edge_attr(graph = g, name = to, value = att$res)
        to <- gsub("\\b(\\w)", "\\U\\1", to, perl = TRUE)
        if (!is.null(att$leg)) {
            leg <- paste("leg", to, sep = "")
            att$leg$title <- title
            g <- set_graph_attr(graph = g, name = leg, value = att$leg)
        }
    } else {
        message("...unable to conclude the request.")
    }
    return(g)
}

#-------------------------------------------------------------------------------
# Map RedeR atts to igraph vertices

#' @name transform.attributes
#' @aliases att.mapv
#' @export
att.mapv <- function(g, dat, refcol = 1) {
    if (!is.data.frame(dat)) {
        stop("'dat' should be a data frame.")
    }
    # check igraph object and main args
    if (!is_igraph(g)) {
        stop("'g' should be an igraph object.")
    }
    # get vecs to match!
    nodes <- V(g)$name
    if (is.null(nodes) || vcount(g) != length(nodes)) {
        stop("require 'name' attribute in the igraph vertices.")
    }
    if (.is_singleInteger(refcol)) {
        if (refcol < 0 || refcol > ncol(dat)) {
            stop("invalid 'refcol' value; it should be a
          column index in 'dat' object.")
        }
        if (refcol == 0) {
            dc <- rownames(dat)
        } else {
            dc <- dat[[refcol[1]]]
        }
    } else if (.is_singleString(refcol)) {
        if (!refcol %in% colnames(dat)) {
            stop("invalid 'refcol' value! it should be a column
          index in 'dat' object!")
        }
        refcol <- which(colnames(dat) == refcol)[1]
        dc <- dat[[refcol]]
    } else {
        stop("'refcol' should be a single integer or character value!")
    }
    # check 'refcol' data type
    if (is.factor(dc)) {
        if (is.character(levels(dc))) {
            dc <- as.character(dc)
        } else if (is.numeric(levels(dc))) {
            dc <- as.numeric(dc)
        } else if (is.integer(levels(dc))) {
            dc <- as.integer(dc)
        } else {
            msg <- paste0("invalid 'refcol' data; supported data types: ",
                "character, numeric, or integer!")
            stop(msg)
        }
    }
    if (class(nodes) == class(dc)) {
        dat <- dat[match(nodes, dc), , drop = FALSE]
    } else {
        stop("'refcol' data type not compatible with vertex names.")
    }
    # transfer data to graph vertices
    for (i in seq_len(ncol(dat))) {
        if (i != refcol) {
            att <- dat[[i]]
            if (is.factor(att)) att <- levels(att)[att]
            g <- set_vertex_attr(graph = g, name = names(dat)[i], value = att)
        }
    }
    return(g)
}
