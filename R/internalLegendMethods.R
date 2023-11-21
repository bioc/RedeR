#-------------------------------------------------------------------------------
.addLegend <- function(obj, x, type = "nodecolor", position = "default",
    orientation = "default", title = type, font.size = 12,
    stretch = 0.1) {
    if (ping(obj) == 0) {
        return(invisible())
    }
    #--- validate args
    .validate.args("singleString", "type", type)
    .validate.args("singleString", "position", position)
    force(title)
    .validate.args("singleString", "title", title)
    .validate.args("singleNumber", "font.size", font.size)
    .validate.args("singleNumber", "stretch", stretch)
    .validate.args("singleString", "orientation", orientation)
    #--- check options
    options <- c(
        "nodecolor", "edgecolor", "nodesize", "edgewidth",
        "nodeshape", "edgetype"
    )
    type <- match.arg(type, options)
    options <- c(
        "bottomright", "bottomleft", "topright", "topleft",
        "default", "remove"
    )
    position <- match.arg(position, options)
    options <- c("vertical", "horizontal", "default")
    orientation <- match.arg(orientation, options)
    vertical <- switch(orientation,
        vertical = TRUE,
        horizontal = FALSE,
        NA
    )
    if (is_igraph(x)) {
        res <- .check.legend.igraph(x, type, title)
        x <- res$x
        title <- res$title
    }
    #--- get addLegend funs
    if (type == "nodecolor") {
        .validate.colors("allColors", "x", x)
        fun <- .addLegend.nodecolor
    } else if (type == "edgecolor") {
        .validate.colors("allColors", "x", x)
        fun <- .addLegend.edgecolor
    } else if (type == "nodesize") {
        .validate.args("numeric_vec", "x", x)
        fun <- .addLegend.nodesize
    } else if (type == "edgewidth") {
        .validate.args("numeric_vec", "x", x)
        fun <- .addLegend.edgewidth
    } else if (type == "nodeshape") {
        .validate.args("allCharacter", "x", x)
        x <- .check.legend.shapetypes(x, "node")
        fun <- .addLegend.nodeshape
    } else if (type == "edgetype") {
        .validate.args("allCharacter", "x", x)
        x <- .check.legend.shapetypes(x, "edge")
        fun <- .addLegend.edgetype
    }
    if (position == "default") {
        fun(obj, x, lgtitle = title, stretch = stretch, vertical = vertical)
    } else {
        fun(obj, x,
            lgtitle = title, stretch = stretch, position = position,
            vertical = vertical
        )
    }
}

#-------------------------------------------------------------------------------
.check.legend.igraph <- function(x, lgtype, lgtitle) {
    if (lgtype == "nodecolor") {
        if (!is.null(.G(x, "legNodeColor")$scale)) {
            xvals <- .G(x, "legNodeColor")$scale
            xlabs <- .G(x, "legNodeColor")$legend
            xtitle <- .G(x, "legNodeColor")$title
        }
    } else if (lgtype == "edgecolor") {
        if (!is.null(.G(x, "legEdgeColor")$scale)) {
            xvals <- .G(x, "legEdgeColor")$scale
            xlabs <- .G(x, "legEdgeColor")$legend
            xtitle <- .G(x, "legEdgeColor")$title
        }
    } else if (lgtype == "nodesize") {
        if (!is.null(.G(x, "legNodeSize")$scale)) {
            xvals <- .G(x, "legNodeSize")$scale
            xlabs <- .G(x, "legNodeSize")$legend
            xtitle <- .G(x, "legNodeSize")$title
        }
    } else if (lgtype == "edgewidth") {
        if (!is.null(.G(x, "legEdgeWidth")$scale)) {
            xvals <- .G(x, "legEdgeWidth")$scale
            xlabs <- .G(x, "legEdgeWidth")$legend
            xtitle <- .G(x, "legEdgeWidth")$title
        }
    } else if (lgtype == "nodeshape") {
        if (!is.null(.G(x, "legNodeShape")$shape)) {
            xvals <- .G(x, "legNodeShape")$shape
            xlabs <- .G(x, "legNodeShape")$legend
            xtitle <- .G(x, "legNodeShape")$title
        }
    } else if (lgtype == "edgetype") {
        if (!is.null(.G(x, "legEdgeType")$shape)) {
            xvals <- .G(x, "legEdgeType")$shape
            xlabs <- .G(x, "legEdgeType")$legend
            xtitle <- .G(x, "legEdgeType")$title
        }
    }
    if (is.null(xtitle)) xtitle <- lgtitle
    if (is.null(xlabs) || length(xlabs) != length(xvals)) {
        xlabs <- as.character(xvals)
    }
    names(xvals) <- xlabs
    return(list(x = xvals, title = xtitle))
}

#-------------------------------------------------------------------------------
.check.legend.shapetypes <- function(x, type = "node") {
    x <- toupper(x)
    if (type == "node") {
        options <- c(
            "ELLIPSE", "RECTANGLE", "ROUNDED_RECTANGLE",
            "TRIANGLE", "DIAMOND"
        )
        idx <- pmatch(x, options, nomatch = NA, duplicates.ok = TRUE)
        if (anyNA(idx)) {
            stop("invalid shape names in 'nodeshape' legend.")
        }
    } else {
        options <- c("LONG_DASH", "SOLID", "DOTTED", "DASHED")
        idx <- pmatch(x, options, nomatch = NA, duplicates.ok = TRUE)
        if (anyNA(idx)) {
            stop("invalid line types in 'edgetype' legend.")
        }
    }
    x <- options[idx]
    return(x)
}

#-------------------------------------------------------------------------------
.addLegend.nodecolor <- function(
    obj, colvec, stretch = 0.1, lgtitle = "",
    dxtitle = 5, ftsize = 12, position = "topright", vertical = NA,
    dxborder = 5, dyborder = 5, lgsize = 20) {
    if (is.na(vertical)) vertical <- FALSE
    position <- switch(position,
        bottomright = "bottomRight",
        bottomleft = "bottomLeft",
        topright = "topRight",
        topleft = "topLeft",
        remove = "null",
        "topRight"
    )
    # set label vec
    if (is.null(names(colvec))) {
        labvec <- letters[c(seq_len(length(colvec)))]
    } else {
        labvec <- names(colvec)
    }
    colvec <- colorRampPalette(colvec, alpha = TRUE)(length(colvec))
    # set boxbend from stretch arg
    if (stretch < 0 || stretch > 1) {
        warning("'stretch' arg should be in [0,1]", call. = FALSE)
        stretch <- 0.1
    }
    boxbend <- 1 - stretch
    maxH <- lgsize * (0.5 + 1)
    lgsize <- maxH / (0.5 + boxbend)
    boxbend <- as.integer(boxbend * 100)
    boxbend <- min(max(boxbend, 0), 100)
    # adjust args
    if (vertical) {
        colvec <- rev(colvec)
        labvec <- rev(labvec)
    }
    vertical <- ifelse(vertical, "true", "false")
    invisible(.rederexpresspost(
        obj, "RedHandler.addLegendColor",
        colvec, labvec, lgsize, boxbend, ftsize, lgtitle, dxtitle, position,
        dxborder, dyborder, vertical, "nodecolor"
    ))
}
#-------------------------------------------------------------------------------
.addLegend.edgecolor <- function(
    obj, colvec, stretch = 0.1, lgtitle = "",
    dxtitle = 5, ftsize = 12, position = "topright", vertical = NA,
    dxborder = 5, dyborder = 5, lgsize = 20) {
    if (is.na(vertical)) vertical <- FALSE
    position <- switch(position,
        bottomright = "bottomRight",
        bottomleft = "bottomLeft",
        topright = "topRight",
        topleft = "topLeft",
        remove = "null",
        "topRight"
    )
    # set label vec
    if (is.null(names(colvec))) {
        labvec <- letters[c(seq_len(length(colvec)))]
    } else {
        labvec <- names(colvec)
    }
    colvec <- colorRampPalette(colvec, alpha = TRUE)(length(colvec))
    # set boxbend from stretch arg
    if (stretch < 0 || stretch > 1) {
        warning("'stretch' arg should be in [0,1]", call. = FALSE)
        stretch <- 0.1
    }
    boxbend <- 1 - stretch
    maxH <- lgsize * (0.5 + 1)
    lgsize <- maxH / (0.5 + boxbend)
    boxbend <- as.integer(boxbend * 100)
    boxbend <- min(max(boxbend, 0), 100)
    # adjust args
    if (vertical) {
        colvec <- rev(colvec)
        labvec <- rev(labvec)
    }
    vertical <- ifelse(vertical, "true", "false")
    invisible(.rederexpresspost(
        obj, "RedHandler.addLegendColor",
        colvec, labvec, lgsize, boxbend, ftsize, lgtitle, dxtitle, position,
        dxborder, dyborder, vertical, "edgecolor"
    ))
}

#-------------------------------------------------------------------------------
.addLegend.nodesize <- function(
    obj, sizevec, stretch = 0, lgtitle = "",
    dxtitle = 5, ftsize = 12, position = "bottomleft", vertical = NA,
    dxborder = 5, dyborder = 5, spacing = 5, edgelen = 50, col = "grey20") {
    # adjust args
    if (is.na(vertical)) vertical <- FALSE
    position <- switch(position,
        bottomright = "bottomRight",
        bottomleft = "bottomLeft",
        topright = "topRight",
        topleft = "topLeft",
        remove = "null",
        "bottomLeft"
    )
    if (is.null(names(sizevec))) {
        labvec <- as.character(sizevec)
    } else {
        labvec <- names(sizevec)
    }
    # set cols
    cols <- rep(col, length(sizevec))
    cols <- colorRampPalette(cols, alpha = TRUE)(length(cols))
    # set spacing from stretch arg
    if (stretch < 0 || stretch > 1) {
        warning("'stretch' arg should be in [0,1]", call. = FALSE)
        stretch <- 0
    }
    spacing <- spacing + (30 * stretch)
    # adjust args
    vertical <- ifelse(vertical, "true", "false")
    invisible(.rederexpresspost(
        obj, "RedHandler.addLegendSize",
        sizevec, labvec, cols, spacing, ftsize, lgtitle, dxtitle, position,
        dxborder, dyborder, vertical, edgelen, "nodesize"
    ))
}
#-------------------------------------------------------------------------------
.addLegend.edgewidth <- function(
    obj, sizevec, stretch = 0, lgtitle = "",
    dxtitle = 5, ftsize = 12, position = "topleft", vertical = NA,
    dxborder = 10, dyborder = 5, spacing = 15, edgelen = 50, col = "grey20") {
    # adjust args
    if (is.na(vertical)) vertical <- TRUE
    position <- switch(position,
        bottomright = "bottomRight",
        bottomleft = "bottomLeft",
        topright = "topRight",
        topleft = "topLeft",
        remove = "null",
        "topLeft"
    )
    if (is.null(names(sizevec))) {
        labvec <- as.character(sizevec)
    } else {
        labvec <- names(sizevec)
    }
    # set cols
    cols <- rep(col, length(sizevec))
    cols <- colorRampPalette(cols, alpha = TRUE)(length(cols))
    # set spacing from stretch arg
    if (stretch < 0 || stretch > 1) {
        warning("'stretch' arg should be in [0,1]", call. = FALSE)
        stretch <- 0
    }
    spacing <- spacing + (30 * stretch)
    # adjust args
    vertical <- ifelse(vertical, "true", "false")
    invisible(.rederexpresspost(
        obj, "RedHandler.addLegendSize",
        sizevec, labvec, cols, spacing, ftsize, lgtitle, dxtitle, position,
        dxborder, dyborder, vertical, edgelen, "edgewidth"
    ))
}

#-------------------------------------------------------------------------------
.addLegend.nodeshape <- function(
    obj, shapevec, stretch = 0, lgtitle = "",
    dxtitle = 5, ftsize = 12, position = "bottomRight", vertical = NA,
    dxborder = 20, dyborder = 5, lgsize = 20, spacing = 10, col = "grey20") {
    if (is.na(vertical)) vertical <- TRUE
    position <- switch(position,
        bottomright = "bottomRight",
        bottomleft = "bottomLeft",
        topright = "topRight",
        topleft = "topLeft",
        remove = "null",
        "bottomRight"
    )
    # set label vec
    if (is.null(names(shapevec))) {
        labvec <- letters[c(seq_len(length(shapevec)))]
    } else {
        labvec <- names(shapevec)
    }
    # set cols
    cols <- rep(col, length(shapevec))
    cols <- colorRampPalette(cols, alpha = TRUE)(length(cols))
    # set spacing from stretch arg
    if (stretch < 0 || stretch > 1) {
        warning("'stretch' arg should be in [0,1]", call. = FALSE)
        stretch <- 0
    }
    spacing <- spacing + (30 * stretch)
    # adjust args
    vertical <- ifelse(vertical, "true", "false")
    invisible(.rederexpresspost(
        obj, "RedHandler.addLegendShape",
        shapevec, labvec, cols, lgsize, spacing, ftsize, lgtitle, dxtitle,
        position, dxborder, dyborder, vertical, "nodeshape"
    ))
}
#-------------------------------------------------------------------------------
.addLegend.edgetype <- function(
    obj, shapevec, stretch = 0, lgtitle = "",
    dxtitle = 5, ftsize = 12, position = "bottomRight", vertical = NA,
    dxborder = 20, dyborder = 5, lgsize = 4, spacing = 15, col = "grey20") {
    if (is.na(vertical)) vertical <- TRUE
    position <- switch(position,
        bottomright = "bottomRight",
        bottomleft = "bottomLeft",
        topright = "topRight",
        topleft = "topLeft",
        remove = "null",
        "bottomRight"
    )
    # set label vec
    if (is.null(names(shapevec))) {
        labvec <- letters[c(seq_len(length(shapevec)))]
    } else {
        labvec <- names(shapevec)
    }
    # set cols
    cols <- rep(col, length(shapevec))
    cols <- colorRampPalette(cols, alpha = TRUE)(length(cols))
    # set spacing from stretch arg
    if (stretch < 0 || stretch > 1) {
        warning("'stretch' arg should be in [0,1]", call. = FALSE)
        stretch <- 0
    }
    spacing <- spacing + (30 * stretch)
    # adjust args
    vertical <- ifelse(vertical, "true", "false")
    invisible(.rederexpresspost(
        obj, "RedHandler.addLegendShape",
        shapevec, labvec, cols, lgsize, spacing, ftsize, lgtitle, dxtitle,
        position, dxborder, dyborder, vertical, "edgeshape"
    ))
}
