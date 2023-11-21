#-------------------------------------------------------------------------------
#' @title Constructor of RedPort-class objects
#'
#' @description Constructor of the RedeR interface for remote procedure calls.
#'
#' @param title A string naming the RedeR interface.
#' @param host Domain name of the remote computer that is running
#' the interface.
#' @param port An integer specifying the port on which the interface should
#' listen for incoming requests.
#' @param checkJava A logical value, whether to check the Java Runtime
#' Environment (JRE) installed on the system.
#' @return An object of the RedPort class.
#' @seealso \code{\link{startRedeR}}
#' @author Sysbiolab.
#' @examples
#'
#' # Initialize RedeR
#' library(RedeR)
#'
#' rdp <- RedPort()
#'
#' # Set global options used in internal methods
#' options(RedPort = RedPort())
#'
#' @name RedPort
#' @aliases RedPort
#' @importFrom utils close.socket make.socket read.socket setTxtProgressBar
#' @importFrom utils txtProgressBar write.socket
#' @export
RedPort <- function(title = 'default', host = '127.0.0.1',
    port = 9091, checkJava = FALSE) {
    .validate.args("singleString", "title", title)
    .validate.args("singleString", "host", host)
    .validate.args("singleNumber", "port", port)
    .validate.args("singleLogical", "checkJava", checkJava)
    uri <- sprintf('http://%s:%s', host, port)
    dp <- new('RedPort', title = title, uri = uri, host = host, port = port)
    attr(dp, "GraphAttr") <- .getGraphAttr()
    attr(dp, "VertexAttr") <- .getVertexAttr()
    attr(dp, "EdgeAttr") <- .getEdgeAttr()
    if (checkJava) .checkJavaVersion()
    return(invisible(dp))
}

#-------------------------------------------------------------------------------
#' @title Call RedeR app from R
#'
#' @description Method to launch RedeR application from R.
#'
#' @param obj A \code{RedPort}-class object.
#' @param filepath A path to the 'reder.jar' file available in the RedeR
#' R package <string>
#' @param maxlag Max acceptable lag time for the R-Java callback confirmation
#' (default=20 s) <numeric>.
#' @param checkcalls A logical value, whether to report errors from the
#' R-to-Java calls.
#' @return System call to start the RedeR application.
#' @seealso \code{\link{startRedeR}}
#' @author Sysbiolab.
#' @examples
#'
#' rdp <- RedPort()
#'
#' \donttest{
#' calld(rdp)
#' }
#'
#' @importFrom utils packageVersion
#' @docType methods
#' @rdname calld-methods
#' @aliases calld
#' @export
#'
setMethod(
    "calld", "RedPort",
    function(obj, filepath = "default", maxlag = 20, checkcalls = FALSE) {
        # Check if the port is not in use by the app
        if (ping(obj) == 1) {
            return("RedeR interface is already in use!")
        }
        .validate.args("singleString", "filepath", filepath)
        .validate.args("singleNumber", "maxlag", maxlag)
        .validate.args("singleLogical", "checkcalls", checkcalls)
        if (!is.numeric(maxlag)) maxlag <- 20
        # (1) get path to the 'reder.jar' file
        if (filepath == "default") {
            filepath <- system.file(package = "RedeR", "java/reder_v3.jar")
        }
        cmd <- "java -jar"
        # (2) check calld
        if (checkcalls) {
            .checkJavaVersion()
            message("Checking R-to-Java calls...")
            command <- paste(cmd, shQuote(filepath), sep = " ")
            res <- system(command,
                ignore.stdout = FALSE, ignore.stderr = FALSE,
                wait = FALSE
            )
            if (is.numeric(res) && res[1] == 0) {
                msg <- c("Call checks did not detect errors.")
                message(msg)
            } else {
                message(res)
            }
        } else {
            # (3)Execute 'calld' and update app settings in RedeR preferences:
            command <- paste(cmd, shQuote(filepath), "openshellDcall",
                obj@port, sep = " ")
            system(command, ignore.stdout = !checkcalls,
                ignore.stderr = !checkcalls, wait = FALSE)
        }
        # (4) Wait response from the app (implement a short-delay)
        testInterface <- function(obj, maxlag = 20) {
            status <- "OFF"
            tdelta <- 0
            t0 <- proc.time()[3] # ...used to start time delay!
            pb <- txtProgressBar(style = 2, char = ".")
            while (status == "OFF") {
                setTxtProgressBar(pb, tdelta / maxlag)
                tdelta <- proc.time()[3] - t0
                if (tdelta > maxlag) {
                    status <- "OFFON"
                }
                if (ping(obj) == 1) {
                    status <- "ON"
                    message("\nRedeR is ready!")
                }
            }
            close(pb)
            # (4) ..send message if connection status is dubious
            if (status == "OFFON") {
                ms1 <- "\nThe Java interface is not responding to "
                ms2 <- "initialization.\n"
                ms3 <- "Please, check whether Java is installed in "
                ms4 <- "your machine.\n"
                ms5 <- "RedeR will need Java Runtime Environment (Java >=11)\n"
                ms6 <- "For a general diagnosis, run the 'calld' function\n"
                ms7 <- "with 'checkcalls=TRUE', for example:\n"
                ms8 <- "> rdp <- RedPort()\n"
                ms9 <- "> calld(rdp, checkcalls=TRUE)"
                message(ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9)
            }
        }
        if (!checkcalls) testInterface(obj = obj, maxlag = maxlag)
    }
)
.checkJavaVersion <- function() {
    msg <- packageVersion("RedeR")
    msg <- paste0("RedeR_", msg,
        " will need Java Runtime Environment (Java >=11)")
    message(msg)
    message("Checking Java version installed on this system...")
    system2("java", args = "-version")
}

#-------------------------------------------------------------------------------
#' @title Exit the RedeR R-to-Java interface
#'
#' @description Close an active RedeR session.
#'
#' @param obj A \code{RedPort}-class object.
#' @return Exit/close the RedeR application.
#' @seealso \code{\link{exitRedeR}}
#' @author Sysbiolab.
#' @examples
#'
#' rdp <- RedPort()
#'
#' \donttest{
#' calld(rdp)
#' exitd(rdp)
#' }
#'
#' @docType methods
#' @rdname exitd-methods
#' @aliases exitd
#' @export
#'
setMethod(
    "exitd", "RedPort",
    function(obj) {
        if (ping(obj) == 0) {
            return(invisible())
        }
        Sys.sleep(0.5)
        invisible(.rederpost(obj, "RedHandler.exit"))
        Sys.sleep(0.5)
    }
)

#-------------------------------------------------------------------------------
#' @title Reset RedeR app
#'
#' @description Reset an active RedeR session.
#'
#' @param obj A \code{RedPort}-class object.
#' @return Reset plotting panel.
#' @seealso \code{\link{resetRedeR}}
#' @author Sysbiolab.
#' @examples
#'
#' rdp <- RedPort()
#'
#' \donttest{
#' calld(rdp)
#' resetd(rdp)
#' }
#'
#' @docType methods
#' @rdname resetd-methods
#' @aliases resetd
#' @export
setMethod(
    "resetd", "RedPort",
    function(obj) {
        if (ping(obj) == 0) {
            return(invisible())
        }
        invisible(.rederpost(obj, "RedHandler.reset"))
    }
)

#-------------------------------------------------------------------------------
#' @title Version
#'
#' @description Returns the RedeR application version.
#'
#' @param obj A \code{RedPort}-class object.
#' @return Version of the running app.
#' @seealso \code{\link{pingRedeR}}
#' @author Sysbiolab.
#' @examples
#'
#' rdp <- RedPort()
#'
#' \donttest{
#' calld(rdp)
#' version(rdp)
#' }
#'
#' @docType methods
#' @rdname version-methods
#' @aliases version
#' @export
#'
setMethod(
    "version", "RedPort",
    function(obj) {
        if (ping(obj) == 0) {
            return("Unable to access the RedeR/Java interface.")
        }
        return(.rederpost(obj, "RedHandler.version"))
    }
)

#-------------------------------------------------------------------------------
#' @title Ping RedeR app
#'
#' @description Test the R-to-Java interface of an active RedeR session.
#'
#' @param obj A \code{RedPort}-class object.
#' @return Ping test for RedeR app, either '1' (accessible) or '0'
#' (not accessible)
#' @seealso \code{\link{pingRedeR}}
#' @author Sysbiolab.
#' @examples
#'
#' rdp <- RedPort('MyPort')
#'
#' \donttest{
#' ping(rdp)
#' # [1] 0
#' calld(rdp)
#' ping(rdp)
#' # [1] 1
#' }
#'
#' @docType methods
#' @rdname ping-methods
#' @aliases ping
#' @export
#'
setMethod(
    "ping", "RedPort",
    function(obj) {
        # Check if RedPort connection is available
        rval <- 0L
        connection <- try(suppressWarnings(
            socketConnection(host = obj@host, port = obj@port, blocking = TRUE)
        ), silent = TRUE)
        calltest1 <- !inherits(connection, "try-error")
        if (calltest1) {
            calltest2 <- try(suppressWarnings(
                .rederpost(obj, "RedHandler.ping")
            ), silent = TRUE)
            if (is.numeric(calltest2) && length(calltest2) == 1) {
                if (calltest2 == 1) {
                    rval <- 1L
                }
            }
            try(suppressWarnings(close(connection)), silent = TRUE)
        }
        return(rval)
    }
)

#-------------------------------------------------------------------------------
# Internal method
setMethod(
    "rederpost", "RedPort",
    function(obj, method, ..., gdata = list(...)) {
        aXML <- function(method, x) {
            method <- paste(c("<methodName>", method, "</methodName>"),
                collapse = "", sep = ""
            )
            x <- lapply(x, function(arg) {
                paste(c("<string><![CDATA[", arg, "]]></string>"),
                    collapse = "", sep = "")
            })
            x <- lapply(x, function(arg) {
                paste(c("<value>", arg, "</value>"), collapse = "", sep = "")
            })
            x <- lapply(x, function(arg) {
                paste(c("<param>", arg, "</param>"), collapse = "", sep = "")
            })
            x <- paste(unlist(x), collapse = "", sep = "")
            x <- paste("<params>", x, "</params>", collapse = "", sep = "")
            doc <- paste(c("<methodCall>", method, x, "</methodCall>"),
                collapse = "", sep = "")
            return(doc)
        }
        rdcall <- .simplePost(
            host = obj@host, port = obj@port,
            datatosend = aXML(method, gdata)
        )
        rdcall <- .postParser(rdcall)
        return(rdcall)
    }
)

#-------------------------------------------------------------------------------
.getVertexAttr <- function() {
    col1 <- c(
        "name", "x", "y",
        "nodeShape", "nodeSize", "nodeColor",
        "nodeLineWidth", "nodeLineColor",
        "nodeLabel", "nodeLabelSize", "nodeLabelColor",
        "nodeBend", "nodeWeight")
    col2 <- c(
        "Node name",
        "Node x-coordinate",
        "Node y-coordinate",
        "Node shapes",
        "Node size",
        "Node color",
        "Line width",
        "Line color",
        "Node label",
        "Label size",
        "Label color",
        "Node bend",
        "Node weight (not implemented)")
    col3 <- c(
    "Character vector (unique IDs)",
    "Numeric vector in (-Inf, Inf)",
    "Numeric vector in (-Inf, Inf)",
    "'ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND'",
    "Numeric vector >=0",
    "Hexadecimal or color name",
    "Numeric vector >=0",
    "Hexadecimal or color name",
    "Character vector",
    "Numeric vector >=0",
    "Hexadecimal or color name",
    "Numeric vector in [0,100]",
    "Numeric vector >=0")
    col4 <- c(
        "V(g)$name <- paste0('Node',1:vcount(g))",
        "V(g)$x <- runif(vcount(g))",
        "V(g)$y <- runif(vcount(g))",
        "V(g)$nodeShape <- 'ELLIPSE'",
        "V(g)$nodeSize <- 20",
        "V(g)$nodeColor <- 'white'",
        "V(g)$nodeLineWidth <- 1",
        "V(g)$nodeLineColor <- 'grey'",
        "V(g)$nodeLabel <- V(g)$name",
        "V(g)$nodeLabelSize <- 12",
        "V(g)$nodeLabelColor <- 'black'",
        "V(g)$nodeBend <- 50",
        "V(g)$nodeWeight <- 0")
    tab <- data.frame('Attribute' = col1, 'Description' = col2, 'Value' = col3,
        'Usage' = col4, check.names = FALSE)
    return(tab)
}
.getEdgeAttr <- function() {
    col1 <- c(
        "edgeLineType", "edgeLineWidth", "edgeLineColor",
        "arrowType", "arrowType",
        "arrowLength", "arrowAngle",
        "edgeWeight")
    col2 <- c(
        "Line types",
        "Line width",
        "Line color",
        "Arrows in directed graphs",
        "Arrows in undirected graphs",
        "Arrow length",
        "Arrowhead angle in degrees",
        "Edge weight")
    col3 <- paste(
        "Integer vector: 0 (A-B), 1 (A-> B), -1 (A-| B), 2 (A <-B),",
        "-2 (A |-B), 3 (A <-> B), -3 (A |-| B), 4 (A |-> B), -4 (A <-| B)")
    col3 <- c(
        "'SOLID', 'DOTTED', 'DASHED', 'LONG_DASH'",
        "Numeric vector >=0",
        "Hexadecimal or color name",
        "Integer vector: -1, 0, 1", col3,
        "Numeric vector >=0",
        "Numeric vector in [10, 90]",
        "Numeric vector >=0")
    col4 <- c(
        "E(g)$edgeLineType <- 'SOLID'",
        "E(g)$edgeLineWidth <- 1",
        "E(g)$edgeLineColor <- 'grey'",
        "E(g)$arrowType <- 1",
        "E(g)$arrowType <- 0",
        "E(g)$arrowLength <- 15",
        "E(g)$arrowAngle <- 20",
        "E(g)$edgeWeight <- 0")
    tab <- data.frame('Attribute' = col1, 'Description' = col2, 
        'Value' = col3, 'Usage' = col4, check.names = FALSE)
    return(tab)
}
.getGraphAttr <- function() {
    col1 <- c(
        "bgcolor", "gscale", "zoom",
        "nestShape", "nestSize", "nestColor",
        "nestLineType", "nestLineWidth", "nestLineColor",
        "nestLabel", "nestLabelSize", "nestLabelColor", "nestLabelCoords")
    col2 <- c(
        "Background color of the app panel",
        "Graph expansion factor in the app panel",
        "Zoom scale applied to the app panel",
        "Container shapes",
        "Container size",
        "Container color",
        "Container line types",
        "Container line width",
        "Container line color",
        "Label of the container",
        "Label size",
        "Label color",
        "Label xy-coord, relative to container"
    )
    col3 <- c(
    "Single color, hexadecimal or name", "Single number in [0, 100]", 
    "Single number in [0, 100]",
    "'ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND'",
    "Single number >=0", "Single color, hexadecimal or name",
    "'SOLID', 'DOTTED', 'DASHED', 'LONG_DASH'",
    "Single number >=0", "Single color, hexadecimal or name",
    "Single string", "Single number >=0",
    "Single color, hexadecimal or name",
    "Numeric vector with two numbers")
    col4 <- c(
        "g$bgcolor <- 'white'", "g$gscale <- 75", "g$zoom <- 100",
        "g$nestShape <- 'ELLIPSE'", "g$nestSize <- 500", 
        "g$nestColor <- 'grey'", "g$nestLineType <- 'SOLID'", 
        "g$nestLineWidth <- 2", "g$nestLineColor <- 'grey'", 
        "g$nestLabel <- 'a_name'", "g$nestLabelSize <- 24", 
        "g$nestLabelColor <- 'grey20'", "g$nestLabelCoords <- c(x=0, y=0)")
    tab <- data.frame('Attribute' = col1, 'Description' = col2,
        'Value' = col3, 'Usage' = col4, check.names = FALSE)
    return(tab)
}

################################################################################
### Simplified remote calls for RedeR, designed for the internal handler
################################################################################
.rederpost <- function(obj, method, ..., gdata = list(...)) {
    aXML <- function(method, x) {
        method <- paste(c("<methodName>", method, "</methodName>"),
            collapse = "", sep = ""
        )
        x <- lapply(x, function(arg) {
            paste(c("<string><![CDATA[", arg, "]]></string>"),
                collapse = "", sep = "")
        })
        x <- lapply(x, function(arg) {
            paste(c("<value>", arg, "</value>"), collapse = "", sep = "")
        })
        x <- lapply(x, function(arg) {
            paste(c("<param>", arg, "</param>"), collapse = "", sep = "")
        })
        x <- paste(unlist(x), collapse = "", sep = "")
        x <- paste("<params>", x, "</params>", collapse = "", sep = "")
        doc <- paste(c("<methodCall>", method, x, "</methodCall>"),
            collapse = "", sep = ""
        )
        return(doc)
    }
    rdcall <- .simplePost(
        host = obj@host, port = obj@port,
        datatosend = aXML(method, gdata)
    )
    rdcall <- .postParser(rdcall)
    return(rdcall)
}

#-------------------------------------------------------------------------------
# express post: direct calls to RedeR
# improve loading performance for large/sequential objects,
# designed for internal handler
.rederexpresspost <- function(obj, method, ..., gdata = list(...)) {
    bXML <- function(method, x) {
        getserial <- function(x) {
            if (is.character(x)) {
                x <- gsub("&", "&amp;", x)
                x <- gsub("<", "&lt;", x)
                x <- gsub(">", "&gt;", x)
            }
            type <- c(
                "integer" = "double", "double" = "double",
                "character" = "string"
            )[typeof(x)]
            head <- paste("<value><", type, ">", sep = "", collapse = "")
            tail <- paste("</", type, "></value>", sep = "", collapse = "")
            doc <- paste(head, x, tail, sep = "", collapse = "")
            if (length(x) == 1) {
                paste("<param>", doc, "</param>", sep = "", collapse = "")
            } else {
                paste("<param><value><array><data>", doc,
                    "</data></array></value></param>",
                    sep = "", collapse = ""
                )
            }
        }
        doc <- sapply(x, function(x) getserial(x))
        doc <- paste(doc, sep = "", collapse = "")
        doc <- paste("<params>", doc, "</params>", sep = "",
            collapse = "")
        mt <- paste("<methodName>", method, "</methodName>",
            sep = "", collapse = "")
        doc <- paste("<methodCall>", mt, doc, "</methodCall>",
            sep = "", collapse = "")
        return(doc)
    }
    rdcall <- .simplePost(host = obj@host, port = obj@port,
        datatosend = bXML(method, gdata))
    rdcall <- .postParser(rdcall)
    return(rdcall)
}

#-------------------------------------------------------------------------------
.simplePost <- function(host, port, datatosend,
    contenttype = "text/xml") {
    lengthdatatosend <- length(charToRaw(datatosend))
    header <- character(0)
    header <- c(header, "POST / HTTP/1.1\n")
    header <- c(header, paste("Content-Type: ", contenttype, "\n", sep = ""))
    header <- c(header, paste("Content-Length: ", lengthdatatosend, "\n",
        sep = ""))
    header <- c(header, "Connection: Keep-Alive\n\n")
    header <- paste(c(header, datatosend, "\n"), collapse = "")
    fp <- make.socket(host = host, port = port, server = FALSE)
    write.socket(fp, header)
    output <- character(0)
    repeat{
        ss <- read.socket(fp, maxlen = 65536L, loop = FALSE)
        if (ss == "") break
        output <- paste(output, ss)
    }
    close.socket(fp)
    output <- strsplit(output, "<\\?xml.*?\\?>", perl = TRUE)[[1]][2]
    output <- gsub("<methodResponse.*?>", "<methodResponse>", output)
    output <- gsub("\\s", "", output)
    return(output)
}

#-------------------------------------------------------------------------------
.postParser <- function(rdcall) {
    type <- c("i4|int", "double", "array")
    type <- sapply(type, function(tp) {
        grepl(tp, x = rdcall)
    })
    if (type[3]) {
        rdcall <- unlist(strsplit(rdcall, "<.?data>", perl = TRUE))[2]
    } else {
        rdcall <- unlist(strsplit(rdcall, "<.?param>", perl = TRUE))[2]
    }
    rdcall <- gsub(pattern = "<value>", "", rdcall)
    rdcall <- unlist(strsplit(rdcall, "</value>"))
    rdcall <- gsub("<.*?>", "", rdcall)
    if (type[1]) rdcall <- as.integer(rdcall)
    if (type[2]) rdcall <- as.numeric(rdcall)
    return(rdcall)
}
