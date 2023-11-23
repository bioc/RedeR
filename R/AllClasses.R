#-------------------------------------------------------------------------------
#' @title RedPort: An S4 class for RedeR graphics
#'
#' @slot title A string naming the XML-RPC port.
#' @slot uri  The uri to the XML-RPC server.
#' @slot host Domain name of the machine that is running the XML-RPC server.
#' @slot port  An integer specifying the port on which the XML-RPC server
#' should listen.
#'
#' @aliases RedPort-class
#' @return An S4 class object.
#' @author Sysbiolab.
#' @section Constructor: \code{\link{RedPort}}
#' @seealso \code{\link{addGraph}}, \code{\link{getGraph}}, \code{\link{relax}},
#' \code{\link{calld}}, @seealso \code{\link{resetd}}, \code{\link{exitd}},
#' \code{\link{ping}}, \code{\link{version}}.
#' @exportClass RedPort
#'
setClass("RedPort",
    representation = representation(title = "character",
        uri = "character",
        host = "character",
        port = "numeric"),
    prototype = prototype(title = "RedPort",
        uri = "http://127.0.0.1:9091",
        host = "127.0.0.1",
        port = 9091
    )
)
#-------------------------------------------------------------------------------
# Test the validity of RRede class
setValidity("RedPort",
    function(object) {
        c1 <- length(object@title) == 1
        c2 <- length(object@uri) == 1
        c3 <- length(object@host) == 1
        c4 <- length(object@port) == 1
        if (c1 && c2 && c3 && c4) TRUE
        else cat("'title', 'uri', 'host', and 'port' must all have length 1")
    })

##definitions of class unions
##setClassUnion("matrix_Or_numeric_Or_NULL", c("matrix", "numeric", "NULL"))
setClassUnion("matrix_Or_missing", c("matrix", "missing"))
setClassUnion("logical_Or_missing", c("logical", "missing"))
##definitions of class unions
##setClassUnion("matrix_Or_numeric_Or_NULL", c("matrix", "numeric", "NULL"))
setClassUnion("matrix_Or_missing", c("matrix", "missing"))
setClassUnion("numeric_Or_missing", c("numeric", "missing"))
setClassUnion("list_Or_missing", c("list", "missing"))
setClassUnion("character_Or_missing", c("character", "missing"))
setClassUnion("numeric_Or_integer", c("numeric", "integer"))
setClassUnion("numeric_Or_integer_Or_missing", c("numeric", "integer", "missing"))
setClassUnion("integer_Or_numeric_Or_character_Or_missing", c("integer", "numeric", "character", "missing"))
setClassUnion("list_Or_missing", c("list", "missing"))
setClassUnion("character_Or_missing", c("character", "missing"))
setClassUnion("numeric_Or_integer", c("numeric", "integer"))
setClassUnion("numeric_Or_integer_Or_missing", c("numeric", "integer", "missing"))
setClassUnion("integer_Or_numeric_Or_character_Or_missing", c("integer", "numeric", "character", "missing"))
