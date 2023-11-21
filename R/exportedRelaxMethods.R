#-------------------------------------------------------------------------------
#' @title Relax
#'
#' @description RedeR's hierarchical force-directed interactive layout.
#'
#' @param obj A \code{RedPort}-class object.
#' @param p1 Edge target length (unit in points; >= 1 ) <numeric>.
#' @param p2 Edge stiffness (arbitrary unit; >= 0 ) <numeric>.
#' @param p3 Node repulsion factor (arbitrary unit; >= 0 ) <numeric>.
#' @param p4 Node perimeter effect (unit in points; >= 0 ) <numeric>.
#' @param p5 Node speed limit (arbitrary unit; >= 0 ) <numeric>.
#' @param p6 Repulsion radius, i.e., this parameter limits the repulsion
#' factor range (unit as in 'p1'; >= 0 ) <numeric>.
#' @param p7 Central pull (arbitrary unit; >= 0 ) <numeric>.
#' @param p8 Nest-nest edge target length, i.e., edge target between
#' linked containers (unit in points; >= 1 ) <numeric>.
#' @param p9 Nest-node repulsion factor, i.e., repulsion among containers and
#' out-nodes (arbitrary unit; >= 0 ) <numeric>.
#'
#' @return Layout a graph in the app panel.
#' @seealso \code{\link{addGraph}}
#' @author Sysbiolab.
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
#' addGraph(rdp, g, layout.random(g))
#' relax(rdp)
#' }
#'
#' @docType methods
#' @rdname relax-methods
#' @aliases relax
#' @export
#'
setMethod(
    "relax", "RedPort",
    function(obj, p1 = 100, p2 = 100, p3 = 100, p4 = 100,
        p5 = 100, p6 = 10, p7 = 10, p8 = 100, p9 = 10) {
        if (ping(obj) == 0) {
            return(invisible())
        }
        .validate.args("singleNumber", "p1", p1)
        .validate.args("singleNumber", "p2", p2)
        .validate.args("singleNumber", "p3", p3)
        .validate.args("singleNumber", "p4", p4)
        .validate.args("singleNumber", "p5", p5)
        .validate.args("singleNumber", "p6", p6)
        .validate.args("singleNumber", "p7", p7)
        .validate.args("singleNumber", "p8", p8)
        .validate.args("singleNumber", "p9", p9)
        p1 <- max(1, p1)
        p2 <- max(0, p2)
        p3 <- max(0, p3)
        p4 <- max(0, p4)
        p5 <- max(0, p5)
        p6 <- max(0, p6)
        p7 <- max(0, p7)
        p8 <- max(1, p8)
        p9 <- max(0, p9)
        return(
            .rederexpresspost(obj, "RedHandler.setRelax",
                p1, p2, p3, p4, p5, p6, p7, p8, p9)
        )
    }
)

#-------------------------------------------------------------------------------
# Internal method
setMethod(
    "isRelaxActive", "RedPort",
    function(obj) {
        if (ping(obj) == 0) {
            return(invisible())
        }
        return(.rederpost(obj, "RedHandler.isRelaxActive"))
    }
)
