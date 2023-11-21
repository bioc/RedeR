################################################################################
### Package documentation
################################################################################
#' @keywords internal
#' @title RedeR: Interactive visualization and manipulation of nested networks
#'
#' @description
#' RedeR is an R-based package combined with a stand-alone Java application
#' for interactive visualization and manipulation of nested networks.
#'
#' @details
#'
#' \tabular{ll}{
#' Package: \tab RedeR\cr
#' Type: \tab Software\cr
#' License: \tab GPL-3\cr
#' Maintainer: \tab Mauro Castro \email{mauro.a.castro@@gmail.com}\cr
#' }
#'
#' @section Index:
#' \tabular{ll}{
#' \link{startRedeR}:
#' \tab Method to launch RedeR application from R.\cr
#' \link{pingRedeR}:
#' \tab Test the R-to-Java interface of an active RedeR session.\cr
#' \link{addGraphToRedeR}:
#' \tab Methods to display igraph objects in the RedeR application.\cr
#' \link{getGraphFromRedeR}:
#' \tab Methods to wrap up RedeR graphs into igraph's R objects.\cr
#' \link{addLegendToRedeR}:
#' \tab Methods to display legends in the RedeR app.\cr
#' \link{relaxRedeR}:
#' \tab Start RedeR's hierarchical force-directed interactive layout.\cr
#' \link{resetRedeR}:
#' \tab Reset an active RedeR session.\cr
#' \link{exitRedeR}:
#' \tab Close an active RedeR session.\cr
#' }
#' Further information is available in the vignettes by typing
#' \code{vignette('RedeR')}. Documented topics are also available in
#' HTML by typing \code{help.start()} and selecting the RedeR package
#' from the menu.
#'
#' @references
#' Castro MAA, Wang X, Fletcher MNC, Meyer KB, Markowetz F. RedeR:
#' R/Bioconductor package for representing modular structures, nested
#' networks and multiple levels of hierarchical associations.
#' Genome Biology 13:R29, 2012.
#'
"_PACKAGE"
#> [1] '_PACKAGE'

################################################################################
### Documentation for datasets
################################################################################
#' @title Pre-processed igraph object for RedeR case studies.
#'
#' @description Preprocessed Human interactome extracted from the Human
#' Protein Reference Database (HPRD) in April 2011 <igraph object>
#' ('name' attribute is mapped to ENTREZ ID).
#'
#' @format igraph
#'
#' @usage data(hs.inter)
#'
#' @source This package.
#'
#' @docType data
#' @keywords hs.inter
#' @name RedeR-data1
#' @aliases hs.inter
#' @return A pre-processed igraph object.
#' @examples
#' data(hs.inter)
NULL

################################################################################
### Documentation for datasets
################################################################################
#' @title Pre-processed dataset for RedeR case studies.
#'
#' @description Preprocessed data from a time-course gene expression and
#' ChIP-on-chip analysis of estrogen receptor (ER) binding sites in the MCF7
#' cell line (Carroll et al, 2006).
#'
#' @format igraph
#'
#' @usage
#' data(ER.limma)
#'
#' @source
#'
#' Carroll JS et al., Genome-wide analysis of estrogen receptor binding sites.
#' Nat Genet. 38(11):1289-97, 2006.
#'
#' Castro MA et al. RedeR: R/Bioconductor package for representing modular
#' structures, nested networks and multiple levels of hierarchical
#' associations. Genome Biology, 13(4):R29, 2012.
#'
#' @details
#'
#' The 'ER.limma' dataset contains results from a differential
#' gene expression analysis described elsewhere (Castro et al., 2012). This
#' dataset also includes annotation of ER-binding sites. The original gene
#' expression dataset (Carroll et al.) consists of 12 time-course
#' Affymetrix U133Plus2.0 microarrays: 3 replicates at 0h, 3 replicates at 3h,
#' 3 replicates at 6h and 3 replicates at 12h. The original dataset is
#' available at the GEO database (GSE11324).
#'
#' \describe{
#' \item{ER.limma}{ A data-frame containing pre-processed results from a
#' 'limma' analysis listing the DE genes only. The data-frame columns list the
#' following information: annotation (ENTREZ and Symbol), time-course fold
#' change (logFC.t3, logFC.t6, logFC.t12), p values (p.value.t3, p.value.t6,
#' p.value.t12), DE genes (degenes.t3, degenes.t6, degenes.t12) and kb distance
#' of the nearest ER-binding site to the TSS (ERbdist). }
#' }
#'
#' @docType data
#' @keywords ER
#' @name RedeR-data2
#' @aliases ER.limma
#' @return A pre-processed dataset.
#' @examples
#' data(ER.limma)
NULL


#------------------------------------------------------------------------------
#' @title Subgraph of a graph
#'
#' @description
#'
#' Creates a subgraph containing nodes specified from a data frame.
#'
#' @param g An 'igraph' object.
#' @param dat A data frame with node names and attributes to be mapped to 'g'.
#' @param refcol The reference column (node names) in the 'dat' object.
#' @param maincomp Logical value, whether to return only the main component
#' of the subgraph.
#' @param connected Logical value, whether to return only connected nodes.
#' @param transdat Logical value, whether to transfer node attributes
#' from the 'dat' object to the subgraph.
#' @return An igraph object.
#' @seealso \code{\link[igraph:subgraph]{subgraph}}
#' @examples
#' # see 'nested subgraphs' section in RedeR's vignette:
#' # vignette("RedeR")
#' 
#' @name subg
#' @importFrom igraph induced_subgraph clusters subgraph
#' @aliases subg
#' @export
subg <- function(
    g, dat, refcol = 1, maincomp = TRUE, connected = TRUE,
    transdat = TRUE) {
    if (is.data.frame(dat)) {
        allids <- as.character(dat[[refcol[1]]])
    } else if (is.vector(dat)) {
        allids <- as.character(dat)
    } else {
        stop("not a data frame!")
    }
    if (!is_igraph(g)) {
        stop("Not an igraph object!")
    }
    if (is.null(V(g)$name)) V(g)$name <- as.character(V(g))
    ids <- allids[allids %in% V(g)$name]
    if (length(ids) != length(allids)) {
        message("...not all genes found in the network!")
    }
    sg <- induced_subgraph(graph = g, vids = ids)
    if (maincomp) {
        comp <- clusters(sg)
        cids <- which.max(comp$csize)
        sg <- induced_subgraph(
            graph = sg,
            vids = V(sg)[comp$membership == cids]
        )
    } else if (connected) {
        dg <- degree(sg) > 0
        nodes <- V(sg)$name[dg]
        sg <- induced_subgraph(graph = sg, vids = nodes)
    }
    if (transdat && is.data.frame(dat)) {
        sg <- att.mapv(g = sg, dat = dat, refcol = refcol)
    }
    return(sg)
}
