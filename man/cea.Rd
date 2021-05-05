\name{cea}
\alias{cea}

\title{Co-expression analysis. }

\description{
Simple function for correlation analysis. This function computes a null distribution via permutation and returns the significant correlation values.
}

\usage{
cea(x, sig=0.01, padj.method="fdr", cor.method="spearman", nper=1000, 
regulators=NULL, plotcea=TRUE, ...)
}

\arguments{
  \item{x}{A matrix or data frame. Varibles should be on rows, samples on columns. 
  The 'x' matrix should be named, with unique row names.}
  \item{sig}{Significance threshold. }
  \item{padj.method}{Correction method passed to "p.adjust" function. }
  \item{cor.method}{Correlation method passed to "cor" function. }
  \item{nper}{Number of permutations. }
  \item{plotcea}{Logical value, option to plot density and the null distributions. }
  \item{regulators}{An optional character vector listing varibles (e.g. genes) regarded as regulators. }
  \item{...}{Additional arguments passed to plotcea option. }
}

\details{       
Additional arguments: 
\describe{
  \item{n.breaks}{If plotcea=TRUE, 'n.breaks' sets the number of histogram breaks (Default=100 <integer>). } 
  \item{plotnull}{If plotcea=TRUE, 'plotnull' sets whether to plot the null distribution (Default=TRUE <logical>).}
  \item{avnull}{If plotcea=TRUE, 'avnull' takes the average null distribution (Default=TRUE <logical>). }
  \item{nullcol}{If plotcea=TRUE, 'nullcol' sets the color of the null distribution (Default="black" <character>). }
}
}

\value{
An adjacency matrix with significant correlation values.
}


\author{Mauro Castro}

\seealso{
\code{\link[stats:cor]{cor}}
\code{\link[stats:p.adjust]{p.adjust}}
}

\examples{
#--- Load a a gene expression matrix
data(ER.deg)
x <- ER.deg$exp

#--- NOTE1: 'x' should have varibles on rows and samples on columns!
#--- NOTE2: 'x' should be named, with unique names on rows.
x[1:3,1:3]
#      GSM286031 GSM286032 GSM286033
#1      7.106765  7.225175  7.217780
#10005  8.108632  8.012673  7.932592
#10007 10.225053 10.068826 10.236548

#--- Get a subset, with 100 genes, for a quick demonstration!
x <- x[sample(1:nrow(x))[1:100], ]

\dontrun{
#--- Run correlation analysis, estimating significant 
#--- associations by permutation.
#--- NOTE: set 'nper' for at least 1000
res <- cea(x=x, nper=100)
}
}

\keyword{cea}
