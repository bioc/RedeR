\name{att}
\alias{att.setv}
\alias{att.sete}
\alias{att.mapv}
\alias{att.mape}
\alias{att.adde}
\alias{att.addv}

\title{Add, map, and set edge and vertex attributes to the RedeR application.}

\description{

Given an 'igraph' object 'g', 'att.addv' adds a new attribute with a fixed
'value' to all nodes or selected nodes using 'filter', while 'att.adde' adds
a new attribute with a fixed 'value' to all edges. 

The 'att.mapv' and 'att.mape' functions map data frames to the 'g'
object, useful for adding batches of attributes.

The 'att.setv' and 'att.sete' functions set attributes available in 
the 'g' object, transforming them into new attribute classes 
(for examples, numeric values into colors or sizes).

}

\usage{

att.addv(g, to, value, filter = NULL, index = V(g))

att.adde(g, to, value, index = E(g))

att.mapv(g, dat, refcol=1)

att.mape(g, dat, refcol=c(1,2))

att.setv(g, from, to='nodeColor', pal=1, cols=NULL, na.col=grey(0.7), 
xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, 
isrev=FALSE, getleg=TRUE, roundleg=1, title=NULL)

att.sete(g, from, to='edgeColor', pal=1, cols=NULL, na.col=grey(0.7), 
xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, 
isrev=FALSE, getleg=TRUE, roundleg=1, title=NULL)

}

\arguments{
  \item{g}{
  An igraph object. 
  }
  \item{from}{
  An attribute name available in 'g' <string>. 
  }
  \item{to}{
  A valid RedeR attribute name (see \code{\link[RedeR:addGraph]{addGraph}} 
  or type 'att.setv()' and 'att.sete()' for a quick list).
  }
  \item{value}{
  A single value to add in a edge or vertex attribute.
  }
  \item{filter}{
  A named list of length = 1, used to filter which nodes will receive the 
  attribute. The attribute 'to' will be added to nodes which have the 
  attribute.
  }
  \item{index}{
  An optional index to set and attribute of a subset of vertices or edges 
  (see \code{\link[igraph:set_vertex_attr]{set_vertex_attr}} and 
  \code{\link[igraph:set_edge_attr]{set_edge_attr}}).
  }
  \item{breaks}{
  A numeric vector of two or more breakpoints to be applied to the 
  attribute values.
  }
  \item{pal}{
  Color scaling option (1 or 2); 'pal=1' will use a single color pallete, while 
  'pal=2' will split 'breaks' at the center, generating two color palletes. The 
  'pal=2' option maybe useful to build separated color palletes for negative 
  and positive values.
  }
  \item{xlim}{
  A numeric vector with three boundaries: c(<lower boundary>, <upper boundary>, 
  <NA>). It corresponds to boundary values to be apply to numeric attributes 
  (e.g. nodeSize). Default: c(20,100,1).
  }
  \item{cols}{
  Vector of colors (either hexadecimals or valid R color names).
  }
  \item{na.col}{
  A color representing eventual NAs. Default: grey(0.7).
  }
  \item{shapes}{
  A string vector with valid RedeR shapes (see 
  \code{\link[RedeR:addGraph]{addGraph}} or type 'att.setv()' or 'att.sete()').
  }
  \item{categvec}{
  Levels to encode attributes as a factor <vector>.
  }
  \item{nquant}{
  Number of breakpoints to split attribute values by quantiles <integer>.
  }  
  \item{isrev}{
  Reverse of attribute values <logical>.
  } 
  \item{getleg}{
  Optional: return legend values <logical>.
  } 
  \item{dat}{
  A data frame with the attributes to be mapped to 'g'.
  } 
  \item{refcol}{
  A reference column in the 'dat' object used to map 'dat' to 'g'. 
  For 'att.mapv', 'refcol' is a single integer value indicating a column with 
  node ids. For 'att.mape', 'refcol' is a vector of two integers indicating two 
  columns with edge ids. Also, for 'att.mapv', 'refcol = 0' indicates 
  'dat' rownames will be used to map 'dat' to 'g'.
  }
  \item{roundleg}{
  Integer indicating the number of decimal places in the legend of 
  numerical attributes.
  }
  \item{title}{
  A title legend.
  } 
}

\value{
Add, map, and set igraph attributes to the RedeR application.
}

\author{Mauro Castro, Clarisse Groeneveld.}

\seealso{
\code{\link[RedeR:addGraph]{addGraph}}
}

\examples{
library(igraph)
data(ER.deg)

sg <- ER.deg$ceg  # an igraph object
dt <- ER.deg$dat  # a data frame object

# add a new vertex attribute, creating 'nodeFontSize' from a fixed value
sg <-  att.addv(sg, to = "nodeFontSize", value = 10) 

# same as above, but applied only to three nodes
sg <- att.addv(sg, to = "nodeFontSize", value = 100, 
               filter = list("name" = V(sg)$name[1:3]))

# map vertex attributes, adding 'dt' into 'g'
sg <- att.mapv(g=sg, dat=dt, refcol=1)

# set a new vertex attribute, creating 'nodeAlias' from 'symbol'
sg <- att.setv(sg, from="Symbol", to="nodeAlias")

# set a new vertex attribute, creating 'nodeColor' from 'logFC.t3'
sg <- att.setv(sg, from="logFC.t3", to="nodeColor", breaks=seq(-1,1,0.2), pal=2)

# set a new vertex attribute, creating 'nodeSize' from 'ERbdist'
sg <- att.setv(sg, from="ERbdist", to="nodeSize", nquant=10, isrev=TRUE, xlim=c(5,40,1))

}

\keyword{attributes}
