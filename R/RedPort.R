

#RedPort class constructor---------------------------------------------------
RedPort = function (title='default', host='127.0.0.1', 
                    port=9091, checkJava=FALSE){
  uri = sprintf ('http://%s:%s', host, port)
  dp = new ('RedPort', title=title, uri=uri, host=host, port=port)
  attr(dp, "GraphAttr") <- .getGraphAttr()
  attr(dp, "VertexAttr") <- .getVertexAttr()
  attr(dp, "EdgeAttr") <- .getEdgeAttr()
  if(checkJava) .checkJavaVersion()
  return(invisible(dp))
}
.getVertexAttr <- function(){
  col1 <- c("name","nodeAlias","coordX","coordY",
            "nodeSize","nodeShape","nodeColor","nodeLineWidth",
            "nodeLineColor","nodeFontSize","nodeFontColor",
            "nodeBend","nodeWeight")
  col2 <- c("Node name",
            "Node alias",
            "X-coordinate of a point in a plane",
            "Y-coordinate of a point in a plane",
            "Node size",
            "Node shape",
            "Node color",
            "Line width",
            "Line color",
            "Font size",
            "Font color",
            "Node bend",
            "Node weight (not implemented)")
  col3 <- c("String, a unique ID",
            "String, a label",
            "Numeric, (-Inf,Inf)",
            "Numeric, (-Inf,Inf)",
            "Numeric, >=0",
            "String: ELLIPSE, RECTANGLE, ROUNDED_RECTANGLE, TRIANGLE, DIAMOND",
            "Hexadecimal or color name",
            "Numeric, >=0",
            "Hexadecimal or color name",
            "Numeric, >=0",
            "Hexadecimal or color name",
            "Numeric, 0-100",
            "Numeric, >=0")
  col4 <- c("V(g)$name <- paste0('Node',1:vcount(g))",
            "V(g)$nodeAlias <- V(g)$name",
            "V(g)$coordX <- runif(vcount(g))",
            "V(g)$coordY <- runif(vcount(g))",
            "V(g)$nodeSize <- 20",
            "V(g)$nodeShape <- 'ELLIPSE'",
            "V(g)$nodeColor <- 'white'",
            "V(g)$nodeLineWidth <- 1",
            "V(g)$nodeLineColor <- 'grey'",
            "V(g)$nodeFontSize <- 12",
            "V(g)$nodeFontColor <- 'black'",
            "V(g)$nodeBend <- 50",
            "V(g)$nodeWeight <- 0")
  tab <- data.frame('Attribute'=col1, 'Description'=col2, 'Value'=col3,
                    'Usage'=col4, check.names=FALSE)
  return(tab)
}
.getEdgeAttr <- function(){
  col1 <- c("edgeWeight","edgeWidth","edgeColor","edgeType",
            "arrowLength","arrowAngle","arrowType","arrowDirection",
            "linkType")
  col2 <- c("Edge weight",
            "Edge width",
            "Edge color",
            "Edge type",
            "Arrow length",
            "Arrowhead angle in degrees",
            "Associations in directed graphs",
            "Associations in undirected graphs",
            "Assignment type between nodes and containers")
  col3 <- c("Numeric, >=0",
            "Numeric, >=0",
            "Hexadecimal or color name",
            "String: SOLID, DOTTED, DASHED, LONG_DASH",
            "Numeric, >=0",
            "Numeric, 10-75",
            "Integer: -1, 0, 1",
            "Integer: 0 (A-B), 1 (A-> B), -1 (A-| B), 2 (A <-B), -2 (A |-B), 3 (A <-> B), -3 (A |-| B), 4 (A |-> B), -4 (A <-| B)",
            "String: nested, notnested")
  col4 <- c("E(g)$edgeWeight <- 0",
            "E(g)$edgeWidth <- 1",
            "E(g)$edgeColor <- 'grey'",
            "E(g)$edgeType <- 'SOLID'",
            "E(g)$arrowLength <- 15",
            "E(g)$arrowAngle <- 20",
            "E(g)$arrowType <- 1",
            "E(g)$arrowDirection <- 0",
            "E(g)$linkType <- 'nested'")
  tab <- data.frame('Attribute'=col1, 'Description'=col2, 'Value'=col3,
                    'Usage'=col4, check.names=FALSE)
  return(tab)
}
.getGraphAttr <- function(){
  col1 <- c("bgColor","zoom", "gzoom", "gscale",
            "isNested","isAnchored","isAssigned","nestAlias",
            "nestSize","nestShape", "nestColor", "nestFontSize",
            "nestLineWidth", "nestLineColor", "nestLineType","nestImage")
  col2 <- c("Background color of the app panel",
            "Zoom scale applyed to graph objects and area", 
            "Zoom scale applyed to graph objects", 
            "Expansion factor applyed to the graph area",
            "Whether to nest nodes into a container",
            "Whether to anchor the container",
            "Whether to assign container name to nodes",
            "Label of the node nesting container",
            "Size of the node nesting container",
            "Shape of the node nesting container",
            "Color of the node nesting container",
            "Container label font size",
            "Container line width",
            "Container line color", 
            "Container line type",
            "Status of the container on the screen")
  col3<- c('Hexadecimal or color name', "Numeric, 0-100", 
           "Numeric, 0-100", "Numeric, 0-100",
           "Logical", "Logical", "Logical", "Logical", "Numeric, >=0",
           "String: ELLIPSE, RECTANGLE, ROUNDED_RECTANGLE, TRIANGLE, DIAMOND",
           "Hexadecimal or color name", "Numeric, >=0",
           "Numeric, >=0", "Hexadecimal or color name",
           "String: SOLID, DOTTED, DASHED, LONG_DASH",
           "String: plain, transparent, or hide")
  col4 <- c("g$bgColor <- 'white'", "g$zoom <- 100", 
            "g$gzoom <- NULL", "g$gscale <- 100",
            "g$isNested <- FALSE", "g$isAnchored  <- FALSE", 
            "g$isAssigned  <- FALSE", "g$nestAlias <- 'a name'", 
            "g$nestSize <- 500", "g$nestShape <- 'ELLIPSE'", 
            "g$nestColor <- 'grey'", "g$nestFontSize <- 18",
            "g$nestLineWidth <- 2", "g$nestLineColor <- 'grey'", 
            "g$nestLineType <- 'SOLID'", "g$nestImage <- 'plain'")
  tab <- data.frame('Attribute'=col1, 'Description'=col2, 'Value'=col3,
                     'Usage'=col4, check.names=FALSE)
  return(tab)
}

