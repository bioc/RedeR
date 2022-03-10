#-------------------------------------------------------------
#Simple function to generate random graphs with a modular structure
gtoy.rm <- function(m=5, nmax=30, nmin=5, p1=0.5, p2=0.03, p3=0.7, 
                    gg=NULL, nn=vcount(gg), noise.range=c(0.2, 0.6), 
                    plot=FALSE, fname=NULL){
	#check args------------------------------------
	if(!is.singleInteger(m) || m<1){
		stop("NOTE: 'm' must be an integer value > 0!")
	}
	if(!is.singleInteger(nmax)){
		stop("NOTE: 'nmax' must be an integer value > 0!")
	}
	if(!is.singleInteger(nmin) || nmin>=nmax){
		stop("NOTE: 'nmin' must be an integer value > nmax!")
	}
	if(!is.singleNumber(p1) || p1>=1 || p1<=0){
		stop("NOTE: 'p1' must be numeric (0.0 < p < 1.0)!")
	}	
	if(!is.singleNumber(p2) || p2>=1 || p2<=0){
		stop("NOTE: 'p2' must be a numeric value (0.0 < p < 1.0)!")
	}		
	if(!is.singleNumber(p3) || p3>=1 || p3<=0){
		stop("NOTE: 'p3' must be a numeric value (0.0 < p < 1.0)!")
	}
  if(is.null(gg)){
    par <- c(m=m, nmax=nmax, nmin=nmin, p1=p1, p2=p2, p3=p3)
    gg <- .simulateModules(par)
    gg <- .simulateSignal(gg)
    gg <- .setgg(gg)
    if(plot){
      .plot.signal(gg, fname, isNew=TRUE)
    }
  } else {
    if(!"gtoy.rm"%in%class(gg))
      stop("please, provide a 'gg' igraph object 
            assessed by the 'gtoy.rm' function!")
    if(!is.singleInteger(nn) || nn<1){
      stop("NOTE: 'nn' must be an integer value > 0!")
    }
    if(!is.numeric(noise.range) || length(noise.range)!=2){
      stop("NOTE: 'noise.range' must be a numeric vector of length=2!")
    }
    noise.range <- sort(noise.range)
    if(noise.range[1]<0 | noise.range[2]>1){
      stop("NOTE: 'noise.range' must be in [0,1]!")
    }
    gg$noise.range <- noise.range
    gg <- .add.random.nodes(gg, nn)
    gg <- .setgg(gg)
    if(plot){
      .plot.signal(gg, fname, isNew=FALSE)
    }
  }
	return(gg)
}
.plot.signal <- function(gg, fname, isNew=TRUE){
  dt <- data.frame(nodeId=V(gg)$name,
                  simulated.signal=V(gg)$simulated.signal,
                  moduleId=V(gg)$module.id)
  if(!is.null(fname))
    pdf(file=paste0(fname,".pdf"), width=5, height=5)
  m <- gg$par["m"]
  moduleId <- sort(unique(dt$moduleId))
  nm <- length(moduleId)
  cols <- rep("grey90", nm)
  border <- rep("black", nm)
  names(moduleId) <- paste0("M", moduleId)
  if(!isNew){
    lb <- gg$noise.range[1]
    ub <- gg$noise.range[2]
    moduleId <- moduleId[-nm]
    cols[nm] <- "white"
    border[nm] <- "red"
  }
  xlim <- c(0.5,m+1.5)
  ylim <- c(0,gg$max)
  plot(0, xlim=xlim, ylim=ylim, ylab="", xlab="", type="n", axes=FALSE)
  if(!isNew)abline(ub, 0, lty=2, col="red", lw=1.2)
  if(!isNew)abline(lb, 0, lty=2, col="red", lw=1.2)
  boxplot(simulated.signal~moduleId,data=dt, col=cols, 
          border=border, ylab="", xlab="", add=TRUE, axes=FALSE)
  axis(1, las=1, labels = names(moduleId), at=moduleId, 
       padj = 1, mgp=c(1,0.1,0), tcl=-0.3, lwd=2.2, cex.axis=1.2)
  axis(2, las=2, mgp=c(0,0.6,0), tcl=-0.3, lwd=2.2, cex.axis=1.2)
  mtext(text="Simulated signal", side=2, line=2.2, cex=1.3)
  mtext(text="Simulated modules", side=1, line=2.2, at=(m+1)/2, cex=1.3)
  if(!isNew)text(x=m+1, y=ub+0.03, "upper bound", cex=0.7, col="red", adj=0.5)
  if(!isNew)text(x=m+0.5, y=(ub+lb)/2, "random\nnodes", cex=0.8, col="red", adj=c(1,0.5))
  if(!isNew)text(x=m+1, y=lb-0.03, "lower bound", cex=0.7, col="red", adj=0.5)
  if(!is.null(fname)) dev.off()
}
.simulateModules <- function(par){
  gg <- igraph::graph.empty(n=0, directed=FALSE)
  mdmap <- c()
  nver <- function(nmax,p1){sum(runif(nmax)>1-p1)}
  for(i in 1:par["m"]){
    v <- max(par["nmin"],nver(par["nmax"],par["p1"]))
    g <- igraph::erdos.renyi.game(n=v, p.or.m=par["p3"], type="gnp", 
                                  directed=FALSE)
    gg <- igraph::graph.disjoint.union(gg,g)
    mdmap <- c(mdmap,rep(i,v))		
  }
  adj <- igraph::get.adjacency(gg,sparse=FALSE)
  adj[,] <- 0
  adj[,] <- runif(nrow(adj)*ncol(adj))
  adj[adj<(1-(par["p2"]/par["m"]))] <- 0
  for(i in 1:par["m"]){
    adj[mdmap==i,mdmap==i] <- 0
  }
  adj[adj>0] <- 1
  adj <- adj+igraph::get.adjacency(gg, sparse=FALSE)
  gg <- igraph::graph.adjacency(adj, mode="undirected", diag=FALSE)
  gg <- igraph::simplify(gg, remove.multiple = TRUE, remove.loops = TRUE)
  gg$par <- par
  V(gg)$name <- paste("n",1:igraph::vcount(gg),sep="")
  V(gg)$module.id <- mdmap
  gg$name <- "gtoy: a random modular graph"
  return(gg)
}
.simulateSignal <- function(gg){
  mods <- unique(V(gg)$module.id)
  simulated.signal <- runif(igraph::vcount(gg))
  sgdiff <- 2
  for(i in mods){
    idx <- V(gg)$module.id==i
    md <- 1 + i*sgdiff-(sgdiff-1)
    simulated.signal[idx] <- simulated.signal[idx]+rnorm(sum(idx), md)
  }
  simulated.signal <- simulated.signal - 1
  simulated.signal <- simulated.signal-min(simulated.signal)
  simulated.signal <- simulated.signal/max(simulated.signal)
  V(gg)$coordZ <- V(gg)$simulated.signal <- simulated.signal
  gg$max <- ceiling(max(V(gg)$simulated.signal))
  return(gg)
}
.add.random.nodes <- function(g1, nn){
  lb <- g1$noise.range[1]
  ub <- g1$noise.range[2]
  m <- g1$par["m"]
  g2 <- erdos.renyi.game(vcount(g1)+nn, 0.005)
  #g2 <- barabasi.game(vcount(g1)+nn,directed=FALSE,power=0)
  V(g2)$name <- paste("n",1:vcount(g2),sep="")
  g2 <- igraph::union(g1, g2, byname=TRUE)
  g2 <- igraph::decompose.graph(g2, min.vertices=1,max.comps=1)[[1]]
  #-- adiciona atividade para os novos nodos
  simulated.signal <- runif(vcount(g2), lb, ub)
  V(g2)$simulated.signal <- simulated.signal
  #-- recupera atividade dos nodos em g1
  idx <- match(V(g1)$name,V(g2)$name)
  V(g2)$simulated.signal[idx] <- V(g1)$simulated.signal
  V(g2)$coordZ <- V(g2)$simulated.signal
  #-- novos nodos recebem 'module.id = m + 1'
  V(g2)$module.id <- m + 1
  V(g2)$module.id[idx] <- V(g1)$module.id
  g2$rid <- m + 1
  g2$name <- "gtoy: a random modular graph -plus- random nodes"
  return(g2)
}
.setgg <- function(gg){
  V(gg)$nodeSize <- 25
  V(gg)$nodeFontSize <- 20
  V(gg)$nodeLineColor <- "grey"
  E(gg)$edgeColor <- "grey"
  E(gg)$edgeWidth <- 2
  cols <- c("darkblue","blue","cyan","green","yellow","orange","red","darkred")
  breaks <- pretty(V(gg)$simulated.signal, n = 9)
  gg <- att.setv(g=gg, from="simulated.signal", to='nodeColor',
                 cols=cols, breaks=breaks, pal=1)
  V(gg)$simulated.signal.color <- V(gg)$nodeColor
  V(gg)$module.color <- terrain.colors(gg$par["m"])[V(gg)$module.id]
  V(gg)$nodeAlias <- as.character(V(gg)$module.id)
  class(gg) <- c("gtoy.rm","igraph")
  if(!is.null(gg$rid)){
    V(gg)$nodeAlias[V(gg)$module.id==gg$rid] <- ""
  }
  return(gg)
}

#-------------------------------------------------------------------
# Add RedeR atts to igraph vertices
att.addv <- function(g, to, value, filter = NULL, index = V(g)){
  if(!is.null(filter)) {
    index <- get.vertex.attribute(g, names(filter)) %in% unlist(filter)
    g <- set_vertex_attr(g, to, value = value, index = index)
  } else {
    g <- set_vertex_attr(g, to, value = value, index = index)
  }
  return(g)
}

#-------------------------------------------------------------------
# Add RedeR atts to igraph vertices
att.adde <- function(g, to, value, index = E(g)) {
  set_edge_attr(g, to, value = value, index = index)
}

#------------------------------------------------------------------------------
# Set RedeR atts to igraph vertices
#..todo: patela 2 somente aceita vetor de cores de numero par!!!
att.setv <- function(g=NULL, from='name', to='nodeColor', pal=1, cols=NULL, na.col=grey(0.7), 
                  xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, 
                  isrev=FALSE, getleg=TRUE, 
                  roundleg=1,title=NULL){
  #check loaded igraph
  igraph.check()
	# set att---------------------------------------------------------
	coltype=c('nodeColor','nodeLineColor','nodeFontColor')
	numtype=c('nodeSize','nodeLineWidth','nodeFontSize','nodeBend','coordX','coordY')
	defaultatt=c(coltype, numtype,'nodeAlias','nodeShape')
	defaultshapes=c('ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND')	
	d1='name, id, or hexadecimal <string or integer>'
	d2='[0,+inf) <numeric>'
	d3='[0,100] <numeric>'
	d4='(-inf,+inf) <numeric>'
	d5='name or id <string or integer>'
	description=c(d1,d1,d1,d2,d2,d2,d3,d4,d4,d5,d5)
	if(is.null(g)){
		message("*List of attributes handled by 'att.setv' function:")
		print(cbind(attribute=defaultatt,id=1:11,description=description),quote=F)
		message("*Shape names and ids:")
		print(cbind(shape=defaultshapes,id=1:5),quote=F)
		return(invisible())
	}
	# check igraph object and main args---------------------------------
	if(!igraph::is.igraph(g)){
	  stop("Not an igraph object!")
	}
	if(!is.character(from))stop("NOTE: arg. 'from' should be a string!")
	from=from[1]
	if(!is.character(to) && !is.numeric(to) && !is.integer(to) )stop("NOTE: arg. 'to' should be a string or an integer!")
	to=to[1]
	# get ref. att---
	fromatt=igraph::get.vertex.attribute(g, from)
	if(is.null(fromatt) || length(fromatt)!=igraph::vcount(g)){
		stop(paste("NOTE: graph attribute '",from,"' is absent or not consistent with node count!",sep=""))
	}
	if(is.numeric(to) || is.integer(to)){
		to=as.integer(to)
		if(to<1 || to>length(defaultatt)){
			message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
			message("*List of attributes handled by 'att.setv' function:")
			print(cbind(attribute=defaultatt,id=1:11,description=description),quote=F)
			message("*Shape names and ids:")
			print(cbind(shape=defaultshapes,id=1:5),quote=F)
			return(g)
		}
		to=defaultatt[to]
	}
	if(!to%in%defaultatt){
		message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
		message("*List of attributes handled by 'att.setv' function:")
		print(cbind(attribute=defaultatt,id=1:11,description=description),quote=F)
		message("*Shape names and ids:")
		print(cbind(shape=defaultshapes,id=1:5),quote=F)
		return(g)
	}
	if(!is.numeric(fromatt) && !is.integer(fromatt) && !is.character(fromatt)){
		ms=paste("NOTE: graph attribute '",from,"' is not consistent with supported data types: character, numeric or integer!",sep="")
		stop(ms)
	}
	# more checks--------------------------------------------------------
	if(!is.null(breaks) && !is.numeric(breaks))stop("NOTE: arg. 'breaks' should be numeric!")
	if(!is.numeric(pal) && !is.integer(pal))stop("arg. 'pal' (pallete) should be an integer!");pal=pal[1]
	if(!pal%in%c(1,2))stop("NOTE: not a valid pallete (pal)! options: 1 or 2!")
	if(!is.numeric(xlim) && !is.integer(xlim))stop("NOTE: arg. 'xlim' should be numeric!")
	if(length(xlim)<3)stop(paste("NOTE: 'xlim=",xlim,"' is not consistent with expected arg. length! ...3!",sep=""))
	if(sum(is.null(xlim)>0))stop("NOTE: 'xlim' arg. does not support null values!")
	if(!is.logical(isrev))stop("NOTE: 'isrev' arg. should be a logical value!")
	if(!is.null(nquant) && !is.numeric(nquant))stop("NOTE: 'nquant' arg. should be an integer!");nquant=nquant[1]
	if(is.null(title) || !is.character(title) )title<-from
	#----------------------------------------	
	# main functions to set attribute scales!
	#----------------------------------------	
	#--simple color palette for 'category' or 'enumerated' data type (i.e. factor levels)
	colorcategory=function(x,cols,na.col,categvec,isrev){
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(isrev)cols=rev(cols)	
		# compute mapping
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec,ordered=FALSE)
		}			
		cols=colorRampPalette(colors=cols)(nlevels(x))
		x.col=cols[x]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get scale (for any legend) and return results
		leg=list(scale=cols,legend=levels(x))
		res=list(res=x.col,leg=leg)
		return(res)
	}	
	#--color scale palette with breaks
	colorscale1=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks<-quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		# adjust breaks and get palette
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		cols=colorRampPalette(colors=cols)(length(bkcenter)-1)
		# set colors to x
		x.col=rep(NA,length(x))
		cuts=cut(x[!is.na(x)], breaks=bkcenter,include.lowest=TRUE)
		x.col[!is.na(x)]=cols[as.integer(cuts)]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get intervals
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}		
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)	
		leg=list(scale=cols,legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)	
		return(res)
	}
	#--neg/pos color scale palette with breaks (left/right)
	colorscale2=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check args
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks<-quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.null(cols))cols=c("darkblue","white","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2		
		# check color vec		
		lt=length(cols)
		if(lt/2==as.integer(lt/2))lt=lt+1	
		cols=colorRampPalette(colors=cols)(lt)
		lfrt=as.integer(lt/2)+1
		# get neg/pos colors
		negCols=cols[1:lfrt]
		posCols=cols[lfrt:lt]
		ct.col=cols[lfrt]
		# check and adjust breaks
		lt=length(bkcenter)
		if(lt/2==as.integer(lt/2)){
			lf=lt/2
			rt=(lt/2)+1
			center=(bkcenter[lf]+bkcenter[rt])/2
			negBreaks=c(-Inf,bkcenter[1:lf],center)
			posBreaks=c(center,bkcenter[rt:lt],+Inf)
		} else {
			lfrt=as.integer(lt/2)+1
			center=bkcenter[lfrt]
			negBreaks=c(-Inf,bkcenter[1:lfrt])
			posBreaks=c(bkcenter[lfrt:lt],+Inf)
		}
		# set main palettes	
		negCols=colorRampPalette(colors=negCols)(length(negBreaks))[-length(negBreaks)]
		posCols=colorRampPalette(colors=posCols)(length(posBreaks))[-1]
		# set minor palettesscale
		na.col=colorRampPalette(colors=c(na.col,na.col))(1)
		ct.col=colorRampPalette(colors=c(ct.col,ct.col))(1)		
		# set colors to x
		x.col=rep(NA,length(x))
		idx=x<center & !is.na(x)
		negcuts=cut(x[idx],breaks=negBreaks,include.lowest=TRUE)
		x.col[idx]=negCols[as.integer(negcuts)]
		idx=x>center & !is.na(x)
		poscuts=cut(x[idx],breaks=posBreaks)
		x.col[idx]=posCols[as.integer(poscuts)]
		x.col[x==center]=ct.col
		x.col[is.na(x.col)]=na.col
		# get intervals
		if(is.null(nquant)){
			interv=c(levels(negcuts),levels(poscuts))
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}
		testlen=length(breaks)/2
		if(as.integer(testlen)<testlen){
			idx=as.integer(testlen)+1
			breaks=breaks[c(1:idx,idx:length(breaks))]
		}
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=c(negCols,posCols),legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)
		return(res)
	}
	#--simple size scale for 'category' or 'enumerated' data type (i.e. factor levels)
	xcategory=function(x, szmin, szmax, na.sz, categvec, isrev){
		# set sz and return vec
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec)
		}		
		szlevs=seq(szmin, szmax, length.out=nlevels(x))
		if(isrev)szlevs=rev(szlevs)
		x.sz=szlevs[x]
		x.sz[is.na(x.sz)]=na.sz
		# get scale (for any legend) and return results
		leg=list(scale=szlevs,legend=levels(x))
		res=list(res=x.sz,leg=leg)
		return(res)		
	}
	#--size scale with breaks
	xscale=function(x,breaks,szmin,szmax,na.sz,xlim,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks<-quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		# adjust breaks
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		# get sz levels
		szlevs=seq(szmin,szmax,length.out=length(bkcenter)-1)
    if(length(xlim)==length(szlevs))szlevs<-xlim
		if(isrev)szlevs=rev(szlevs)		
		# set sz to x
		x.sz=rep(NA,length(x))
		cuts=cut(x[!is.na(x)],breaks=bkcenter,include.lowest=TRUE)
		x.sz[!is.na(x)]=szlevs[as.integer(cuts)]
		x.sz[is.na(x.sz)]=na.sz
		# get interval
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}		
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=szlevs,legend=breaks,interval=interv)
		res=list(res=x.sz,leg=leg)
		return(res)
	}
	#--set node shapes for 'category' or 'enumerated' data type (i.e. factor levels)
	shapecategory=function(x,shapes,categvec,isrev){
		validnames=c('ELLIPSE', 'RECTANGLE', 'ROUNDED_RECTANGLE', 'TRIANGLE', 'DIAMOND')
		if(is.null(shapes)){
			shapes=validnames
		} else if(is.numeric(shapes) || is.integer(shapes)){
			shapes=as.integer(shapes)
			if(sum(sum(shapes<1),sum(shapes>5))>0){
				message("Error: NOTE: arg. 'shapes' with one or more inconsistent shape ids!")
				message("*List of shape names and ids handled by 'att.setv' function:")
				print(cbind(shape= validnames,id=1:5),quote=F)
				return(g)
			} 
			shapes=validnames[shapes]
		} else {
			if(sum(shapes%in%validnames)!=length(shapes)){
				message("Error: NOTE: arg. 'shapes' with one or more inconsistent shape names!")
				message("*List of shape names and ids handled by 'att.setv' function:")
				print(cbind(shape= validnames,id=1:5),quote=F)
				return(g)
			}			
		}
		if(sum(is.na(x))>0){
			stop("NOTE: 'NA' not supported for node shape mapping!")
		}
		if(isrev)shapes=rev(shapes)
		x=as.factor(x)
		if(length(levels(x))>length(shapes)){
			stop("NOTE: graph att. 'from' with more levels than shape options!")
		}
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			if(length(categvec)!=length(shapes)){
				if(length(categvec)>length(shapes)){
					stop("NOTE: arg. 'categvec' with more levels than shapes!")
				} else {
					shapes=shapes[1:length(categvec)]
				}
			}
			x=factor(x,levels=categvec)
		}
		att=shapes[x]
		shapes=shapes[1:nlevels(x)]
		# get shapes (for any legend) and return results
		leg=list(shape=shapes,legend=levels(x))
		res=list(res=att,leg=leg)
		return(res)
	}	
	# end of main functions!
	#-----------------------
	# map attribute to default names!---------------------------------------
	att=NULL
	if(to%in%coltype){
		if(is.null(breaks) && is.null(nquant)){
			att=colorcategory(fromatt,cols,na.col,categvec,isrev)
		} else {
			if(pal==1){
				att=colorscale1(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else if(pal==2){
				att=colorscale2(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else {
				att=colorcategory(fromatt,cols,na.col,categvec,isrev)
			}
		}
	} else if(to%in%numtype){
		szmin=xlim[1]
		szmax=xlim[2]
		na.sz=xlim[length(xlim)]
		xlim<-xlim[-length(xlim)]
		if(!to=='coordX' && !to=='coordY'){
			szmin=max(0,szmin)
			szmax=max(0,szmax)
			na.sz=max(0,na.sz)			
			if(to=='nodeBend'){
				szmin=min(100,szmin)
				szmax=min(100,szmax)
				na.sz=min(100,na.sz)
			}		
		}
		if(is.null(breaks) && is.null(nquant)){
			att=xcategory(fromatt,szmin,szmax,na.sz,categvec,isrev)
		} else {
			att=xscale(fromatt,breaks,szmin,szmax,na.sz,xlim,isrev,nquant,roundleg)
		}
	} else if(to=='nodeShape'){
		att=shapecategory(fromatt,shapes,categvec,isrev)
	} else if(to=='nodeAlias'){
		att=list()
		att$res=as.character(fromatt)
		getleg=FALSE
	}
	# return updated graph
	if(!is.null(att)){
		g=igraph::set.vertex.attribute(graph=g, name=to, value=att$res)
		if(is.logical(getleg) && getleg){
			to=gsub("\\b(\\w)","\\U\\1",to,perl=TRUE)
			leg=paste("leg",to,sep="")
			att$leg$title<-title
			g=igraph::set.graph.attribute(graph=g, name=leg, value=att$leg)
		}
	} else {
		message("...unable to conclude the command!")
	}
	return(g)
}

#------------------------------------------------------------------------------
# Set RedeR atts to igraph edges
att.sete <- function(g=NULL, from='name', to='edgeColor', pal=1, cols=NULL, na.col=grey(0.7),
                  xlim=c(20,100,1), shapes=NULL, breaks=NULL, categvec=NULL, nquant=NULL, 
                  isrev=FALSE, getleg=TRUE, roundleg=1,title=NULL){
  #check loaded igraph
  igraph.check()
	# set att---------------------------------------------------------
	coltype=c('edgeColor')
	numtype=c('edgeWidth','edgeWeight','arrowDirection')
	defaultatt=c(coltype, numtype, 'edgeType')
	defaultshapes=c('SOLID', 'DOTTED', 'DASHED', 'LONG_DASH')	
	d1='name, id, or hexadecimal <string or integer>'
	d2='[0,+inf) <numeric>'
	d3='0, 1, 2, or 3 <integer>'
	d4='name or id <string or integer>'
	description=c(d1,d2,d2,d3,d4)
	if(is.null(g)){
		message("*List of attributes handled by 'att.sete' function:")
		print(cbind(attribute=defaultatt,id=1:5,description=description),quote=F)
		message("*Edge types (names and ids):")
		print(cbind(type=defaultshapes,id=1:4),quote=F)
		return(invisible())
	}
    # check igraph object and main args---------------------------------
    if(!igraph::is.igraph(g)){
        stop("Not an igraph object!")
    }	
	if(!is.character(from))stop("NOTE: arg. 'from' should be a string!")
	from=from[1]
	if(!is.character(to) && !is.numeric(to) && !is.integer(to) )stop("NOTE: arg. 'to' should be a string or an integer!")
	to=to[1]
	# get ref. att---
	fromatt=igraph::get.edge.attribute(g, from)	
	if(is.null(fromatt) || length(fromatt)!=igraph::ecount(g)){
		stop(paste("NOTE: graph attribute '",from,"' is absent or not consistent with edge count!",sep=""))
	}
	if(is.numeric(to) || is.integer(to)){
		to=as.integer(to)
		if(to<1 || to>length(defaultatt)){
			message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
			message("*List of attributes handled by 'att.sete' function:")
			print(cbind(attribute=defaultatt,id=1:5,description=description),quote=F)
			message("*Shape names and ids:")
			print(cbind(shape=defaultshapes,id=1:4),quote=F)
			return(g)
		}
		to=defaultatt[to]
	}
	if(!to%in%defaultatt){
		message(paste("Error: NOTE: arg 'to=",to,"' is not consistent with available attr. options!",sep=""))
		message("*List of attributes handled by 'att.sete' function:")
		print(cbind(attribute=defaultatt,id=1:5,description=description),quote=F)
		message("*Shape names and ids:")
		print(cbind(shape=defaultshapes,id=1:4),quote=F)
		return(g)
	}
	if(!is.numeric(fromatt) && !is.integer(fromatt) && !is.character(fromatt)){
		ms=paste("NOTE: graph attribute '",from,"' is not consistent with supported data types: character, numeric or integer!",sep="")
		stop(ms)
	}
	# more checks--------------------------------------------------------
	if(!is.null(breaks) && !is.numeric(breaks))stop("NOTE: arg. 'breaks' should be numeric!")
	if(!is.numeric(pal) && !is.integer(pal))stop("arg. 'pal' (pallete) should be an integer!");pal=pal[1]
	if(!pal%in%c(1,2))stop("NOTE: not a valid pallete (pal)! options: 1 or 2!")
	if(!is.numeric(xlim) && !is.integer(xlim))stop("NOTE: arg. 'xlim' should be numeric!")
	if(length(xlim)<3)stop(paste("NOTE: 'xlim=",xlim,"' is not consistent with expected arg. length! ...3!",sep=""))
	if(sum(is.null(xlim)>0))stop("NOTE: 'xlim' arg. does not support null values!")
	if(!is.logical(isrev))stop("NOTE: 'isrev' arg. should be a logical value!")
	if(!is.null(nquant) && !is.numeric(nquant))stop("NOTE: 'nquant' arg. should be an integer!");nquant=nquant[1]
	if(is.null(title) || !is.character(title) )title<-from
	#----------------------------------------	
	# main functions to set attribute scales!
	#----------------------------------------	
	#--simple color palette for 'category' or 'enumerated' data type (i.e. factor levels)
	colorcategory=function(x,cols,na.col,categvec,isrev){
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(isrev)cols=rev(cols)	
		# compute mapping
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec,ordered=FALSE)
		}			
		cols=colorRampPalette(colors=cols)(nlevels(x))
		x.col=cols[x]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get scale (for any legend) and return results
		leg=list(scale=cols,legend=levels(x))
		res=list(res=x.col,leg=leg)
		return(res)
	}	
	#--color scale palette with breaks
	colorscale1=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks=quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}
		if(is.null(cols))cols=c("darkblue","blue","orange","cyan","red","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		# adjust breaks and get palette
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		cols=colorRampPalette(colors=cols)(length(bkcenter)-1)
		# set colors to x
		x.col=rep(NA,length(x))
		cuts=cut(x[!is.na(x)],breaks=bkcenter,include.lowest=TRUE)
		x.col[!is.na(x)]=cols[as.integer(cuts)]
		x.col[is.na(x.col)]=colorRampPalette(colors=c(na.col,na.col))(1)
		# get scale (for any legend) and return results
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=cols,legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)	
		return(res)
	}
	#--neg/pos color scale palette with breaks (left/right)
	colorscale2=function(x,breaks,cols,na.col,isrev,nquant,roundleg){
		# check args
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks=quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.null(cols))cols=c("darkblue","white","darkred")
		if(is.null(na.col)){na.col=grey(0.7)} else {na.col=na.col[1]}
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		if(isrev)cols=rev(cols)
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2		
		# check color vec		
		lt=length(cols)
		if(lt/2==as.integer(lt/2))lt=lt+1	
		cols=colorRampPalette(colors=cols)(lt)
		lfrt=as.integer(lt/2)+1
		# get neg/pos colors
		negCols=cols[1:lfrt]
		posCols=cols[lfrt:lt]
		ct.col=cols[lfrt]
		# check and adjust breaks
		lt=length(bkcenter)
		if(lt/2==as.integer(lt/2)){
			lf=lt/2
			rt=(lt/2)+1
			center=(bkcenter[lf]+bkcenter[rt])/2
			negBreaks=c(-Inf,bkcenter[1:lf],center)
			posBreaks=c(center,bkcenter[rt:lt],+Inf)
		} else {
			lfrt=as.integer(lt/2)+1
			center=bkcenter[lfrt]
			negBreaks=c(-Inf,bkcenter[1:lfrt])
			posBreaks=c(bkcenter[lfrt:lt],+Inf)
		}
		# set main palettes	
		negCols=colorRampPalette(colors=negCols)(length(negBreaks))[-length(negBreaks)]
		posCols=colorRampPalette(colors=posCols)(length(posBreaks))[-1]
		# set minor palettesscale
		na.col=colorRampPalette(colors=c(na.col,na.col))(1)
		ct.col=colorRampPalette(colors=c(ct.col,ct.col))(1)		
		# set colors to x
		x.col=rep(NA,length(x))
		idx=x<center & !is.na(x)
		negcuts=cut(x[idx],breaks=negBreaks,include.lowest=TRUE)
		x.col[idx]=negCols[as.integer(negcuts)]
		idx=x>center & !is.na(x)
		poscuts=cut(x[idx],breaks=posBreaks)
		x.col[idx]=posCols[as.integer(poscuts)]
		x.col[x==center]=ct.col
		x.col[is.na(x.col)]=na.col
		# get intervals
		if(is.null(nquant)){
			interv=c(levels(negcuts),levels(poscuts))
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}
		testlen=length(breaks)/2
		if(as.integer(testlen)<testlen){
			idx=as.integer(testlen)+1
			breaks=breaks[c(1:idx,idx:length(breaks))]
		}
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=c(negCols,posCols),legend=breaks, interval=interv)
		res=list(res=x.col,leg=leg)
		return(res)
	}
	#--simple size scale for 'category' or 'enumerated' data type (i.e. factor levels)
	xcategory=function(x, szmin, szmax, na.sz, categvec, isrev){
		# set sz and return vec
		x=as.factor(x)
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			x=factor(x,levels=categvec)
		}		
		szlevs=seq(szmin, szmax, length.out=nlevels(x))
		if(isrev)szlevs=rev(szlevs)
		x.sz=szlevs[x]
		x.sz[is.na(x.sz)]=na.sz
		# get scale (for any legend) and return results
		leg=list(scale=szlevs,legend=levels(x))
		res=list(res=x.sz,leg=leg)
		return(res)		
	}
	#--size scale with breaks
	xscale=function(x,breaks,szmin,szmax,na.sz,isrev,nquant,roundleg){
		# check arg
		if(!is.null(nquant)){
			if(nquant<2)stop("NOTE: require at least two quantiles!")
			breaks=quantile(x,probs=seq(0,1,length.out=nquant),na.rm=TRUE,names=FALSE)
			breaks<-unique(breaks);nquant=length(breaks)
			if(length(breaks)<3)stop("NOTE: not enough intervals for 'nquant'!")
		}	
		if(is.character(x))stop("NOTE: 'breaks' arg. can not be applyed to characters!")
		if(sum(is.null(breaks))>0)stop("NOTE: breaks do not support null values!")
		if(length(breaks)<3)stop("NOTE: require at least three breaks!")
		# adjust breaks
		bkcenter=(breaks[-length(breaks)]+breaks[-1])/2
		bkcenter=c(-Inf,bkcenter,+Inf)
		# get sz levels
		szlevs=seq(szmin,szmax,length.out=length(bkcenter)-1)
		if(isrev)szlevs=rev(szlevs)		
		# set sz to x
		x.sz=rep(NA,length(x))
		cuts=cut(x[!is.na(x)],breaks=bkcenter,include.lowest=TRUE)
		x.sz[!is.na(x)]=szlevs[as.integer(cuts)]
		x.sz[is.na(x.sz)]=na.sz
		# get scale (for any legend) and return results
		if(is.null(nquant)){
			interv=levels(cuts)
		} else {
			interv=seq(0,1,length.out=nquant+1)[-1]
			interv=paste(interv*100,"%",sep="")
		}	
		breaks=format(breaks, digits=roundleg, nsmall=roundleg)
		leg=list(scale=szlevs,legend=breaks,interval=interv)
		res=list(res=x.sz,leg=leg)
		return(res)
	}
	#--set node shapes for 'category' or 'enumerated' data type (i.e. factor levels)
	shapecategory=function(x,shapes,categvec,isrev){
		validnames=c('SOLID', 'DOTTED', 'DASHED', 'LONG_DASH')
		if(is.null(shapes)){
			shapes=validnames
		} else if(is.numeric(shapes) || is.integer(shapes)){
			shapes=as.integer(shapes)
			if(sum(sum(shapes<1),sum(shapes>5))>0){
				message("Error: NOTE: arg. 'edgeType' with one or more inconsistent ids!")
				message("*List of edgetype names and ids handled by 'att.sete' function:")
				print(cbind(shape= validnames,id=1:4),quote=F)
				return(g)
			} 
			shapes=validnames[shapes]
		} else {
			if(sum(shapes%in%validnames)!=length(shapes)){
				message("Error: NOTE: arg. 'edgeType' with one or more inconsistent names!")
				message("*List of edgetype names and ids handled by 'att.sete' function:")
				print(cbind(shape= validnames,id=1:4),quote=F)
				return(g)
			}			
		}
		if(sum(is.na(x))>0){
			stop("NOTE: 'NA' not supported for edge type mapping!")
		}
		if(isrev)shapes=rev(shapes)
		x=as.factor(x)
		if(length(levels(x))>length(shapes)){
			stop("NOTE: graph att. 'from' with more levels than edgetype options!")
		}
		if(!is.null(categvec)){
			if(sum(levels(x)%in%categvec)!=length(levels(x))){
				stop("NOTE: graph att. 'from' with one or more levels not represented in 'categvec' arg.!")
			}
			if(length(categvec)!=length(shapes)){
				if(length(categvec)>length(shapes)){
					stop("NOTE: arg. 'categvec' with more levels than edge types!")
				} else {
					shapes=shapes[1:length(categvec)]
				}
			}
			x=factor(x,levels=categvec)
		}
		att=shapes[x]
		# get shapes (for any legend) and return results
		leg=list(shape=shapes,legend=levels(x))
		res=list(res=att,leg=leg)
		return(res)
	}	
	# end of main functions!
	#-----------------------
	# map attribute to default names!---------------------------------------
	att=NULL
	if(to%in%coltype){
		if(is.null(breaks) && is.null(nquant)){
			att=colorcategory(fromatt,cols,na.col,categvec,isrev)
		} else {
			if(pal==1){
				att=colorscale1(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else if(pal==2){
				att=colorscale2(fromatt,breaks,cols,na.col,isrev,nquant,roundleg)
			} else {
				att=colorcategory(fromatt,cols,na.col,categvec,isrev)
			}
		}
	} else if(to%in%numtype){
		c('edgeWidth','edgeWeight','arrowDirection')
		szmin=xlim[1]
		szmax=xlim[2]
		na.sz=xlim[3]
		szmin=max(0,szmin)
		szmax=max(0,szmax)
		na.sz=max(0,na.sz)			
		if(to=='arrowDirection'){
			szmin=min(3,szmin)
			szmax=min(3,szmax)
			na.sz=min(3,na.sz)
		}
		if(is.null(breaks) && is.null(nquant)){
			att=xcategory(fromatt,szmin,szmax,na.sz,categvec,isrev)
		} else {
			att=xscale(fromatt,breaks,szmin,szmax,na.sz,isrev,nquant,roundleg)
		}
	} else if(to=='edgeType'){
		att=shapecategory(fromatt,shapes,categvec,isrev)
	}
	# return updated graph
	if(!is.null(att)){
		g=igraph::set.edge.attribute(graph=g, name=to, value=att$res)
		if(is.logical(getleg) && getleg){
			to=gsub("\\b(\\w)","\\U\\1",to,perl=TRUE)
			leg=paste("leg",to,sep="")
			att$leg$title<-title
			g=igraph::set.graph.attribute(graph=g, name=leg, value=att$leg)
		}
	} else {
		message("...unable to conclude the command!")
	}
	return(g)
}
	
#------------------------------------------------------------------------------
# Map RedeR atts to igraph vertices
att.mapv <- function(g, dat, refcol=1){
  #check loaded igraph
  igraph.check()
	if(!is.data.frame(dat)){
		stop("'dat' should be a data frame!")
	}
	# check igraph object and main args---------------------------------
	if(!igraph::is.igraph(g)){
	  stop("'g' should be an igraph object!")
	}
	# get vecs to match!
	nodes=V(g)$name
	if(is.null(nodes) || igraph::vcount(g)!=length(nodes)){
		stop("NOTE: require 'name' attribute in igraph vertices!")
	}
	if(is.singleInteger(refcol)){
	  if(refcol<0 || refcol>ncol(dat)){
	    stop("NOTE: invalid 'refcol' value! it should be a column index in 'data' object!")
	  }
	  if(refcol==0){
	    dc=rownames(dat)
	  } else {
	    dc=dat[[refcol[1]]]
	  }
	} else if(is.singleString(refcol)){
	  if(!refcol%in%colnames(dat)){
	    stop("NOTE: invalid 'refcol' value! it should be a column index in 'data' object!")
	  }
	  refcol <- which(colnames(dat)==refcol)[1]
	  dc <- dat[[refcol]]
	} else {
	  stop("NOTE: 'refcol' should be a single integer or character value!")
	}
	# check attribute data class
	if(is.factor(dc)){
		if(class(levels(dc))=="character"){
			dc=as.character(dc)
		} else if(class(levels(dc))=="numeric"){
			dc=as.numeric(dc)
		} else if(class(levels(dc))=="integer"){
			dc= as.integer(dc)
		} else {
			stop("NOTE: invalid ref. attribute! supported data classes: character, numeric or integer!")
		}
	}
	# check node data class and map attributes to nodes!
	if(class(nodes)==class(dc)){
		dat=dat[match(nodes,dc), , drop=FALSE]
	} else if(class(nodes)=="character"){
		dat=dat[match(nodes,as.character(dc)), , drop=FALSE]
	} else if(class(nodes)=="numeric"){
		dat=dat[match(nodes,as.numeric(dc)), , drop=FALSE]
	} else if(class(nodes)=="integer"){
		dat=dat[match(nodes,as.integer(dc)),, drop=FALSE]
	} else {
		stop("NOTE: invalid node names! supported data classes: character, numeric or integer!")
	}
	# check whether resulting dataset size matches nodes!
	if(nrow(dat)!=length(nodes)){
		stop("NOTE: one or more graph nodes are not listed in the dataset (i.e. 'refcol' ids)!")
	}
	# transfer data to graph vertices
	for(i in 1:ncol(dat)){
		if(i!=refcol){
			att=dat[[i]]
			if(is.factor(att)){
			  lv <- check.numeric(levels(att))
			  att <- lv[att]
			}
			g=igraph::set.vertex.attribute(graph=g, name=names(dat)[i],value=att)
		}
	}
	return(g)
}
check.numeric <- function(x){
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  x[x %in% c("",".", "NA")] <- NA
  xx <- x[!is.na(x)]
  if (length(xx)>0){
    bl <- suppressWarnings(!any(is.na(as.numeric(xx))))
    if(bl) x <- as.numeric(x)
  }
  return(x)
}

#------------------------------------------------------------------------------
# Map RedeR atts to igraph edges
att.mape <- function(g, dat, refcol=c(1,2)){
  #check loaded igraph
  igraph.check()
	if(!is.data.frame(dat)){
		stop("not a data frame!")
	}
    # check igraph object
    if(!igraph::is.igraph(g)){
        stop("Not an igraph object!")
    }
  if(!all.integerValues(refcol)){
    stop("NOTE: invalid 'refcol' value! it should be a two-column index in 'data' object!")
  }
  if(any(refcol<1) || any(refcol>ncol(dat))){
    stop("NOTE: invalid 'refcol' value; it should be a two-column index in 'data' object!")
  }
	# get vecs to match!
	nodes=V(g)$name
	if(is.null(nodes) || igraph::vcount(g)!=length(nodes)){
		stop("NOTE: require 'name' attribute in igraph vertices!")
	}
	edges=igraph::get.edgelist(g)
	edgevec=array(NA,dim=nrow(edges))
	for(i in 1:nrow(edges)){
		edgevec[i]=paste(edges[i,],collapse=",")
	}
	dc=as.matrix(dat[,refcol])
	dcvec=array(NA,dim=nrow(dc))
	for(i in 1:nrow(dc)){
		dcvec[i]=paste(dc[i,c(1,2)],collapse=",")
	}
	idx1=match(edgevec, dcvec)
	dcvec=array(NA,dim=nrow(dc))
	for(i in 1:nrow(dc)){
		dcvec[i]=paste(dc[i,c(2,1)],collapse=",")
	}
	idx2=match(edgevec, dcvec)
	idx1[is.na(idx1)]=idx2[is.na(idx1)]
	# check whether resulting dataset size matches nodes!
	if(sum(is.na(idx1))>0){
		stop("NOTE: one or more edges are not listed in the dataset (i.e. 'refcol' ids)!")
	}	
	dat=dat[idx1,,drop=FALSE]
	# check whether resulting dataset size matches nodes!
	if(nrow(dat)!=nrow(edges)){
		stop("NOTE: one or more edges are not listed in the dataset (i.e. 'refcol' ids)!")
	}
	# transfer data to graph edges
	for(i in 1:ncol(dat)){
		if(!i%in%refcol){
			att=dat[[i]]
			if(is.factor(att))att=levels(att)[att]
			g=igraph::set.edge.attribute(graph=g, name=names(dat)[i],value=att)
		}
	}
	return(g)
}

#------------------------------------------------------------------------------
# get subg from an igraph object
subg <- function(g, dat, refcol=1, maincomp=TRUE, connected=TRUE, transdat=TRUE){
	if(is.data.frame(dat)){
		allids=as.character(dat[[refcol[1]]])
	} else if(is.vector(dat)){
		allids=as.character(dat)
	} else {
		stop("not a data frame!")
	}
	if(!igraph::is.igraph(g)){
	  stop("Not an igraph object!")
	}
	if(is.null(V(g)$name))V(g)$name=as.character(V(g))
	ids=allids[allids%in%V(g)$name]
	if(length(ids)!=length(allids)){
	  message("...note: not all genes found in the network!")
	}
	sg=igraph::induced.subgraph(graph=g,vids=ids)
	if(maincomp){
		comp <- igraph::clusters(sg)
		cids <- which.max(comp$csize)
		sg <- igraph::induced.subgraph(graph=sg, vids=V(sg)[comp$membership == cids])
	} else if(connected){
		dg=igraph::degree(sg)>0
		nodes=V(sg)$name[dg]
		sg=igraph::induced.subgraph(graph=sg,vids=nodes)
	}
	if(transdat && is.data.frame(dat)){
		sg <- att.mapv(g=sg, dat=dat, refcol=refcol)
	}
	return(sg)
}

#Simple function to generate random null distributions for co-expression analysis
#------------------------------------------------------------------------------
cea <- function(x, sig=0.01, padj.method="fdr", 
             cor.method="spearman", nper=1000, 
             regulators=NULL, plotcea=TRUE,...){
	if(is.data.frame(x)){
		x=as.matrix(x)
	} else {
		if(!is.matrix(x))stop("NOTE: 'x' should be a matrix!")
	}
  if( !is.singleNumber(sig) ||  sig<0 || sig>1 )
    stop("NOTE: 'sig' should be a single numeric value in the interval [0,1]!")
  if(!is.singleString(padj.method))
    stop("NOTE: 'padj.method' should be a single character value!")
  if(!is.singleString(cor.method))
    stop("NOTE: 'cor.method' should be a single character value!")
  if(!is.singleInteger(nper) || nper<1)
    stop("NOTE: 'nper' should be a single positive integer value!")
  if(!is.null(regulators) && !is.character(regulators))
    stop("NOTE: 'regulators' should be a character vector!")
  if(!is.singleLogical(plotcea))
    stop("NOTE: 'plotcea' should be a single logical value!")
  if(is.null(rownames(x))){
    stop("NOTE: matrix 'x' should have rownames!")
  }
  if(!is.null(regulators)){
    if(is.character(regulators)){
      if(!all(regulators%in%rownames(x))){
        stop("NOTE: all 'regulators' should be listed in 'x' rownames!")
      }
      alltargets <- c(regulators,setdiff(rownames(x),regulators))
    } else {
      stop("NOTE: matrix 'x' should have rownames!")
    }
  } else {
    regulators <- alltargets <- rownames(x)
  }
	cat("Step 1 ...computing correlation",fill=TRUE)
	# get correlation matrix
	x=t(x)
	corrMt=cor(x[,regulators],x[,alltargets], method = cor.method)
	diag(corrMt[regulators,regulators]) <- 0
	uniqueVec=unique(sort(corrMt))
	cat("Step 2 ...computing null distribution",fill=TRUE)
	# builds the null distribution via permutation
	ctsum=numeric(length(uniqueVec))
	nulldist=list()
	pb=txtProgressBar(min=0, max=nper, initial=0, char="=", style=1)
	nr <- length(regulators)
	nt <- length(alltargets)
	nrt <- (nt*nr)-nr
	for(i in 1:nper){
		permt <- matrix(sample(x),nrow=nrow(x),ncol=ncol(x))
		permt <- cor(permt[,1:nr],permt[,1:nt],method=cor.method)
		diag(permt[1:nr,1:nr]) <- NA
		permt <- sort(permt)
		#find intervals
		ct=findInterval(uniqueVec,permt)
		ctsum=ctsum+ct
		nl=density(permt)
		nulldist$x=cbind(nulldist$x,nl$x)
		nulldist$y=cbind(nulldist$y,nl$y)		
		setTxtProgressBar(pb, value=i)
	}
	#close(pb)
	cat("\n")
	cat("Step 3 ...computing probs", fill=TRUE)
	probs <- ctsum/(nrt*nper)
	probs <- 1 - ( ( probs - 0.5 ) * 2 )^2
	probs <- probs[match(as.numeric(corrMt),uniqueVec)]
	cat("Step 4 ...adjusting pvals",fill=TRUE)
	# adjust pvals
	pvalAdj=p.adjust(probs,method=padj.method)
	pvalAdj=matrix(pvalAdj,nrow=nrow(corrMt),ncol=ncol(corrMt))
	# decides on the significance
	decision=pvalAdj>sig
	decisionMt=corrMt
	decisionMt[decision]=0.0
	rescea=list(corr.mt=corrMt, decision.mt=decisionMt, pvalue.adj=pvalAdj, null.dist=nulldist)
	# function to plot decision matx
	ptcea=function(rescea, ptype=4, bk=0.2, n.breaks=100, plotnull=TRUE, avnull=TRUE, nullcol="black"){
		if(!is.numeric(bk))bk=0.2
		bk=min(1,max(0.1,bk))
		n.breaks=as.integer(n.breaks)
		if(!is.numeric(ptype) && !is.integer(ptype))ptype=1
		if(!ptype%in%c(1,2,3,4,5))ptype=1
		if(!is.numeric(n.breaks) && !is.integer(n.breaks))n.breaks=100
		if(n.breaks<2)n.breaks=2
		#
		if(ptype<=3){
			null.dist=rescea$null.dist
			decision.dist=as.numeric(rescea$decision.mt)
			decision.neg=density(decision.dist,to=-bk)	
			decision.pos=density(decision.dist,from=bk)	
			maxdeci=max(decision.neg$y, decision.pos$y)
			#
			corr.dist=as.numeric(rescea$corr.mt)
			corr.dist=density(corr.dist)	
			maxcorr=max(corr.dist$y)
			#maxdata=min(c(maxdeci,maxcorr))
			maxdata=maxcorr
		} else {
			null.dist=rescea$null.dist
			decision.dist=as.numeric(rescea$decision.mt)
			#
			decision.hist=hist(decision.dist, plot=FALSE, breaks=n.breaks)
			maxdeci=max(decision.hist$density)
			#
			decision.neg=density(decision.dist,to=-bk)	
			decision.pos=density(decision.dist,from=bk)	
			#
			corr.dist=as.numeric(rescea$corr.mt)
			corr.dist=hist(corr.dist, plot=FALSE, breaks=n.breaks)	
			maxcorr=max(corr.dist$density)
			maxdata=maxcorr		
		}
		#
		if(ptype==1){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#
			y=apply(null.dist$y,1,mean)
			y=y/max(y)
			x=apply(null.dist$x,1,mean)	
			polygon(x=x,y=y,col="grey", lty="blank")
			#
			x=decision.neg$x		
			y=decision.neg$y/maxdata
			polygon(x=x,y=y, col="blue",lty="blank")
			#
			x=decision.pos$x		
			y=decision.pos$y/maxdata
			polygon(x=x,y=y, col="red",lty="blank")		
			#  
			x=corr.dist$x  	
			y=corr.dist$y/maxdata
			lines(x=x,y=y, col="black", lty="dashed", lwd=2.0)
			#   
			legend("topleft",c("all associations","sig. neg. associations",
				"sig. pos. association","null distribution"), pch=c(NA_integer_ ,15, 15, 15), 
				pt.cex=1, lty=c(2, 0, 0, 0), merge=TRUE, col=c('black','blue','red','grey'), bty="n", cex=0.7)  
		} 
		if(ptype==2){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$x  	
			y=corr.dist$y/maxdata
			polygon(x=x,y=y, col="grey", lty="blank")
			#
			x=decision.neg$x		
			y=decision.neg$y/maxdata
			polygon(x=x,y=y, col="blue",lty="blank")
			#
			x=decision.pos$x		
			y=decision.pos$y/maxdata
			polygon(x=x,y=y, col="red",lty="blank")
			#	
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)				
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")					
				}		
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations","null distributions"), pch=c(15, 15, 15, NA_integer_), 
					pt.cex=1, lty=c(0, 0, 0, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all association","sig. neg. associations",
					"sig. pos. associations"), pch=c(15, 15, 15), 
					pt.cex=1, lty=c(0, 0, 0), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}
		if(ptype==3){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$x  	
			y=corr.dist$y/maxdata
			polygon(x=x,y=y, col="grey", lty="blank")
			#
			x=decision.neg$x		
			y=decision.neg$y/maxdata
			lines(x=x,y=y, col="blue",lty="dashed", lwd=3.0)
			#
			x=decision.pos$x		
			y=decision.pos$y/maxdata
			lines(x=x,y=y, col="red", lty="dashed", lwd=3.0)
			#
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)				
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")					
				}			
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations","null distributions"), pch=c(15, 15, 15, NA_integer_), 
					pt.cex=1, lty=c(0, 0, 0, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all association","sig. neg. associations",
					"sig. pos. associations"), pch=c(15, 15, 15), 
					pt.cex=1, lty=c(0, 0, 0), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}	
		if(ptype==4){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
			xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$breaks  	
			y=corr.dist$density/maxdata	
			rect(x[-length(x)], 0, x[-1], y,col='grey90',border='grey30')
			#	
			xd=decision.hist$breaks		
			yd=decision.hist$density/maxdata
			idx=xd>bk;
			if(sum(idx)>0){
			  x=c(0,xd[idx]);y=c(0,yd[idx])
			  rect(x[-length(x)], 0, x[-1], y,col='red',border='darkred')
			}
			idx=xd<(-bk);
			if(sum(idx)>0){
			  x=c(xd[idx],0);y=c(yd[idx],0)	
			  rect(x[-length(x)], 0, x[-1], y,col='blue',border='darkblue')
			}
			#
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")	
				}			
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations","null distribution"), pch=c(15, 15, 15, NA_integer_), 
					pt.cex=1, lty=c(0, 0, 0, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all associations","sig. neg. associations",
					"sig. pos. associations"), pch=c(15, 15, 15), 
					pt.cex=1, lty=c(0, 0, 0), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}
		if(ptype==5){
			plot(x=c(-1,1),y=c(0,1), col="black",type="n", main="Gene co-expression analysis", 
				 xlab="Correlation coefficient", ylab="Density")
			#  
			x=corr.dist$breaks  	
			y=corr.dist$density/maxdata	
			rect(x[-length(x)], 0, x[-1], y,col='grey90',border='grey70')
			#
			x=c(decision.neg$x,0)		
			y=c(decision.neg$y/maxdata,0)
			lines(x=x,y=y, col="blue", lty="dashed", lwd=3.0)
			#
			x=c(0,decision.pos$x)		
			y=c(0,decision.pos$y/maxdata)
			lines(x=x,y=y, col="red", lty="dashed", lwd=3.0)
			#
			if(plotnull){	
				y=null.dist$y/max(null.dist$y)
				x=null.dist$x
				if(avnull){
					x=apply(x,1,mean)
					y=apply(y,1,mean)
					lines(x,y, lwd=3.0, col=nullcol, lty="dashed")
				} else {
					for(i in 1:min(100,ncol(x)))lines(x=x[,i],y=y[,i],lwd=1.0, col=i, lty="dashed")	
				}			
				legend("topleft",c("all associations","sig. neg. associations",
								   "sig. pos. associations","null distribution"), pch=c(15, NA_integer_, NA_integer_, NA_integer_), 
					   pt.cex=1, lty=c(0, 2, 2, 2), merge=TRUE, col=c('grey','blue','red','black'), bty="n", cex=0.7)
			} else {
				legend("topleft",c("all associations","sig. neg. associations",
								   "sig. pos. associations"), pch=c(15, NA_integer_, NA_integer_), 
					   pt.cex=1, lty=c(0, 2, 2), col=c('grey','blue','red'), bty="n", cex=0.7)			
			}
		}
	}
	if(plotcea){
		ptcea(rescea,...=...)
	}
	return(rescea$decision.mt)
}
##-----------------------------------------------------------------------------
#Use default igraph atts if available------------------
check.igraph.format <- function(g){
  if(!is.null(V(g)$color) && is.null(V(g)$nodeColor) )V(g)$nodeColor=V(g)$color
  if(!is.null(V(g)$frame.color) && is.null(V(g)$nodeLineColor) )V(g)$nodeLineColor=V(g)$frame.color
  if(!is.null(V(g)$size) && is.null(V(g)$nodeSize) )V(g)$nodeSize=V(g)$size*2.5
  if(!is.null(V(g)$label) && is.null(V(g)$nodeAlias) )V(g)$nodeAlias=V(g)$label
  if(!is.null(V(g)$label.cex) && is.null(V(g)$nodeFontSize) )V(g)$nodeFontSize=V(g)$label.cex*16
  if(!is.null(V(g)$label.color) && is.null(V(g)$nodeFontColor) )V(g)$nodeFontColor=V(g)$label.color
  if(!is.null(V(g)$shape) && is.null( ) ){
    shapes<-V(g)$shape
    shapes[shapes=="circle"]="ELLIPSE"
    shapes[shapes!="circle"]="RECTANGLE"
    V(g)$nodeShape<-shapes
  }    			
  if(!is.null(E(g)$width) && is.null(E(g)$edgeWidth) )E(g)$edgeWidth<-E(g)$width
  if(!is.null(E(g)$color) && is.null(E(g)$edgeColor) )E(g)$edgeColor<-E(g)$color
  if(!is.null(E(g)$weight) && is.null(E(g)$edgeWeight) )E(g)$edgeWeight<-E(g)$weight
  if(!is.null(E(g)$lty) && is.null(E(g)$edgeType) ){
    edgeType <- E(g)$lty
    idx <- c(0:6)
    names(idx) <- c("blank","solid","dashed","dotted","dotdash","longdash","twodash")
    if(!is.numeric(edgeType)){
      edgeType[!edgeType%in%names(idx)] <- "solid"
      edgeType <- idx[edgeType]
    }
    idx <- idx[edgeType+1]
    edgeType[idx==0] <- "SOLID"
    edgeType[idx==1] <- "SOLID"
    edgeType[idx==2] <- "DASHED"	
    edgeType[idx==3] <-"DOTTED"
    edgeType[idx==4] <- "DOTTED"
    edgeType[idx==5] <- "LONG_DASH"	
    edgeType[idx==6] <- "DASHED"
    E(g)$edgeType <- edgeType
  }
  if(is.null(V(g)$nodeLineColor) && !is.null(V(g)$color) ) V(g)$nodeLineColor="black"
  return(g)
}
##-----------------------------------------------------------------------------
check.igraph.direction <- function(g){
  #Check direction and type
  #---set mode if available---#
  arrowType = E(g)$arrowType
  if(!is.null(arrowType) && length(arrowType)>0){
    c1=!is.integer(arrowType)
    c2=!is.numeric(arrowType)
    c3=(sum(arrowType< -1)>0 || sum(arrowType>1)>0)
    if(c1 && c2){
      stop("NOTE: 'arrow type' must be provided as integers!")
    } else if(sum(is.na(arrowType))>0){
      stop("NOTE: invalid 'arrow type' declaration: 'NA' found'!")
    } else if(c3){
      stop("NOTE: invalid 'arrow type' input (options: -1, 0 or 1)")
    }
    g<-arrowtype4reder(g)
    #---
    #E(g)$arrowLength<-5
    #E(g)$arrowAngle<-15
    #idx<-E(g)$arrowType<0
    #E(g)$arrowLength[idx]<-3   
    #E(g)$arrowAngle[idx]<-90
    E(g)$arrowDirection<-E(g)$arrowType
  } else {
    # set direction to edge attributes
    idxmutual<-igraph::is.mutual(g)
    E(g)$arrowDirection=1
    E(g)$arrowDirection[idxmutual]=3
    #g=igraph::as.undirected(g, mode="collapse") //essa funcao nao retorno ordem correta!!! controlado agora em J!
    c1=length(igraph::list.edge.attributes(g))>0
    c2=sum(idxmutual)>0
    if(c1 && c2){
      warning("NOTE: attributes from mutual edges were collapsed to unique edges (see 'addGraph' doc).")
      #isso sera feito no lado Java, ultimo link define valor final!!!
    }
    #Remove multiple edges and loops (obs. for directed graphs order does matter)
    if(!igraph::is.simple(g)){
      g=igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
      warning("NOTE: loops and/or multiple edges were removed from your graph (see 'addGraph' doc)!")
    }
  }
  return(g)
}
##-----------------------------------------------------------------------------
##format arrowType
arrowtype4reder <- function(g){
  #---get edges and mode
  edgeMtx<-igraph::get.adjacency(g, sparse=FALSE,attr=NULL, names=FALSE)
  modeMtx<-igraph::get.adjacency(g, sparse=FALSE,attr="arrowType", names=FALSE)
  ix<-igraph::get.edgelist(g,names=FALSE)
  #---differentiate upper.tri/lower.tri
  modeMtx[upper.tri(modeMtx)]<-modeMtx[upper.tri(modeMtx)]*10
  edgeMtx[modeMtx!=0]<-modeMtx[modeMtx!=0]*10
  #---set arrowType for reder
  arrowType<-NULL
  sapply(1:nrow(ix),function(i){
    tp<-edgeMtx[ix[i,1],ix[i,2]]+edgeMtx[ix[i,2],ix[i,1]]
    arrowType<<-c(arrowType,tp)
  })
  arrowType[arrowType==1 | arrowType==2]<-0
  #---map mutual edges and remove duplicated
  idx<-is.mutual(g) & ix[,1]>ix[,2]
  arrowType<-arrowType[!idx]
  g<-delete.edges(g,edges=which(idx))
  #---set mode---#
  # arrow key, related to 'A->B' orientation
  #  0 = undirected:  0 (A-B or B-A)
  # +1 = simple: +10 (A->B)
  # -1 = simple: -10 (A-/B)  
  # +2 = simple: +100 (B->A)
  # -2 = simple: -100 (B-/A)
  # +3 = double: +110 (same mode, A->B and B->A)
  # -3 = double: -110 (same mode, A-\B and B-\A)
  # +4 = double: +90 (inverse mode, A->B and B-\A)
  # -4 = double: -90 (inverse mode, A-\B and B->A)
  key<-c(0,10,-10,100,-100,110,-110,90,-90)
  lab<-c(0,1,-1,1,-1,3,-3,4,-4)
  arrowType<-sapply(1:length(arrowType),function(i){
    tp<-arrowType[i]
    lab[which(key==tp)]
  })
  E(g)$arrowType<-arrowType
  return(g)
}
# chech igraph compatibility
igraph.check<-function(){
  b1<-"package:igraph0" %in% search()
  b2<- "igraph0" %in%  loadedNamespaces()
  if( b1 || b2) {
    stop("\n\n ...conflict with 'igraph0': please use the new 'igraph' package!")
  }
}
##-----------------------------------------------------------------------------
##build nested maps from hclust objects
treemap <- function(hc){
  A=hc$merge
  B=list()
  C=list()
  D=list()
  E=list()
  nest=list()
  if(is.null(hc$labels))hc$labels=as.character(sort(hc$order))
  for(i in 1:nrow(A)){
    ai=A[i,1]
    if(ai < 0){
      B[[i]]= -ai
      C[[i]]=1
    } else {
      B[[i]]=B[[ai]]      
      C[[i]]=C[[ai]]+1 
    }
    ai=A[i,2]
    if(ai < 0){
      B[[i]]=sort(c(B[[i]],-ai))
    } else {
      B[[i]]=sort(c(B[[i]],B[[ai]]))
      C[[i]]=max(C[[i]],C[[ai]]+1)
    }
    p=match(i,A)
    D[[i]]=ifelse(p>nrow(A),p-nrow(A),p)
    nest[[i]]=hc$labels[B[[i]]]
  }
  D[[nrow(A)]]=nrow(A)+1
  for(i in 1:nrow(A)){
    step=1
    find=D[[i]]  
    while(find<D[[nrow(A)]]){
      find=D[[find]]
      step=step+1
    }
    E[[i]]=step
  }
  # get dendogram xy position
  nn=nrow(A) + 1
  xaxis=c()
  yaxis=hc$height
  tp=rep(0,2)
  mm=match(1:length(hc$order),hc$order)
  for(i in 1:(nn-1)) {
    ai=A[i,1]
    if(ai < 0){
      tp[1]=mm[-ai]
    } else {
      tp[1]=xaxis[ai]
    }
    ai=A[i,2]
    if(ai < 0){
      tp[2]=mm[-ai]
    } else {
      tp[2]=xaxis[ai]
    }
    xaxis[i]=mean(tp)
  }
  xyaxis=data.frame(xaxis=xaxis,yaxis=yaxis,stringsAsFactors=FALSE)
  # return res
  C=as.numeric(C)
  D=as.numeric(D)
  E=as.numeric(E)
  N=hc$merge>0
  N=N[,1]+N[,2]
  obj<-list(nest=nest,compids=B,labels=hc$labels,parent=D,leafdist=C,
            rootdist=E,height=hc$height,nnest=N, xyaxis=xyaxis)
  #---get unified edges
  N<-nrow(hc$merge);nn<-N+1
  hcEdges<-NULL
  eLength<-NULL
  junk<-sapply(1:N,function(i){
    y1<-hc$merge[i,1]
    y2<-hc$merge[i,2]
    if(y1>0){
      l1<-hc$height[i] - hc$height[y1]
    } else {
      l1<-hc$height[i]
    }
    if(y2>0){
      l2<-hc$height[i] - hc$height[y2]
    } else {
      l2<-hc$height[i]
    }    
    tp<-cbind(rbind(c(i,y1),c(i,y2)),c(l1,l2))
    hcEdges<<-rbind(hcEdges,tp)
    NULL
  })
  colnames(hcEdges)<-c("parentNode","childNode","length")
  hcEdges<-data.frame(hcEdges,stringsAsFactors=FALSE)
  hcEdges$parentHeight<-obj$height[hcEdges$parentNode]
  #---get unified nodes
  hcl<-data.frame(node=hc$labels,mergeId=-c(1:nn),hcId=c(1:nn), type="leaf",
                  stringsAsFactors=FALSE)
  hcn<-data.frame(node=paste("N",c(1:N),sep=""),mergeId=c(1:N),hcId=c(1:N),
                  type="nest",stringsAsFactors=FALSE)
  hcNodes<-rbind(hcl,hcn)
  hcEdges$parentNode<-hcNodes$node[match(hcEdges$parentNode,hcNodes$mergeId)]
  hcEdges$childNode<-hcNodes$node[match(hcEdges$childNode,hcNodes$mergeId)]
  #---update and return
  obj$hcNodes<-hcNodes;obj$hcEdges<-hcEdges
  return(obj)
}

#-------------------------------------------------------------------
#-------------------------------------------------------------------
# simplified remote calls for RedeR, designed for internal handler
#-------------------------------------------------------------------
#-------------------------------------------------------------------

#-------------------------------------------------------------------
.rederpost<-function(obj, method, ..., gdata=list(...)){
  aXML<-function(method, x){
    method <- paste(c("<methodName>",method,"</methodName>"), 
                    collapse = "",sep="")
    x <- lapply(x,function(arg){
      paste(c("<string><![CDATA[",arg,"]]></string>"), collapse="", 
            sep="")
    })
    x <- lapply(x,function(arg){
      paste(c("<value>",arg,"</value>"), collapse="", sep="")
    })
    x <- lapply(x,function(arg){
      paste(c("<param>",arg,"</param>"), collapse="", sep="")
    })
    x <- paste(unlist(x),collapse="", sep="")
    x <- paste("<params>",x,"</params>",collapse="", sep="")
    doc <- paste(c("<methodCall>",method,x,"</methodCall>"), 
                 collapse="", sep="")
    return(doc)
  }
  rdcall <- .simplePost(host=obj@host, port=obj@port,
                        datatosend=aXML(method,gdata) )
  rdcall <- .postParser(rdcall)
  return(rdcall)
}

#-------------------------------------------------------------------
# express post: direct calls to RedeR 
# improve loading performance for large/sequential objects,
# designed for internal handler
.rederexpresspost<-function(obj, method, ..., gdata=list(...)){	
  bXML<-function(method, x){
    getserial<-function(x){
      if(is.character(x)){
        x<-gsub("&","&amp;",x)
        x<-gsub("<","&lt;",x)
        x<-gsub(">","&gt;",x)
      }
      type<-c("integer"="double", "double"="double", 
              "character"="string")[typeof(x)]
      head<-paste("<value><",type,">",sep="",collapse="")
      tail<-paste("</",type,"></value>",sep="",collapse="")
      doc<-paste(head,x,tail,sep="",collapse="")
      if(length(x)==1){
        paste("<param>",doc,"</param>",sep="", collapse="")					
      } else {
        paste("<param><value><array><data>",doc,
              "</data></array></value></param>",sep="", collapse="")				
      }
    }
    doc<-sapply(x, function(x) getserial(x) )
    doc<-paste(doc,sep="", collapse="")
    doc<-paste("<params>",doc,"</params>",sep="", collapse="")	
    mt<-paste("<methodName>",method,"</methodName>", sep="", collapse="")
    doc<-paste("<methodCall>",mt,doc,"</methodCall>",sep="", collapse="")
    return(doc)	
  }
  rdcall <- .simplePost(host=obj@host, port=obj@port, 
                        datatosend=bXML(method,gdata))
  rdcall <- .postParser(rdcall)
  return(rdcall)	
}

#-------------------------------------------------------------------
.simplePost<-function (host, port, datatosend, contenttype="text/xml"){
  lengthdatatosend <- length(charToRaw(datatosend))
  header <- character(0)
  header <- c(header, "POST / HTTP/1.1\n")
  header <- c(header, paste("Content-Type: ", contenttype, "\n", 
                            sep = ""))
  header <- c(header, paste("Content-Length: ", 
                            lengthdatatosend, "\n", sep = ""))
  header <- c(header, "Connection: Keep-Alive\n\n")
  header <- paste(c(header, datatosend, "\n"), collapse = "")
  fp <- make.socket(host=host, port=port, server=FALSE)
  write.socket(fp, header)
  output <- character(0)
  repeat{
    ss <- read.socket(fp, maxlen=65536L, loop=FALSE)
    if (ss == "") break
    output <- paste(output, ss)
  }
  close.socket(fp)
  output <- strsplit(output, "<\\?xml.*?\\?>", perl = T)[[1]][2]
  output <- gsub("<methodResponse.*?>", "<methodResponse>", output) 
  output <- gsub("\\s", "", output)
  return(output)
}

#-------------------------------------------------------------------
.postParser <- function(rdcall){
  type <- c("i4|int","double","array")
  type <- sapply(type,function(tp){grepl(tp, x=rdcall)})
  if(type[3]){
    rdcall <- unlist(strsplit(rdcall, "<.?data>", perl=TRUE))[2]
  } else {
    rdcall <- unlist(strsplit(rdcall, "<.?param>", perl=TRUE))[2]
  }
  rdcall <- gsub(pattern = "<value>", "", rdcall)
  rdcall <- unlist(strsplit(rdcall, "</value>"))
  rdcall <- gsub("<.*?>", "", rdcall)
  if(type[1])rdcall <- as.integer(rdcall)
  if(type[2])rdcall <- as.numeric(rdcall)
  return(rdcall)
}

#-------------------------------------------------------------------
is.singleNumber <- function(para){
  (is.integer(para) || is.numeric(para)) && length(para) == 1L && !is.na(para)
}
is.singleInteger <- function(para){
  lg <- (is.integer(para) || is.numeric(para)) && length(para) == 1L && !is.na(para)
  if (lg) lg <- ( (para+1) / (ceiling(para)+1) ) == 1
  return(lg)
}
is.singleString <- function(para){
  is.character(para) && length(para) == 1L && !is.na(para)
}
is.singleLogical <- function(para){
  is.logical(para) && length(para) == 1L && !is.na(para)
}
all.binaryValues <- function(para){
  all(para %in% c(0, 1, NA))
}
all.integerValues <- function(para){
  lg <- (all(is.integer(para)) || all(is.numeric(para))) && !any(is.na(para))
  if (lg) lg <- all(( (para+1) / (ceiling(para)+1) ) == 1)
  return(lg)
}
all.characterValues <- function(para){
  all(is.character(para)) && !any(is.na(para))
}
is.color <- function(x){
  res <- try(col2rgb(x),silent=TRUE)
  return(!"try-error"%in%class(res))
}

