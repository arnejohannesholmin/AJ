#*********************************************
#*********************************************
#' Extracts a subset of TSD data according to the array subset 'ind' and/or the cartesian subset 'range' and/or the logical/numeric vector of subscripts 'subset'. 
#'
#' @param data  is a list of elements named according to the TSD file format.
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param range  is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep (as used in subset()). Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param strict  is TRUE if strict inequality is to be used when subsetting according to 'range'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom cpplot3d pplot3d_sv2pos.TSD
#' @importFrom sonR read.event
#' @importFrom TSD zeros
#'
#' @export
#' @rdname vbsc2informationProfile
#'
vbsc2informationProfile<-function(event=1, t=1, N=1e5, acca=NULL, var=c("vbsc","sgsc","pr0s","sgs0"), ind=list(-(1:100),NULL), range=list(), subset=NULL, cruise=2009116, origin=c(0,0,0), angle=0, w=10, dens=10, breaks=seq(-100,100,dens), ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-23 - Clean version.
	########### DESCRIPTION: ###########
	# Extracts a subset of TSD data according to the array subset 'ind' and/or the cartesian subset 'range' and/or the logical/numeric vector of subscripts 'subset'. 
	########## DEPENDENCIES: ###########
	# extract()
	############ VARIABLES: ############
	# ---data--- is a list of elements named according to the TSD file format.
	# ---ind--- is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
	# ---range--- is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
	# ---subset--- is a numeric or logical vector/expression indicating elements or rows to keep (as used in subset()). Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
	# ---strict--- is TRUE if strict inequality is to be used when subsetting according to 'range'.
		
	
	##################################################
	##################################################
	##### Preparation #####
	variables=c("psxv","psyv","pszv","rtzv","psze","volx","dira","dire","lenb","esnm","numb","eqba","sint","voxels","ctd",var[1])
	t=read.event(event=event,cruise=cruise,t=t,var="indt")$indt
	numt=length(t)
	
		
	##### Execution #####
	# Define the output matrix:
	h=zeros(length(breaks)-1,numt)
	
	cat("Processing ping nr:\n")
	for(i in seq_len(numt)){
		cat(t[i]," of ",numt,"\n",sep="")
		# Consider a strip of the surface projected data, positioned so that the information wave is intersects with the strip, and divide the stip into rectangular subsections, in which the total backscattered energy is calculated:
		data=read.event(event=event,cruise=cruise,t=t[i],var=variables,...)
		p=pplot3d_sv2pos.TSD(data,N=N,acca=acca,plot=FALSE,ind=ind,range=range,subset=subset,nlim=Inf)$p1
		# Translate to the origin:
		p=p-matrix(origin,ncol=ncol(p),nrow=nrow(p),byrow=TRUE)
		# Rotate by the angle 'angle':
		p=rotate(p,by="z",ang=angle)
		# Only consider the values inside the strip:
		p=p[-w<p[,2] & p[,2]<w,]
		
		# Define the number of values in the breaks:
		int=findInterval(p[,1],breaks)
		int=by(int,int,length)
		int=int[!as.numeric(names(int)) %in% c(0,length(breaks))]
		# Insert the counts:
		h[as.numeric(names(int)),i]=as.numeric(int)	
		}
			
	
	##### Output #####
	h
	##################################################
	##################################################
	}
