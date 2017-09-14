#*********************************************
#*********************************************
#' Applies a Gaussian 3-D kernel on MS70 sonar data given as a list 'data' with names according to the TSD-convension, and the bandwidths given in the three element vector 'h':
#'
#' @param data  is the list of data with names according to the TSD-convension.
#' @param h  is the three element vector of bandwidths.
#' @param w  is the boundary of the kernel, outside which the it is 0.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD ind.expand
#'
#' @export
#' @rdname kernSmooth3dGaussMultipleNoNA.TSD
#'
kernSmooth3dGaussMultipleNoNA.TSD=function(data,h=c(1,1,1),w=h*3,ind=list()){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-01-24 - Clean version.
	# Last: 2012-06-21 - Added the function ind.expand() to treat the indices given in 'ind'.
	########### DESCRIPTION: ###########
	# Applies a Gaussian 3-D kernel on MS70 sonar data given as a list 'data' with names according to the TSD-convension, and the bandwidths given in the three element vector 'h':
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---data--- is the list of data with names according to the TSD-convension.
	# ---h--- is the three element vector of bandwidths.
	# ---w--- is the boundary of the kernel, outside which the it is 0.
		

	##################################################
	##################################################
	##### Preparation, execution #####
	if(length(h)<3){
		h=rep(h,length.out=3)
		}
	
	pr0s2vbsc=0
	if(length(data$pr0s)>0 && length(data$vbsc)>0){
		warning("not-school-probability 'pr0s' used in the function")
		names(data)[names(data) %in% c("vbsc","pr0s")]=rev(names(data)[names(data) %in% c("vbsc","pr0s")])
		pr0s2vbsc=2
		}
	else if(length(data$pr0s)>0){
		names(data)[names(data)=="pr0s"]="vbsc"
		pr0s2vbsc=1
		}
	else if(length(data$vbsc)==0){
		stop("Acoustic data 'vbsc' or not-school-probability data 'pr0s' must be present in the data")
		}
	if(any(length(data$lenb)==0,length(data$numb)==0,length(data$freq)==0)){
		stop("Beam configuration data 'lenb', 'numb' and 'freq' must be present in the data")
		}
	
	# Get the indices for NA in the acoustic data:
	nas=which(rowSums(is.na(data$vbsc))>0)
		
	# Get the full dimensions of the acoustic data (length of beams, number of beams horizontally, number of beams vertically, number of time steps):
	olddim=c(max(data$lenb),data$numb/length(unique(data$freq)),length(unique(data$freq)),length(data$vbsc)/(max(data$lenb)*data$numb))
	
	# Extract the indices input to [], and discard indices at NAs:
	ind=ind.expand(ind,olddim)
	ind[[1]]=setdiff(ind[[1]],nas)
	
	# Expand the dimensions to 3-D:
	data$vbsc[is.na(data$vbsc)] = NaN # ???
	dim(data$psxx)=olddim[1:3]
	dim(data$psyx)=olddim[1:3]
	dim(data$pszx)=olddim[1:3]
	dim(data$vbsc)=olddim
	
	# Execution:
	U <- .C("kernSmooth3dGaussMultipleNoNA", as.double(data$psxx[ind[[1]],ind[[2]],ind[[3]],drop=FALSE]), as.double(data$psyx[ind[[1]],ind[[2]],ind[[3]],drop=FALSE]), as.double(data$pszx[ind[[1]],ind[[2]],ind[[3]],drop=FALSE]), as.double(data$vbsc[ind[[1]],ind[[2]],ind[[3]],ind[[4]],drop=FALSE]), as.integer(length(ind[[1]])), as.integer(length(ind[[2]])), as.integer(length(ind[[3]])), as.integer(length(ind[[4]])), as.double(h[1]), as.double(h[2]), as.double(h[3]), as.double(w[1]), as.double(w[2]), as.double(w[3]), as.double(data$vbsc[ind[[1]],ind[[2]],ind[[3]],ind[[4]],drop=FALSE]), PACKAGE="sonR")
	data$vbsc[ind[[1]],ind[[2]],ind[[3]],ind[[4]]] = U[[15]]
	# Insert NAs:
	data$vbsc[nas,,,] = NA # ???
	
	# Reduce the dimensions to the original:
	olddim=c(olddim[1],prod(olddim[2:3]),olddim[4])
	dim(data$psxx)=olddim[1:2]
	dim(data$psyx)=olddim[1:2]
	dim(data$pszx)=olddim[1:2]
	dim(data$vbsc)=olddim
	
		
	##### Output #####
	if(pr0s2vbsc==2){
		names(data)[names(data) %in% c("vbsc","pr0s")]=rev(names(data)[names(data) %in% c("vbsc","pr0s")])
		}
	else if(pr0s2vbsc==1){
		names(data)[names(data)=="vbsc"]="pr0s"
		}
	# Output a list of the three vectors:
	data
	##################################################
	##################################################
	}
