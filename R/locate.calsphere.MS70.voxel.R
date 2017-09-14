#*********************************************
#*********************************************
#' Locates the voxel containing the calibration sphere of calibration data from the MS70 sonar. The method used is a geometrical clustering of the 'K' voxels of highest intensity, folowed by a ranging of the clusters based on the sum of the intensities of each cluster. If 'oldpos' is missing the calibration sphere is assigned the voxel position of the largest cluster. If 'oldpos' is present, the location needs to be in the proximity of the old position, given by 'step'. Also the size of the chosen cluster needs to be close to the old size. If these conditions are not met, NULL is returned for the 'voxel'.
#'
#' @param data  is a list of data as returned from read.event(...,var=c("vbsc","voxels","beams")).
#' @param ind  is a list of indexes subsetting the data using subset_TSD() (see subset_TSD() for details).
#' @param K  is the number of the largest voxels to include in the clustering forming the basis of the funstion (see DESCRIPTION above).
#' @param oldpos  is a vector of three elements (radial, horizontal, vertical) representing the old voxel position of the calibration sphere.
#' @param oldsize  is a the sum of the voxels in old cluster.
#' @param step  is a vector of three elements (radial, horizontal, vertical) representing the proximity around the old position in which the new position is accepted.
#' @param margin  is a matrix of three elements (radial, horizontal, vertical) representing the distances separating clusters.
#' @param sizemargin  is a vector of two factors representing the interval on each size of 'oldsize' in which the new size must be contained. If sizemargin=[0.5,1.5] and 'size' of the relevant cluster is 10, the interval will be [5,15]. 
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR subset_TSD
#' @importFrom TSD arr.ind2ind
#'
#' @export
#' @rdname locate.calsphere.MS70.voxel
#'
locate.calsphere.MS70.voxel=function(data,ind=list(-(1:30),NULL,NULL),K=100,oldpos=NULL,oldsize=NULL,step=c(2,2,2),margin=c(3,2,2),sizemargin=c(0.5,1.5)){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-04-14 - Clean version.
	########### DESCRIPTION: ###########
	# Locates the voxel containing the calibration sphere of calibration data from the MS70 sonar. The method used is a geometrical clustering of the 'K' voxels of highest intensity, folowed by a ranging of the clusters based on the sum of the intensities of each cluster. If 'oldpos' is missing the calibration sphere is assigned the voxel position of the largest cluster. If 'oldpos' is present, the location needs to be in the proximity of the old position, given by 'step'. Also the size of the chosen cluster needs to be close to the old size. If these conditions are not met, NULL is returned for the 'voxel'.
	########## DEPENDENCIES: ###########
	# subset_TSD()
	############ VARIABLES: ############
	# ---data--- is a list of data as returned from read.event(...,var=c("vbsc","voxels","beams")).
	# ---ind--- is a list of indexes subsetting the data using subset_TSD() (see subset_TSD() for details).
	# ---K--- is the number of the largest voxels to include in the clustering forming the basis of the funstion (see DESCRIPTION above).
	# ---oldpos--- is a vector of three elements (radial, horizontal, vertical) representing the old voxel position of the calibration sphere.
	# ---oldsize--- is a the sum of the voxels in old cluster.
	# ---step--- is a vector of three elements (radial, horizontal, vertical) representing the proximity around the old position in which the new position is accepted.
	# ---margin--- is a matrix of three elements (radial, horizontal, vertical) representing the distances separating clusters.
	# ---sizemargin--- is a vector of two factors representing the interval on each size of 'oldsize' in which the new size must be contained. If sizemargin=[0.5,1.5] and 'size' of the relevant cluster is 10, the interval will be [5,15]. 
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# 'sizemargin' needs to be of length > 1:
	if(length(sizemargin)<2){
		stop("'sizemargin' must be of length > 1")
		}
	sizemargin=range(sizemargin)
	# Check the dimension of the volume backscatter data:
	if(length(dim(data$vbsc))==3 && dim(data$vbsc)[3]==1){
		data$vbsc=drop(data$vbsc)
		}
	else if(length(dim(data$vbsc))!=2 && dim(data$vbsc)[3]!=1){
		stop("Only one ping must be given in data$vbsc (three dimensional array found)")
		}
	# The dimension of the sonar array (number of beams of the same frequency x number of frequencies):
	num3=length(unique(data$freq))
	num2=length(data$freq)/num3
	num1=nrow(data$vbsc)
	dim(data$vbsc)=c(num1,num2,num3)
	# Subset the data:
	data=subset_TSD(data,ind=ind,insert.NA=TRUE)
	if(length(oldpos)>0){
		thisind=oldpos[1]+seq(-5,5)
		thisind=thisind[thisind>0]
		data=subset_TSD(data,ind=list(thisind,NULL),insert.NA=TRUE)
		}
	
	
	########## Execution ##########
	# 'cl' is a list of the indexes of the 'K' voxels of highest intensity that are put in the different clusters:
	cl=list(1)
	# Sort to get the 'K' voxels of highest intensity, and the reversed order of the voxels:
	sorted_vbsc=sort(data$vbsc,decreasing=TRUE)
	order_vbsc=order(data$vbsc,decreasing=TRUE)
	# Identify the voxel of highest intensity:
	maxat=which(data$vbsc==sorted_vbsc[1],arr.ind=TRUE)[1,]
	# 'centers' is an array with rows representing the center voxels of the clusters:
	centers=array(maxat,dim=c(1,3))
	
	# Move through the K-1 remaining voxels of high intensity and assign voxels to new clusters if they are outside of the proximity defined by 'margin', of any of the previous center voxels (using 'inany'):
	for(i in 2:K){
		maxat=which(data$vbsc==sorted_vbsc[i],arr.ind=TRUE)[1,]
		inany=logical(nrow(centers))
		for(j in seq_along(inany)){
			inany[j] = (maxat[1] %in% (centers[j,1]+seq(-margin[1],margin[1])))  &  (maxat[2] %in% (centers[j,2]+seq(-margin[2],margin[2])))  &  (maxat[3] %in% (centers[j,3]+seq(-margin[3],margin[3])))
			}
		if(!any(inany)){
			centers=rbind(centers,maxat)
			cl=c(cl,list(i))
			}
		else{
			cl[min(which(inany))]=list(c(unlist(cl[min(which(inany))]),i))
			}
		}
	# Get the sum of the voxels for each cluster:
	size=sapply(cl,function(x) sum(data$vbsc[order_vbsc[x]], na.rm=TRUE))
	
	# Order 'cl', 'centers' and 'size' according to size:
	osize=order(size,decreasing=TRUE)
	size=size[osize]
	centers=centers[osize,,drop=FALSE]
	cl=cl[osize]
	
	# Get the centers of mass of each cluster:
	cm.TSD=function(x){
		colSums(cbind(data$psxx[order_vbsc[x]],data$psyx[order_vbsc[x]],data$pszx[order_vbsc[x]]) * data$vbsc[order_vbsc[x]]*data$volx[order_vbsc[x]]) / sum(data$vbsc[order_vbsc[x]] * data$volx[order_vbsc[x]], na.rm=TRUE)
		}
	cm=t(sapply(cl,cm.TSD))
	# Update 'centers' to be at the voxel closest to the mass center of the cluster:
	newcenters=centers
	if(!all(is.na(rowSums(cm)))){
		for(i in seq_len(nrow(centers))){
			neigh=as.matrix(expand.grid( centers[i,1]+seq(-margin[1],margin[1]), centers[i,2]+seq(-margin[2],margin[2]), centers[i,3]+seq(-margin[3],margin[3]) ))
			neighind=arr.ind2ind(neigh,c(num1,num2,num3))
			dist=sqrt( (cm[i,1]-data$psxx[neighind])^2 + (cm[i,2]-data$psyx[neighind])^2 + (cm[i,3]-data$pszx[neighind])^2 )
			newcenters[i,]=neigh[which.min(dist),]
			}
		}
	
	# Cross check with the old position and the old size:
	if(length(size)>1){
		# Check whether 'size' is close to the previous size:
		if(length(oldsize)>0){
			valid=logical(length(size))
			while(sum(valid)==0){
				sizemargin=sizemargin*sizemargin
				sizeinterval=oldsize*sizemargin
				valid=sizeinterval[1]<size & size<sizeinterval[2]
				}
			
			}
		# If 'oldsize' is not given, restrict to being larger than max(size)*min(oldsize):
		else{
			valid=size>max(size)*min(sizemargin)
			}
		}
	else{
		valid=TRUE
		}
	
	# If only one cluster is in the prefered size interval, this is assumed to be the calibration sphere:
	if(sum(valid)==1){
		at=which(valid)
		}
	# Else select the cluster that is closest to the old position:
	else if(sum(valid)>1){
		# Check for consistency to old detection:
		if(length(oldpos)>0){
			at=which(valid)
			dist=logical(length(at))
			for(i in at){
				dist[i] = sqrt( (newcenters[i,1]-oldpos[1])^2 + (newcenters[i,2]-oldpos[2])^2 + (newcenters[i,3]-oldpos[3])^2 )
				}
			at=at[which.min(dist)]
			}
		# Else if 'oldpos' is not present, select the first of the clusters that are in the prefered size interval:
		else{
			at=which(valid)[1]
			}
		}
	# Extract the voxel:
	voxel=newcenters[at,]
	
	
	########## Output ##########
	# Return a list of the relevant information: The voxel located to hold the calibration sphere by the method of choosing between ckusters of voxels of high values based on proximity to the previously located voxel in space and intensity is returned as 'voxel'. The center (highest) voxels of all clusters of voxels are returned as 'centers', where one of these centers is 'voxel'. The list of indexes corresponding to the clusters with center voxels given in 'centers'. The numbering index of the chosen cluster in the list 'cl' of clusters. The sum of the voxels of each cluster (ignoring the volume of the voxels) is returned as 'size'. The center of mass of all the clusters is stored in 'cm'.
	list(voxel=voxel,centers=newcenters,cl=cl,at=at,size=size,cm=cm)
	##################################################
	##################################################
	}
