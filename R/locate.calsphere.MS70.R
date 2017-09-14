#*********************************************
#*********************************************
#' Uses locate.calsphere.MS70.voxel() to locate the voxel containing the calibration sphere of MS70 calibration data, and the split beam angle information "angl" and "angt" to furhter pinpoint the position.
#'
#' @param event  is a string naming the calibration event.
#' @param t  is either a numeric vector of time step indexes, or "all" which implies treating all time steps.
#' @param ind  is a list of indexes subsetting the data using subset_TSD() (see subset_TSD() for details).
#' @param K  is the number of the largest voxels to include in the clustering forming the basis of the funstion (see DESCRIPTION above).
#' @param step  is a vector of three elements (radial, horizontal, vertical) representing the proximity around the old position in which the new position is accepted.
#' @param margin  is a matrix of three elements (radial, horizontal, vertical) representing the distances separating clusters.
#' @param firstpos  is a vector of three elements (radial, horizontal, vertical) representing the first voxel position of the calibration sphere.
#' @param sizemargin  is a vector of two factors representing the interval on each size of 'oldsize' in which the new size must be contained. If sizemargin=[0.5,1.5] and 'size' of the relevant cluster is 10, the interval will be [5,15]. 
#' @param write  TRUE if the data should be written to the event directory in a file named "eventname_Calibration_sphere_location.tsd", where 'eventname' is the name of the event, extracted from the string 'event'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR read.event
#' @importFrom TSD arr.ind2ind car2sph NAs sph2car write.TSD
#'
#' @export
#' @rdname locate.calsphere.MS70
#'
locate.calsphere.MS70<-function(event,t,ind=list(-(1:30),NULL,NULL),K=50,step=c(2,2,2),margin=c(3,2,2),firstpos=NULL,sizemargin=c(0.1,10),write=FALSE,filename="_Calibration_sphere_location.tsd"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-04-14 - Clean version.
	########### DESCRIPTION: ###########
	# Uses locate.calsphere.MS70.voxel() to locate the voxel containing the calibration sphere of MS70 calibration data, and the split beam angle information "angl" and "angt" to furhter pinpoint the position.
	########## DEPENDENCIES: ###########
	# NAs(), read.event(), locate.calsphere.MS70(), arr.ind2ind(), car2sph(), sph2car(), rotate(), write.TSD()
	############ VARIABLES: ############
	# ---event--- is a string naming the calibration event.
	# ---t--- is either a numeric vector of time step indexes, or "all" which implies treating all time steps.
	# ---ind--- is a list of indexes subsetting the data using subset_TSD() (see subset_TSD() for details).
	# ---K--- is the number of the largest voxels to include in the clustering forming the basis of the funstion (see DESCRIPTION above).
	# ---step--- is a vector of three elements (radial, horizontal, vertical) representing the proximity around the old position in which the new position is accepted.
	# ---margin--- is a matrix of three elements (radial, horizontal, vertical) representing the distances separating clusters.
	# ---firstpos--- is a vector of three elements (radial, horizontal, vertical) representing the first voxel position of the calibration sphere.
	# ---sizemargin--- is a vector of two factors representing the interval on each size of 'oldsize' in which the new size must be contained. If sizemargin=[0.5,1.5] and 'size' of the relevant cluster is 10, the interval will be [5,15]. 
	# ---write--- TRUE if the data should be written to the event directory in a file named "eventname_Calibration_sphere_location.tsd", where 'eventname' is the name of the event, extracted from the string 'event'.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# Define the number of neighbours of each voxel:
	neighbours=matrix( c(3,rep(5,23),3, rep(c(5,rep(8,23),5),18), 3,rep(5,23),3), nrow=25)
	# Set 't' to seq_len(numt) if t=="all":
	numt=read.event(event=event,var="numt")$numt
	if(identical(t,"all")){
		t=seq_len(numt)
		}
	else{
		t=t[t<=numt]
		}
	
	
	########## Execution ##########
	# Define the variables to output:
	oldpos=firstpos
	oldsize=NULL
	oldsizes=NAs(10)
	weights=seq(1,0.1,-0.1)
	posV=NAs(3,length(t))
	posG=NAs(3,length(t))
	sizes=NAs(length(t))
	cmV=NAs(3,length(t))
	cmG=NAs(3,length(t))
	voxels=NAs(3,length(t))
	nneigh=NAs(length(t))
	ratio=NAs(length(t))
	centerbeam=NAs(length(t))
	allbeams=NAs(length(t))
	angl=NAs(length(t))
	angt=NAs(length(t))
	# Store the sum of the voxels close to the center voxel for each beam:
	bvbs=NAs(500,length(t))
	
	# Move through the time steps and locate the calibration sphere:
	for(i in seq_along(t)){
		cat(t[i],"\t")
		# Read data:
		d=read.event(event=event,var=c("vbsc","voxels","beams","angl","angt","vessel"),t=t[i])
		# Locate the voxel:
		dd=locate.calsphere.MS70.voxel(d,K=K,ind=ind,step=step,oldpos=oldpos,oldsize=oldsize,margin=margin,sizemargin=sizemargin)
		# Pinpoint the calibration sphere if the location was successful:
		if(length(dd$voxel)==3){
			# Store the voxel of the located calibration sphere, if valid according to the maximum displacement rule used in locate.calsphere.MS70.voxel():
			voxels[,i]=dd$voxel
			# Store the number of neighbouring voxels:
			nneigh[i]=neighbours[dd$voxel[2],dd$voxel[3]]
			# Collapse the time intervals around the located voxel to get the angular spread:
			radind=intersect(dd$voxel[1]+seq(-3,3),seq_len(nrow(d$vbsc)))
			# Store all the beams:
			bvbs[,i]=colSums(d$vbsc[radind,]*d$volx[radind,])
			angular=array(bvbs[,i],dim=c(25,20))
			# Define the neighbourhood for calculating the spread around the max voxel, and find the ratio of the max voxel and its neighbourhood sum:
			neighbourhood=arr.ind2ind(as.matrix(expand.grid(dd$voxel[2]+seq(-2,2),dd$voxel[3]+seq(-2,2))),c(25,20))
			centerbeam[i]=angular[dd$voxel[2],dd$voxel[3]]
			allbeams[i]=sum(angular[neighbourhood],na.rm=TRUE)
			ratio[i]=centerbeam[i] / allbeams[i]
			
			# Add the angles 'angl' and 'angt':
			if(!any(is.null(d$angl),is.null(d$angt))){
				index=arr.ind2ind(as.matrix(expand.grid(dd$voxel[2],dd$voxel[3])),c(25,20))
				angl[i]=d$angl[dd$voxel[1],index]
				angt[i]=d$angt[dd$voxel[1],index]			
			
				# Get the radial position from the centers of mass (not needed in the calculations but needs a value):
				r=car2sph(dd$cm[dd$at,]-c(d$psxv,d$psyv,d$pszv+d$psze))[1]
				# Get the x- and y-position in the beam in which the calibration sphere is located. The angles 'angl' and 'angt' are relative to the z-axis of the beam coordinate system in the negative y-direction and the x-direction respectively:
				x=sin(angt[i]*pi/180)
				y=-sin(angl[i]*pi/180)
				# Transform to spherical coordinates:
				thetaphi=car2sph(x,y,sqrt(1-x^2-y^2))[2:3]
				# And back to cartesian, utilizing the range 'r':
				posB=sph2car(c(r,thetaphi))
				# Rotate to the coordinate system of the vessel:
				posV[,i]=rotate(posB,"xz",c(d$dire[index],pi/2-d$dira[index]))
				# Roll and pitch compensation only involves that the heading of the vessel is relevant:
				thisposG=c(d$psxv,d$psyv,d$pszv+d$psze)+rotate(posV[,i],"z",-d$rtzv)
				posG[,i]=thisposG
				}
			# Store the current voxel, to be used in the next location:
			oldpos=dd$centers[dd$at,]
			# Store the size of the selected cluster:
			oldsizes=c(dd$size[dd$at],oldsizes[1:9])
			notna=!is.na(oldsizes)
			oldsize=sum(oldsizes[notna]*weights[notna])/sum(weights[notna])
			# Store the sizes:
			sizes[i]=dd$size[dd$at]
			# Add the center of mass of the located sphere in the global coordinate system:
			cmG[,i]=dd$cm[dd$at,]
			# Subtract the vessel position and rotate back to the vessel coordinate system:
			cmV[,i]=rotate(dd$cm[dd$at,]-c(d$psxv,d$psyv,d$pszv+d$psze),"z",d$rtzv)
			}
		# print the size and the voxel located
		cat("oldsize:",round(oldsize,digits=5),"\t")
		cat("logsize:",round(10*log10(sizes[i]),digits=2),"\t")
		cat("voxel:",dd$voxel,"\n")
		}
	
	if(write){
		# Get the name of the event:
		eventname=rev(strsplit(event,"/",fixed=TRUE)[[1]])
		if(tolower(eventname[1])!="tsd"){
			warning("The path to the event specified by 'event' does not end with \"tsd\". Naming of the file written may be unexpected")
			}
		eventname=eventname[3]
		# Write the position in the coordinate system (V) of the vessel (posv), the position in the global (G) coordinate system (posg), the center of mass of the clusters in (V) (cmsv), the center of mass of the clusters in (G) (cmsg), the total backscatter of the calibration sphere as represented by those of the K largest voxels that are assigned to the cluster identified to be the echo of the calibration sphere (bscs), the voxel identified to contain the calibration sphere by the center of mass (cloc), the number of neighbouring voxels in the angular direction (nngh) and the ratio of the center voxel given by 'cloc' to the total backscatter in the neighbourhood (including the center voxel) given by seq(-3:3) in the radial direction and seq(-2,2) in both angular directions, that is 7*5*5 = 175 voxels (c2nh):
		#datatowrite=list(posv=posV,posg=posG,cmsv=cmV,cmsg=cmG,bscs=sizes,cloc=array(as.double(voxels),dim=dim(voxels)),nngh=nneigh,ctrb=centerbeam,allb=allbeams,c2nh=ratio,angl=angl,angt=angt,bvbs=bvbs)
		datatowrite=list(pVxC=posV[1,],pVyC=posV[2,],pVzC=posV[3,],pGxC=posG[1,],pGyC=posG[2,],pGzC=posG[3,],cVxC=cmV[1,],cVyC=cmV[2,],cVzC=cmV[3,],cGxC=cmG[1,],cGyC=cmG[2,],cGzC=cmG[3,],bscs=sizes,cloc=array(as.double(voxels),dim=dim(voxels)),nngh=nneigh,ctrb=centerbeam,allb=allbeams,c2nh=ratio,angl=angl,angt=angt,bvbs=bvbs)
		write.TSD(x=datatowrite,con=file.path(event,paste(eventname,filename,sep="")),ts=1:2)
		}	
	
	
	########## Output ##########
	# Return a list of the relevant information:
	list(posG=posG,posV=posV,cmG=cmG,cmV=cmV,sizes=sizes,voxels=voxels,nneigh=nneigh,centerbeam=centerbeam,allbeams=allbeams,ratio=ratio,angl=angl,angt=angt,bvbs=bvbs)
	##################################################
	##################################################
	}
