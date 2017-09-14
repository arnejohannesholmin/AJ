#*********************************************
#*********************************************
#' Reads an LSSS work file.
#'
#' @param f  is the file to read.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD arr.ind2ind FILETIME2utim
#'
#' @export
#' @rdname echoIBM.read.workLSSS2
#'
echoIBM.read.workLSSS2<-function(f,lenb=1324,numb=500,flatten=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-07-07 - Clean version.
	# Last: 2013-01-07 - Changed names to comply with the standardization of names.
	# Last: 2013-01-11 - Restructured and simplified.
	########### DESCRIPTION: ###########
	# Reads an LSSS work file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---f--- is the file to read.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	##### Define functions used in when reading the files: #####
	# Function for removing leading whitspace:
	removeLeadingWhitespace=function(x){
		gsub("^[[:space:]]+|[[:space:]]+$","",x)
		}
	# Function for extracting a variable following a 'string' in the lines, where the variable length or the end string is is 'w':
	getvar=function(string,w,l){
		at=grep(string,l)
		out=gregexpr(string,l[at])
		startx=unlist(out)
		lengthx=sapply(out,function(x) attributes(x)$match.length)
		if(is.character(w)){
			w=gregexpr(w,l[at])
			for(i in seq_along(w)){
				w[[i]]=min(w[[i]][w[[i]] > (startx[i]+lengthx[i])])-1
				}
			w=unlist(w)
			}
		else{
			w=startx+lengthx+w-1
			}
		substr(l[at], startx+lengthx, w)
		}
	# Function for extracting x, y, and z:
	readxyz=function(x){
		get_xyz=function(y){
			atx=gregexpr("x=\"",y)[[1]]
			aty=gregexpr("\" y=\"",y)[[1]]
			atz=gregexpr("\" z=\"",y)[[1]]
			atend=gregexpr("\"/>",y)[[1]]
			as.numeric(c(substr(y,atx+3,aty-1), substr(y,aty+5,atz-1), substr(y,atz+5,atend-1)))
			}
		matrix(unlist(lapply(x,get_xyz)),byrow=TRUE,nrow=length(x),ncol=3)
		}
	# Function for extracting the segmentation data:
	readbeam_channel=function(x){
		get_channel=function(y){
			atbeamLength=gregexpr("\" beamLength=\"",y)[[1]]
			as.numeric(substr(y,1,atbeamLength-1))
			}
		get_beamLength=function(y){
			atbeamLength=gregexpr("\" beamLength=\"",y)[[1]]
			atrange=gregexpr("\" range=\"",y)[[1]]
			as.numeric(substr(y,atbeamLength+14,atrange-1))
			}
		get_range=function(y){
			atrange=gregexpr("\" range=\"",y)[[1]]
			atend=gregexpr("\"/>",y)[[1]]
			temp=as.numeric(strsplit(substr(y,atrange+9,atend-1)," ",fixed=TRUE)[[1]])
			out=NULL
			for(i in seq_len(length(temp)/2)){
				start=sum(temp[seq_len(1+(i-1)*2)])
				end=start+temp[2*i]-1
				out=c(out,start:end)
				}
			out
			}
		list(channel=lapply(x,get_channel),beamLength=lapply(x,get_beamLength),voxels=lapply(x,get_range))
		}
	##### End of functions #####
	
	
	# Read the work file and remove leading whitespace in every line:
	l=readLines(f)
	l=removeLeadingWhitespace(l)
	
	# Get the number of schools:
	at_schoolData=cbind(grep("<schoolData",l),grep("</schoolData>",l))
	nschools=nrow(at_schoolData)
	
	
	
	# Discard the schools and consider pings:
	if(flatten){
		mSampleIndex=NULL
		mChannelNo=NULL
		mMinSv=NULL
		mMaxSv=NULL
		mMinDepth=NULL
		mMaxDepth=NULL
		mMinFanNumber=NULL
		mMaxFanNumber=NULL
		CorrectedBoxCenter=NULL
		CorrectedBoxExtent=NULL
		CorrectedBoxRotation=NULL
		UncorrectedBoxCenter=NULL
		UncorrectedBoxExtent=NULL
		UncorrectedBoxRotation=NULL
		GridVolumeCorrected=NULL
		GridVolume=NULL
		GridSurface=NULL
		AvgLogSv=NULL
		
		schoolMask=list()
		utim=NULL
				
		# Run through the schools:
		for(s in seq_len(nschools)){
			thisl=l[at_schoolData[s,1]:at_schoolData[s,2]]
			# Get the number of time steps of the school:
			utim=c(utim,FILETIME2utim(getvar("<parameter name=\"mNTDate\">",18,thisl)))
			
			# Get the parameters of the segmentation for the school:
			mChannelNo=c(mChannelNo,as.numeric(getvar("<parameter name=\"mChannelNo\">","<",thisl)))
			mSampleIndex=c(mSampleIndex,as.numeric(getvar("<parameter name=\"mSampleIndex\">","<",thisl)))
			#MaxPingFromSeed=as.numeric(getvar("<parameter name=\"MaxPingFromSeed\">","<",thisl))
			mMinSv=c(mMinSv,as.numeric(getvar("<parameter name=\"mMinSv\">","<",thisl)))
			mMaxSv=c(mMaxSv,as.numeric(getvar("<parameter name=\"mMaxSv\">","<",thisl)))
			mMinDepth=c(mMinDepth,as.numeric(getvar("<parameter name=\"mMinDepth\">","<",thisl)))
			mMaxDepth=c(mMaxDepth,as.numeric(getvar("<parameter name=\"mMaxDepth\">","<",thisl)))
			#GrowAboveDetectedDepth=as.numeric(getvar("<parameter name=\"GrowAboveDetectedDepth\">","<",thisl))
			mMinFanNumber=c(mMinFanNumber,as.numeric(getvar("<parameter name=\"mMinFanNumber\">","<",thisl)))
			mMaxFanNumber=c(mMaxFanNumber,as.numeric(getvar("<parameter name=\"mMaxFanNumber\">","<",thisl)))
			#GrowOnProcessedFiles=as.numeric(getvar("<parameter name=\"GrowOnProcessedFiles\">","<",thisl))
			#DoSmoothing=as.numeric(getvar("<parameter name=\"DoSmoothing\">","<",thisl))
			#ErodeDilateFilterWidth=as.numeric(getvar("<parameter name=\"ErodeDilateFilterWidth\">","<",thisl))
			
			# Get the output data of the school:
			atallPing=seq(grep("<allPing>",thisl),grep("</allPing>",thisl))
			CorrectedBoxCenter=rbind(CorrectedBoxCenter,readxyz(getvar("<CorrectedBoxCenter",1000,thisl[atallPing])))
			CorrectedBoxExtent=rbind(CorrectedBoxExtent,readxyz(getvar("<CorrectedBoxExtent",1000,thisl[atallPing])))
			CorrectedBoxRotation=c(CorrectedBoxRotation,getvar("<CorrectedBoxRotation>","<",thisl[atallPing]))
			#CorrectedBoxMaxElongation=readxyz(getvar("<CorrectedMaxElongation",1000,thisl[atallPing]))
			#CorrectedBoxMinElongation=readxyz(getvar("<CorrectedMinElongation",1000,thisl[atallPing]))
			UncorrectedBoxCenter=rbind(UncorrectedBoxCenter,getvar("<UncorrectedBoxCenter",1000,thisl[atallPing]))
			UncorrectedBoxExtent=rbind(UncorrectedBoxExtent,getvar("<UncorrectedBoxExtent",1000,thisl[atallPing]))
			UncorrectedBoxRotation=c(UncorrectedBoxRotation,getvar("<UncorrectedBoxRotation>","<",thisl[atallPing]))
			#UncorrectedMaxElongation=readxyz(getvar("<UncorrectedMaxElongation",1000,thisl[atallPing]))
			#UncorrectedMinElongation=readxyz(getvar("<UncorrectedMinElongation",1000,thisl[atallPing]))
			GridVolume=c(GridVolume,as.numeric(getvar("<GridVolume>","<",thisl[atallPing])))
			GridVolumeCorrected=c(GridVolumeCorrected,as.numeric(getvar("<GridVolumeCorrected>","<",thisl[atallPing])))
			GridSurface=c(GridSurface,as.numeric(getvar("<GridSurface>","<",thisl[atallPing])))
			#GeoReference=as.numeric(readxyz(getvar("<GeoReference",1000,thisl[atallPing])))
			AvgLogSv=c(AvgLogSv,as.numeric(getvar("<AvgLogSv>","<",thisl[atallPing])))
			# Get corrected Sv, which is not given explicitely in the work files, but can be calculated as 10*log10(10^(AvgLogSv/10)*GridVolume/GridVolumeCorrected) = AvgLogSv+10*log10(GridVolume/GridVolumeCorrected):
			atschoolMask1=grep("<schoolMask",thisl)
			atschoolMask2=grep("</schoolMask>",thisl)
			
			if(length(atschoolMask1)){
				# Get the school mask for each ping:
				thisl=thisl[atschoolMask1:atschoolMask2]
				# Get the pings:
				atpingMask=cbind(grep("<pingMask",thisl),grep("</pingMask>",thisl))
				UNIX_pingNtDate=FILETIME2utim(getvar("pingNtDate=\"",18,thisl))
				# Add also the time points of the school, and uniqueify afterwards:
				utim=c(utim,UNIX_pingNtDate)
				npings=length(UNIX_pingNtDate)
				
				for(p in seq_len(npings)){
					beam_channel=readbeam_channel(getvar("<beam channel=\"",1000,thisl[atpingMask[p,1]:atpingMask[p,2]]))
					schoolMask=c(schoolMask,list(arr.ind2ind(cbind(unlist(beam_channel$voxels),rep(unlist(beam_channel$channel),sapply(beam_channel$voxels,length))),c(max(lenb),numb))))
					}
				}
			else{
				schoolMask=c(schoolMask,NA)
				}
			}
		
		utim=unique(utim)
		# Output a list of the schools:
		sgsc=schoolMask
		Lspt=cbind(mSampleIndex, mChannelNo)
		LlSv=mMinSv # Minimum Sv used in the segmentation, for each school
		LuSv=mMaxSv # Maximum Sv used in the segmentation, for each school 
		Lldp=mMinDepth #  Minimum depth used in the segmentation, for each school
		Ludp=mMaxDepth #  Maximum depth used in the segmentation, for each school
		Llfn=mMinFanNumber # Minimum fan number used in the segmentation, for each school
		Lufn=mMaxFanNumber # Maximum fan number used in the segmentation, for each school
		Lbxc=CorrectedBoxCenter # 
		Lbxe=CorrectedBoxExtent # 
		Lbxr=CorrectedBoxRotation # 
		Ltvl=GridVolumeCorrected # 
		Ltvu=GridVolume # 
		Lsar=GridSurface # 
		AvgLogSvCorrected=AvgLogSv+10*log10(GridVolume/GridVolumeCorrected)
		LmSv=AvgLogSvCorrected # 
		Lmsv=10^(LmSv/10) # 
		LmSu=AvgLogSv # 
		Lmsu=10^(LmSu/10) # 
		Ltbs=Ltvl*Lmsv # 
		LtTS=10*log10(Ltbs) # 
		Ltbu=Ltvu*Lmsu # 
		LtTu=10*log10(Ltbu) # 
			
		list(utim=utim, sgsc=sgsc, Lspt=Lspt, LlSv=LlSv, LuSv=LuSv, Lldp=Lldp, Ludp=Ludp, Llfn=Llfn, Lufn=Lufn, Lbxc=Lbxc, Lbxe=Lbxe, Lbxr=Lbxr, Ltvl=Ltvl, Ltvu=Ltvu, Lsar=Lsar, LmSv=LmSv, Lmsv=Lmsv, LmSu=LmSu, Lmsu=Lmsu, Ltbs=Ltbs, LtTS=LtTS, Ltbu=Ltbu, LtTu=LtTu)
		}
	# Organize in a list for each school:
	else{
		schools=vector("list",nschools)
		
		# Run through the schools:
		for(s in seq_len(nschools)){
			thisl=l[at_schoolData[s,1]:at_schoolData[s,2]]
			# Get the number of time steps of the school:
			#UNIX_mNTDate=FILETIME2utim(getvar("<parameter name=\"mNTDate\">",18,thisl))
			
			# Get the parameters of the segmentation for the school:
			mChannelNo=as.numeric(getvar("<parameter name=\"mChannelNo\">","<",thisl))
			mSampleIndex=as.numeric(getvar("<parameter name=\"mSampleIndex\">","<",thisl))
			MaxPingFromSeed=as.numeric(getvar("<parameter name=\"MaxPingFromSeed\">","<",thisl))
			mMinSv=as.numeric(getvar("<parameter name=\"mMinSv\">","<",thisl))
			mMaxSv=as.numeric(getvar("<parameter name=\"mMaxSv\">","<",thisl))
			mMinDepth=as.numeric(getvar("<parameter name=\"mMinDepth\">","<",thisl))
			mMaxDepth=as.numeric(getvar("<parameter name=\"mMaxDepth\">","<",thisl))
			GrowAboveDetectedDepth=as.numeric(getvar("<parameter name=\"GrowAboveDetectedDepth\">","<",thisl))
			mMinFanNumber=as.numeric(getvar("<parameter name=\"mMinFanNumber\">","<",thisl))
			mMaxFanNumber=as.numeric(getvar("<parameter name=\"mMaxFanNumber\">","<",thisl))
			GrowOnProcessedFiles=as.numeric(getvar("<parameter name=\"GrowOnProcessedFiles\">","<",thisl))
			DoSmoothing=as.numeric(getvar("<parameter name=\"DoSmoothing\">","<",thisl))
			ErodeDilateFilterWidth=as.numeric(getvar("<parameter name=\"ErodeDilateFilterWidth\">","<",thisl))
			
			# Get the output data of the school:
			atallPing=seq(grep("<allPing>",thisl),grep("</allPing>",thisl))
			CorrectedBoxCenter=readxyz(getvar("<CorrectedBoxCenter",1000,thisl[atallPing]))
			CorrectedBoxExtent=readxyz(getvar("<CorrectedBoxExtent",1000,thisl[atallPing]))
			CorrectedBoxRotation=readxyz(getvar("<CorrectedBoxRotation",1000,thisl[atallPing]))
			CorrectedBoxMaxElongation=readxyz(getvar("<CorrectedMaxElongation",1000,thisl[atallPing]))
			CorrectedBoxMinElongation=readxyz(getvar("<CorrectedMinElongation",1000,thisl[atallPing]))
			UncorrectedBoxCenter=readxyz(getvar("<UncorrectedBoxCenter",1000,thisl[atallPing]))
			UncorrectedBoxExtent=readxyz(getvar("<UncorrectedBoxExtent",1000,thisl[atallPing]))
			UncorrectedBoxRotation=readxyz(getvar("<UncorrectedBoxRotation",1000,thisl[atallPing]))
			UncorrectedMaxElongation=readxyz(getvar("<UncorrectedMaxElongation",1000,thisl[atallPing]))
			UncorrectedMinElongation=readxyz(getvar("<UncorrectedMinElongation",1000,thisl[atallPing]))
			GridVolume=as.numeric(getvar("<GridVolume>","<",thisl[atallPing]))
			GridVolumeCorrected=as.numeric(getvar("<GridVolumeCorrected>","<",thisl[atallPing]))
			GridSurface=as.numeric(getvar("<GridSurface>","<",thisl[atallPing]))
			GeoReference=as.numeric(readxyz(getvar("<GeoReference",1000,thisl[atallPing])))
			AvgLogSv=as.numeric(getvar("<AvgLogSv>","<",thisl[atallPing]))
			# Get corrected Sv, which is not given explicitely in the work files, but can be calculated as 10*log10(10^(AvgLogSv/10)*GridVolume/GridVolumeCorrected) = AvgLogSv+10*log10(GridVolume/GridVolumeCorrected):
			AvgLogSvCorrected=AvgLogSv+10*log10(GridVolume/GridVolumeCorrected)
			
	        # Get the school mask for each ping:
	        atschoolMask=seq(grep("<schoolMask",thisl),grep("</schoolMask>",thisl))
	        thisl=thisl[atschoolMask]
			# Get the pings:
			atpingMask=cbind(grep("<pingMask",thisl),grep("</pingMask>",thisl))
			UNIX_pingNtDate=FILETIME2utim(getvar("pingNtDate=\"",18,thisl))
			npings=length(UNIX_pingNtDate)
			schoolMask=vector("list",npings)
			
			for(p in seq_len(npings)){
				beam_channel=readbeam_channel(getvar("<beam channel=\"",1000,thisl[atpingMask[p,1]:atpingMask[p,2]]))
				schoolMask[[p]]=arr.ind2ind(cbind(unlist(beam_channel$voxels),rep(unlist(beam_channel$channel),sapply(beam_channel$voxels,length))),c(max(lenb),numb))
				}
			
			utim=UNIX_pingNtDate
			sgsc=schoolMask
			Lspt=c(mSampleIndex, mChannelNo)
			LlSv=mMinSv # Minimum Sv used in the segmentation, for each school
			LuSv=mMaxSv # Maximum Sv used in the segmentation, for each school 
			Lldp=mMinDepth #  Minimum depth used in the segmentation, for each school
			Ludp=mMaxDepth #  Maximum depth used in the segmentation, for each school
			Llfn=mMinFanNumber # Minimum fan number used in the segmentation, for each school
			Lufn=mMaxFanNumber # Maximum fan number used in the segmentation, for each school
			Lbxc=CorrectedBoxCenter # 
			Lbxe=CorrectedBoxExtent # 
			Lbxr=CorrectedBoxRotation # 
			Ltvl=GridVolumeCorrected # 
			Ltvu=GridVolume # 
			Lsar=GridSurface # 
			LmSv=AvgLogSvCorrected # 
			Lmsv=10^(LmSv/10) # 
			LmSu=AvgLogSv # 
			Lmsu=10^(LmSu/10) # 
			Ltbs=Ltvl*Lmsv # 
			LtTS=10*log10(Ltbs) # 
			Ltbu=Ltvu*Lmsu # 
			LtTu=10*log10(Ltbu) # 
				
			schools[[s]]=list(utim=utim, sgsc=sgsc, Lspt=Lspt, LlSv=LlSv, LuSv=LuSv, Lldp=Lldp, Ludp=Ludp, Llfn=Llfn, Lufn=Lufn, Lbxc=Lbxc, Lbxe=Lbxe, Lbxr=Lbxr, Ltvl=Ltvl, Ltvu=Ltvu, Lsar=Lsar, LmSv=LmSv, Lmsv=Lmsv, LmSu=LmSu, Lmsu=Lmsu, Ltbs=Ltbs, LtTS=LtTS, Ltbu=Ltbu, LtTu=LtTu)
			}
		
		# Output a list of the schools:
		names(schools)=seq_along(schools)
		schools
		}
	##################################################
	##################################################
	}
