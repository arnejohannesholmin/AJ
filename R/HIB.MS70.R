#*********************************************
#*********************************************
#' Processing and writing datasets by the HIB-algorithm. See 'The HIB-algorithm.pdf' for documentation and read.event() for importing HIBs. The processing needs to be performed on data that is not compensated for absorption and attenuation through the TVG function. 
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR read.event
#'
#' @export
#' @rdname HIB.MS70
#'
HIB.MS70<-function(cruise=2009116,event=1,t="all",C=1e-6,nu=1,sep=101,turns=10,near_range=50,TVG=FALSE,dir.data=NULL,tofile=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-06-09 - Finished.
	# Update: 2009-07-24 - Changed name from process.HIB() to process.write.HIB().
	# Last: 2010-02-19 - Changed name to HIB.MS70 and changed to support the directory structure of cruises and events.
	########### DESCRIPTION: ###########
	# Processing and writing datasets by the HIB-algorithm. See 'The HIB-algorithm.pdf' for documentation and read.event() for importing HIBs. The processing needs to be performed on data that is not compensated for absorption and attenuation through the TVG function. 
	########## DEPENDENCIES: ###########
	# read.event(), HIB()
	############ VARIABLES: ############
	# - 'cruise' is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
	# - 'event' is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
	# - 't' is a vector of the numbers of the pings to be filtered, as listed from 1 to the number of pings in the event. If t=="all", all files are read.
	# - 'C' is the cutpoint as specified in the HIB-algorithm (see HIB()).
	# - 'nu' is the number of sequences accepted as incorrectly classified HIBs.
	# - 'sep' is the number of voxels separating HIBs. If two HIBs are separated by less than 'sep' voxels, the HIBs are merged.
	# - 'turns' is the number of pings to process at each step in the algorithm. If large, R might encounter memory limitation problems.
	# - 'near_range' specifies the number of voxles in the radial direction that are defined as the near-range. The near-range consists of extremely large values, and should not be considered in the HIB-algorithm.
	# - 'dir.data' is the path to the directory in which the projects are stored, defaulted by the variable 'datasets_directory' specified in "/Library/Frameworks/R.framework/Versions/2.10/Resources/library/base/R/Rprofile".
	# - 'tofile' is TRUE if HIBs shoudl be written to file, and FALSE if not.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# The number of pings of the event:
	numt=read.event(var="numt",cruise=cruise,event=event,dir.data=dir.data)$numt
	if(is.null(numt)){
		warning(paste("No event found (cruise: ",cruise,", event: ",event,")",sep=""))
		return()
		}
	if(identical(t,"all")){
		t=1:numt
		}
	lt=length(t)
	# Constructing a matrix of start and end indexes to be used in thesepings=pings[pingmatrix[i,1]:pingmatrix[i,2]] (see below):
	startp=seq(1,by=turns,l=ceiling(lt/turns))
	endp=seq(turns,by=turns,l=ceiling(lt/turns))
	# Output file name:
	path=read.event(cruise=cruise,event=event,dir.out=TRUE,dir.data=dir.data)
	eventname=tail(strsplit(path,"/")[[1]],3)[1]
	hibfile=paste(path,"/",eventname,".hib",sep="")
	
	
	##### Executions and output #####
	# The output of HIB():
	hibout=NULL
	hout=NULL
	# The for loop moves through the blocks specified by pingmatrix:
	for(i in seq_along(startp)){
		# The currently processed pings:
		thesepings=startp[i]:endp[i]
		# Unfiltered acoustic data:
		vbsc=read.event(var="vbsc",cruise=cruise,event=event,t=thesepings,drop.out=FALSE,TVG=TVG,hib=NULL,dir.data=dir.data)$vbsc
		# Reshape 'vbsc' to incorporate the full dimensions of the voxel volume (length of beams, # beams horizontally, # beams vertically, # pings)
		beams=read.event(var="beams",cruise=cruise,event=event,dir.data=dir.data)
		dimvbsc=dim(vbsc)
		dim(vbsc)=c(dimvbsc[1],length(unique(beams$dira)),length(unique(beams$dire)),dimvbsc[3])
		# Reading the value of 'h' at each step in the for loop and appending to the output 'hout':
		h<-HIB(vbsc[(near_range+1):dim(vbsc)[1],,,,drop=FALSE],C=C,nu=nu,output="h",along="first")
		if(!is.null(h)){
			hout=c(hout,h)
			}
		else{
			hout=c(hout,0)
			}
		# Reading the value of 'hib' at each step in the for loop and appending to the output 'hibout':
		hib<-HIB(vbsc[(near_range+1):dim(vbsc)[1],,,,drop=FALSE],C=C,output="hib",h=h,sep=sep,along="first")
		# Adjusting the ping numbers and the numbers along the beams:
		if(!identical(hib,integer(0))){
			# Insert the right ping number:
			hib[,3]=thesepings[hib[,3]]
			# Add the 'near_range':
			hib[,4:5]=hib[,4:5]+near_range
			}
		hibout=rbind(hibout,hib)
		# Print info about the progress:
		cat(paste("Pings",startp[i],"to",min(endp[i],numt),"done.\n"))
		}
	# Order the hib matrix in the following priority: pings/time steps -> horizontal fan -> beam inside horizontal fan (3,2,1):
	if(length(hibout)>0){
		hibout=hibout[order(hibout[,3],hibout[,2],hibout[,1]),,drop=FALSE]
		}
	
	# Write to file:
	if(tofile){
		# Lines to be written to the output file:
		lines=c(paste("Event:",path),paste("C:",C),paste("turns:",turns),paste("near_range:",near_range),paste("nu:",nu),paste("numt:",numt),"h:",paste(hout,sep=" ",collapse=" "))
		# Write the hib file:
		for(i in seq_along(lines)){
			write(lines,hibfile,sep="\n")
			}
		if(is.null(hibout)){
			write("No HIBs found",hibfile,append=TRUE)
			}
		else{
			suppressWarnings(write.table(hibout,hibfile,append=TRUE,row.names=FALSE,sep="\t"))
			}
		}
	# The output:
	list(h=hout,hib=hibout)
	##################################################
	##################################################
	}
