#*********************************************
#*********************************************
#' Locates the path of an event specified by numeric or sting for both 'event' and 'cruise' (used in read.event()).
#'
#' @param event  is the identifier of the event in which the .work files are located.
#' @param segdir  is an optional directory in which to put the segmentation files, defaulted to .
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR event.path list.files_caseInsensitive read.event
#' @importFrom TSD NAs s2dhms write.TSD
#' @importFrom tools file_ext
#'
#' @export
#' @rdname echoIBM.workLSSS2sgsc.event
#'
echoIBM.workLSSS2sgsc.event<-function(event,segdir=NULL,cruise=2009116,t="all",esnm="MS70",dir.data=NULL,code=NULL,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-07-07 - Clean version.
	# Update: 2013-01-07 - Changed names to comply with the standardization of names.
	# Last: 2013-01-12 - Changed method radically, using the new function echoIBM.read.workLSSS2().
	########### DESCRIPTION: ###########
	# Locates the path of an event specified by numeric or sting for both 'event' and 'cruise' (used in read.event()).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---event--- is the identifier of the event in which the .work files are located.
	# ---segdir--- is an optional directory in which to put the segmentation files, defaulted to .
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# Read the time and the number of beams:
	data=read.event(event=event.path(event=event,cruise=cruise,esnm=esnm,dir.data=dir.data),t=t,var=c("utim","ftim","indt","freq","lenb"))
	freq=data$freq
	numb=length(freq)
	utim=data$utim
	ftim=data$ftim
	if(ftim[1]>1e13){
		ftim=round(as.numeric(substring(ftim,9)))
		}
	else{
		ftim=round(as.numeric(gsub(":","",s2dhms(substring(ftim,9),clock.out=TRUE))))
		}
	numt=length(utim)
	lenb=max(data$lenb)
	
	# Locate the directory of the work files:
	if(length(grep("work",basename(event)))==0){
		event=event.path(event=event,cruise=cruise,esnm=esnm,dir.data=dir.data,dir.type="work")$event
		}
	# Locate the 'segdir':
	if(length(segdir)==0){
		segdir=file.path(event.path(event=event,dir.type="tsd"),"seg")$event
		if(is.na(file.info(segdir)$isdir)){
			dir.create(segdir)
			}
		}
	
	# Get the file names of the work files:
	f=list.files_caseInsensitive(event,full.names=TRUE,recursive=TRUE)
	f=f[file_ext(f)=="work"]
	if(length(f)==0){
		stop("No work files found in the event")
		}
	
	# Read the first file:
	tempout=echoIBM.read.workLSSS2(f[1],lenb=lenb,numb=numb,flatten=TRUE)
	if(length(f)>1){
		for(i in seq(2,length(f))){
			thisout=echoIBM.read.workLSSS2(f[i],lenb=lenb,numb=numb,flatten=TRUE)
			for(l in seq_along(thisout)){
				if(is.list(thisout[[l]]) || length(dim(thisout[[l]]))==0){
					tempout[[l]]=c(tempout[[l]],thisout[[l]])
					}
				else if(length(dim(thisout[[l]]))==2){
					tempout[[l]]=c(tempout[[l]],thisout[[l]])
					}
				}
			}
		}
	
	# Create the final output, as a list of lists, vectors or matrices with dimension according to the number of time steps requested:
	out=vector("list",length(tempout))
	names(out)=names(tempout)
	for(l in seq_along(out)){
		if(is.list(tempout[[l]])){
			out[[l]]=vector("list",numt)
			}
		else if(length(dim(tempout[[l]]))==0){
			out[[l]]=NAs(numt)
			}
		else if(length(dim(tempout[[l]]))==2){
			out[[l]]=NAs(numt,dim(tempout[[l]])[2])
			}
		}
	
	# Insert the data into the final output:
	for(i in seq_along(tempout$utim)){
		utimMatch=match(round(tempout$utim[i],digits=1),round(utim,digits=1))
		if(!is.na(utimMatch)){
			for(l in seq_along(out)){
				if(length(tempout[[l]])>=i){
					out[[l]][[utimMatch]]=tempout[[l]][[i]]
					}
				}
			}
		}
	# Add frequency and length of beams:
	out=c(out,data[c("lenb","freq")])
	out$utim=utim
	out$indt=data$indt
		
	# Replace the NAs in the the schoolMask with NULL:
	areNa=sapply(out$sgsc,function(x) if(length(x)==1 && is.na(x)) TRUE else FALSE)
	if(any(areNa)){
		out$sgsc[areNa]=vector("list",sum(areNa))
		}
	
	
	if(length(code)>0){
		con=file.path(segdir, paste(rev(strsplit(segdir, "/", fixed=TRUE)[[1]])[4], "_lsth_", format(10^(out$LlSv[1]/10),scientific=TRUE), "_usth_", format(10^(out$LuSv[1]/10),scientific=TRUE), "_code_", code, "_T", ftim[1], ".seg", sep=""))
		}
	else{
		con=file.path(segdir, paste(rev(strsplit(segdir, "/", fixed=TRUE)[[1]])[4], "_lsth_", format(10^(out$LlSv[1]/10),scientific=TRUE), "_usth_", format(10^(out$LuSv[1]/10),scientific=TRUE), "_T", ftim[1], ".seg", sep=""))
		}
	
	write.TSD(con=con,x=out, keep.null=TRUE, ...)
	out
	##################################################
	##################################################
	}
