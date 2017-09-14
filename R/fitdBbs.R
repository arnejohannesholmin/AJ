#*********************************************
#*********************************************
#' Deduces a formula for the dBbs using Xsbr, and DISCARD PINGS WHERE THE AREA IS NOT CROSSING.
#'
#' @param event  is the event holding the segmentation data.
#' @param dBbs  are the dBbs values used in the segmentation data.
#' @param segpar  is the file number of the initial segmentation (defined from the background level).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR read.event
#'
#' @export
#' @rdname fitdBbs
#'
fitdBbs<-function(event="/Users/arnejh/Data/echoIBM/SX90_biomassEstimation/Events/SX90_biomassEstimation_event_one_school_at_the_time_directional_fish_realisticNoise/SX90/tsd", dBan=8, dBbs=1:20){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-08-17 - Clean version.
	########### DESCRIPTION: ###########
	# Deduces a formula for the dBbs using Xsbr, and DISCARD PINGS WHERE THE AREA IS NOT CROSSING.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---event--- is the event holding the segmentation data.
	# ---dBbs--- are the dBbs values used in the segmentation data.
	# ---segpar--- is the file number of the initial segmentation (defined from the background level).
	

	##################################################
	##################################################
	# Read all noise thresholded segmentation data:
	segnoise = read.event(event=event, t="all", var=c("Xtha","XSBR","Xcmx","Xcmy"), segpar=list(dBan=dBan, dBbs=NA))
	
	# Read all signal thresholded segmentation data:
	segsignal=vector("list",length(dBbs))
	for(i in seq_along(segsignal)){
		segsignal[[i]] = read.event(event=event, t="all", var=c("Xtha","XqSv","XSBR"), segpar=list(dBan=dBan, dBbs=dBbs[i]))
		}
	
	# And the theoretical and vessel info:
	Ttha = writeTtha(event)
	v = read.event(event=event, var="vessel", t="all")
	indSchool = c(rep(seq_len(120), each=180),12)
	
	# Get the values of dBbs that return the closest fit to the theoretical area for each ping in which the entire school is inside the volume:
	Area = sapply(segsignal, "[[", "Xtha")
	QSv = sapply(segsignal, "[[", "XqSv")
	SBR = sapply(segsignal, "[[", "XSBR")
	TArea = matrix(Ttha$Ttha, ncol=length(dBbs), nrow=length(Ttha$Ttha))
	diffArea = Area - TArea
	
	# Get information from the initial segmentation:
	dBbsFit = dBbs[apply(abs(diffArea), 1, which.min)]
	Areanoise = segnoise$Xtha
	Sizenoise = 2*sqrt(Areanoise/pi)
	SBRnoise = segnoise$XSBR
	Rangenoise = sqrt((segnoise$Xcmx-v$psxv)^2 + (segnoise$Xcmy-v$psyv)^2)
	
	# Identify the pings where the school was initially segmented too large, and subsequently to small at the signal threshold was reduced. Also require the :
	valid = (apply(diffArea,1,min) <= 0) & (apply(diffArea,1,max) >= 0) & Ttha$frcS==1
	# Also try to select 1, 10, 20, and all valid pings of each school:
	AreaList = Areanoise
	AreaList[!(valid %in% TRUE)] = NA
	AreaList = split(AreaList, indSchool)
	# Largest area:
	validList_1 = lapply(AreaList, function(xx) {o <- order(xx, decreasing=TRUE); head(o[!is.na(xx[o])], 1)})
	valid_1 = sort(unlist(lapply(0:119, function(xx) validList_1[[xx+1]] + 180*xx)))
	# 10 largest areas:
	validList_10 = lapply(AreaList, function(xx) {o <- order(xx, decreasing=TRUE); head(o[!is.na(xx[o])], 10)})
	valid_10 = sort(unlist(lapply(0:119, function(xx) validList_10[[xx+1]] + 180*xx)))
	# 20 largest areas:
	validList_20 = lapply(AreaList, function(xx) {o <- order(xx, decreasing=TRUE); head(o[!is.na(xx[o])], 20)})
	valid_20 = sort(unlist(lapply(0:119, function(xx) validList_20[[xx+1]] + 180*xx)))
	
	# Linear regression of the model dBbs = S + SBR:
	lm_1 = lm(dBbsFit ~ Sizenoise + SBRnoise, subset = valid_1)
	lm_10 = lm(dBbsFit ~ Sizenoise + SBRnoise, subset = valid_10)
	lm_20 = lm(dBbsFit ~ Sizenoise + SBRnoise, subset = valid_20)
	lm_all = lm(dBbsFit ~ Sizenoise + SBRnoise, subset = valid)
	
	list(lm_1=lm_1, lm_10=lm_10, lm_20=lm_20, lm_all=lm_all)
	##################################################
	##################################################
	}
