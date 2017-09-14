#*********************************************
#*********************************************
#' Plots a histogram of 'x' added a line of the maximum likelihood fitted exponential distribution to 'x'.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname histexp
#'
histexp<-function(x,rate=NULL,cut=NULL,type="l",scale=1,breaks="Sturges",coll="red",lengthx=1000,lwd=1.5,add=FALSE,plot.all=TRUE,pres=cut*1e-4,max.iter=100,plot=TRUE,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Plots a histogram of 'x' added a line of the maximum likelihood fitted exponential distribution to 'x'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the data.
	# - 'rate' is the parameter in the exponential distribution.
	# - 'cut' is either a numeric giving the cut point 'C', or a string specifying the cutpoint 'C' and an operator 'type' used in meanexp.conditional(). As an example cut="x<2" or simply cut="<2" will estimate the expectation of the exponential distribution based on the proportion of tha data smaller than 2, and scale the corresponding exponential line to fit the specified proportion. If given as numeric, 'type' is also needed.
	# - 'type' is "l" if 'mean' is the mean of X|X<C and "g" if 'mean' is the mean of X|X>C.
	# - 'scale' is a numeric scaling the exponential line.
	# - 'coll' is the colour for the exponential line.
	# - 'breaks' is used in hist().
	# - 'lengthx' is the length of the exponential line.
	# - 'lwd' is the line width of the exponential line.
	# - 'add' is TRUE if the line exponential is to be added to a plot.
	# - '...' variables passed on to hist().
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(is.numeric(cut)){
		rate=1/meanexp.conditional(x=x,C=cut,pres=pres,max.iter=max.iter,type=type)
		if(plot.all){
			if(type=="l"){
				scale=mean(x<C,na.rm=TRUE)/(1-exp(-C*rate))
				}
			else{
				scale=mean(x>C,na.rm=TRUE)/exp(-C*rate)
				}
			}
		else{
			if(type=="l"){
				scale=1/(1-exp(-C*rate))
				x=x[x<C]
				}
			else{
				scale=1/exp(-C*rate)
				x=x[x>C]
				}
			}
		}
	else if(is.character(cut)){
		# Remove whitespace:
		cut=gsub("[[:blank:]]","",cut)
		# Only cut the data if 'cut' has more than one character:
		if(nchar(cut)>1){
			# Get the value of 'C' used in meanexp.conditional():
			num=gregexpr("[[:digit:].]",cut)[[1]]
			# If there are characters between the numeric, choose the first sequence
			splnum=which(diff(num)>1)
			if(length(splnum)>0){
				num=num[1:splnum[1]]
				}
			C=as.numeric(substr(cut,min(num),max(num)))
			
			# Get the operator:
			fun=gregexpr("[<>]",cut)[[1]]
			if(length(fun)>0 && is.numeric(C)){
				if(length(fun)>1){
					warning("only the first of '<' and '>' used")
					}
				type=substr(cut,fun[1],fun[1])
				if(type=="<"){
					type=c("l","g")[1+(fun>max(num))]
					}
				else{
					type=c("g","l")[1+(fun>max(num))]
					}
				rate=1/meanexp.conditional(x=x,C=C,pres=pres,max.iter=max.iter,type=type)
				if(plot.all){
					if(type=="l"){
						scale=mean(x<C,na.rm=TRUE)/(1-exp(-C*rate))
						}
					else{
						scale=mean(x>C,na.rm=TRUE)/exp(-C*rate)
						}
					}
				else{
					if(type=="l"){
						scale=1/(1-exp(-C*rate))
						x=x[x<C]
						}
					else{
						scale=1/exp(-C*rate)
						x=x[x>C]
						}
					}
				}
			}
		}
	if(is.null(rate)){
		rate=1/mean(x,na.rm=TRUE)
		if(rate<=0){
			stop("Rate estimated to be non-positive")
			}
		}
	
	
	##### Execution and output #####
	expx=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=lengthx)
	expy=dexp(expx,rate)*scale
	if(!add){
		hh=suppressWarnings(hist(x,plot=FALSE,breaks=breaks,...))
		# Adjusting for the total area of the histogram:
		area=sum(hh$counts*diff(hh$breaks))
		hh$counts=hh$counts/area
		if(plot){
			if(any(names(list(...))=="ylim")){
				plot(hh,...)
				}
			else{
				plot(hh,ylim=range(c(hh$density,expy)),...)
				}
			}
		# Reverting hh to its original scale:
		hh$counts=hh$counts*area
		}
	else{
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		}
	if(plot){
		lines(expx,expy,col=coll,lwd=lwd,...)
		}
	invisible(list(h=hh,expx=expx,expy=expy,rate=rate))
	##################################################
	##################################################
	}
