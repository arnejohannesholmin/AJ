#*********************************************
#*********************************************
#' Plots a histogram of 'x' added a line of the maximum likelihood fitted Poisson distribution to 'x'.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname histpois
#'
histpois<-function(x,mu=NULL,breaks="Sturges",coll="red",lengthx=1000,lwd=1.5,add=FALSE,plot=TRUE,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	# Last: 2009-05-05 - Corrected for differing ranges of the output from hist() and from the fitted Gaussian distribution, by adjusting ylim.
	########### DESCRIPTION: ###########
	# Plots a histogram of 'x' added a line of the maximum likelihood fitted Poisson distribution to 'x'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the data.
	# - 'mu' and 'sigma' are the parameters in the Poisson distribution.
	# - 'breaks' has the same effect as in hist().
	# - 'coll' is the colour for the Poisson line.
	# - 'lengthx' is the length of the Poisson line.
	# - 'lwd' is the line width of the Poisson line.
	# - 'add' is TRUE if the line Poisson is to be added to a plot.
	# - '...' variables passed on to hist().
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Fitting a Gaussian distribution to the data by :
	if(is.null(mu)){
		mu=mean(x,na.rm=TRUE)
		if(mu<=0){
			stop("'mu' estimated to be non-positive")
			}
		}
	
	
	##### Execution and output #####
	poisx=floor(min(x,na.rm=TRUE)):ceiling(max(x,na.rm=TRUE))
	poisy=dpois(poisx,mu)
	if(!add){
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		# Adjusting for the total area of the histogram:
		area=sum(hh$counts*diff(hh$breaks))
		hh$counts=hh$counts/area
		if(plot){
			plot(hh,ylim=range(c(hh$density,poisy)))
			}
		# Reverting hh to its original scale:
		hh$counts=hh$counts*area
		}
	else{
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		}
	if(plot){
		lines(poisx,poisy,col=coll,lwd=lwd,...)
		}
	invisible(list(hist=hh,poisx=poisx,poisy=poisy,mu=mu))
	##################################################
	##################################################
	}
