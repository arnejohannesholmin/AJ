#*********************************************
#*********************************************
#' Plots a histogram of 'x' added a line of the maximum likelihood fitted Gaussian distribution to 'x'.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats dnorm sd
#' @importFrom stats sd
#'
#' @export
#' @rdname histnorm
#'
histnorm<-function(x,mu=NULL,sigma=NULL,breaks="Sturges",coll="red",lengthx=1000,lwd=1.5,add=FALSE,plot=TRUE,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	# Last: 2009-05-05 - Corrected for differing ranges of the output from hist() and from the fitted Gaussian distribution, by adjusting ylim.
	########### DESCRIPTION: ###########
	# Plots a histogram of 'x' added a line of the maximum likelihood fitted Gaussian distribution to 'x'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the data.
	# - 'mu' and 'sigma' are the parameters in the Gaussian distribution.
	# - 'breaks' has the same effect as in hist().
	# - 'coll' is the colour for the Gaussian line.
	# - 'lengthx' is the length of the Gaussian line.
	# - 'lwd' is the line width of the Gaussian line.
	# - 'add' is TRUE if the line Gaussian is to be added to a plot.
	# - '...' variables passed on to hist().
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Fitting a Gaussian distribution to the data by :
	if(is.null(mu) && is.null(sigma)){
		mu=mean(x,na.rm=TRUE)
		sigma=sd(x,na.rm=TRUE)
		}
	else if(is.null(mu)){
		mu=mean(x,na.rm=TRUE)
		}
	else if(is.null(sigma)){
		sigma=sqrt(sum((x-mu)^2,na.rm=TRUE)/length(x))
		}
	normx=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length.out=lengthx)
	normy=dnorm(normx,mu,sigma)
	
	
	##### Execution and output #####
	if(!add){
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		# Adjusting for the total area of the histogram:
		area=sum(hh$counts*diff(hh$breaks))
		hh$counts=hh$counts/area
		if(plot){
			plot(hh,ylim=range(c(hh$density,normy)))
			}
		# Reverting hh to its original scale:
		hh$counts=hh$counts*area
		}
	else{
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		}
	if(plot){
		lines(normx,normy,col=coll,lwd=lwd,...)
		}
	invisible(list(hist=hh,normx=normx,normy=normy,mu=mu,sigma=sigma))
	##################################################
	##################################################
	}
