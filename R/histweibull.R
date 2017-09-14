#*********************************************
#*********************************************
#' Plots a histogram of 'x' added a line of the maximum likelihood fitted Weibull distribution to 'x'.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname histweibull
#'
histweibull<-function(x,breaks="Sturges",coll="red",lengthx=1000,lwd=1.5,add=FALSE,plot=TRUE,...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-04-04 - Finished.
	########### DESCRIPTION: ###########
	# Plots a histogram of 'x' added a line of the maximum likelihood fitted Weibull distribution to 'x'.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'x' is the data.
	# - 'coll' is the colour for the weibull line.
	# - 'breaks' is used in hist().
	# - 'lengthx' is the length of the weibull line.
	# - 'lwd' is the line width of the weibull line.
	# - 'add' is TRUE if the weibull line is to be added to a plot.
	# - '...' variables passed on to hist().
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Holds the function fitdistr():
	# Remove missing values:
	x=x[!is.na(x)]
	# Fit the data to the weibull distribution (scale to resonable values):
	par=fitdistr(x/mean(x),densfun=dweibull,list(shape = 1, scale = 1))$est
	par[2]=par[2]*mean(x)
	
	
	##### Execution and output #####
	weibullx=seq(min(x),max(x),length.out=lengthx)
	weibully=dweibull(weibullx,par[1],par[2])
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
				plot(hh,ylim=range(c(hh$density,weibully)),...)
				}
			}
		# Reverting hh to its original scale:
		hh$counts=hh$counts*area
		}
	else{
		hh=hist(x,plot=FALSE,breaks=breaks,...)
		}
	if(plot){
		lines(weibullx,weibully,col=coll,lwd=lwd,...)
		}
	invisible(list(h=hh,weibullx=weibullx,weibully=weibully,par=c(shape=par[1],rate=par[2])))
	##################################################
	##################################################
	}
