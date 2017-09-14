#*********************************************
#*********************************************
#' Locating the parameter of an exponentially distributed 'X' based on the mean of Y=X|X<C or Y=X|X>C. Limited accuracy if the mean is close to the limiting value of C/2 (for which the distribution of the lower values is almost uniform). 2012-05-23: The function returns biased estimates for the case type=="l", especially for small sample sizes. The reason for this is not yet established. Use the function meanexp.quantile() instead. 
#'
#' @param mean  is the mean of the values of the exponentially distributed lower or greater than 'C'.
#' @param C  it the value defining 'Y'.
#' @param x  it a vector of the data from which 'mean' is calculated (overrides 'mean' if given).
#' @param beta  is the initial parameter value, defaulted to 'C'. Should not be chosen larger than 'C'.
#' @param pres  is the required presicion of the iteration.
#' @param max.iter  is the maximum number of iterations.
#' @param type  is "l" if 'mean' is the mean of X|X<C and "g" if 'mean' is the mean of X|X>C.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#'
#' @export
#' @rdname meanexp.conditional
#'
meanexp.conditional=function(mean,C,x=NULL,beta=C,pres=C*1e-4,max.iter=100,type="l"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-08-07 - Clean version.
	########### DESCRIPTION: ###########
	# Locating the parameter of an exponentially distributed 'X' based on the mean of Y=X|X<C or Y=X|X>C. Limited accuracy if the mean is close to the limiting value of C/2 (for which the distribution of the lower values is almost uniform). 2012-05-23: The function returns biased estimates for the case type=="l", especially for small sample sizes. The reason for this is not yet established. Use the function meanexp.quantile() instead. 
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---mean--- is the mean of the values of the exponentially distributed lower or greater than 'C'.
	# ---C--- it the value defining 'Y'.
	# ---x--- it a vector of the data from which 'mean' is calculated (overrides 'mean' if given).
	# ---beta--- is the initial parameter value, defaulted to 'C'. Should not be chosen larger than 'C'.
	# ---pres--- is the required presicion of the iteration.
	# ---max.iter--- is the maximum number of iterations.
	# ---type--- is "l" if 'mean' is the mean of X|X<C and "g" if 'mean' is the mean of X|X>C.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# If 'Y' is defined by the user as the values of 'X' greater than C, the result is simply mean-C:
	if(identical(type,"g")){
		# Calculate the value of 'mean' if the data are given:
		if(!is.null(x)){
			mean=sum(x[x>C],na.rm=TRUE)/sum(x>C,na.rm=TRUE)
			}
		if(mean<=C){
			stop("'C' cannot exceed 'mean'")
			}
		return(mean-C)
		}
	# Calculate the value of 'mean' if the data are given:
	if(!is.null(x)){
		mean=sum(x[x<C],na.rm=TRUE)/sum(x<C,na.rm=TRUE)
		}
	l=max(length(C),length(mean))
	# Error if mean>=C:
	if(any(mean>=C)){
		stop("'C' must exceed 'mean'")
		}
	# 'd' is the difference between the current and the previous value ov 'beta', set to be larger than 'pres' to assure that the interation starts:
	d=pres+1
	# 'betav' is the vector storing the beta values:
	betam=zeros(l,max.iter)
	betam[,1]=beta
	# 'iter' is the number of interations performed +1:
	iter=1
	
	
	##### Execution and output #####
	if(any(mean>C/2)){
		warning(paste("Iteration does not converge for (",sum(mean>C/2),") beams: ",paste(which(mean>C/2),collapse=", "),", and will stop at 'max.iter' and give a bad estimate. Try increasing C",sep=""))
		}
	# The following estimation is based on maximum likelihood estimation:
	# f(x|x<c) = f(x) / F(C)
	# L(lambda|x_1, ... , x_n) = prod( f(x|x<c) )
	#             = prod( lambda/(1-exp(-lambda*c)) * exp(-lambda*x_i) )
	# =>
	# l(lambda|x_1, ... , x_n) = log( L(lambda|x) )
	# l(lambda|x_1, ... , x_n) = sum( log( lambda/(1-exp(-lambda*c)) ) + log(exp(-lambda*x_i)) )
	# l(lambda|x_1, ... , x_n) = n * log( lambda/(1-exp(-lambda*c)) - lambda sum(x_i) 
	# d l / d lambda           = n * 1/(lambda/(1-exp(-lambda*c))) * (1-exp(-lambda*c) - lambda*c*exp(-lambda*c)) / (1-exp(-lambda*c))^2 - sum(x_i) = 0
	# => (where mean_c of x is over the values < c)
	# mean_c(x_i) = 1/lambdahat - c * exp(-lambdahat*c) / (1-exp(-lambdahat*c))
	# mean_c(x_i) = 1/lambdahat - c / (exp(-lambdahat*c) - 1)
	# mean_c(x_i) = betahat - c / (exp(-c/betahat) - 1)
	while(d>pres && iter<max.iter){
		### Updating 'betav': ###
		# Old calculation (slower):
		#Plow=exp(-C/betam[,iter])
		#Fc=1-Plow
		#betam[,iter+1]=mean+C*Plow/Fc
		Fc=exp(C/betam[,iter])-1
		betam[,iter+1]=mean+C/Fc
		# Updating 'd':
		d=abs(max(betam[,iter+1]-betam[,iter]))
		# Update iter
		iter=iter+1
		}
	betam[,iter]
	#betam[,]
	##################################################
	##################################################
	}
