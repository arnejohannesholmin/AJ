#*********************************************
#*********************************************
#' Converts power to volume backscattering coefficient from data read from a Simrad raw file.
#'
#' @param x  is a list of the data.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_power2sv
#'
readEKRaw_power2sv<-function(x){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Converts power to volume backscattering coefficient from data read from a Simrad raw file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a list of the data.
	

	##################################################
	##################################################
	# Detect the type of raw file:
	if(length(x$data$pings$equivalentbeamangle)>0){
		raw=1
		}
	else{
		raw=0
		}
	
	dimpower=dim(x$data$pings$power)
		
	#  extract calibration parameters for this xcvr
	c = x$data$pings$soundvelocity
	f = x$data$pings$frequency
	# Get the correct gain to use:
	if(raw==1){
		x$data$pings$gain = x$data$pings$gaintx + x$data$pings$gainrx
		phi = x$data$pings$equivalentbeamangle
		}
	else{
		gain = x$data$config$gain
		phi = x$data$config$equivalentbeamangle
		gain = array(gain,dim=dimpower[-1])
		phi = array(phi,dim=dimpower[-1])
		}
	pt = x$data$pings$transmitpower
	tau = x$data$pings$pulselength
	sacr = x$data$config$sacorrectiontable[,1]
	sacr = array(sacr,dim=dimpower[-1])
	# Calculate wavelength:
	lambda =  c / f
	# Impedance:
	if(raw==1){
		impedance = 53
		}
	else{
		impedance = 1
		}

	#C = pt * (10^(gain/10))^2 * lambda^2 * c * tau * 10^(phi/10) * impedance /  (32 * pi^2) * 2*sacr
	C = 10^drop( - (2*(gain + sacr) + phi) / 10 )  *  (32*pi^2) /  drop(pt * lambda^2 * c * tau)
	if(length(dimpower)==3){
		C = array(C,dim=dimpower[c(2,3,1)])
		C = aperm(C,c(3,1,2))
		}
	if(length(dimpower)==2){
		C = array(C,dim=dimpower[c(2,1)])
		C = t(C)
		}
	
	# The power field in the raw files is already in dB, so we need to linearize it here:
	x$data$pings$sv = 10^(x$data$pings$power/10) * C
	#x$data$pings$sv = 10^(x$data$pings$power/10) / C
	# Return:
	x
	##################################################
	##################################################
	}
