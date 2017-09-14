#*********************************************
#*********************************************
#' Calculates the spherical surface integral of a beam pattern from a acoustic fish models at radius 1 meter, for the given reference frequency and reference length of the scoustic model.
#'
#' @param case  is the identifier of the case, given as a string holding the path of the directory of the TSD files required (empirical beam pattern file and ctd file).
#' @param f0  is the reference frequency at which 'chi' should be calculated, defaulted to 38000 Hz.
#' @param L0  is the reference length of the acoustic model, at which 'chi' should be calculated, defaulted to 32 cm fish of which 0.26 is swim bladder length (Gorska and Ona 2003).
#' @param pres  is the desired presition of the integration (used in integrateonsphere()).
#' @param max.cells  is the maximum number of cells in the grid (used in integrateonsphere()).
#' @param method  is "closest" if the chi value of the closest grid point is to be selected, and "linear" if linear interpolation should be used to extract the chi value.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom echoIBM beamPattern.TSD integrateonsphere
#' @importFrom gdata case
#' @importFrom TSD read.TSDs zeros
#'
#' @export
#' @rdname get.ssil
#'
get.ssil<-function(case=NULL,f0=38000,L0=0.32*0.26,kL=NULL,pres=1e-6,max.cells=1e6,method=c("linear","closest")){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-06-22 - Clean version.
	# Update: 2011-06-23 - Changed to interpolate between psi's and not inside the beam pattern function (speeds up the linear interpolation).
	# Last: 2011-06-26 - Changed name from 'get.psi0' to 'get.chi0'.
	# Last: 2011-08-19 - Changed name from 'get.chi0' to 'get.ssil' and added the input 'kL'.
	########### DESCRIPTION: ###########
	# Calculates the spherical surface integral of a beam pattern from a acoustic fish models at radius 1 meter, for the given reference frequency and reference length of the scoustic model.
	########## DEPENDENCIES: ###########
	# read.TSDs(), beamPattern.TSD(), integrateonsphere()
	############ VARIABLES: ############
	# ---case--- is the identifier of the case, given as a string holding the path of the directory of the TSD files required (empirical beam pattern file and ctd file).
	# ---f0--- is the reference frequency at which 'chi' should be calculated, defaulted to 38000 Hz.
	# ---L0--- is the reference length of the acoustic model, at which 'chi' should be calculated, defaulted to 32 cm fish of which 0.26 is swim bladder length (Gorska and Ona 2003).
	# ---pres--- is the desired presition of the integration (used in integrateonsphere()).
	# ---max.cells--- is the maximum number of cells in the grid (used in integrateonsphere()).
	# ---method--- is "closest" if the chi value of the closest grid point is to be selected, and "linear" if linear interpolation should be used to extract the chi value.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Read the relevant data:
	#data=read.case(var=c("ctd","pbpf","graf","gref","grsf","grif","dbpf","ebpf"),case=case)
	data=read.TSDs(case,var=c("asps","pbpf","graf","gref","grsf","grif","dbpf","ebpf"))
	# Set defaults if necessary variables are missing:
	if(length(data$asps)==0){
		data$asps=1500
		}
	if(length(data$pbpf)==0){
		data$pbpf="ls"
		}
	# Prepare the beam pattern function of the fish:
	data$bptf=beamPattern.TSD(data[c("pbpf","graf","gref","grsf","grif","dbpf","ebpf")])
	if(length(kL)==0){
		# Define the product of wavenumber and length of the acoustic model, for the reference frequency and length:
		kL=2*pi*f0/data$asps * L0
		}
		
	
	##### Execution and output #####
	# If empirical beam pattern is present, it may be a function of one or two angles:
	if(!length(data$ebpf)==0){
		chi=zeros(2)
		at_grsf=max(which(data$grsf<kL))+0:1
		dist=abs(kL-data$grsf[at_grsf])
		for(i in seq_along(at_grsf)){
			if(!length(data$graf)==0 && !length(data$gref)==0){
				chi[i]=integrateonsphere(function(x) data$bptf(list(dira=x[,1],dire=x[,2],wnsz=data$grsf[at_grsf[i]])),ndim=2,pres=pres,max.cells=max.cells,print=FALSE)$out
				}
			else if(!length(data$gref)==0){
				chi[i]=integrateonsphere(function(x) data$bptf(list(dire=x,wnsz=data$grsf[at_grsf[i]])),ndim=1,pres=pres,max.cells=max.cells,print=FALSE)$out
				}
			else{
				stop("Elevation grid angles \"gref\" is missing")
				}	
			}
		# Interpolate between the two values of 'chi':
		if(tolower(substring(method,1,1))=="c"){
			chi[which.min(dist)]
			}
		else if(tolower(substring(method,1,1))=="l"){
			chi[1]+diff(chi)*dist[1]/sum(dist)
			}
		else{
			stop("Invalid interpolation method")
			}
		}
	# Else use the parametric beam pattern function:
	else if(!length(data$pbpf)==0){
		integrateonsphere(function(x) data$bptf(list(dire=x,wnsz=kL)),ndim=1,pres=pres,max.cells=max.cells,print=FALSE)$out
		}
	else{
		stop("Beam pattern of the targets missing")
		}
	##################################################
	##################################################
	}
