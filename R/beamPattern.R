#*********************************************
#*********************************************
#' Returns a function for the beam pattern of an acoustic source. The function beamPattern() has 3 methods depending on the type of the input object 'data'. Inputs are prioritized as (1) functions, (2) NULL, (3) empirical tables, (4) character strings naming functions.
#'
#' @param data  may be of 3 different types.
#' @param method  is "closest" if the beam pattern value of the closest grid point is to be selected, and "linear" if linear interpolation should be used to extract the beam pattern value (time demanding and only available for 2D grids).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD mod
#' @importFrom fBasics linearInterpp
#'
#' @export
#' @rdname beamPattern
#'
beamPattern<-function(data="circularPiston",method=c("closest","linear")){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-10-19 - Clean version.
	# Update: 2010-01-25 - Simplified version, dropping the file input, as the empirical beam patterns are implemented in the TSD-file format.
	########### DESCRIPTION: ###########
	# Returns a function for the beam pattern of an acoustic source. The function beamPattern() has 3 methods depending on the type of the input object 'data'. Inputs are prioritized as (1) functions, (2) NULL, (3) empirical tables, (4) character strings naming functions.
	########## DEPENDENCIES: ###########
	# circularPiston(), lineSource()
	############ VARIABLES: ############
	# ---data--- may be of 3 different types.
	#
	#
	#	(1)	a string representing the name of one of a set of predefined functions:
	#	
	#		(I)		"circularPiston" - circularPiston(ang,kb)
	#					'ang' is the inceidence angle to the circular piston.
	#					'kb' is the relative size of the circular piston, represented by the product of wave number of the sound and radius of the circular piston.
	#
	#		(II)	"quadraticPiston" (not implemented)
	#		
	#		(III) 	"lineSource" - lineSource(ang,kL)
	#					'ang' is the inceidence angle to the line source.
	#					'kL' is the relative size of the line source, represented by the product of wave number of the sound and length of the line source.
	#
	#		(IV) 	"ellipticSource" (not implemented))
	#		
	#		(V) 	"pointSource" - returning ones at the same dimension as the inputs.
	#		
	#
	#	(2)	a function of 2 or 3 arguments representing the beam pattern of a source:
	#
	#		(I) 	2 arguments:
	#					'ang' is the angle of incidence to the source.
	#					'kS' is the relative size of the source.
	#
	#		(II) 	3 arguments:
	#					'theta' and 'phi' are the angles of incidence to the source.
	#					'kS' is the relative size of the source.
	#
	#
	#	(3) a list representing the empirical beam pattern of a source in intervals or at points of the arguments. Names for the elements of the list adopted from read.TSD(). Asterix "*" is either "f", "1" or "2", representing the school (fish), the echo sounder at emission, or the echo sounder at reception:
	#
	#		[['pbp*']] - The parametric beam pattern of the source as a function of one or two angle variables and a variable representing the relative size of the source. Given either as a function or as the name of a predefined function (one of "pointSource", "lineSource" or "circularPiston").
	#		[['gra*']] - A vector of arbitrary length representing the grid vector of azimuth angle to the source. Only required if empirical beam pattern 'ebp*' as a function of two angles of direction is given.
	#		[['gre*']] - A vector of arbitrary length representing the grid vector of elevation (incidence) angle to the source. Must be given along with the empirical beam pattern 'ebp*'.
	#		[['grs*']] - A vector of arbitrary length representing the grid vector of size of the source relative to the wavelength, represented by the product of size of the source and wave number.
	#		[['gri*']] - A vector of arbitrary length representing the grid numbering index of the sources.
	#		[['dbp*']] - A vector representing the dimension of the empirical beam pattern.
	#		[['ebp*']] - An array of dimension no less than c(length(data$graf), length(data$gref), length(data$rszf)) representing the empirical beam pattern values of the fish.
	# ---method--- is "closest" if the beam pattern value of the closest grid point is to be selected, and "linear" if linear interpolation should be used to extract the beam pattern value (time demanding and only available for 2D grids).
		

	##################################################
	##################################################
	##### Preparation, execution #####
	# 'mod' gives the mode of the function, which is whether one or two angles of direction are used ('phi' or 'theta' and 'phi'), and whether the product 'ka' of wave number and size or the numbering index 'ind' is used:
	#	mod	inputs
	#	1	phi,ka
	#	2	phi,ind
	#	3	theta,phi,ka
	#	4	theta,phi,ind
	mod=1
	
	# (1) If 'data' is given as a function, this funciton is returned with the requirement that the number of arguments to the function is 3:
	if(is.function(data)){
		# Checking the number of arguments to the function given by 'data':
		nargs=length(formals(data))
		if(nargs %in% 2:3){
			fun=data
			if(nargs==3){
				mod=3
				}
			}
		else{
			stop("The function given by 'data' must take 2 or 3 arguments (one or two angles and the relative size of the source)")
			}
		}	
		
	# (0) If data==NULL, an array of ones having the same dimension as 'ang' is returned:
	else if(is.null(data) || sum(grep("pointsource",tolower(data),ignore.case=TRUE))>0){
		fun=pointSource
		}
	
	# (2) If 'data' is is given as a list defining the empirical beam pattern given incidence angle, wave number and size of the source:
	else if(is.list(data)){
		
		nameroot=substr(names(data),1,3)
		object=substr(names(data),4,4)
		if(!all(object==object[1],na.rm=TRUE)){
			stop("Data not from the same category (\"school\", \"1\" (emission), \"2\" (reception))")
			}
		names(data)<-nameroot
		
		# Empirical beam patterns overrides parametric beam patterns:
		if(!is.null(data$pbp) && is.null(data$ebp)){
			data=data$pbp
			if(is.function(data)){
				# Checking the number of arguments to the function given by 'data':
				nargs=length(formals(data))
				if(nargs %in% 2:3){
					fun=data
					if(nargs==3){
						mod=3
						}
					}
				else{
					stop("The function given by 'data' must take 2 or 3 arguments (one or two angles and the relative size of the source)")
					}
				}
			}
		else if(!is.null(data$ebp)){
			if(!is.null(data$pbp)){
				warning("Both parametric and empirical beam pattern present in 'data'. Empirical chosen")
				}
			# The dimension of the empirical beam pattern:
			if(is.null(data$dbp)){
				data$dbp=dim(data$ebp)
				}
			else{
				dim(data$ebp)=data$dbp
				}
			ndbp=length(data$dbp)
			# Error handling:
			# The empirical beam pattern need to be two or three dimensional array:
			if(ndbp<2 || ndbp>3){
				stop("Invalid dimension of the empirical beam pattern (must be 2d or 3d)")
				}
			# Two dimensions implies one angle of direction:
			else if(ndbp==2){
				if(is.null(data$gre)){
					stop("'data$gre*' (incidence angle to the source) missing with no default")
					}
				input=list(data$gre)
				linput=2
				}
			# Three dimensions implies two angles of direction:
			else if(ndbp==3){
				if(any(is.null(data$gra),is.null(data$gre))){
					stop("Both 'data$gra*' and 'data$gre*' must be given")
					}
				input=list(data$gra,data$gre)
				linput=3
				mod=3
				}
			# Relative size grid vector has precedence over numbering index grid vector:
			if(!is.null(data$grs)){
				input[[linput]]=data$grs
				}
			else if(!is.null(data$gri)){
				input[[linput]]=data$gri
				mod=mod+1
				}
			else{
				stop("One of 'data$grs' and 'data$gri' must be given")
				}
			
			# Execution:
			if(any(sapply(input,is.unsorted))){
				stop("The grid vectors need to be sorted increasingly")
				}
			lengths=unlist(lapply(input,length))
			# The lengths of the explanatory variables should be equal to or one larger than the dimensions of the response variable:
			lendiff=lengths-data$dbp
			if(!all(lendiff %in% 0:1)){
				warning("Lengths of explanatory variables should be equal to or one larger than the dimensions of the response variable")
				}
			
			if(method[1]=="closest" || ndbp==3){
				fun=function(...){
					# Constructing the argument column matrix:
					ang=cbind(...)
					# Finding the indexes of the input arguments in the grid given by 'data':
					for(i in 1:ncol(ang)){
						# If the length of the grid variable equals the corrsponding dimension of the empirical beampattern, the closest grid point is selected, rounding 0.5 up to 1:
						if(lendiff[i]==0){
							ang[,i]=findInterval(ang[,i],c(-Inf,input[[i]][-length(input[[i]])]+diff(input[[i]])/2,Inf),rightmost.closed=TRUE,all.inside=TRUE)
							}
						else{
							ang[,i]=findInterval(ang[,i],input[[i]],all.inside=TRUE,rightmost.closed=TRUE)
							}
						}
					# Extracting the beam pattern values:
					data$ebp[ang]
					}
				}
			else if(method[1]=="linear"){
				input=expand.grid(input)
				fun=function(...){
					# Constructing the argument column matrix:
					ang=cbind(...)
					# outside = ang[,1]<input[[1]][1] & input[[1]][length(input[[1]])]<ang[,1] & ang[,2]<input[[2]][1] & input[[2]][length(input[[2]])]<ang[,2]
					# Finding the indexes of the input arguments in the grid given by 'data':
					fBasics::linearInterpp(input[[1]], input[[2]], data$ebp, ang[,1], ang[,2])
					}
				}
			else{
				stop("Wrong 'method'")
				}
			}
		else{
			if(object[1]=="f"){
				warning("Parametric and empirical beam pattern function missing and defaulted to \"lineSource\"")
				fun=lineSource
				}
			else if(object[1] %in% c("1","2")){
				warning("Parametric and empirical beam pattern function missing and defaulted to \"circularPiston\"")
				fun=circularPiston
				}
			}
		}
	
	# (3) If 'data' is given as a character string, it needs to match one of the names of predefined beampattern functions:
	if(is.character(data)){
		# Choosing the function specified by the user:
		if(sum(grep("circularPiston_ellipticRadius",tolower(data),ignore.case=TRUE))>0){
			fun=circularPiston_ellipticRadius
			mod=3
			}
		else if(sum(grep("circularpiston",tolower(data),ignore.case=TRUE))>0){
			fun=circularPiston
			}
		else if(sum(grep("linesource",tolower(data),ignore.case=TRUE))>0){
			fun=lineSource
			}
		else if(sum(grep("pointsource",tolower(data),ignore.case=TRUE))>0){
			fun=pointSource
			}
		else{
			stop("'data' does not represent any predefined function. Legal values are 'circularPiston' and 'lineSource'")
			}
		}
	
		
	##### Output #####
	list(fun=fun,mod=mod)
	##################################################
	##################################################
	}
