#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom stats quantile
#'
#' @export
#' @rdname SX90.getSchools
#'
SX90.getSchools = function(data, Ttha, includeEdge = FALSE){
	
	# The naming convension used for the four character variable names in this function is as follows:
	# Character 1: Type of estimation method, here the SX90 sonar segmentation method, denoted by "X"
	# Character 2: Type of estimate over pings for the school:
	#	A: average
	#	X: max
	#	Q: quantile (90 %)
	# Character 3-4: Type of variable, where the first character may refer to an estimation type, such as 
	#	a: average
	#	d: median
	#	m: mean
	#	p: semi parametric Gumbel estimate
	#	q: quantile (90 %)
	#	x: max
	#	i: min
	#	v: smeared over valid volume/area
	
	# Function for extracting summary data for each school:
	out = list(Xtha=NULL,XaSv=NULL)
	schoolind = Ttha$indS
	if(!includeEdge){
		schoolind[Ttha$frcS<1] = NA
		}
	
	##### 1. Area (Xa[a]S): #####
	out$Xtha = vector("list", max(schoolind,na.rm=TRUE))
	if(length(data$Xtha)){
		# Split:
		XthaSplit = split(data$Xtha, schoolind)
		out$Xtha[as.numeric(names(XthaSplit))] = XthaSplit
		# Extract max, mean , quantile:
		out$XAha = sapply(out$Xtha, function(xx) if(length(xx)) mean(xx,na.rm=TRUE) else NA) # Average (horizontal) area of the school
		out$XXha = sapply(out$Xtha, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Maximum (horizontal) area of the school
		out$XQha = sapply(out$Xtha, function(xx) if(length(xx)) quantile(xx,0.9,na.rm=TRUE) else NA) # Quantile (90 %) of the (horizontal) area of the school
		}
	
	##### 2. Volume (Xa[v]S): #####
	out$Xtvl = vector("list", max(schoolind,na.rm=TRUE))
	if(length(data$Xtvl)){
		# Split:
		XtvlSplit = split(data$Xtvl, schoolind)
		out$Xtvl[as.numeric(names(XtvlSplit))] = XtvlSplit
		# Extract max, mean , quantile:
		out$XAvl = sapply(out$Xtvl, function(xx) if(length(xx)) mean(xx,na.rm=TRUE) else NA) # Average volume of the school
		out$XXvl = sapply(out$Xtvl, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Maximum volume of the school
		out$XQvl = sapply(out$Xtvl, function(xx) if(length(xx)) quantile(xx,0.9,na.rm=TRUE) else NA) # Quantile (90 %) of the volume of the school
		}
	
	##### 3. sv (Xa[s]S): #####
	out$Xasv = vector("list", max(schoolind,na.rm=TRUE))
	if(length(data$Xasv)){
		# Split:
		XasvSplit = split(data$Xasv, schoolind)
		out$Xasv[as.numeric(names(XasvSplit))] = XasvSplit
		# Extract max, mean , quantile:
		out$XAas = sapply(out$Xasv, function(xx) if(length(xx)) mean(xx,na.rm=TRUE) else NA) # Average sv of the school
		out$XXas = sapply(out$Xasv, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Maximum sv of the school
		out$XQas = sapply(out$Xasv, function(xx) if(length(xx)) quantile(xx,0.9,na.rm=TRUE) else NA) # Quantile (90 %) of the sv of the school
		}
	
	##### 4. bs (Xa[b]S): #####
	out$Xtbs = vector("list", max(schoolind,na.rm=TRUE))
	if(length(data$Xtbs)){
		# Split:
		XtbsSplit = split(data$Xtbs, schoolind)
		out$Xtbs[as.numeric(names(XtbsSplit))] = XtbsSplit
		# Extract max, mean , quantile:
		out$XAbs = sapply(out$Xtbs, function(xx) if(length(xx)) mean(xx,na.rm=TRUE) else NA) # Average backscattering cross section of the school
		out$XXbs = sapply(out$Xtbs, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Maximum backscattering cross section of the school
		out$XQbs = sapply(out$Xtbs, function(xx) if(length(xx)) quantile(xx,0.9,na.rm=TRUE) else NA) # Quantile (90 %) of the backscattering cross section of the school
		}
	
	##### 5. sbr (Xa[w]S) Here w is used as linear signal to background ratio: #####
	out$Xsbr = vector("list", max(schoolind,na.rm=TRUE))
	if(length(data$Xsbr)){
		# Split:
		XsbrSplit = split(data$Xsbr, schoolind)
		out$Xsbr[as.numeric(names(XsbrSplit))] = XsbrSplit
		# Extract max, mean , quantile:
		out$XAbr = sapply(out$Xsbr, function(xx) if(length(xx)) mean(xx,na.rm=TRUE) else NA) # Average SBR of the school
		out$XXbr = sapply(out$Xsbr, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Maximum SBR of the school
		out$XQbr = sapply(out$Xsbr, function(xx) if(length(xx)) quantile(xx,0.9,na.rm=TRUE) else NA) # Quantile (90 %) of the SBR of the school
		}
	
	##### 6. Theoretical area (Tt[a]S): #####
	out$Ttha = vector("list", max(schoolind,na.rm=TRUE))
	if(length(Ttha$Ttha)){
		# Split:
		TthaSplit = split(Ttha$Ttha, schoolind)
		out$Ttha[as.numeric(names(TthaSplit))] = TthaSplit
		# Extract max:
		out$TAha = sapply(out$Ttha, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Theoretical maximum (horizontal) area of the school
		}
	
	##### 7. range (Xa[r]S): #####
	out$Xhra = vector("list", max(schoolind,na.rm=TRUE))
	if(length(data$Xhra)){
		# Split:
		XhrSSplit = split(data$Xhra, schoolind)
		out$Xhra[as.numeric(names(XhrSSplit))] = XhrSSplit
		# Extract max, mean , quantile:
		out$Xahr = sapply(out$Xhra, function(xx) if(length(xx)) mean(xx,na.rm=TRUE) else NA) # Average (horizontal) range to the school
		out$Xxhr = sapply(out$Xhra, function(xx) if(length(xx)) max(xx,na.rm=TRUE) else NA) # Maximum (horizontal) range to the school
		out$XQhr = sapply(out$Xhra, function(xx) if(length(xx)) quantile(xx,0.9,na.rm=TRUE) else NA) # Quantile (90 %) of the (horizontal) range to the school
		}
	
	# To logarithms:
	##### 3. Sv (Xa[S]S): #####
	if(length(out$XAas)){
		out$XAaS = 10*log10(out$XAas)
		}
	if(length(out$XXas)){
		out$XXaS = 10*log10(out$XXas)
		}
	if(length(out$XQas)){
		out$XQaS = 10*log10(out$XQas)
		}
	
	##### 4. TS (Xa[T]S): #####
	if(length(out$XAbs)){
		out$XATS = 10*log10(out$XAbs)
		}
	if(length(out$XXbs)){
		out$XXTS = 10*log10(out$XXbs)
		}
	if(length(out$XQbs)){
		out$XQTS = 10*log10(out$XQbs)
		}
	
	##### 5. SBR (XA[R]S) Here R is used as signal to background Ratio: #####
	if(length(out$XAbr)){
		out$XABR = 10*log10(out$XAbr)
		}
	if(length(out$XXbr)){
		out$XXBR = 10*log10(out$XXbr)
		}
	if(length(out$XQbr)){
		out$XQBR = 10*log10(out$XQbr)
		}
	
	# Return the list of non-empty variables:
	out[sapply(out,length)>0]
	}
