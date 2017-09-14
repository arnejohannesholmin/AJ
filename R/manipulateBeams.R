#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD read.TSD write.TSD
#'
#' @export
#' @rdname manipulateBeams
#'

# Create a small function for reading and manipulating beam configuration data for a fishery sonar. This in only used in the script "SX90_BlindZone.R":
manipulateBeams = function(reference_beamsFile, outfile, bmmd=0, numt=1, utim=NULL, maxRange=NULL, tilt=NULL, plsl=NULL){
	# Read beam configuration and select the desired beam mode:
	bmmd = read.TSD(reference_beamsFile, t="all", var="bmmd")$bmmd
	if(length(bmmd)){
		beams = read.TSD(reference_beamsFile, t=which(bmmd==0)[1], header=FALSE)
		}
	else{
		beams = read.TSD(reference_beamsFile, t=1, header=FALSE)
		}
	# Repeat for all time steps:
	beams = lapply(beams, function(xx) if(length(xx)==0) NULL else matrix(rep(xx, numt), ncol=numt))
	
	# Change tilt:
	if(length(tilt)){
		beams$dirx = array(rep(tilt, length.out=length(beams$dirx)), dim=dim(beams$dirx))
		beams$dire = pi/2 + (beams$dirx)*pi/180
		}
	# Change pulse length to 6 ms, which corresponds to 450 m maximum range (see "SIMRAD SX90 Pulse length table.pdf"):
	if(length(plsl)){
		beams$plsl = array(rep(plsl, length.out=length(beams$plsl)), dim=dim(beams$plsl))
		}
	
	# Change length of the beams:
	if(length(maxRange)){
		beams$lenb = array(rep(ceiling(maxRange / (beams$sint*beams$asps/2)), length.out=length(beams$lenb)), dim=dim(beams$lenb))
		}
	
	# Change length of the beams:
	if(length(utim)){
		beams$utim = rep(utim, l=numt)
		}

	# Write the beam ocnfiguration file:
	write.TSD(beams, outfile, numt=numt)
	}
