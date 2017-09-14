#*********************************************
#*********************************************
#' ---pos--- is a string representing the name of a directory containing the positions to plot located in .school files, if previously generated. If pos==NULL, positions are generated from the event.
#' ---cruise--- is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
#' ---event--- is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' ---acca--- is the acoustic scattering cross sectional area (assuming omnidirectional targets).
#' ---posout--- specifies the directory in which to put the files containing the regenerated points. If posout==FALSE, no position files are saved, if posout==TRUE or posout==NULL TSD files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'posout' is a character string, a directory named accordingly is created and TSD files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' ---imgout--- specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' ---names_img--- is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
#' ---names_pos--- is a string representing the names of the .school files (excluding numbering and the file extension ".school"). As an example names_pos="regen" and length(t)<1000 result in the names "regen001.school", "regen002.school", ..., but only some of the file names will be used, depending on the size limit  of the files.
#' ---fmt--- is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
#' ---t1--- is the start index for the numbers included in the image names and .school file names. If t1=123, the first image file will be named "..../sv2pos/acca___/png/frame123.png", for the default values of 'fmt' and 'names_img'.
#' ---ndigits--- is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
#' ---filesize--- is the maximum size of the .school files (only used if pos==NULL).
#' ---ind--- is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' ---range--- is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
#' ---subset--- is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' ---xyzlim--- is either a matrix of dimension [2 x 3] where the columns represent the xlim, ylim and zlim of the 3D plot, or an interger index number of the ping from which ranges the xlim, ylim and zlim are set .
#' ---ideal--- is TRUE to represent the simple case where the speed of sound 'data$asps' is invariant of depth.
#' ---cs--- is either "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' ---allert--- is the limit of the number of points inducing a warning asking the user to continue.
#' ---adds--- is an optional list of variables overriding the variables in 'data'.
#' ---esnm--- is the names of the echo sounder, given as a four character string. Currently implemented is "MS70" (may be given in lover case)
#' ---TVG--- is FALSE if no Time Varied Gain compensation is to be performed on the acoustic data.
#' ---seabed--- is the depth of the seabed, at which the beams are reflected when calculating the midpoints of voxels.
#' ---ignoreheave--- see soundbeam.MS70().
#' ---rot--- see soundbeam.MS70().
#' ---compensation--- is a vector of string giving which rotation values that are compensated for in the sonar. Only c("pitch","roll") is available for the current version. Used in soundbeam.MS70.
#' ---origin--- is a vector of two elements representing the origin of the global coordinate system (G), or the numbering index of the ping regarded as the origin of (G) (ignoring heave so that the x-y-plane of (G) is on the surface of the sea).
#' ---dir.data--- is the path to the directory in which the projects are stored (see read.event ).
#' ---Pa--- is TRUE if pressure data are to be returned in Pascal.
#' ---bgns--- is FALSE if estimated background noise is to be subtracted from the data, resulting in some negative sv-values.
#' ---fun--- is the name of the funciton used for obtaining the number of points in each voxel by "rounding off" to integers. The string given in 'fun' must be the name of a function taking 'x' as the input, such as "floor", "round", "ceiling" and "mod", where the last function is defined in sv2pos() and draws between floor(x) and ceiling(x) with probability P(ceiling(x))=x%%1.
#' ---schoolcol--- is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' ---schoolsize--- is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' ---schoolsample--- is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
#' ---clock--- is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
#' NA
#' ---view--- defines the view point of the user, either given as FALSE/NULL for interactive selection of the view point, a 4x4 matrix representing the 'userMatrix' used in rgl.viewpoint() in the "rgl" package, or a string representing one of a set of predefined viewpoints:
#' NA
#' ---zoom--- is the zoom value of the plot, utilized in rgl.viewpoint().
#' ---asp3d--- is used to set the dimension of the plotting frame (see aspect3d())
#' ---school--- is either a logical indicating whether fish positions stored in .school files present in the event specified by 'event' (and 'cruise' and 'dir.data') are to be plotted colored 'col.school' and sized 'size.school', or a string representing the directory of the .school files to read fish positions from.
#' ---add--- is TRUE if points are to be added to an existing plot.
#' ---...--- are inputs passed on to plot3d().
#' NA
#'
#' @param pos  is a string representing the name of a directory containing the positions to plot located in .school files, if previously generated. If pos==NULL, positions are generated from the event.
#' @param cruise  is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param acca  is the acoustic scattering cross sectional area (assuming omnidirectional targets).
#' @param posout  specifies the directory in which to put the files containing the regenerated points. If posout==FALSE, no position files are saved, if posout==TRUE or posout==NULL TSD files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'posout' is a character string, a directory named accordingly is created and TSD files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' @param imgout  specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' @param names_img  is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
#' @param names_pos  is a string representing the names of the .school files (excluding numbering and the file extension ".school"). As an example names_pos="regen" and length(t)<1000 result in the names "regen001.school", "regen002.school", ..., but only some of the file names will be used, depending on the size limit  of the files.
#' @param fmt  is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
#' @param t1  is the start index for the numbers included in the image names and .school file names. If t1=123, the first image file will be named "..../sv2pos/acca___/png/frame123.png", for the default values of 'fmt' and 'names_img'.
#' @param ndigits  is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
#' @param filesize  is the maximum size of the .school files (only used if pos==NULL).
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param range  is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param xyzlim  is either a matrix of dimension [2 x 3] where the columns represent the xlim, ylim and zlim of the 3D plot, or an interger index number of the ping from which ranges the xlim, ylim and zlim are set .
#' @param ideal  is TRUE to represent the simple case where the speed of sound 'data$asps' is invariant of depth.
#' @param cs  is either "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' @param allert  is the limit of the number of points inducing a warning asking the user to continue.
#' @param adds  is an optional list of variables overriding the variables in 'data'.
#' @param esnm  is the names of the echo sounder, given as a four character string. Currently implemented is "MS70" (may be given in lover case)
#' @param TVG  is FALSE if no Time Varied Gain compensation is to be performed on the acoustic data.
#' @param seabed  is the depth of the seabed, at which the beams are reflected when calculating the midpoints of voxels.
#' @param ignoreheave  see soundbeam.MS70().
#' @param rot  see soundbeam.MS70().
#' @param compensation  is a vector of string giving which rotation values that are compensated for in the sonar. Only c("pitch","roll") is available for the current version. Used in soundbeam.MS70.
#' @param origin  is a vector of two elements representing the origin of the global coordinate system (G), or the numbering index of the ping regarded as the origin of (G) (ignoring heave so that the x-y-plane of (G) is on the surface of the sea).
#' @param dir.data  is the path to the directory in which the projects are stored (see read.event ).
#' @param Pa  is TRUE if pressure data are to be returned in Pascal.
#' @param bgns  is FALSE if estimated background noise is to be subtracted from the data, resulting in some negative sv-values.
#' @param fun  is the name of the funciton used for obtaining the number of points in each voxel by "rounding off" to integers. The string given in 'fun' must be the name of a function taking 'x' as the input, such as "floor", "round", "ceiling" and "mod", where the last function is defined in sv2pos() and draws between floor(x) and ceiling(x) with probability P(ceiling(x))=x%%1.
#' @param schoolcol  is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsize  is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsample  is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
#' @param clock  is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
#' @param view  defines the view point of the user, either given as FALSE/NULL for interactive selection of the view point, a 4x4 matrix representing the 'userMatrix' used in rgl.viewpoint() in the "rgl" package, or a string representing one of a set of predefined viewpoints:
#' @param zoom  is the zoom value of the plot, utilized in rgl.viewpoint().
#' @param asp3d  is used to set the dimension of the plotting frame (see aspect3d())
#' @param school  is either a logical indicating whether fish positions stored in .school files present in the event specified by 'event' (and 'cruise' and 'dir.data') are to be plotted colored 'col.school' and sized 'size.school', or a string representing the directory of the .school files to read fish positions from.
#' @param add  is TRUE if points are to be added to an existing plot.
#' @param ...  are inputs passed on to plot3d().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl rgl.viewpoint
#'
#' @importFrom rgl aspect3d lines3d par3d plot3d points3d rgl.postscript rgl.snapshot rgl.viewpoint text3d
#' @importFrom sonR read.event roty
#' @importFrom TSD s2dhms sph2car write.TSD zeros
#' @importFrom stats runif
#'
#' @export
#' @rdname animate.event_3d
#'
animate.event_3d <- function(pos=NULL, cruise=2009116, event=1, t=1, acca=1e-2, posout=NULL, imgout=NULL, names_img="frame", names_pos="regen", fmt="png", t1=1, ndigits=NULL, filesize=3e8, ind=list(-(1:100),NULL), range=list(), subset=NULL, xyzlim=1, ideal=TRUE, cs="g", allert=1e7, adds=NULL, esnm="MS70", TVG=TRUE, seabed=-500, ignoreheave=TRUE, rot=2, compensation=c("pitch","roll"), origin=1, dir.data=NULL, Pa=TRUE, bgns=TRUE, fun="floor", schoolcol="red", schoolsize=0.3, schoolsample=1, clock="bbl", view=c("top","bottom","south","west","north","east"), zoom=0.7, asp3d="iso", school=FALSE, add=FALSE, xlab="x", ylab="y", zlab="z", sonar.grid=TRUE, clean=FALSE, focus=-200, stereoang=0.01, colleft="red", colright="blue", force=FALSE,...){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-05-08 - Clean version.
	# Last: 2010-07-06 - Canged to be more dynamic when it comes to writing images from already regenerated fish positions, intruducing the options 'posout' and 'imgout'.
	########### DESCRIPTION: ###########
	# ---pos--- is a string representing the name of a directory containing the positions to plot located in .school files, if previously generated. If pos==NULL, positions are generated from the event.
	# ---cruise--- is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
	# ---event--- is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
	# ---acca--- is the acoustic scattering cross sectional area (assuming omnidirectional targets).
	# ---posout--- specifies the directory in which to put the files containing the regenerated points. If posout==FALSE, no position files are saved, if posout==TRUE or posout==NULL TSD files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'posout' is a character string, a directory named accordingly is created and TSD files stored. If already existing, the used is asked to confirm overwriting the existing files.
	# ---imgout--- specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
	# ---names_img--- is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
	# ---names_pos--- is a string representing the names of the .school files (excluding numbering and the file extension ".school"). As an example names_pos="regen" and length(t)<1000 result in the names "regen001.school", "regen002.school", ..., but only some of the file names will be used, depending on the size limit  of the files.
	# ---fmt--- is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
	# ---t1--- is the start index for the numbers included in the image names and .school file names. If t1=123, the first image file will be named "..../sv2pos/acca___/png/frame123.png", for the default values of 'fmt' and 'names_img'.
	# ---ndigits--- is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
	# ---filesize--- is the maximum size of the .school files (only used if pos==NULL).
	# ---ind--- is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
	# ---range--- is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
	# ---subset--- is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
	# ---xyzlim--- is either a matrix of dimension [2 x 3] where the columns represent the xlim, ylim and zlim of the 3D plot, or an interger index number of the ping from which ranges the xlim, ylim and zlim are set .
	# ---ideal--- is TRUE to represent the simple case where the speed of sound 'data$asps' is invariant of depth.
	# ---cs--- is either "g" or "v" representing the global coordinate and the coordinate system of the vessel.
	# ---allert--- is the limit of the number of points inducing a warning asking the user to continue.
	# ---adds--- is an optional list of variables overriding the variables in 'data'.
	# ---esnm--- is the names of the echo sounder, given as a four character string. Currently implemented is "MS70" (may be given in lover case)
	# ---TVG--- is FALSE if no Time Varied Gain compensation is to be performed on the acoustic data.
	# ---seabed--- is the depth of the seabed, at which the beams are reflected when calculating the midpoints of voxels.
	# ---ignoreheave--- see soundbeam.MS70().
	# ---rot--- see soundbeam.MS70().
	# ---compensation--- is a vector of string giving which rotation values that are compensated for in the sonar. Only c("pitch","roll") is available for the current version. Used in soundbeam.MS70.
	# ---origin--- is a vector of two elements representing the origin of the global coordinate system (G), or the numbering index of the ping regarded as the origin of (G) (ignoring heave so that the x-y-plane of (G) is on the surface of the sea).
	# ---dir.data--- is the path to the directory in which the projects are stored (see read.event ).
	# ---Pa--- is TRUE if pressure data are to be returned in Pascal.
	# ---bgns--- is FALSE if estimated background noise is to be subtracted from the data, resulting in some negative sv-values.
	# ---fun--- is the name of the funciton used for obtaining the number of points in each voxel by "rounding off" to integers. The string given in 'fun' must be the name of a function taking 'x' as the input, such as "floor", "round", "ceiling" and "mod", where the last function is defined in sv2pos() and draws between floor(x) and ceiling(x) with probability P(ceiling(x))=x%%1.
	# ---schoolcol--- is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
	# ---schoolsize--- is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
	# ---schoolsample--- is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
	# ---clock--- is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
	#		"bbl" = bottom (minimum of z) bottom (minimum of y) left (minimum of x)
	#		"bbr" = bottom (minimum of z) bottom (minimum of y) right (maximum of x)
	#		"btl" = bottom (minimum of z) top (maximum of y) left (minimum of x)
	#		"btr" = bottom (minimum of z) top (maximum of y) right (maximum of x)
	#		"tbl" = top (maximum of z) bottom (minimum of y) left (minimum of x)
	#		"tbr" = top (maximum of z) bottom (minimum of y) right (maximum of x)
	#		"ttl" = top (maximum of z) top (maximum of y) left (minimum of x)
	#		"ttr" = top (maximum of z) top (maximum of y) right (maximum of x)
	# ---view--- defines the view point of the user, either given as FALSE/NULL for interactive selection of the view point, a 4x4 matrix representing the 'userMatrix' used in rgl.viewpoint() in the "rgl" package, or a string representing one of a set of predefined viewpoints:
	#		"t" = Plot seen from above with north upwards in the plot.
	#		"b" = Plot seen from below with north downwards in the plot.
	#		"s" = Plot seen from the south.
	#		"w" = Plot seen from the west.
	#		"n" = Plot seen from the north.
	#		"e" = Plot seen from the east.
	# ---zoom--- is the zoom value of the plot, utilized in rgl.viewpoint().
	# ---asp3d--- is used to set the dimension of the plotting frame (see aspect3d())
	# ---school--- is either a logical indicating whether fish positions stored in .school files present in the event specified by 'event' (and 'cruise' and 'dir.data') are to be plotted colored 'col.school' and sized 'size.school', or a string representing the directory of the .school files to read fish positions from.
	# ---add--- is TRUE if points are to be added to an existing plot.
	# ---...--- are inputs passed on to plot3d().
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(tolower(esnm)!="ms70"){
		stop("Only the MS70 sonar is implemented for this function")
		}
	# If 'pos' is a character string pointing to a directory containing previously generated fish position data, 'posout' is set to FALSE:
	if(is.character(pos)){
		posout=FALSE
		}
	
	### A simple but unelegant way of plotting a projection of the sonar volume onto the top horizontal plane:
	plot_volume_projection=function(data,xyzlim,length_edges=1000,bowdens=100,col_lines="orange",col_bows="orange"){
		# Radial, azimuth and elevation partitioning:
		R=seq(0,data$asps*data$sint/2*(max(data$lenb)-1),l=length_edges)
		dtheta=abs(median(diff(unique(data$dira))))
		theta=c(sort(unique(data$dira))-dtheta/2,max(data$dira)+dtheta/2)+data$rtzv
		# Define points along the edges of the beams:
		edges=zeros(length_edges,length(theta),3)
		edges[,,1]=data$psxv+outer(R,cos(theta))
		edges[,,2]=data$psyv+outer(R,sin(theta))
		edges[,,3]=xyzlim[6]
		# Select the points inside the plotting volume:
		inside=xyzlim[1]<edges[,,1] & xyzlim[2]>edges[,,1] & xyzlim[3]<edges[,,2] & xyzlim[4]>edges[,,2]
		edges[,,1][!inside]=NA
		edges[,,2][!inside]=NA
		# Plot the lines:
		for(i in seq_along(theta)){
			# Only plot if the bow has more than one non-NA point:
			if(sum(!apply(edges[,i,],1,function(x) any(is.na(x))))>1){
				lines3d(edges[,i,],col=col_lines)
				}
			}
		# Define points along bows at distances separated d by 'bowdens':
		bows=zeros(length(theta),ceiling(max(R)/bowdens),3)
		for(i in seq_along(bows[1,,1])){
			bows[,i,]=matrix(c(data$psxv,data$psyv,xyzlim[6]),ncol=3,nrow=length(theta),byrow=TRUE)+sph2car(cbind(bowdens*i,theta,pi/2))
			}
		# Select the points inside the plotting volume:
		inside=xyzlim[1]<bows[,,1] & xyzlim[2]>bows[,,1] & xyzlim[3]<bows[,,2] & xyzlim[4]>bows[,,2]
		bows[,,1][!inside]=NA
		bows[,,2][!inside]=NA
		# Plot the bows:
		for(i in seq_along(bows[1,,1])){
			# Only plot if the bow has more than one non-NA point:
			if(sum(!apply(bows[,i,],1,function(x) any(is.na(x))))>1){
				lines3d(bows[,i,],col=col_bows)
				}		
			}
		}
	
	
	##### Execution and output #####
	# Get the time vector used in the names of the directories:
	t_dirname=read.event(cruise=cruise,event=event,var="indt",t=t)$indt
	# Create the names of the frames:
	tchar=paste("00000000",seq_along(t_dirname)+t1-1,sep="")
	if(is.null(ndigits) || ndigits==0 || ndigits<nchar(length(t_dirname))){
		ndigits=nchar(length(t_dirname))
		}
	
	# 'posout', the directory to which the regenerated points are written:
	if(!identical(posout,FALSE)){
		if(is.null(posout) || isTRUE(posout)){
			posout=file.path(dirname(read.event(cruise=cruise,event=event,dir.out=TRUE)),"sv2pos",paste("acca_",format(acca,scientific=TRUE),"_indt_",t_dirname[1],"__",tail(t_dirname,1),sep=""),"tsd")
			}
		names_pos=paste(posout,"/",names_pos,substring(tchar,nchar(tchar)-ndigits+1),".school",sep="")
		
		if(!file.exists(as.character(posout))){
			suppressWarnings(dir.create(posout,recursive=TRUE))
			}
		#else if(length(list.files(posout))>0){
		else if(any(basename(list.files(posout)) %in% basename(names_pos)) && !force){
			answer=readline(paste("Regenerated fish position data located in \"",posout,"\". Overwrite (y/n) \n\n",sep=""))
			if(answer!="y"){
				posout=FALSE
				}
			}
		}
	
	# 'imgout', the directory to which the images of regenerated points are written:
	if(!identical(imgout,FALSE)){
		if(is.null(imgout) || isTRUE(imgout)){
			imgout=file.path(dirname(read.event(cruise=cruise,event=event,dir.out=TRUE)),"sv2pos",paste("acca_",format(acca,scientific=TRUE),"_indt_",t_dirname[1],"__",tail(t_dirname,1),sep=""),fmt)
			}
		names_img=paste(imgout,"/",names_img,substring(tchar,nchar(tchar)-ndigits+1),".",fmt,sep="")
	
		if(!file.exists(as.character(imgout))){
			suppressWarnings(dir.create(imgout,recursive=TRUE))
			}
		#else if(length(list.files(imgout))>0){
		else if(any(basename(list.files(imgout)) %in% basename(names_img)) && !force){
			answer=readline(paste("Images of regenerated fish position data located in \"",imgout,"\". Overwrite (y/n) \n\n",sep=""))
			if(answer!="y"){
				imgout=FALSE
				}
			}
		}
	
	
	# Read the first ping:
	thisposred=NULL
	thisschool=NULL
	if(is.character(pos)){
		time=read.event(event=pos,t=t,var="time")
		if(length(time$indt)<length(t) && t!="all"){
			stop(paste("Time step ",t," not present in ",pos,"\n",sep=""))
			}
		t=time$indt
		utim=unlist(time$utim)
		data=read.event(cruise=cruise,event=event,t=t[1],var=c("volx","ctd","beams","vessel","time"),ideal=ideal,esnm="MS70",TVG=TVG,seabed=seabed,ignoreheave=ignoreheave,rot=rot,compensation=compensation,origin=origin,dir.data=dir.data,Pa=Pa,bgns=bgns)
		# Read the previously regenerated points:
		thisposred=read.event(event=pos,var="school",t=t[1])
		thisposred=cbind(thisposred$psxf,thisposred$psyf,thisposred$pszf)
		}
	if(is.null(thisposred)){
		# Get the time variable from 't':
		time=read.event(cruise=cruise,event=event,t=t,var="time")
		t=time$indt
		utim=unlist(time$utim)
		ppp=proc.time()
		data=read.event(cruise=cruise,event=event,t=t[1],var=c("vbsc","voxels","ctd","beams","vessel","time"),ideal=ideal,esnm="MS70",TVG=TVG,seabed=seabed,ignoreheave=ignoreheave,rot=rot,compensation=compensation,origin=origin,dir.data=dir.data,Pa=Pa,bgns=bgns)
		# Regenerate the school for the first ping:
		thisposred=sv2pos.MS70(data=c(adds,data),fun=fun,ind=ind,range=range,subset=subset,acca=acca,ideal=ideal,cs=cs,allert=allert,plot=FALSE)$p1
		}
	if(is.null(xyzlim) && length(thisposred)==0){
		stop("Current ping is empty. Use 'xyzlim' to set valid plotting frame")
		}

	if(!identical(imgout,FALSE)){
		if(isTRUE(school)){
			thisschool=read.event(cruise=cruise,event=event,t=t[1],var="school",dir.data=dir.data)
			thisschool=data.frame(thisschool$psxf,thisschool$psyf,thisschool$pszf)
			thisschool=thisschool[sample(1:nrow(thisschool),nrow(thisschool)*schoolsample),,drop=FALSE]
			}
		else if(is.character(school)){
			thisschool=read.event(event=school,t=t[1],var="school")
			thisschool=data.frame(thisschool$psxf,thisschool$psyf,thisschool$pszf)
			thisschool=thisschool[sample(1:nrow(thisschool),nrow(thisschool)*schoolsample),,drop=FALSE]
			}
			
		# Set the range of the plotting volume:
		# 'xyzlim_all' is used if 'xyzlim' is given as a 6 column matrix with rows for each ping (number of rows must be lager than the number of pings):
		xyzlim_all=NULL
		if(identical(xyzlim,FALSE) || length(xyzlim)==0){
			xlim=c(min(thisposred[,1],thisschool$psx),max(thisposred[,1],thisschool$psx))
			ylim=c(min(thisposred[,2],thisschool$psy),max(thisposred[,2],thisschool$psy))
			zlim=c(min(thisposred[,3],thisschool$psz),max(thisposred[,3],thisschool$psz))
			# Add an additional column for identifying that the input value of 'xyzlim' was FALSE or of length 0:
			xyzlim=cbind(xlim,ylim,zlim,1)
			}
		else if(is.list(range) && length(range)==3){
			xlim=c(min(range$psx),max(range$psx))
			ylim=c(min(range$psy),max(range$psy))
			zlim=c(min(range$psz),max(range$psz))
			xyzlim=cbind(xlim,ylim,zlim)
			}
		else if(length(xyzlim)==1){
			if(xyzlim==1){
				xlim=c(min(thisposred[,1],thisschool$psx),max(thisposred[,1],thisschool$psx))
				ylim=c(min(thisposred[,2],thisschool$psy),max(thisposred[,2],thisschool$psy))
				zlim=c(min(thisposred[,3],thisschool$psz),max(thisposred[,3],thisschool$psz))
				xyzlim=cbind(xlim,ylim,zlim)
				}
			else{
				thisposred=NULL
				thisschool=NULL
				if(is.character(pos)){
					time=read.event(event=pos,t=t,var="time")
					if(length(time$indt)<length(t) && t!="all"){
						stop(paste("Time step ",t," not present in ",pos,"\n",sep=""))
						}
					t=time$indt
					utim=unlist(time$utim)
					data=read.event(cruise=cruise,event=event,t=t[xyzlim],var=c("volx","ctd","beams","vessel","time"),ideal=ideal,esnm="MS70",TVG=TVG,seabed=seabed,ignoreheave=ignoreheave,rot=rot,compensation=compensation,origin=origin,dir.data=dir.data,Pa=Pa,bgns=bgns)
					# Read the previously regenerated points:
					thisposred=read.event(event=pos,var="school",t=t[xyzlim])
					thisposred=cbind(thisposred$psxf,thisposred$psyf,thisposred$pszf)
					}
				if(is.null(thisposred)){
					# Get the time variable from 't':
					time=read.event(cruise=cruise,event=event,t=t,var="time")
					t=time$indt
					utim=unlist(time$utim)
					data=read.event(cruise=cruise,event=event,t=t[xyzlim],var=c("vbsc","voxels","ctd","beams","vessel","time"),ideal=ideal,esnm="MS70",TVG=TVG,seabed=seabed,ignoreheave=ignoreheave,rot=rot,compensation=compensation,origin=origin,dir.data=dir.data,Pa=Pa,bgns=bgns)
					# Regenerate the school for the first ping:
					thisposred=sv2pos.MS70(data=c(adds,data),fun=fun,ind=ind,range=range,subset=subset,acca=acca,ideal=ideal,cs=cs,allert=allert,plot=FALSE)$p1
					}
				xlim=c(min(thisposred[,1],thisschool$psx),max(thisposred[,1],thisschool$psx))
				ylim=c(min(thisposred[,2],thisschool$psy),max(thisposred[,2],thisschool$psy))
				zlim=c(min(thisposred[,3],thisschool$psz),max(thisposred[,3],thisschool$psz))
				xyzlim=cbind(xlim,ylim,zlim)
				}
			}
		else if(identical(ncol(xyzlim)-6,0) && nrow(xyzlim)>=length(t)){
			xyzlim_all=xyzlim
			xyzlim=xyzlim_all[1,]
			}
		if(any(is.infinite(xyzlim))){
			stop("Infinite plotting frame, possibly due to one of the following 4 reasons: \n(1) Ranges given by 'xyzlim' are invalid \n(2) If xyzlim points to the ranges of one of the pings, this ping is empty \n(1) If xyzlim==NULL, the current ping is empty \n(1) If the list 'range' has length 3, these ranges are invalid (maybe wrongly named list elements)")
			}
		
		# Define the clock:
		ischarclock=is.character(clock)
		clockstr=c("bbl","bbr","btl","btr","tbl","tbr","ttl","ttr")
		clockind=as.matrix(expand.grid(1:2,3:4,5:6))
		if(ischarclock){
			atclockstr=which(clockstr==clock)
			if(length(atclockstr)>0){
				clock=xyzlim[clockind[atclockstr[1],]]
				}
			else{
				clock=xyzlim[clockind[1,]]
				}
			}
		
		# Plot the regenerated points in 3D:
		t0=utim[1]
		sub=paste(paste(s2dhms(t0)[2:3],":",collapse="",sep=""),round(s2dhms(t0)[4],digits=2),sep="")
		
		# Restrict the points to the range set by 'xyzlim':
		inside=xyzlim[1]<thisposred[,1] & xyzlim[2]>thisposred[,1] & xyzlim[3]<thisposred[,2] & xyzlim[4]>thisposred[,2] & xyzlim[5]<thisposred[,3] & xyzlim[6]>thisposred[,3]
		plot3d(thisposred[inside,,drop=FALSE],xlim=xyzlim[1:2],ylim=xyzlim[3:4],zlim=xyzlim[5:6],xlab=xlab,ylab=ylab,zlab=zlab,add=add,col=colleft,...)
		if(!is.null(thisschool)){
			points3d(thisschool,col=schoolcol,size=schoolsize)
			}
		if(length(asp3d)>0){
			aspect3d(asp3d)
			}
		if(!any(identical(clock,FALSE),length(clock)==0)){
			text3d(clock,text=sub,adj=c(0,0),color="blue")
			}
		if(!add && sonar.grid){
			plot_volume_projection(data,xyzlim)
			}
		
		# Instruct the user to adjust the plotting volume, unless parameters to rgl.viewpoint() are given as the list 'view':
		if(length(view)==0 || identical(view,FALSE)){
			ans=readline("Adjust the plotting window to the disired size, shape, zoom and angle. Then hit \"return\"\n\n")
			}
		else{
			if(is.character(view)){
				view=substr(view[1],1,1)
				small_change=0.00001
				charview=c("t","b","s","w","n","e")
				rot_matrices=cbind(c(1,0,0, 0,1,0, 0,0,1),  c(1,0,0, 0,-1,0, 0,0,-1),  c(1,0,0, 0,0,-1, 0,1,0),  c(0,0,-1, -1,0,0, 0,1,0),  c(-1,0,0, 0,0,1, 0,1,0),  c(0,0,1, 1,0,0, 0,1,0)) + runif(9*6,0,small_change)
				rot_matrices[rot_matrices<=-1]=-1
				rot_matrices[rot_matrices>=1]=1
				view=rbind(cbind(matrix(rot_matrices[,view==charview],ncol=3,nrow=3),0),c(0,0,0,1))
				#view=list(userMatrix=rbind(cbind(matrix(rot_matrices[,view==charview],ncol=3,nrow=3),0),c(0,0,0,1)))
				}
			do.call(rgl.viewpoint,list(userMatrix=view,zoom=zoom))
			#do.call(rgl.viewpoint,c(view,list(zoom=zoom)))
			}
		
		# Rotate to get the stereo points:
		Azxyxz=solve(par3d()$userMatrix[1:3,1:3]) %*% roty(-stereoang) %*% par3d()$userMatrix[1:3,1:3]
		focuspoint=rotate(c(0,0,-focus),A=solve(par3d()$userMatrix[1:3,1:3]))
		midpoint=c(mean(par3d()$bbox[1:2]),mean(par3d()$bbox[3:4]),mean(par3d()$bbox[5:6]))-focuspoint
		#midpoint=apply(matrix(par3d()$bbox,ncol=3),2,mean)
		thisposblue=rotate(thisposred-matrix(midpoint,ncol=ncol(thisposred),nrow=nrow(thisposred),byrow=TRUE),A=Azxyxz)+matrix(midpoint,ncol=ncol(thisposred),nrow=nrow(thisposred),byrow=TRUE)
		# Restrict the points to the range set by 'xyzlim':
		inside=xyzlim[1]<thisposred[,1] & xyzlim[2]>thisposred[,1] & xyzlim[3]<thisposred[,2] & xyzlim[4]>thisposred[,2] & xyzlim[5]<thisposred[,3] & xyzlim[6]>thisposred[,3] & xyzlim[1]<thisposblue[,1] & xyzlim[2]>thisposblue[,1] & xyzlim[3]<thisposblue[,2] & xyzlim[4]>thisposblue[,2] & xyzlim[5]<thisposblue[,3] & xyzlim[6]>thisposblue[,3]
		plot3d(thisposblue[inside,,drop=FALSE],xlim=xyzlim[1:2],ylim=xyzlim[3:4],zlim=xyzlim[5:6],xlab=xlab,ylab=ylab,zlab=zlab,add=TRUE,col=colright,...)
		
		# Store the par3d():
		thispar3d=par3d()
		# Save the plot to file:
		if(length(fmt)>0){
				if(fmt=="png"){
				rgl.snapshot(names_img[1],top=FALSE)
				}
			else{
				rgl.postscript(names_img[1],fmt=fmt)
				}
			}
		}
	
	
	if(!identical(posout,FALSE)){
		# Store the regenerated points in TSD file:
		totallength=0
		# Write the first time step to a file named by the first .school file name, given in names_pos[1]. Reserve enough room in the header to write the entire sequence of time steps (reserve=length(t)):
		thisTSDfile=names_pos[1]
		bytes=write.TSD(x=list(psxf=thisposred[,1],psyf=thisposred[,2],pszf=thisposred[,3],utim=utim[1]),con=thisTSDfile,numt=1,reserve=length(t)-1,keep.null=TRUE)
		}
		
	cat("Frame ",1,"\n",sep="")
	
	
	# Continue with the rest of the pings:
	for(i in seq_along(t[-1])+1){
		# Read the ping nr i:
		thisposred=NULL
		thisschool=NULL
		if(is.character(pos)){
			thisposred=read.event(event=pos,var="school",t=t[i])
			thisposred=cbind(thisposred$psxf,thisposred$psyf,thisposred$pszf)
			}
		if(is.null(thisposred)){
			# Get the time variable from 't':
			data=read.event(cruise=cruise,event=event,t=t[i],var=c("vbsc","voxels","ctd","beams","vessel","time"),ideal=ideal,esnm="MS70",TVG=TVG,seabed=seabed,ignoreheave=ignoreheave,rot=rot,compensation=compensation,origin=origin,dir.data=dir.data,Pa=Pa,bgns=bgns)
			# Regenerate the school for the first ping:
			thisposred=sv2pos.MS70(data=c(adds,data),fun=fun,ind=ind,range=range,subset=subset,acca=acca,ideal=ideal,cs=cs,allert=allert,plot=FALSE)$p1
			}
		
		if(!identical(imgout,FALSE)){
			if(!is.null(xyzlim_all)){
				xyzlim=xyzlim_all[i,]
				}
			else if(identical(ncol(xyzlim)-4,0)){
				xlim=c(min(thisposred[,1],thisschool$psx),max(thisposred[,1],thisschool$psx))
				ylim=c(min(thisposred[,2],thisschool$psy),max(thisposred[,2],thisschool$psy))
				zlim=c(min(thisposred[,3],thisschool$psz),max(thisposred[,3],thisschool$psz))
				# Add an additional column for identifying that the input value of 'xyzlim' was FALSE or of length 0. If any of 'xlim', 'ylim' or 'zlim' are infinite, the old 'xyzlim' is used:
				if(!any(is.infinite(xlim),is.infinite(ylim),is.infinite(zlim))){
					xyzlim=cbind(xlim,ylim,zlim,1)
					}
				}
			if(isTRUE(school)){
				thisschool=read.event(cruise=cruise,event=event,t=t[i],var="school",dir.data=dir.data)
				thisschool=data.frame(thisschool$psxf,thisschool$psyf,thisschool$pszf)
				thisschool=thisschool[sample(1:nrow(thisschool),nrow(thisschool)*schoolsample),,drop=FALSE]
				}
			else if(is.character(school)){
				thisschool=read.event(event=school,t=t[i],var="school")
				thisschool=data.frame(thisschool$psxf,thisschool$psyf,thisschool$pszf)
				thisschool=thisschool[sample(1:nrow(thisschool),nrow(thisschool)*schoolsample),,drop=FALSE]
				}
			
			if(any(is.infinite(xyzlim))){
				stop("Infinite plotting frame, possibly due to one of the following 4 reasons: \n(1) Ranges given by 'xyzlim' are invalid \n(2) If xyzlim points to the ranges of one of the pings, this ping is empty \n(1) If xyzlim==NULL, the current ping is empty \n(1) If the list 'range' has length 3, these ranges are invalid (maybe wrongly named list elements)")
				}
		
			# Define the clock:
			if(ischarclock){
				if(length(atclockstr)>0){
					clock=xyzlim[clockind[atclockstr[1],]]
					}
				else{
					clock=xyzlim[clockind[1,]]
					}
				}
			
			# Plot the regenerated points in 3D:
			t0=utim[i]
			sub=paste(paste(s2dhms(t0)[2:3],":",collapse="",sep=""),round(s2dhms(t0)[4],digits=2),sep="")
			
			# Restrict the points to the range set by 'xyzlim':
			inside=xyzlim[1]<thisposred[,1] & xyzlim[2]>thisposred[,1] & xyzlim[3]<thisposred[,2] & xyzlim[4]>thisposred[,2] & xyzlim[5]<thisposred[,3] & xyzlim[6]>thisposred[,3]
			plot3d(thisposred[inside,,drop=FALSE],xlim=xyzlim[1:2],ylim=xyzlim[3:4],zlim=xyzlim[5:6],xlab=xlab,ylab=ylab,zlab=zlab,add=add,col=colleft,...)
			
			if(!is.null(thisschool)){
				points3d(thisschool,col=schoolcol,size=schoolsize)
				}
			if(length(asp3d)>0){
				aspect3d(asp3d)
				}
			if(!any(identical(clock,FALSE),length(clock)==0)){
				text3d(clock,text=sub,adj=c(0,0),color="blue")
				}
			if(!add && sonar.grid){
				plot_volume_projection(data,xyzlim)
				}
			if(!identical(thispar3d,par3d())){
				do.call(rgl.viewpoint,list(userMatrix=view,zoom=zoom))
				}
			
			# Rotate to get the stereo points:
			Azxyxz=solve(par3d()$userMatrix[1:3,1:3]) %*% roty(-stereoang) %*% par3d()$userMatrix[1:3,1:3]
			
			focuspoint=rotate(c(0,0,-focus),A=solve(par3d()$userMatrix[1:3,1:3]))
			midpoint=c(mean(par3d()$bbox[1:2]),mean(par3d()$bbox[3:4]),mean(par3d()$bbox[5:6]))-focuspoint
			#midpoint=apply(matrix(par3d()$bbox,ncol=3),2,mean)
			thisposblue=rotate(thisposred-matrix(midpoint,ncol=ncol(thisposred),nrow=nrow(thisposred),byrow=TRUE),A=Azxyxz)+matrix(midpoint,ncol=ncol(thisposred),nrow=nrow(thisposred),byrow=TRUE)
			#thisposblue=rotate(thisposred-matrix(midpoint,ncol=ncol(thisposred),nrow=nrow(thisposred),byrow=TRUE),by="z",ang=stereoang)+matrix(midpoint,ncol=ncol(thisposred),nrow=nrow(thisposred),byrow=TRUE)
			# Restrict the points to the range set by 'xyzlim':
			inside=xyzlim[1]<thisposred[,1] & xyzlim[2]>thisposred[,1] & xyzlim[3]<thisposred[,2] & xyzlim[4]>thisposred[,2] & xyzlim[5]<thisposred[,3] & xyzlim[6]>thisposred[,3] & xyzlim[1]<thisposblue[,1] & xyzlim[2]>thisposblue[,1] & xyzlim[3]<thisposblue[,2] & xyzlim[4]>thisposblue[,2] & xyzlim[5]<thisposblue[,3] & xyzlim[6]>thisposblue[,3]
			plot3d(thisposblue[inside,,drop=FALSE],xlim=xyzlim[1:2],ylim=xyzlim[3:4],zlim=xyzlim[5:6],xlab=xlab,ylab=ylab,zlab=zlab,add=TRUE,col=colright,...)
			
			# Save the plot to png file:
			if(length(fmt)>0){
				if(fmt=="png"){
					rgl.snapshot(names_img[i],top=FALSE)
					}
				else{
					rgl.postscript(names_img[i],fmt=fmt)
					}
				}
			}
			
		if(!identical(posout,FALSE)){
			totallength=totallength+bytes
			if(totallength>filesize){
				newfile=TRUE
				totallength=0
				}
			else{
				newfile=FALSE
				}
		
			if(newfile){
				thisTSDfile=names_pos[i]
				bytes=write.TSD(x=list(psxf=thisposred[,1],psyf=thisposred[,2],pszf=thisposred[,3],utim=utim[i]),con=thisTSDfile,numt=1,reserve=length(t),keep.null=TRUE)
				}
			else{
				bytes=write.TSD(x=list(psxf=thisposred[,1],psyf=thisposred[,2],pszf=thisposred[,3],utim=utim[i]),con=thisTSDfile,numt=1,append=TRUE,keep.null=TRUE)
				}
			}		
			
		cat("Frame ",i,"\n",sep="")
		}
	invisible()
	##################################################
	##################################################
	}
