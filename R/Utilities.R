#*********************************************
#*********************************************
#' Modify file name, adding text before file extension, changing file extension or changing directory.
#'
#' @param f		The file path(s).
#' @param add	The string to add to each path.
#' @param ext	The new file extension.
#' @param dir	The new directory.
#' @param ...	
#'
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
#' @rdname fmod
#'
fmod <- function(f, add, ext=NULL, dir=NULL, ...){
	if(length(dir)==0){
		dir <- dirname(f)
		}
	file.path(dir, paste0(tools::file_path_sans_ext(basename(f)), add, ".", if(length(ext)) ext else tools::file_ext(f)))
}


#*********************************************
#*********************************************
#' Run an ffmpeg command.
#'
#' Run an ffmpeg command on the files 'f' with output files 'fout', using parameters 'par' and pre-parameters 'pre'.
#'
#' @param f		The file path(s).
#' @param f		The output file path(s).
#' @param par	Parameters, given as c(parameterName, parameterValue), such as c("-acodec", "copy", "-to", "00.:00:13")
#' @param pre	Pre-parameters, given as c(parameterName, parameterValue), such as c("-ss", "00:00:00"),
#' @param y		Logical: If TRUE overwrite.
#' @param run	Logical: If TRUE run the command.
#' @param dir	Directory in which to place the output files if 'fout' is not given.
#' @param ...	Used in fmod().
#'
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
#' @rdname ffmpeg
#'
ffmpeg <- function(f, fout=NULL, par=NULL, pre=NULL, y=TRUE, run=TRUE, dir=NULL, ...){
	str <- paste(
		"/sw/bin/ffmpeg", 
		paste(pre, collapse=" "), 
		if(y) "-y", 
		paste("-i", f, collapse=" "), 
		paste(par, collapse=" "), 
		#paste(names(l), l, collapse=" ", sep=" "), 
		if(length(fout)) fout else fmod(f, "_new", dir=dir, ...))
	if(run){
		system(str, ignore.stdout=TRUE)
		}
	print(str)
	str
}


#*********************************************
#*********************************************
#' Cut video using ffmpeg.
#'
#' @param f		The file path(s).
#' @param f		The output file path(s).
#' @param par	Parameters, given as c(parameterName, parameterValue), such as c("-acodec", "copy", "-to", "00.:00:13")
#' @param pre	Pre-parameters, given as c(parameterName, parameterValue), such as c("-ss", "00:00:00"),
#' @param y		Logical: If TRUE overwrite.
#' @param run	Logical: If TRUE run the command.
#' @param dir	Directory in which to place the output files if 'fout' is not given.
#' @param ...	Used in fmod().
#'
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @export
#' @rdname ffmpeg
#'
cutVideo <- function(f, ss="00:00:00", t=NULL, to=NULL, fout=NULL, par=NULL, ...){
	str <- ffmpeg(f=f, fout=fout, par=c(
		if(length(ss)) c("-ss", ss), 
		if(length(t)) c("-t", t), 
		if(length(to)) c("-to", to),
		if(length(par)) par), 
		...)
	str
}

getSingleFrame <- function(f, ss="00:00:00"){
	fSnap <- fmod(f[1], "", "png")
	ffmpeg(f=f, fout=fSnap, pre=c("-ss", ss), par=c("-frames:v", 1))
	fSnap
}
	
getImDim <- function(f){
	fSnap <- getSingleFrame(f)
	imdim <- rev(dim(png::readPNG(fSnap))[1:2])
	unlink(fSnap)
	imdim
}	
	
replayAtDark <- function(f, dark=0.1, buffer=5, dt=5, delay=1.5, cropfact=0.2, fps=25, startbuffer=10, endbuffer=0, dir=NULL, ask=TRUE, ...){
	# Extract the first frame, in order to read the image dimensions:
	ftxt <- fmod(f, "", "txt", dir=dir)
	fCropped <- fmod(f, "_cropped", "mov", dir=dir)
	
	imdim <- getImDim(f[1])
	cropdim <- ceiling(imdim * cropfact)
	croppos <- floor(imdim/2 - cropdim/2)
	
	
	if(file.exists(ftxt)){
		s <- scan(ftxt)
	}
	else{
		# Crop the center:
		ffmpeg(f=f, fout=fCropped, par=c("-filter:v", paste0("\"crop=", paste(c(cropdim, croppos), collapse=":"), "\""), "-strict", "-2"))

		# Save single frames:
		ffmpeg(f=fCropped, fout=file.path(dirname(f),"output_%09d.png"))
		unlink(fCropped)

		# Read all the cropped images and save the means:
		l <- list.files(dirname(f), full.names=TRUE)
		l <- l[tools::file_ext(l)=="png"]
		s <- sapply(l, function(xx) mean(png::readPNG(xx)))
		unlink(l)
		
		write(s, ftxt)
	}
	atend <- which(s < dark)
	atend <- atend[which(diff(c(0, atend)) > 1)] / fps
	atend <- atend[c(Inf, diff(atend)) > dt]
	atend <- atend[atend > startbuffer]
	T = length(s) / fps
	atend <- atend[atend < (T - endbuffer)]
	
	print(atend)
	# Plot the means:
	plot(seq_along(s)/fps, s, type="l")
	abline(h = dark)
	#abline(v = atend, col=2)
	segments(x0=atend, x1=atend, ,y0=max(s)-0.1, y1=max(s),  col=2)
	
	if(ask){
		ans <- readline("Approve detections: y/n")
		if(tolower(ans)!="y"){
			stop("Rerun with different settings or different scorerers")
		}
	}

	# Extract events:
	fout <- character(length(atend))
	for(i in seq_along(atend)){
		fout[i] <- fmod(f, paste0("_goalNr_", i), dir=dir)
		if(file.exists(fout[i])){
			warning(paste("Not overwriting", fout))
			}
		else{
			#ffmpeg(f=f, fout=fout[i], par=c("-acodec", "copy", "-vcodec", "copy", "-ss", max(0, atend[i] - buffer - delay), "-t", buffer))
			ffmpeg(f=f, fout=fout[i], par=c(
				"-strict", -2, 
				"-ss", max(0, atend[i] - buffer - delay), 
				"-t", buffer))
			}
		}
	fout
}
	
addScoreText <- function(f, buffer=5, text="", textsize=100, textcol="yellow", fps=25, introduration=c(2, 3), introblack=0.5, dir=NULL, ...){
	fout <- character(length(f))
	fout2 <- character(length(f))
	dir <- dirname(f[1])
	#intro <- file.path(dir,c("intro1.mov","intro2.mov"))
	lf <- length(f)
	
	imdim <- getImDim(f[1])
	#inputIntro <- paste0("color=c=black:s=", imdim[1], "x", imdim[2], ":d=", introduration)
	
	#f <- c(f, rep(inputIntro, 2))
	#text <- c(text, "Rundas mål", substring(basename(dirname(dir)), 12))
	textcol <- rep(textcol, length.out=lf)
	
	for(i in seq_along(f)){
		thistext <- text[i]
		thistextcol <- textcol[i]
		
		if(!is.numeric(textsize)){
			thisMaxNchar <- max(nchar(strsplit(thistext, "\n")[[1]]))
			thistextsize <- ceiling(3600/thisMaxNchar)
			}
		else{
			thistextsize <- textsize
			}
		
		fout[i] <- fmod(f[i], "_text", dir=dir)
		fout2[i] <- fmod(f[i], "_text_fade", dir=dir)
		printt(thistextcol)
		if(file.exists(fout[i])){
			warning(paste("Not overwriting", fout))
			}
		else{
			ffmpeg(f=f[i], fout=fout[i], 
				par=c(
				"-acodec", "copy", 
				"-vf", paste0("\"drawtext=enable='between(t,", 1, ",", buffer-1, ")':fontcolor=", thistextcol, ": fontsize=", thistextsize, ": x=", ceiling(imdim[1]*0.05), ":y=", imdim[2]*0.05, ": text='", thistext, "': fontfile=/Library/Fonts/Tahoma.ttf: rate=30000/1001\"")))
			
			ffmpeg(f=fout[i], fout=fout2[i], 
				par=c(
				"-acodec", "copy", 
				"-vf", paste0("\"fade=in:0:", fps, "\"")))
			}
		}

	fout2
}

createIntro <- function(text=NULL, name=NULL, imdim=NULL, textsize=100, textcol="yellow", fps=25, introduration=c(2, 3), introblack=c(0, 0.5), dir=NULL, ...){
	if(length(imdim)==0){
		stop("imdim must be given")
	}
	f <- paste0("color=c=black:s=", imdim[1], "x", imdim[2], ":d=", introduration)
	if(length(name)==0){
		name <- c("intro1.mp4","intro2.mp4")
	}
	fout <- file.path(dir, name)
	if(length(text)==0){
		text <- c("Rundens mål", gsub("_", " ", substring(basename(dirname(dir)), 12), fixed=TRUE))
	}
	textcol <- rep(textcol, length(text))
	tempfile <- file.path(dir, "tmp.mp4")
	
	for(i in seq_along(text)){
		thistext <- text[i]
		thistextcol <- textcol[i]
		printt(c(thistextcol))
		
		if(!is.numeric(textsize)){
			thistextsize <- ceiling(3400/nchar(thistext))
		}
		else{
			thistextsize <- textsize
		}
	
		ffmpeg(f=f[i], fout=tempfile, 
			par=c(
				"-vf", paste0("\"", "drawtext=enable='between(t,", introblack[1], ",", introduration[i]-introblack[2], ")': fontcolor=", thistextcol, ": fontsize=", thistextsize, ": x=", ceiling(imdim[1]*0.05), ":y=", imdim[2]*0.4, ": text='", thistext, "': fontfile=/Library/Fonts/Tahoma.ttf: rate=30000/1001", "\""), 
				"-r", fps),
			pre=c("-f", "lavfi"))
			
		ffmpeg(f=c("anullsrc=channel_layout=stereo:sample_rate=44100", tempfile), fout=fout[i], 
			par=c("-shortest",
				"-c:v", "copy",
				"-c:a", "aac", 
				"-strict", "-2"),
			pre=c("-f", "lavfi"))

		}
	unlink(tempfile)
	fout
}

mergeVideosMp4 <- function(f, fout, ...){
	l = length(f)
	ffmpeg(f=f, fout=fout, par=c(
		paste0("-filter_complex \"", 
		paste(paste0("[", rep(seq_len(l)-1, each=2), ":", rep(0:1, l), "]"), collapse=" "),
		paste0(" concat=n=", l, ":v=1:a=1 [v] [a]\"")),
		"-map", "\"[v]\"",
		"-map", "\"[a]\"",
		"-c:a", "aac",
		"-q:a", 2,
		"-strict", -2,
		"-vcodec", "libx264"), ...)
}

goalOfTheRound <- function(goals, dir, textcol="yellow", tmpdir="tmpdir", fps=30, dark=0.4, discard=FALSE, startbuffer=10, endbuffer=0, ...){
	# Get scorers:
	goals <- lapply(goals, function(xx) gsub(" ", "", xx, fixed=TRUE))
	op1 <- lapply(goals, "[", 1)
	op2 <- lapply(goals, "[", 2)
	opponents <- cbind(unlist(lapply(goals, "[", 1)), unlist(lapply(goals, "[", 2)))
	scorerer <- unlist(lapply(goals, "[", -(1:2)))
	N <- length(scorerer)
	textcol <- rep(textcol, l=N)
	NgoalsPerMatch <- sapply(goals, length) - 2
	hasGoals <- which(NgoalsPerMatch>0)
	NgoalsPerMatch <- NgoalsPerMatch[hasGoals]
	op1 <- rep(unlist(op1[hasGoals]), NgoalsPerMatch)
	op2 <- rep(unlist(op2[hasGoals]), NgoalsPerMatch)
	opponents <- cbind(op1, op2)
	
	opponents <- lapply(seq_len(N), function(i) setdiff(opponents[i,], scorerer[i]))
	if(any(sapply(opponents, length)>1)){
		warning("Some scorerers do not match opponents")
	}
	opponents <- sapply(opponents, head, 1)
	text <- paste0(seq_len(N), ". ", scorerer, " (vs ", opponents, ")")
	writeLines(text, file.path(dir, "goals.txt"))
	
	l <- list.files(dir, full.names=TRUE)
	l <- l[tolower(file_ext(l)) == "mp4"]
	ffinal <- file.path(dir, paste0(basename(dir), ".mp4"))
	tmpdir <- file.path(dir, tmpdir)
	dir.create(tmpdir)
	
	# Extract goals:
	dark <- rep(dark, length.out=length(l))
	startbuffer <- rep(startbuffer, length.out=length(l))
	endbuffer <- rep(endbuffer, length.out=length(l))
	fout <- unlist(lapply(which(sapply(goals, length)>0), function(i) replayAtDark(l[i], dark=dark[i], fps=fps, buffer=7, dir=tmpdir, startbuffer=startbuffer[i], endbuffer=endbuffer[i], ...)))
	# Add text
	
	# Discard missing scorerers:
	#discard <- is.na(unlist(lapply(goals, "[", -(1:2))))
	keep <- !rep(discard, length.out=length(l))
	fout <- fout[keep]
	textcol <- textcol[keep]
	N <- sum(keep)
	#fout <- unlist(lapply(seq_along(fout), function(i) addScoreText(fout[i], buffer=7, textsize=NULL, textcol=textcol[i], text=text[i], fps=30)))
	fout <- addScoreText(fout, buffer=7, textsize=NULL, textcol=textcol, text=text, fps=30, dir=tmpdir, ...)
	
	intro <- createIntro(imdim=getImDim(l[1]), textsize=NULL, textcol=textcol, fps=fps, introduration=c(2, 3), introblack=c(0, 0.5), dir=tmpdir, ...)
	
	# Merge into one file:
	mergeVideosMp4(f=c(intro, fout), fout=ffinal, ...)
}












































imshow <- function(x, pos=c(0,0), zoom=c(0,1,0,1), add=FALSE, na.col="white", ...){
	dimx <- dim(x)
	ldimx <- length(dimx)
	zoom <- round(zoom * dimx[c(1,1,2,2)])
	if(ldimx==3){
		x <- x[seq(zoom[1], zoom[2]), seq(zoom[3], zoom[4]), ]
	}
	else if(ldimx==2){
		x <- x[seq(zoom[1], zoom[2]), seq(zoom[3], zoom[4])]
	}
	w <- dim(x)[1]
	h <- dim(x)[2]
	if(!add){
		par(mar=double(4))
		plot(NULL, xlab="",ylab="", axes=FALSE, xlim=c(0,w), ylim=c(0,h))
	}
	x <- addColor(x, ind=is.na(x), col=na.col)
	rasterImage(x, pos[1], pos[2], pos[1] + w, pos[2] + h, ...)
}


ffthsift <- function(x){
	dimx <- dim(x)
	half <- round(dimx/2)
	out <- x
	
	x0 <- seq(half[1])
	x1 <- x0 + dimx[1] - half[1]
	y0 <- seq(half[2])
	y1 <- y0 + dimx[2] - half[2]
	
	out[x0, y0] <- x[x1, y1]
	out[x1, y0] <- x[x0, y1]
	out[x0, y1] <- x[x1, y0]
	out[x1, y1] <- x[x0, y0]
	out
}

addColor <- function(x, ind, col){
	dimx <- dim(x)
	rgb <- col2rgb(col) / 256
	imlen <- prod(dimx[1:2])
	x[ind] <- rgb[1]
	if(length(dimx)>2){
		x[ind + imlen] <- rgb[2]
		x[ind + 2*imlen] <- rgb[3]
	}
	x
}

LoG <- function(x, sigma=4, kerntemp=NULL, add=0){
	LoGOne <- function(x1col, kern, kerntemp, dimkerntemp){
		kern[seq_len(dimkerntemp[1]) + round(dimx[1]/2) - round(dimkerntemp[1]/2), seq_len(dimkerntemp[2]) + round(dimx[2]/2)  - round(dimkerntemp[1]/2)] <- kerntemp
		f <- fft(x1col) * fft(kern)
		f <- fft(f, inverse=TRUE)
		f <- Mod(ffthsift(f))
		f/max(f, na.rm=TRUE)
		}
	dimx <- dim(x)
	# Find puck by Laplacian of Gaussian:
	kern <- array(0, dim=dimx[1:2])
	if(length(kerntemp)==0){
		kerntemp <- spatialfil::convKernel(sigma=sigma, k=c("LoG"))$matrix + add
	}
	printt(sum(kerntemp))
	printt(sum(abs(kerntemp)))
	printt(sum(kerntemp^2))
	dimkerntemp <- dim(kerntemp)
	if(length(dimx)){
		f <- LoGOne(x, kern, kerntemp, dimkerntemp)
	}
	else{
		f <- apply(x, 3, LoGOne, kern, kerntemp, dimkerntemp)
	}
	dim(f) <- dimx
	f
}

imdist <- function(x, center){
	sqrt((x[,,1] - center[1])^2 + (x[,,2] - center[2])^2 + (x[,,3] - center[3])^2)
}


red <- function(x, thr=0.1){
	#x[x==0] = 
	##(x[,,1]/x[,,2])^2 + (x[,,1]/x[,,3])^2
	#(x[,,2] * x[,,3]) / x[,,1]^2
	sumed <- (x[,,1]+x[,,2]+x[,,3])
	isBlack <- sumed==0
	out <- x[,,1]/sumed
	out[x[,,1]<thr] = 0
	out[isBlack] <- 0
	out
	
	
	x[,,1] / x[,,2] + x[,,1] / x[,,3]
}


imvar <- function(x, inv=FALSE, out=c("cv", "sd", "var")){
	m <- (x[,,1] + x[,,2] + x[,,3]) / 3
	v <- ( (x[,,1]-m)^2 + (x[,,2]-m)^2 + (x[,,3]-m)^2 ) / 2
	if(out[1]=="var"){
		out <- v
	}
	else if(out[1]=="sd"){
		out <- sqrt(v)
	}
	else{
		out <- sqrt(v) / m
	}
	if(inv){
		out <- 1 - out
	}
	#out[is.na(out)] <- 0
	out[out<0] <- 0
	out[out>1] <- 1
	out
}


getTableEdges <- function(s, dimx, cropQ=0.8, cropFrac=c(0.2, 0.1), cropBuffer=0.05){
	# Get the number of whites along rows and columns, and identify the table:
	N1 <- rowMeans(s > quantile(s, cropQ))
	N2 <- colMeans(s > quantile(s, cropQ))
	edges <- cbind(range(which(N1 > cropFrac)), range(which(N2 > cropFrac))) + round(c(-cropBuffer, cropBuffer) * min(dimx[1:2]))
	edges[edges<=0] <- 1
	edges
}


cropTable <- function(x, edges){
	if(!any(is.infinite(edges))){
		x[seq(edges[1,1], edges[2,1]), seq(edges[1,2], edges[2,2]), , drop=FALSE]
	}
	else{
		x
	}
}



getFeatureUsingLoG <- function(file, sigma=4, kerntemp=NULL, add=0, crop=FALSE, cropQ=0.8, cropFrac=c(0.2, 0.1), cropBuffer=0.05, RGBcenter=c(0.1, 0.1, 0.1), thr=0.03, plot=FALSE, puckCol="green"){
	
	# Read the file and get the absolute:
	x <- png::readPNG(file)
	dimx <- dim(x)
	s <- x[,,1] + x[,,2] + x[,,3]
	
	if(crop){
		edges <- getTableEdges(s=s, dimx=dimx, cropQ=cropQ, cropFrac=cropFrac, cropBuffer=cropBuffer)
		x_cropped <- cropTable(x=x, edges=edges)
	}
	else{
		edges <- cbind(c(1, dimx[1]), c(1, dimx[2]))
		x_cropped <- x
	}
	
	# Get the diff from the RGBcenter value:
	#d <- imdist(x_cropped, RGBcenter)
	browser()
	
	############## redBW using r/g >2 and r/b > 2 ?
	
	d <- red(x_cropped)
	imshow(x)
	imshow(d/max(d))
	f <- LoG(d, sigma=sigma, kerntemp=kerntemp, add=add)
	imshow(f)
	return(f)
	
	#puck <- which(f > quantile(f, thr), arr.ind=TRUE)
	puck <- which(f > thr * prod(dim(f)), arr.ind=TRUE)
	puck[,1] <- puck[,1] + edges[1,1]
	puck[,2] <- puck[,2] + edges[1,2]
	puckind <- arr.ind2ind(puck, dimx[1:2])
	if(plot){
		xp <- addColor(x, puckind, puckCol)
		
		imshow(xp)
	}
	puck
}






getGoalCounter <- function(x, k=rep(c(-1,20,-3,20,-1), c(50,5, 15, 5, 50))){
	redStep <- function(x, i, k=rep(c(-1,20,-3,20,-1), c(50,5, 15, 5, 50))){
		if(sum(k)<0){
			warning("Sum k < 0")
		}
		k <- k/sum(k)
		x <- x - min(x, na.rm=TRUE)
		d <- filter(x, k)
	}
	redStepAll <- apply(x, 1, redStep)
	maxr <- quantile(redStepAll, 0.99)
	
}






getGoalCounter <- function(x, k=rep(c(-1,20,-3,20,-1), c(50,5, 15, 5, 50))){
	redStep <- function(x, thr=3, q=0.6){
		x <- x > quantile(x, q)
		d = diff(x)
		flat <- 1+c(which(d==0))
		dd = diff(flat)
		flat <- flat[dd>thr]
		x[-flat] = 0
		x
	}
	redStepAll <- apply(x, 1, redStep)
}





puckTrack <- function(file, sigma=4, cropQ=0.8, cropFrac=c(0.2, 0.1), cropBuffer=0.05, RGBcenter=c(0.1, 0.1, 0.1), thr=0.03, plot=FALSE, puckCol="green"){
	
	# Read the file and get the absolute:
	x <- png::readPNG(file)
	dimx <- dim(x)
	s <- x[,,1] + x[,,2] + x[,,3]
	
	if(cropTable){
		edges <- getTableEdges(s=s, dimx=dimx, cropQ=cropQ, cropFrac=cropFrac, cropBuffer=cropBuffer)
		x_cropped <- cropTable(x=x, edges=edges)
	}
	
	# Get the diff from the RGBcenter value:
	d <- imdist(x_cropped, RGBcenter)
	f <- LoG(d, sigma=sigma)
	imshow(f)
	return(f)
	
	#puck <- which(f > quantile(f, thr), arr.ind=TRUE)
	puck <- which(f > thr * prod(dim(f)), arr.ind=TRUE)
	puck[,1] <- puck[,1] + edges[1,1]
	puck[,2] <- puck[,2] + edges[1,2]
	puckind <- arr.ind2ind(puck, dimx[1:2])
	if(plot){
		xp <- addColor(x, puckind, puckCol)
		
		imshow(xp)
	}
	puck
}







redDist <- function(x, red=c(0.8, 0.3, 0.3)){
	out <- sqrt((x[,,1] - red[1])^2 + (x[,,2] - red[2])^2 + (x[,,3] - red[3])^2 )
	out / max(out)
}


redish <- function(x, step=1, ln=4){
	if(length(dim(x))==2){
		dim(x) <- c(dim(x)[1], 1, dim(x)[2])
	}
	#r1 <- step + x[,,2] / x[,,1]
	#r2 <- step + x[,,3] / x[,,1]
	r1 <- x[,,2] / x[,,1]
	r2 <- x[,,3] / x[,,1]
	invred <- (r1 + r2)
	out <- 1 / invred
	#if(bright){
	#	out <- out * (x[,,1] + x[,,2] + x[,,3]) / 3
	#}
	
	if(FALSE){
		magn <- (x[,,1] + x[,,2] + x[,,3])/3
		magn <- 1 + magn * (exp(ln)-1)
		magn <- log(magn) / ln
		out <- out + magn / 10
	}
	
	
	maxx <- pmax(x[,,1], x[,,2], x[,,3])
	#out <- out * maxx
	out <- out * 1/(1+exp(-(maxx-0.5)*10))
	
	#out <- x[,,1] / x[,,2] + x[,,1] / x[,,3]
	#out[x[,,2]==0] <- 0
	out[x[,,1]==0] <- 0
	out[invred==0] <- 0
	out
}


beta <- function(x, loc=0, steep=1, fact=1){
	1/(1+exp(-(steep * (fact * x) - sign(fact) * loc * steep)*10))
}
beta2 <- function(x, loc=0, steep=1){
	b1 <- beta(x, loc=loc, steep=steep, fact=1)
	b2 <- beta(x, loc=1-loc, steep=steep, fact=-1)
	b1 + b2
}



redish <- function(x){
	#browser()
	x <- round(x * 255)
	h <- rgb2hsv(c(x[,,1]), c(x[,,2]), c(x[,,3]))
	out <- beta2(x, loc=0.85, steep=4)
	#out <- h[1,]^(-0.5) * (1-h[1,])^(-0.5)
	out[is.infinite(out)] <- NA
	out <- out * h[2,] * (0.5 + h[3,]/2)
	dim(out) <- dim(x)[1:2]
	out
}


whiteish <- function(x, exp=2, magn=20){
	if(length(dim(x))==2){
		dim(x) <- c(dim(x)[1], 1, dim(x)[2])
	}
	minx <- pmin(x[,,1], x[,,2], x[,,3])
	maxx <- pmax(x[,,1], x[,,2], x[,,3])
	minx^exp * magn * (1 - (maxx - minx))^exp
}














fftImg <- function(x){
	fftOne <- function(x, kern, kerntemp, dimkerntemp){
		f <- fft(x)
		f <- Mod(ffthsift(f))
		f/max(f, na.rm=TRUE)
		}
	
	dimx <- dim(x)
	
	if(length(dimx)){
		f <- fftOne(x, kern, kerntemp, dimkerntemp)
	}
	else{
		f <- apply(x, 3, fftOne, kern, kerntemp, dimkerntemp)
	}
	dim(f) <- dimx
	f
}








