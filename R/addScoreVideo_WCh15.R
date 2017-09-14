#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom TSD NAs zeros
#'
#' @export
#' @rdname addScoreVideo_WCh15
#'
addScoreVideo_WCh15<-function(t=0, T=60, s=cbind(0,0), S=NULL, w=1000, h=1000, margin=c(2,2), dir="~/Pictures", filename="image", logo="~/Documents/Diverse/Bordhockey/Debatt2000/E09_VM15/Yngve Aasheim-Alexey Zaharov best av 7/VMStigabrett.png", fps=25, fadet=0.5, pos=c(100,900, 250), names=c("Aasheim","Nilsson"), cex=1.5, family="serif", col="navyblue", font=4, cores=1){
	
	im = readPNG(logo)
	dimim = dim(im)
	
	N = round(T*fps)
	sec = seq_len(N)/fps
	num = numstr(seq_len(N))
	
	plotscore = sec>margin[1] & sec<(T-margin[1])
	brightness = NAs(N)
	fadeN = round(fps*fadet)
	fade = seq(0,1,l=fadeN)
	
	atfade = seq_len(fadeN)-1 + margin[1]*fps - fadeN
	atfade = atfade[atfade>0]
	brightness[atfade] = fade
	atfade = seq_len(fadeN) + (T-margin[1])*fps
	atfade = atfade[atfade<N]
	brightness[atfade] = rev(fade)
	
	ind = rowSums(outer(sec, t, ">"))
	
	
	plotone = function(i, dir, filename, num, w, h, sec, margin, fadet, im, pos, dimim, S, cex, family, col, font, brightness){
		cat(i,", ",sep="")
		imfile = file.path(dir, paste0(filename, num[i],".png"))
		png(imfile, width=w, height=h, units="px", bg="transparent")
		pp(oma=zeros(4), mar=zeros(4))
		plot(NULL, xlab="",ylab="", axes=FALSE, xlim=c(0,w), ylim=c(0,h))
		if(sec[i]>(margin[2]-fadet) && sec[i]<T-(margin[2]-fadet)){
			rasterImage(im, pos[1], pos[2], pos[1]+dimim[2], pos[2]+dimim[1])
			if(length(S)>0){
				gamescores = paste0("(",S,")")
				}
			else{
				gamescores = NULL
				}
			text(pos[3], pos[2]+dimim[1]*3/4, paste0(names[1]," ",s[ind[i],1], gamescores[1]), cex=cex, family=family, col=col, font=font, adj=c(0,0.5))
			text(pos[3], pos[2]+dimim[1]/4, paste0(names[2]," ",s[ind[i],2], gamescores[2]), cex=cex, family=family, col=col, font=font, adj=c(0,0.5))
			}
		dev.off()
		if(!is.na(brightness[i])){
			im2 = readPNG(imfile)
			im2 = im2 * brightness[i]
			writePNG(im2, imfile)
			}
		}
	
	if(cores>1){
		# Generate the clusters of time steps:
		printt(234)
		cl<-makeCluster(cores, outfile="")
		parLapply(cl, seq_len(N), plotone, dir=dir, filename=filename, num=num, w=w, h=h, sec=sec, margin=margin, fadet=fadet, im=im, pos=pos, dimim=dimim, S=S, cex=cex, family=family, col=col, font=font, brightness=brightness)
		# End the parallel processing:
		stopCluster(cl)
		}
	else{
		printt(6235)
		for(i in seq_len(N)){
			plotone(i, dir=dir, filename=filename, num=num, w=w, h=h, sec=sec, margin=margin, fadet=fadet, im=im, pos=pos, dimim=dimim, S=S, cex=cex, family=family, col=col, font=font, brightness=brightness)
			}
		}
	
	}
	
	
