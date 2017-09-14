#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD zeros
#'
#' @export
#' @rdname plotone
#'
plotone = function(i, dir, imfiles, aspectRatio, sec, margin, fadet, logo, bg, pos, dimlogo, dimbg, score, matchScore, cex, family, col, font, brightness, ind, names, adj, T){
	cat(i,", ",sep="")
	png(imfiles[i], width=aspectRatio[1], height=aspectRatio[2], units="px", bg="transparent")
	pp(oma=zeros(4), mar=zeros(4))
	plot(NULL, xlab="",ylab="", axes=FALSE, xlim=c(0,aspectRatio[1]), ylim=c(0,aspectRatio[2]))
	if(sec[i]>(margin[1]-fadet) && sec[i]<T-(margin[2]-fadet)){
		if(length(logo)){
			rasterImage(logo, pos[1], pos[2], pos[1]+dimlogo[2], pos[2]+dimlogo[1])
			}
		if(length(bg)){
			rasterImage(bg, pos[1]+dimlogo[2], pos[2], pos[1]+dimlogo[2]+dimbg[2], pos[2]+dimbg[1])
			}
		if(length(matchScore)>0){
			gamescores = paste0("(",matchScore,")")
			}
		else{
			gamescores = NULL
			}
		text(pos[1]+dimlogo[2]+adj, pos[2]+dimlogo[1]*3/4, paste0(names[1]," ",score[ind[i],1], gamescores[1]), cex=cex[i,1], family=family, col=col, font=font, adj=c(0,0.5))
		text(pos[1]+dimlogo[2]+adj, pos[2]+dimlogo[1]/4, paste0(names[2]," ",score[ind[i],2], gamescores[2]), cex=cex[i,2], family=family, col=col, font=font, adj=c(0,0.5))
		}
	dev.off()
	if(!is.na(brightness[i])){
		im2 = readPNG(imfiles[i])
		im2 = im2 * brightness[i]
		writePNG(im2, imfiles[i])
		}
	}
