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
#' @export
#' @rdname timeclick
#'
timeclick<-function(names, T=300, pre=5, sps=5, score=cbind(0,0)){
	# Plot the panel:
	printt(T)
	plot(NULL, xlim=c(0,T), ylim=c(0,1))
	t=NULL
	run=0
	# Run a clock indicating when to start the video in the external video player (counting down to 0):
	t0 = unclass(Sys.time())
	if(pre>0){
		dt = 0
		oldsec = -pre
		newsec = NULL
		text(T/2, 0.5, oldsec, cex=10)
		oldint=0
		while(dt<pre){
			if(floor(dt*sps)>oldint){
				oldint = floor(dt*sps)
				text(T/2, 0.5, oldsec, cex=10, col="white")
				newsec = round(dt-pre,digits=nchar(sps))
				text(T/2, 0.5, newsec, cex=10, )
				oldsec = newsec
				}
			dt = unclass(Sys.time())-t0
			}
		text(T/2, 0.5, oldsec, cex=10, col="white")
		t0 = t0 + pre
		}
	
	abline(h=c(1/4, 2/4, 3/4))
	text(x=rep(T/2,3), y=c(1/8, 3/8, 5/8, 7/8), labels=c("Start/Stop", "Goal", names[1], names[2]), cex=5)
	while(run<2){
		l=locator(1)
		if(l$y<1/4){
			run=run+1
			t=rbind(t,c(unclass(Sys.time()), 0))
			}
		else if(l$y<2/4){
			t=rbind(t,c(unclass(Sys.time()), NA))
			}
		else if(l$y<3/4){
			t[nrow(t),2] = 1
			score = rbind(score, c(score[nrow(score),1]+1, score[nrow(score),2]))
			}
		else{
			t[nrow(t),2] = 2
			score = rbind(score, c(score[nrow(score),1], score[nrow(score),2]+1))
			}
		if(nrow(t)==1){
			lines(rep(t[,1]-t0, 2), c(-0.05,1), lwd=2, col=3)
			printt(rep(t[,1]-t0, 2))
			printt(c(-0.05,1))
			text(T*0.9, 5/8, score[1,1], cex=5, col=4)
			text(T*0.9, 7/8, score[1,2], cex=5, col=4)
			}
		else if(run==2){
			lines(rep(tail(t[,1], 1)-t0,2), c(-0.05,1), lwd=2, col=3)
			}
		else if(nrow(score)==nrow(t)){
			lines(rep(tail(t[,1], 1)-t0,2), c(-0.05,0.05), lwd=2, col=3)
			lines(rep(tail(t[,1], 1)-t0,2), c(-0.05,0.05)+(tail(t[,2],1)+2)/4-1/8, lwd=2, col=3)
			text(T*0.9, 5/8, score[nrow(score)-1,1], cex=5, col="white")
			text(T*0.9, 7/8, score[nrow(score)-1,2], cex=5, col="white")
			text(T*0.9, 5/8, score[nrow(score),1], cex=5, col=4)
			text(T*0.9, 7/8, score[nrow(score),2], cex=5, col=4)
			}
		}
	
	t[,1] = t[,1] - t0
	cbind(t,rbind(score,score[nrow(score),]))
	}
