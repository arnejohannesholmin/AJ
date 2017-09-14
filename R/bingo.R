#*********************************************
#*********************************************
#' Draws 'n' integers between 'a' and 'b'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR circle
#' @importFrom TSD zeros
#' @importFrom stats runif
#'
#' @export
#' @rdname bingo
#'
bingo<-function(n=10,a=1,b=100,timelag=10,N=200,sleep=TRUE,intro=TRUE,Nintro=50,players=NULL, text=NULL, maxw=20, startsleep=0.001){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-09 - Clean version.
	########### DESCRIPTION: ###########
	# Draws 'n' integers between 'a' and 'b'.
	########## DEPENDENCIES: ###########
	# zeros(), circle()
	############ VARIABLES: ############
	# - 'n' is the maximum number to draw.
	# - 'a' is the lower value.
	# - 'b' is the upper value.
	# - 'timelag' is the slowness og the random sampling.
	# - 'N' is the number of prints for the sampled integers.
	# - 'sleep' is TRUE if the integer display is to be delayed by the Sys.sleep() function. If FALSE, the integer display can be delayed by increasing 'N'.
	# - 'intro' is TRUE if intro messages shoudl be run.
	# - 'players' are the number of players, for which tables are printed out in xls format.
	
	
	##################################################
	##################################################
	
	splitInLines <- function(x, maxw=20){
		splitOne <- function(x){
			ncharx <- nchar(x)
			nlines <- ceiling(ncharx / maxw)
			x <- strsplit(x, " ")[[1]]
			l <- sapply(x, nchar)
			x <- split(x, ceiling(cumsum(l) / maxw))
			x <- unlist(lapply(x, paste, collapse=" "))
			paste(x, collapse="\n")
		}
		sapply(x, splitOne)
	}
	
	
	
	par(mar=zeros(4),oma=zeros(4))
	
	if(!is.null(players)){
		
		boards=zeros(15,5,players)
		for(i in 1:players){
			for(j in 1:3){
				boards[1:5+(j-1)*5,,i]=sample(a:b,25)
				}
			write.table(matrix(boards[,,i],15,5),paste("/Users/kineugelstad/Desktop/player",i,".xls",sep=""),sep="\t",col.names=FALSE,row.names=FALSE)
			}
		}
	
		
	if(intro){
		plot(NULL,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2))
		polygon(c(-0.3,-0.3,1.3,1.3),c(-0.3,1.3,1.3,-0.3),col="black",border="white")
		Nseq1=c(seq(0,500,l=Nintro),seq(500,50,l=Nintro))
		Nseq2=seq(200,0,l=Nintro)
		Nseq=c(seq(50,500,l=Nintro),seq(500,50,l=Nintro))
		Nseq3=c(seq(50,500,l=Nintro),seq(500,0,l=Nintro))
		#Nseq1=c(1:500,500:61)
		#Nseq2=200:0
		#Nseq=c(61:500,500:61)
		#Nseq3=c(61:500,500:1)
		
		for(i in Nseq2){
			text(0.5,0.5,"START",font=4,cex=3,col=rgb(0,0,1-i/200))
			#Sys.sleep(0.001)
			}
		eee=c(0,0)
		while((0.3>eee[1] && eee[1]<0.7) || (0.3>eee[2] && eee[2]<0.7)){
			eee=locator(1)
			}
		for(i in rev(Nseq2)){
			text(0.5,0.5,"START",font=4,cex=3,col=rgb(0,0,1-i/200))
			}
		
		
		for(i in Nseq1){
			text(0.5,0.5,"WELCOME",font=4,cex=6,col=grey(i/500))
			Sys.sleep(startsleep)
			}
		for(i in Nseq){
			text(0.5,0.5,"TO A REAL",font=4,cex=6,col=grey(i/500))
			Sys.sleep(startsleep)
			}
		for(i in Nseq){
			text(0.5,0.5,"MOTHERFUCKING",font=4,cex=6,col=grey(i/500))
			Sys.sleep(startsleep)
			}
		for(i in Nseq){
			text(0.5,0.5,"BINGO EXPERIENCE",font=4,cex=6,col=grey(i/500))
			Sys.sleep(startsleep)
			}
		 Sys.sleep(1)
		for(i in Nseq1){
			text(0.5,0.5,"RIGHT HERE AT THE",font=4,cex=6,col=grey(i/500))
			Sys.sleep(startsleep)
			}
		for(i in Nseq){
			text(0.5,0.5,"REINS NATION",font=4,cex=6,col=grey(i/500))
			Sys.sleep(startsleep)
			}
		Ml=10
		M=expand.grid(seq(-0.3,1.3,l=Ml),seq(-0.3,1.3,l=Ml))
		Mseq=sample(1:Ml^2)
		for(i in seq_along(Mseq)){
			points(M[Mseq[i],],col="lightskyblue",cex=i^1.5,pch=".")
			}
		Sys.sleep(0.5)
		hh=c("H","I","T","I","T","!")
		startx=c(0.1,0.25,0.4,0.7,0.85,1)
		sinx=sin(1:50/4)
		arx=zeros(50,6)
		for(j in 1:6){
			arx[,j]=arima.sim(list(ar=c(0.5,-0.6)),n=50)/5
			}
		for(i in 1:10){		
			for(j in 1:6){
				Sys.sleep(0.001)
				i=2
				text(startx[j],0.5,hh[j],font=4,cex=6,col="brown")
				}
			}
		yseq=seq(0.5,-0.5,l=50)
		for(i in 2:15){
			for(j in 1:6){
				Sys.sleep(0.025)
				text(arx[i-1,j]+startx[j],arx[i,j]+0.5,hh[j],font=4,cex=6.2,col="lightskyblue")
				text(arx[i,j]+startx[j],arx[i+1,j]+0.5,hh[j],font=4,cex=6,col="brown")
				}
			}
		}
	
	scale=22/N^3
	angadd=5
	i=1
	bingos=sample(a:b)
	if(length(text)){
		text <- splitInLines(text, maxw=maxw)
		bingos <- text[bingos]
		scale <- scale / 3.6
	}
	
		
	while(i<=n){
		s=sign(runif(1,-1,1))
		plot(NULL,xlim=c(-0.2,1.2),ylim=c(-0.2,1.2))
		text(c(0,1,0,1),c(1,1,0,0),c("Reins","Nation","Bingo","Contest"),cex=4,col="brown",font=4)
		col0=runif(1)
		col1=(col0-0.6)%%1
		ang=runif(1,0,2*pi)
		r=runif(1,0.1,0.3)
		angseq=seq(ang,ang+angadd*s,length.out=N)
		cc=circle(ang=angseq,origin=c(-cos(ang+angadd),-sin(ang+angadd))*r*s,r=r)
		for(j in 1:N){
			text(0.5+cc[j,1],0.5+cc[j,2],bingos[i],col=hsv(col0),cex=j^3*scale)
			if(sleep){
				Sys.sleep(0.00001/timelag)
				}
			text(0.5+cc[j,1],0.5+cc[j,2],bingos[i],col=hsv(col1),cex=j^3*scale)
			if(sleep){
				Sys.sleep(0.000001/timelag)
				}
			}
		text(0.5+cc[j,1],0.5+cc[j,2],bingos[i],col=hsv(col0),cex=j^3*scale)
		Sys.sleep(0.1)
		eee=locator(1)
		i=i+1
		}
	return(bingos)
	##################################################
	##################################################
	}
