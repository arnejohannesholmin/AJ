#*********************************************
#*********************************************
#' The classic Master Mind game.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom echoIBM rounded.rect
#' @importFrom sonR circle contains
#' @importFrom stats runif
#'
#' @export
#' @rdname mastermind
#'
mastermind<-function(save.results=FALSE,save.dir="Desktop"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-05-11 - Finished.
	########### DESCRIPTION: ###########
	# The classic Master Mind game.
	########## DEPENDENCIES: ###########
	# circle(), rounded.rect(), contains()
	############ VARIABLES: ############
	# - 'save.results' is TRUE if the result (number of attempt) of the game should be saved in the file "MasterMind2009_player.txt" located in save.dir. If existing, the result is appended to the end of the file. 'save.results' can be the name of the player.
	# - 'save.dir' is the directory in which the result file is located, or will be located, defaulted to Desktop.
	
	
	##################################################
	##################################################
	if(is.character(save.results)){
		user=save.results
		userfile=paste("MasterMind2009_",user,".txt")
		save.results=TRUE
		}
	else if(is.logical(save.results)){
		user="player1"
		userfile=paste("MasterMind2009_",user,".txt")
		}
	else if(!is.logical(save.results)){
		stop("Name must be given in quotes.")
		}
	# Reading the log file, if it exists and defining log variables:
	setwd("~")
	setwd(save.dir)
	lfiles=list.files()
	if(any(list.files()==userfile)){
		res=scan(userfile)
		game=length(res)+1
		attempts=1
		Tattempts=sum(res)+1
		}
	else{
		game=1
		attempts=1
		Tattempts=1
		}
	
	# Generating the solution:
	solution=ceiling(runif(4,0,6))
	#print(solution)
	
	# Midpoint of the game along the x-axis (maybe altered in the future):
	mid=5
	# Plot frame, and aspect values:
	par(oma=double(4),mar=double(4),mai=double(4),xaxt="n",yaxt="n")
	plot(0:10,0:10,xlim=c(0,10),col=NA)
	asp=par()$din[1]*diff(par()$usr[3:4])/(par()$din[2]*diff(par()$usr[1:2]))
	hframe=par()$din[2]
	
	# Height of the plot frame and height of the rows:
	h=diff(par()$usr[3:4])
	hrows=h/16
	# Width of the game, adjusted for the plot frame aspect:
	w=5/asp
	
	# The different circles to be plotted (normal, small and big):
	r=h/60
	rsmall=h/200
	rbig=h/50
	circ=circle(0,r)
	circ[,1]=circ[,1]/asp
	circsmall=circle(0,rsmall)
	circsmall[,1]=circsmall[,1]/asp
	circbig=circle(0,rbig)
	circbig[,1]=circbig[,1]/asp
	
	# Color values:
	circcol="grey31"
	colv=c("white","yellow1","green2","blue","red1","black")
	indexcolv=c(NA,"white","black")
	
	# Positions of the holes ('oholes' are origins in the blank holes to be filled with pins, 'osolholes' are the origins of the solution, 'oindexholes' are the origins of the index pin holes and 'opins' are the origins of the 6 displayed pins):
	rows=c(11:2)*hrows
	oholes=expand.grid(mid-w/2+w*seq(0.4,0.85,length.out=4),rows)
	osolholes=expand.grid(mid-w/2+w*seq(0.4,0.85,length.out=4),12.5*hrows)
	oindexholes=expand.grid(mid-w/2+seq(w*0.1,w*0.25,length.out=4),rows)
	# Positions of the pins
	opins=cbind(mid-w/2+w*seq(0.15,0.85,length.out=6),14*hrows)
	# Positions (origins asw used in rounded.rect()) of the buttons:
	ogame=c(mid-w/2+w*0.12,hrows*0.7)
	oattempts=c(mid-w/2+w*0.27,hrows*0.7)
	oTattempts=c(mid-w/2+w*0.42,hrows*0.7)
	onext=c(mid-w/2+w*0.64,hrows*0.7)
	oexit=c(mid-w/2+w*0.85,hrows*0.7)
	# Positions of the label:
	xlabel=mid-w/2+w*c(0.32,0.93,0.93,0.32)
	ylabel=c(11.7,11.7,13.3,13.3)*hrows
	xlabellarger=mid-w/2+w*c(0.31,0.94,0.94,0.31)
	ylabellarger=c(11.8,11.8,13.2,13.2)*hrows
	
	
	# Plotting the game board:
	rounded.rect(c(mid,5),w,h*(1-1/15),col="brown",border=NA,adjust=TRUE)
	
	# Plotting the holes:
	for(i in 1:dim(oholes)[1]){
		polygon(circ[,1]+oholes[i,1],circ[,2]+oholes[i,2],col=circcol,border=NA)
		}
	# Ploting the index holes:
	for(i in 1:dim(oindexholes)[1]){
		polygon(circsmall[,1]+oindexholes[i,1],circsmall[,2]+oindexholes[i,2],col=circcol,border=NA)
		}
	# Plotting the pins:
	for(i in 1:6){
		polygon(circbig[,1]+opins[i,1],circbig[,2]+opins[i,2],col=colv[i],border=NA)
		}

	# Plotting the buttons:
	rounded.rect(ogame,w/9,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
	rounded.rect(oattempts,w/9,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
	rounded.rect(oTattempts,w/9,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
	rounded.rect(onext,w/6,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
	rounded.rect(oexit,w/6,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
	text(mid-w/2+w*0.12,hrows*0.7,game,vfont=c("sans serif","bold"))
	text(mid-w/2+w*0.27,hrows*0.7,attempts,vfont=c("sans serif","bold"))
	text(mid-w/2+w*0.42,hrows*0.7,Tattempts,vfont=c("sans serif","bold"))
	text(mid-w/2+w*0.64,hrows*0.7,"next",vfont=c("sans serif","bold"))
	text(mid-w/2+w*0.85,hrows*0.7,"exit",vfont=c("sans serif","bold"))
	
	# Plotting the start button:
	polygon(xlabel,ylabel,col="grey34",border=NA)
	text(mean(xlabel),mean(ylabel),"start",cex=hrows*hframe*0.3,vfont=c("sans serif","bold"))
	
	# Starting the game. (locator() gives the position of a mouse click):
	point=locator(1)
	### Action to be taken if the user clicks on the "exit" button. For convenience wrapped in a function exitaction():
	if(contains(point$x,oexit[1]+c(-1,1)*w/6/2,"b") && contains(point$y,oexit[2]+c(-1,1)*hrows/2/2,"b")){
		# Ask if the user wishes to quit:
		polygon(xlabel,ylabel,col="orange2",border=NA)
		text(mean(xlabel),mean(ylabel),"quit game?",cex=hrows*hframe*0.3,col=circcol,font=2)
		point=locator(1)
		if(contains(point$x,xlabel[1:2],"b") && contains(point$y,ylabel[2:3],"b")){
			# Blanking the game board:
			rounded.rect(c(mid,5),w,h*(1-1/15),col="grey55",border=NA,adjust=TRUE)
			text(5,5,"game over",cex=h/10)
			return()
			}
		else{
			polygon(xlabel,ylabel,col="brown4",border=NA)
			text(c(mid-w/2+w*c(0.63,0.63)),c(12.8,12.2)*hrows,c("master","mind"),cex=hrows*hframe*0.3,col=circcol,font=2)
			}
		}
	###
	
	# The user needs to click on the "start" button to start the game:
	begin=FALSE
	if(contains(point$x,xlabel[1:2],"b") && contains(point$y,ylabel[2:3],"b")){
		begin=TRUE
		}
	while(!begin){
		point=locator(1)
		if(contains(point$x,xlabel[1:2],"b") && contains(point$y,ylabel[2:3],"b")){
			begin=TRUE
			}
		}
	# When the user has clicked on the start button, the label covering the solution is plotted:
	polygon(xlabel,ylabel,col="brown4",border=NA)
	text(c(mid-w/2+w*c(0.63,0.63)),c(12.8,12.2)*hrows,c("master","mind"),cex=hrows*hframe*0.3,col=circcol,font=2)
	
	# Starting the actual game:
	# 'exit' can be clicked any time of the game, to enter the game. A confirm button is then plotted on the label.
	point=locator(1)
	### Action to be taken if the user clicks on the "exit" button. For convenience wrapped in a function exitaction():
	if(contains(point$x,oexit[1]+c(-1,1)*w/6/2,"b") && contains(point$y,oexit[2]+c(-1,1)*hrows/2/2,"b")){
		# Ask if the user wishes to quit:
		polygon(xlabel,ylabel,col="orange2",border=NA)
		text(mean(xlabel),mean(ylabel),"quit game?",cex=hrows*hframe*0.3,col=circcol,font=2)
		point=locator(1)
		if(contains(point$x,xlabel[1:2],"b") && contains(point$y,ylabel[2:3],"b")){
			# Blanking the game board:
			rounded.rect(c(mid,5),w,h*(1-1/15),col="grey55",border=NA,adjust=TRUE)
			text(5,5,"game over",cex=h/10)
			return()
			}
		else{
			polygon(xlabel,ylabel,col="brown4",border=NA)
			text(c(mid-w/2+w*c(0.63,0.63)),c(12.8,12.2)*hrows,c("master","mind"),cex=hrows*hframe*0.3,col=circcol,font=2)
			}
		}
	###
		
	# 'thisrow' is the current row/attempt:	
	thisrow=1
	# 'selected' is the pin selected at the time
	selected=0
	# 'placed.pins' is a matrix holding the pins selected by the used
	placed.pins=matrix(double(40),ncol=4,nrow=10)
	while(1){
		# If the user has chosen to exit the game:
		if(contains(point$x,oexit[1]+c(-1,1)*w/6/2,"b") && contains(point$y,oexit[2]+c(-1,1)*hrows/2/2,"b")){
			# Ask if the user wishes to quit:
			polygon(xlabel,ylabel,col="orange2",border=NA)
			text(mean(xlabel),mean(ylabel),"quit game?",cex=hrows*hframe*0.3,col=circcol,font=2)
			point=locator(1)
			if(contains(point$x,xlabel[1:2],"b") && contains(point$y,ylabel[2:3],"b")){
				# Blanking the game board:
				rounded.rect(c(mid,5),w,h*(1-1/15),col="grey55",border=NA,adjust=TRUE)
				text(5,5,"game over",cex=h/10)
				return()
				}
			else{
				polygon(xlabel,ylabel,col="brown4",border=NA)
				text(c(mid-w/2+w*c(0.63,0.63)),c(12.8,12.2)*hrows,c("master","mind"),cex=hrows*hframe*0.3,col=circcol,font=2)
				}
			}
		# 'dpins' and 'dholes' are the distances from the 6 pins or from the 4 currently used holes to the point of the mouse click. 'mode' is chosen to be 1 if the mouse click is closer to the pins than to the currently used holes:
		dpins=sqrt((point$x-opins[,1])^2+(point$y-opins[,2])^2)
		dholes=sqrt((point$x-oholes[1:4+(thisrow-1)*4,1])^2+(point$y-oholes[1:4+(thisrow-1)*4,2])^2)
		mindpins=min(dpins)
		mindholes=min(dholes)
		wmindpins=which.min(dpins)
		wmindholes=which.min(dholes)
		mode=which.min(c(mindpins,mindholes))
		# 'mode' is 3 of the user has clicked on the "next" button or the "master mind" field:
		if((contains(point$x,onext[1]+c(-1,1)*w/6/2,"b") && contains(point$y,onext[2]+c(-1,1)*hrows/2/2,"b")) || (contains(point$x,xlabel[1:2],"b") && contains(point$y,ylabel[2:3],"b"))){
			mode=3
			}
		
		# If mode==1 a pin is simply selected:
		if(mode==1){
			selected=wmindpins
			}
		# If mode==2 the selected pin is placed in the chosen hole:
		else if(mode==2 && mindholes<h/15){
			placed.pins[thisrow,wmindholes]=selected
			polygon(circbig[,1]+oholes[(thisrow-1)*4+wmindholes,1],circbig[,2]+oholes[(thisrow-1)*4+wmindholes,2],col=colv[selected],border=NA)
			}
		# If mode==3 and all the holes of the current row has been selected, the proposed solution is checkde against the real sollution:
		else if(mode==3 && all(placed.pins[thisrow,]>0)){
			rounded.rect(onext,w/6,hrows/2,r=0.2,col="grey14",border="grey25",adjust=TRUE,by="h")
			Sys.sleep(0.15)
			rounded.rect(onext,w/6,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
			text(mid-w/2+w*0.64,hrows*0.7,"next",vfont=c("sans serif","bold"))
			# Updating the log (game, attempts and Tattempts):
			rounded.rect(ogame,w/9,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
			rounded.rect(oattempts,w/9,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
			rounded.rect(oTattempts,w/9,hrows/2,r=0.2,col="grey44",border="grey55",adjust=TRUE,by="h")
			text(mid-w/2+w*0.12,hrows*0.7,game,vfont=c("sans serif","bold"))
			text(mid-w/2+w*0.27,hrows*0.7,attempts,vfont=c("sans serif","bold"))
			text(mid-w/2+w*0.42,hrows*0.7,Tattempts,vfont=c("sans serif","bold"))
			# 'index' give the number of corrrectly placed pins and the number of present pins:
			index=double(4)
			# 'thisind' is used to indicate which of the user placed pins that should be checked. If the user has selected 2 pins that are correctly chosen and correctly placed (CC), these should not be examined further with regard to correctly chosen but incorrectly placed pins (CI).
			thisind=1:4
			# Simple check for CC:
			for(i in 1:4){
				if(placed.pins[thisrow,i]==solution[i]){
					index[i]=2
					}
				}
			# Updating 'thisind':
			thisind=thisind[index!=2]
			# 'nwhite' is the number of CI:
			nwhite=0
			# If for example red is present in the proposed solution by the user at 2 positions, but only appear once in the real solution, only one white index pin should be plotted for the red pins. The for loop checks for all 6 colors: 
			for(i in 1:6){
				nwhite=nwhite+min(sum(placed.pins[thisrow,thisind]==i),sum(solution[thisind]==i))
				}
			# Updating 'index' by adding the CCs and the CIs:
			index=sort(c(index,rep(1,nwhite)),decreasing=TRUE)[1:4]
			# If all elements of 'index' is 2, the user has found the solution:
			if(sum(index)==8){
				# Plotting the solution:
				polygon(xlabellarger,ylabellarger,col="brown",border=NA)
				for(i in 1:4){
					polygon(circbig[,1]+osolholes[i,1],circbig[,2]+osolholes[i,2],col=colv[solution[i]],border=NA)
					}
				# Victory message:
				text(5,5,"you win!!!",cex=h/2)
				if(save.results){
					lfiles=list.files()
					if(any(list.files()==userfile)){
						write(attempts,userfile,append=TRUE)
						}
					else{
						write(attempts,userfile)
						}
					}
				return()
				}
			# Else, the index pins are plotted:
			for(i in 1:4){
				polygon(circsmall[,1]+oindexholes[(thisrow-1)*4+i,1],circsmall[,2]+oindexholes[(thisrow-1)*4+i,2],col=indexcolv[index[i]+1],border=NA)
				}
			# Shifting to the next row:
			thisrow=thisrow+1
			if(thisrow==11){
				# Looser message:
				text(5,5,"you loose!!!",cex=h/2)
				return()
				}
			# Updating the log:
			attempts=attempts+1
			Tattempts=Tattempts+1
			}
			# New mouse click from the user:
			point=locator(1)
		}
	##################################################
	##################################################
	}
