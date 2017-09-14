#*********************************************
#*********************************************
#' Creates a new random playlist from the existing playlist 'infile'. The playlist 'infile' must be created by using the "Archive-Library-Export playlist" feature ("Arkiv-Bibliotek-Eksporter spilleliste" in norwegian) in "simple text"/"plain text" mode ("Enkel tekst" in norwegian).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD setrange
#'
#' @export
#' @rdname iTunes.random
#'
iTunes.random<-function(infile=NULL,outfile="Random_playlist.txt",size=100,Rating=1,Frequency=1,Genre=list("all",1),exclude=NULL,replace=FALSE,language="norwegian"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-01-18 - Clean version.
	########### DESCRIPTION: ###########
	# Creates a new random playlist from the existing playlist 'infile'. The playlist 'infile' must be created by using the "Archive-Library-Export playlist" feature ("Arkiv-Bibliotek-Eksporter spilleliste" in norwegian) in "simple text"/"plain text" mode ("Enkel tekst" in norwegian).
	########## DEPENDENCIES: ###########
	# setrange()
	############ VARIABLES: ############
	# - 'infile' is the iTunes playlist text file containing all the songs that is to be used as base for the randomization.
	# - 'outfile' is the target iTunes playlist text file.
	# - 'size' is the desired length of the new playlist.
	# - 'Rating' is the weight of the priorization of "Rating"/"Vurdering" as a chategory in iTunes. If set to 1 no priorization is done.
	# - 'Frequency' is the weight of the priorization of "Play Count"/"Ganger spilt" as a chategory in iTunes. If set to 1 no priorization is done.
	# - 'Genre' is is the weight of the priorization of "Genre"/"Sjanger" as a chategory in iTunes. 'Genre' is a list of two elements, where the first is a vector of genres and the last is a vector of nonnegative wights, where 0 excludes the corresponding genre, and all genres not given will have weight 0 .
	# - 'exclude' is used if some artists, genres, or any other chategory is to be excluded from the playlist. 'exclude' need to be a list ot two elements, where the first is a vector of categories, and the second is a vector of names or other specifics of the elements that are to be excluded. See the example file for more on this option.
	# - 'replace' is TRUE if sampling with replacement is to be used.
	# - 'language' is the language of the iTunes version. If not "norwegian", english is used.
	

	##################################################
	##################################################
	##### Preparation #####
	# The legal categories used to prioritize:
	if(language=="norwegian"){
		categories=c("Vurdering","Ganger spilt","Sjanger")
		}
	else{
		categories=c("Rating","Play count","Genre")
		}
	
	# If missing, get the input file by file.choose():
	if(is.null(infile)){
		infile=file.choose()
		}
	lastslash=tail(gregexpr("/",infile)[[1]],1)	
	directory=substr(infile,0,lastslash)
	if(length(agrep(directory,outfile))==0){
		outfile=paste(directory,outfile,sep="")
		}
	
	# Check the first character of 'infile'. If equal to "\xff\xfe", the first character of infile is removed to make it readable by read.table:
	f=file(infile,"rb")
	first=readChar(f,nchars=2,useBytes=TRUE)
	close(f)
	# Error is not in simple text mode:
	if(first=="\xff\xfe"){
		stop("The input playlist file must be a simple text file, not Unicode text file (choose \"simple text\" when exporting the playlist)")}
	
	# Read the iTunes playlist text file:
	it=read.table(infile,header=TRUE,sep="\t",quote="",comment.char="",check.names=FALSE)
	cat("The categories: \n")
	print(names(it))
	
	# Number of songs:
	nsongs=nrow(it)
	genres=unique(it[,"Sjanger"])
	
	# Execute the 'exclude' option:
	if(!is.null(exclude)){
		# Assure that the elements of the list 'exclude' have equal length:
		nexclude=length(exclude[[1]])
		exclude[[2]]=rep(exclude[[2]],length.out=nexclude)
		# 'include' is a logical vector of which songs to include:
		include=!logical(nsongs)
		for(i in 1:nexclude){
			include=include* as.numeric(length(grep(exclude[[2]][i],it[,exclude[[1]][i]],ignore.case=TRUE)))
			}
		it=it[as.logical(include),]
		nsongs=nrow(it)
		genres=unique(it[,"Sjanger"])
		}
	
	# Adding prioritization of rating:
	pr1=it[,categories[1]]
	pr1=replace(pr1,is.na(pr1),0)
	pr1=setrange(pr1,c(1,Rating))
	
	# Adding prioritization of rating:
	pr2=it[,categories[2]]
	pr2=replace(pr2,is.na(pr2),0)
	pr2=setrange(pr2,c(1,Frequency))
	
	# Adding prioritization of genres:
	if(Genre[[1]]=="all"){
		pr3=1
		}
	else{
		m=match(it[,categories[3]],Genre[[1]])
		pr3=Genre[[2]][m]
		pr3=replace(pr3,is.na(pr3),0)
		}
	
	
	##### Execution #####
	sample=sample(1:nsongs,size=size,replace=replace,prob=rep(pr1+pr2+pr3,length.out=nsongs))
	
	
	##### Output #####
	# Writing the new iTunes playlist text file:
	# Locating existing random playlists. If 'outfile' exists, integers are added to the name until the file name is unique in the given directory:
	count=0
	oldoutfile=outfile
	while(file.exists(outfile) && count<100){
		count=count+1
		outfile=sub(".txt",paste(count,".txt",sep=""),oldoutfile)
		}
	
	# Open new playlist file:
	f=file(outfile,"w")
	# Write header:
	writeLines(names(it),con=f,sep="\t")
	# Write random playlist:
	write.table(it[sample,],file=f,append=TRUE,quote=FALSE,sep="\t",na="",row.names=FALSE,col.names=FALSE)
	close(f)
	invisible(sample)
	##################################################
	##################################################
	}
