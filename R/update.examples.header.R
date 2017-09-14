#*********************************************
#*********************************************
#' Writes the function name, description and variables explainaiton of the functions given by 'functions' to the available examples files given by 'examples'. The example file must have the structure given by "/Applications/R/example template".
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname update.examples.header
#'
update.examples.header<-function(functions,examples){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-23 - Clean version.
	########### DESCRIPTION: ###########
	# Writes the function name, description and variables explainaiton of the functions given by 'functions' to the available examples files given by 'examples'. The example file must have the structure given by "/Applications/R/example template".
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'functions' is a vector of paths to files and directories containing files containing the R functions. The function files need to be witten according to the file structure used by Arne Johannes Holmin and need to have the file extension ".R".
	# - 'examples' is a vector of paths to files and directories containing files containing the R function examples. The example files need to be witten according to the file structure used by Arne Johannes Holmin and need to have the file extension ".R".
		
	
	##################################################
	##################################################
	##### Preparation #####
	# Extract the path names of the function files and the function example files:
	funlist=NULL
	exlist=NULL
	for(i in seq_along(functions)){
		funlist=c(funlist,suppressWarnings(list.files(functions[i],full.names=TRUE,recursive=TRUE)))
		}
	for(i in seq_along(examples)){
		exlist=c(exlist,suppressWarnings(list.files(examples[i],full.names=TRUE,recursive=TRUE)))
		}
	funlist=funlist[!identical(funlist,character(0))]
	exlist=sort(exlist[!identical(exlist,character(0))])
	# Determine the file extensions and strip of invalid extensions:
	funext=substring(funlist,unlist(lapply(gregexpr(".",funlist,fixed=TRUE),function(x) tail(x,1))))
	funlist=sort(funlist[tolower(funext)==".r"])
	# Extract the function names given in the paths of the function files and the function example files:
	funnames=lapply(strsplit(funlist,"/"),function(x) tail(x,1))
	funnames=cbind(substr(funnames,1,nchar(funnames)-2),"")
	exnames=lapply(strsplit(exlist,"/"),function(x) tail(x,1))
	exnames=cbind(substr(exnames,9,nchar(exnames)-2),"")
	
	
	##### Execution #####
	# 'functionHeaderLine' is a vector holding the positions of the lines at which the functions are defiend in the function files:
	functionHeaderLine=ones(length(funlist))
	# Extract the function names given by the function files, and the parameters:
	param=double(length(funlist))
	for(i in seq_along(funlist)){
		l=suppressWarnings(readLines(funlist[i]))
		# Extract the function name:
		ateq=c(gregexpr("<-",l[1],fixed=TRUE)[[1]],gregexpr("=",l[1],fixed=TRUE)[[1]],gregexpr("function",l[1],fixed=TRUE)[[1]])
		if(identical(as.double(ateq[-length(ateq)]),c(-1.0,-1.0)) && tail(ateq,1)==-1){
			lengthl=length(l)
			j=1
			while(j<lengthl){
				j=j+1
				ateq=c(gregexpr("<-",l[j],fixed=TRUE)[[1]],gregexpr("=",l[j],fixed=TRUE)[[1]],gregexpr("function",l[j],fixed=TRUE)[[1]])
				if(!identical(as.double(ateq[-length(ateq)]),c(-1.0,-1.0)) && tail(ateq,1)!=-1){
					functionHeaderLine[i]=j
					break
					}
				}
			}
		# Remove the location of "function":
		ateq=ateq[-length(ateq)]
		ateq=min(ateq[ateq>0])
		funnames[i,2]=substr(l[functionHeaderLine[i]],1,ateq-1)
		# Extract the parameters:
		arpar1=gregexpr("(",l[functionHeaderLine[i]],fixed=TRUE)[[1]]
		arpar2=gregexpr(")",l[functionHeaderLine[i]],fixed=TRUE)[[1]]
		param[i]=substr(l[functionHeaderLine[i]],arpar1,arpar2)
		}
	
	# Extract the function names given by the function example files:
	for(i in seq_along(exlist)){
		l=suppressWarnings(readLines(exlist[i]))
		atex1=gregexpr("### Name: ",l[1],fixed=TRUE)[[1]]+10
		atex2=gregexpr("(",l[1],fixed=TRUE)[[1]]-1
		exnames[i,2]=substr(l[1],atex1,atex2)
		}
	
	# Errors for improperly specified function names in files, and mismatching function names in file names and within files for function files:
	funmissing=apply(funnames,1,function(x) any(nchar(x)==0))
	funmismatch=funnames[,1]!=funnames[,2]
	if(any(funmissing)){
		stop(paste("Function name not correctly specified in files:\n",paste(funlist[funmissing],collapse="\n",sep=""),sep=""))
		}
	if(any(funmismatch)){
		stop(paste("Mismatching function name specified in function file name and within function file for:\n",paste("\t",funnames[funmismatch,1],collapse="\n",sep=""),sep=""))
		}
	
	# Warning for mismatching function names in file names and within files for example files:
	exmismatch=exnames[,1]!=exnames[,2]
	if(any(exmismatch)){
		warning(paste("Mismatching function name specified in example file name and within example file for:\n",paste("\t",exnames[exmismatch,1],collapse="\n",sep=""),sep=""))
		}
	
	# Warning for missing example files:
	corresponding_fun=funnames[,1] %in% exnames[,1]
	if(sum(!corresponding_fun)>0){
		warning(paste("Example files missing for the following functions:\n",paste(funlist[!corresponding_fun],collapse="\n",sep=""),sep=""))
		}
	corresponding_ex=exnames[,1] %in% funnames[,1]
	
	# Clean function list and use the cleaned versions from here on:
	cleanfunlist=funlist[corresponding_fun]
	cleanfunnames=funnames[corresponding_fun,1]
	cleanparam=param[corresponding_fun]
	# Clean example list and use the cleaned versions from here on:
	cleanexlist=exlist[corresponding_ex]
	cleanexnames=exnames[corresponding_ex,1]
	
	### Rewrite the header of the example files: ###
	# Save the old example files to a spare directory on the desktop:
	sparefolder=paste("/Users/arnejohannesholmin/Desktop/old_R-examples ",format(Sys.time(), "%Y.%m.%d-%H:%M:%S"),sep="")
	dir.create(sparefolder)
	# For loop through the example files
	for(i in seq_along(cleanexlist)){
		cat("Rewrite example file ",cleanfunnames[i]," ...\n")
		# Read function file and example file:
		lf=suppressWarnings(readLines(cleanfunlist[i]))
		le=suppressWarnings(readLines(cleanexlist[i]))
		# Determine the position at which the header ends and where the example starts in the current example file:
		headerend=which(diff(substr(le,1,1)=="#")==-1)[1]
		
		isgood=c()
		if(is.na(headerend)){
			writeLines(le,paste(sparefolder,"/example_",cleanfunnames[i],".R",sep=""))
			writeLines(c(paste("### Name: funname","### Title: Bla bla.","### Aliases:","### Keywords:","### Variables:","# - 'x' is bla bla.","# - 'y' is bla bla.","","","### ** Examples","",sep="\n")),cleanexlist[i])
			}
		else{
			startexample=grep("### ** Examples",le,fixed=TRUE)
			if(length(startexample)==0){
				stop(paste("The required line \"### ** Examples\" missing in xample file ",cleanexlist[i],sep=""))
				}
			examplestart=max(which(diff(substr(le,1,1)=="#")==-1)[1]+2,startexample[[1]])
			
			# Get the index of the line of the description (must be a single line):
			l1=grep("########### DESCRIPTION: ######",lf)[1]+1
			ntrailing1=gregexpr("#",lf[l1],fixed=TRUE)[[1]][1] + as.numeric(length(grep("# ",lf[l1],fixed=TRUE))>0)+1
			title=substring(lf[l1],ntrailing1)
			
			# Get the indexes of the start line and end line of the variables:
			l2=grep("############ VARIABLES: ######",lf)[1]+1
			l3=grep("##################################################",lf)[1]
			# Remove any empty lines and extract the variables:
			l3=l3-sum(rm.whsp(lf[l2:l3])=="")-1
			variables=lf[l2:l3]
			
			# Construct the header:
			header=c(paste("### Name: ",cleanfunnames[i],cleanparam[i],sep=""),paste("### Title: ",title,sep=""),"### Aliases:","### Keywords:","### Variables:",gsub("\t#","#",variables))
			
			# Write the header:
			newex=c(header,c("",""),le[examplestart:length(le)])
			atoutput=grep("############ OUTPUT",newex)[1]
			if(!is.na(atoutput)){
				newex[atoutput]="### Output:"
				}
			writeLines(le,paste(sparefolder,"/example_",cleanfunnames[i],".R",sep=""))
			writeLines(newex,cleanexlist[i])
			}
		}
	##################################################
	##################################################
	}
