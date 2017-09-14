#*********************************************
#*********************************************
#' Renames all files in the directory 'case' which contain the case name given as the last directory in the path of 'case', to the new case name given by newname.
#'
#' @param case  is the path to the directory holding the simulation case, ending with the case name after the last "/".
#' @param newname  is the new case name.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom gdata case
#'
#' @export
#' @rdname echoIBM_rename
#'
echoIBM_rename<-function(case,newname){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-03-01 - Clean version.
	########### DESCRIPTION: ###########
	# Renames all files in the directory 'case' which contain the case name given as the last directory in the path of 'case', to the new case name given by newname.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---case--- is the path to the directory holding the simulation case, ending with the case name after the last "/".
	# ---newname--- is the new case name.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	ncharcase=nchar(case)
	# Get the list of files in the case (reverse because the directory names may be changed, and this should happen last):
	filelist=rev(list.files(case,recursive=TRUE,full.names=TRUE))
	# Get the case name:
	casename=basename(case)
	
	# Get the files containing the casename in more positions than in the directory of the case:
	toberenamed=which(sapply(gregexpr(casename,filelist),length)>1)
	
	newnames=filelist
	# Rename the files:
	for(i in seq_along(filelist)){
		newnames[i]=paste(substr(filelist[i],1,ncharcase),gsub(casename,newname,substring(filelist[i],ncharcase+1)),sep="")
		}
	
	
	########## Execution and output ##########
	# Display info to the user:
	ans=readline(paste("A number of ",length(toberenamed)," files will be renamed from containing \"",casename,"\" to containing \"",newname,"\". View the file names? (y/n)",sep=""))
	# Display the files to be renamed:
	if(identical(tolower(ans),"y")){
		for(i in toberenamed){
			cat("File ",i,"\n",filelist[i],"\n--->\n",newnames[i],"\n\n",sep="")
			}
		}
	# Ask the user for confirmation:
	ans=readline("Continue? (y/n)")
	
	# Rename the files:
	if(identical(tolower(ans),"y")){
		for(i in toberenamed){
			newname_i=paste(substr(filelist[i],1,ncharcase),gsub(casename,newname,substring(filelist[i],ncharcase+1)),sep="")
			file.rename(filelist[i],newname_i)
			}
		}
	# Rename the case directory:
	newcase=paste(dirname(case),newname,sep="/")
	file.rename(case,newcase)
	##################################################
	##################################################
	}
