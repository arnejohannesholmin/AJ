#*********************************************
#*********************************************
#' Returns the level of tournaments.
#'
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname getLevel
#'
getLevel<-function(tournament){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-01-03 - Clean version.
	########### DESCRIPTION: ###########
	# Returns the level of tournaments.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# - 'tournament' is a vector of tournament names.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Define the tournament first 8 characters to be matched to the tournament names in 'tournament':
	CH=c("world ch","european")
	BG6=c("oslo ope","norway o","swedish ","riga ope","helsinki","czech op","moscow c")
	WT=c("norgesme","norwegia","berlin o","us open ","rose bow","trophy o","stiga no")
	NBA=c("trondhei","jaren op","jaeren o","bergen o","lamberts","lamberse","tonsberg","kvernala","nordnes ")
	LOCAL=c("bergensl","jarliga ","jarligae","jaerliga","osloliga","trondhei","tonsberg","studentm","bergen c","km oslo ","km berge")
	tournaments=c(CH,BG6,WT,NBA,LOCAL)
	levels=c(rep(1,length(CH)),rep(2,length(BG6)),rep(3,length(WT)),rep(4,length(NBA)),rep(5,length(LOCAL)))
	
	
	##### Execution #####
	# Match the first 8 characters in 'tournament' to 'tournaments', and set not found to 1:
	L=levels[match(substr(tolower(tournament),1,8),tournaments)]
	L[is.na(L)]=6
	L[substr(tolower(tournament),1,10)=="trondheims"]=5
	L[substr(tolower(tournament),1,10)=="tonsbergli"]=5
	
	
	##### Output #####
	L
	##################################################
	##################################################
	}
