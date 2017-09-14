#*********************************************
#*********************************************
#' Zips the columns of matrices into each other so that for two matrices matrix1 and matrix2 with dimensions [3,4] and [5,2] the end result is cbind(matrix1[,1],matrix2[,1],matrix1[,2],matrix2[,2],matrix1[,3],matrix1[,4]), with NAs on the spare postitions in the matrix.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all NAs
#'
#' @export
#' @rdname czip
#'
czip<-function(...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-09-15 - Clean version.
	########### DESCRIPTION: ###########
	# Zips the columns of matrices into each other so that for two matrices matrix1 and matrix2 with dimensions [3,4] and [5,2] the end result is cbind(matrix1[,1],matrix2[,1],matrix1[,2],matrix2[,2],matrix1[,3],matrix1[,4]), with NAs on the spare postitions in the matrix.
	########## DEPENDENCIES: ###########
	# flatten.list(), NAs(), dim_all()
	############ VARIABLES: ############
	########### DESCRIPTION: ###########
	# - '...' are vectors, matrices or lists of vectors and matrices to be zipped into a merged matrix.
	
	
	##################################################
	##################################################
	# Read the inputs to a list:
	l=list(...)
	# If any lists are given, extract the matrices and vectors stored in those lists, and fill them in to 'l'
	l=flatten.list(l)
	
	# Store the dimensions and the number of dimensions of the list, and issue an error if three dimensional arrays or higher are given:
	dims=lapply(l,dim_all)
	ldims=sapply(dims,length)
	if(any(ldims>2)){
		# Take the arrays and split ut into matrices:
		l[ldims>2]=lapply(l[ldims>2], function(x) lapply(split(x,rep(seq_len(prod(dim(x)[-(1:2)])),each=prod(dim(x)[1:2]))), function(y) array(y,dim=dim(x)[1:2])) )
		l=flatten.list(l)
		dims=lapply(l,dim_all)
		ldims=sapply(dims,length)
		warning("Matrixes extracted from arrays of three or more dimensions")
		}
	# Expand vectors to one-column matrices:
	l[ldims==1]=lapply(l[ldims==1],function(x) array(x,dim=c(length(x),1)))
	# Update 'dims'
	dims=sapply(l,dim_all)
	
	# The output matrix:
	lout=NAs(max(dims[1,]),sum(dims[2,]))
	# Get the column positions of the matrices in the final output:
	dimsM=NAs(length(l),max(dims[2,]))
	for(i in seq_along(l)){
		dimsM[i,seq_len(dims[2,i])]=i
		}
	dimsM=dimsM[!is.na(dimsM)]
	
	# Insert the matrices at the apropriate positions:
	for(i in seq_along(l)){
		lout[seq_len(dims[1,i]),which(dimsM==i)]=l[[i]]
		}
	lout
	##################################################
	##################################################
	}
