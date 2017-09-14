#*********************************************
#*********************************************
#' Plots eigenvectors of lengths given by 'eigenvalues' in 2D or 3D.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl lines3d plot3d
#' @importFrom TSD ones
#'
#' @export
#' @rdname ploteigen
#'
ploteigen<-function(eigenvectors,eigenvalues=NULL,add=FALSE,center=0,square=TRUE,col="black",...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2008-08-31 - Finished.
	# Last:  2009-02-18 - Clean up and support for output from eigen().
	########### DESCRIPTION: ###########
	# Plots eigenvectors of lengths given by 'eigenvalues' in 2D or 3D.
	########## DEPENDENCIES: ###########
	# undrop(), ones()
	############ VARIABLES: ############
	# - 'eigenvectors' is either a list with names $vectors and $values or an array of dimension (P,K,K), containing the eigenvectors, where P and K are the number and the dimension of the eigenvectors/eigenvalues pairs.
	# - 'eigenvalues' has dimension (P,K) corresponding to 'eigenvectors'. If missing, all eignevalues are set to 1.
	# - 'add' is whether the plot is to be added to the existing plot.
	# - 'center' is the origin from which the eigenvectors are plotted.
	# - 'square' is TRUE if a square plot frame is used, and FALSE if xlim, ylim (and zlim) are adjusted to the plotted vectors.
	# - 'col' is a vector of colours for the eigenvectors.
	# - ... is passed to plot and plot3d
	
	
	##################################################
	##################################################
	##### Preparation #####
	# The input can be given in a list, as the output from eigen():
	if(is.list(eigenvectors)){
		eigenvalues=eigenvectors$val
		eigenvectors=eigenvectors$vec
		}
	# Dimensions of 'eigenvectors':
	dimv=dim(eigenvectors)
	ndimv=length(dimv)
	# Square eigenvector matrix is required:
	if(dimv[ndimv-1]!=dimv[ndimv]){
		stop("Eigenvector matrix must be square")
		}
	# If eigenvalues are not given, all lengths will be set to 1:
	if(is.null(eigenvalues)){
		eigenvalues=ones(dimv[1:(ndimv-1)])
		}
	# If only one set of eigenvectors/eigenvalues pairs is given, undropping is needed:
	if(ndimv==2){
		dimv=c(1,dimv)
		dim(eigenvectors)=dimv
		dim(eigenvalues)=dimv[1:2]
		}
	# 'P' and 'K' are the number and the dimension of the eigenvectors/eigenvalues pairs:
	P=dimv[1]
	K=dimv[2]
	# 'center' needs to have length equal to the number of eigenvectors:
	center=rep(center,length.out=K)
	# Colours for the eigenvectors:
	col=rep(col,P)
	
	
	##### Execution and output #####
	# R returns squared eigenvalues:
	printt(eigenvectors)
	eigenvalues<-sqrt(eigenvalues)
	printt(eigenvalues)
	eigenvalues=outer(eigenvalues,ones(dimv[ndimv]))
	printt(eigenvalues)
	#eigenvalues=outer(eigenvalues/apply(eigenvalues,1,sum),ones(dimv[ndimv]))
	eigenvalues=aperm(eigenvalues,c(1,3,2))
	printt(eigenvalues)
	eigenvectors=eigenvectors*eigenvalues
	printt(eigenvectors)
	
	# Plotting in 2D:
	if(dimv[ndimv]==2){
		if(add==FALSE){
			if(square){
				len=max(eigenvalues)
				plot(NULL,xlim=c(-len,len)+center[1],ylim=c(-len,len)+center[2],xlab="x",ylab="y",...)
				}
			else{
				plot(NULL,xlab="x",ylab="y",xlim=range(eigenvectors[,1,],center[1]),ylim=range(eigenvectors[,2,],center[2]),...)
				}
			}
		for(p in 1:P){
			printt(rbind(center[1],eigenvectors[p,,1]))
			lines(rbind(center[1],eigenvectors[p,,1]),col=col[p],...)
			printt(rbind(center[2],eigenvectors[p,,2]))
			lines(rbind(center[2],eigenvectors[p,,2]),col=col[p],...)
			}
		}
	# Plotting in 3D:
	else if(dimv[ndimv]==3){
		if(add==FALSE){
			if(square){
				len=max(eigenvalues)
				plot3d(NULL,xlim=c(-len,len)+center[1],ylim=c(-len,len)+center[2],zlim=c(-len,len)+center[3],xlab="x",ylab="y",zlab="z")
				}
			else{
				plot3d(NULL,xlim=range(eigenvectors[,1,],center[1]),ylim=range(eigenvectors[,2,],center[2]),zlim=range(eigenvectors[,3,],center[3]),xlab="x",ylab="y",zlab="z")
				}
			
			}
		for(p in 1:P){
			lines3d(rbind(center[1],eigenvectors[p,,1]),col=col[p],...)
			lines3d(rbind(center[2],eigenvectors[p,,2]),col=col[p],...)
			lines3d(rbind(center[3],eigenvectors[p,,3]),col=col[p],...)
			}
		}
	##################################################
	##################################################
	}
