#*********************************************
#*********************************************
#' Tests for exponentiality using the Kolmogorov-Smirnov test for exponential distribution when mean is unknown and must be estimated from the data. Critical values are from Monte Carlo simulations done by Finkelstein and Schafer 1972, but resimulated by Holmin (see "/Applications/echoIBM/Diverse/Finkelstein.Schafer.S.R").
#'
#' @param x  is a vector to be tested for exponentiality. Must have length 2, ..., 20, 25, 30, 100 or 1000.
#' @param p  is the significance level, one of 0.2, 0.15, 0.1, 0.05, 0.01, 0.005, 0.001.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname fs.test.exp
#'
fs.test.exp<-function(x,p=0.1){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-04-19 - Clean version.
	# Last: 2012-05-09 - Updated the table (with minor differences from the original table of Finkelstein and Schafer 1972).
	########### DESCRIPTION: ###########
	# Tests for exponentiality using the Kolmogorov-Smirnov test for exponential distribution when mean is unknown and must be estimated from the data. Critical values are from Monte Carlo simulations done by Finkelstein and Schafer 1972, but resimulated by Holmin (see "/Applications/echoIBM/Diverse/Finkelstein.Schafer.S.R").
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---x--- is a vector to be tested for exponentiality. Must have length 2, ..., 20, 25, 30, 100 or 1000.
	# ---p--- is the significance level, one of 0.2, 0.15, 0.1, 0.05, 0.01, 0.005, 0.001.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# Define the legal significance levels and the legal vector lengths:
	alpha=c(0.2,0.15,0.1,0.05,0.01,0.005,0.001)
	N=c(2:20,25,30,100,996,1000)
	
	# Read the critical values from the table created using the script "Finkelstein.Schafer.S.R" (read "Finkelstein-Schafer_all.dat" instead of "Finkelstein-Schafer_all.xls" due to the risk that the xls-file fails):
	crit=read.table("/Applications/echoIBM/Frameworks/R/Finkelstein-Schafer_all.dat",header=TRUE)[seq_along(N),]
  
	# Discard NAs:
	x=x[!is.na(x)]
	# Report an error if the significance level or vector length is invalid (this is a hilghly limited function):
	n=length(x)
	if(!(p %in% alpha)){
		stop("Significance level 'p' must be one of 0.2, 0.15, 0.1, 0.05, 0.01, 0.05, 0.001")
		}
	if(!(n %in% N)){
		stop("Length of 'x' must be identical to 25, 30, 100, 996, or 1000, if > 20")
		}
	
	
	########## Execution and output ##########
	# Extract the relevant critical value:
	thiscrit=crit[n==N,p[1]==alpha]
	
	# Calculate the Finkelstein-Schafer statistic:
	F <- pexp(sort(x), 1/mean(x))
	delta=apply(cbind(abs(F-(0:(n - 1))/n), abs(F-1:n/n)),1,max)
	S=sum(delta)
	
	# Output:
	list(reject=S>thiscrit,Sn=S,critical=thiscrit,n=n,alpha=p,mean=mean(x)) 	
	##################################################
	##################################################
	}
