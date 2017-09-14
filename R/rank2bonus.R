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
#' @rdname rank2bonus
#'
rank2bonus=function(N,A,level=4,bronze=TRUE){
	
	# Add 1 for A in national and international tournaments:
	add1=as.numeric(A<N)
	
	# For local and other tournaments set A to 6 to ensure the points 3,2,1,0,...:
	if(level>4){
		A=6
		}
	# Define tne number of players in A, which is set to all players if not specified smaller than the number of players:
	A=min(N,A)
	# The number of olayers in B
	B=N-A
	printt(N)
	printt(A)
	printt(B)
	
	# Define max points in A and B:
	nr1A=c(16,16,11,11,3,1)
	nr1B=c(3,3,3,3,0,0)
	# Calculate the bonus points in A and B:
	bonusA=floor(((1-seq(0,A-1)/A)^2) * nr1A[level]) + add1
	bonusB=nr1B[level]+1-seq_len(B)
	bonusB[bonusB<0]=0
	# Equal points for no bronze match:
	if(!bronze && N>3){
		bonusA[3:4]=3
		}
	# Output:
	c(bonusA,bonusB) + 1
	}
