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
#' @rdname dbetaCor3d
#'
dbetaCor3d <- function(N=100, cor=c(0.8, 0.4), lower=c(50, 50, 10), mode=c(150, 150, 30), upper=c(300, 300, 100), alpha=2){
	require(mvtnorm)
	
	if(length(cor)<3){
		cor = c(rep(1, 3-length(cor)), cor)
		}
	lower = rep(lower, l=3)
	mode = rep(mode, l=3)
	upper = rep(upper, l=3)
	m = (mode-lower) / (upper-lower)
	beta = 2 + (alpha-1)/m - alpha
	S = matrix(c(	cor[1], cor[2], cor[3],
					cor[2], cor[1], cor[3],
					cor[3], cor[3], cor[1]), 3, 3) # Correlation matrix
	ABC <- rmvnorm(mean=c(0,0,0), sig=S, n=N) # Our gaussian variables
	U <- pnorm(ABC) # Now U is uniform
	x <- qbeta(U[,1], alpha, beta[1])
	y <- qbeta(U[,2], alpha, beta[2])
	z <- qbeta(U[,3], alpha, beta[3])
	# Transform to meters:
	x = lower[1] + x * (upper[1]-lower[1])
	y = lower[2] + y * (upper[2]-lower[2])
	z = lower[3] + z * (upper[3]-lower[3])
	cat("Mean: ", lower + alpha/(alpha+beta) * (upper-lower), "\n")
	cbind(x, y, z)
	}
