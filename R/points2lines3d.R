#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs zeros
#'
#' @export
#' @rdname points2lines3d
#'
points2lines3d=function(x, y=NULL, steps=1, atP=3){
	if(length(y)>0){
		temp = zeros(c(dim(x),1+steps))
		temp[,,1] = x
		for(i in seq_len(steps)){
			temp[,,i+1] = x+(y-x)*i/steps
			}
		x = temp
		atP = 3
		}
	# Get the number of points N, the number of pings P:
	d = dim(x)
	N = d[1]
	P = d[atP]
	PaddedNA = P + 1
	
	# Add NAs between arrows (an extra ping with NAs):
	out = NAs(N*PaddedNA, 3)
	s = seq(1, nrow(out), PaddedNA)
	# Arrange the points with NA at the end of each track:
	for(i in seq_len(P)){
		if(atP==1){
			out[s+(i-1),] = x[i,,]
			}
		else if(atP==2){
			out[s+(i-1),] = x[,i,]
			}
		else if(atP==3){
			out[s+(i-1),] = x[,,i]
			}
		}
	out
	}
