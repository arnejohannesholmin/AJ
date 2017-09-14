#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs
#'
#' @export
#' @rdname beforeafter
#'
beforeafter <- function(x, vec){
	# Remove identical values, since we are only interested in smaller and larger values:
	x = x[!x %in% vec]
	at = findInterval(x, vec)
	s = split(x, at)
	before = lapply(s[names(s) %in% seq(0,length(vec)-1)], max)
	after = lapply(s[names(s) %in% seq_len(length(vec))], min)
	
	out = NAs(length(vec),3)
	printt(before)
	printt(vec)
	out[as.numeric(names(before))+1,1] = unlist(before)
	out[,2] = vec
	out[as.numeric(names(after)),3] = unlist(after)
	out
	}
