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
#' @rdname finsDuplicatedWords
#'
finsDuplicatedWords = function(x="~/Documents/Produktivt/Prosjekt/PHD_Holmin/Artiklar i doktorgrada/Paper 02, Probabilistic modeling and simulation of noise in multibeam sonar data/Latex/Version 9_noise_cleaned_after_comments_from_Dag_and_Rolf/HolminPaper2_noise_clean_with_figures_easedNotaton_afterComments.tex", comment="%"){
	x = readLines(x)
	x = gsub(comment, "", x, fixed=TRUE)
	d = unlist(strsplit(x," ",fixed=TRUE))
	d = unlist(strsplit(d,"(",fixed=TRUE))
	d = unlist(strsplit(d,")",fixed=TRUE))
	d = unlist(strsplit(d,".",fixed=TRUE))
	d = unlist(strsplit(d,",",fixed=TRUE))
	d = unlist(strsplit(d,";",fixed=TRUE))
	d = unlist(strsplit(d,"?",fixed=TRUE))
	d = unlist(strsplit(d,"!",fixed=TRUE))
	dd = which(d[-1] == d[-length(d)])
	at = outer(dd,-2:2, "+")
	data.frame(pos=dd, word=apply(at, 2, function(xx) d[xx]))	
	}
