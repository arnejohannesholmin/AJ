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
#' @rdname html2mediawikiAll
#'
html2mediawikiAll<-function(dir,outdir=NULL){
	if(length(outdir)==0){
		outdir=dir
		}
	setwd(dir)
	ll=list.files(dir,recursive=TRUE)
	# Read only html:
	ll=ll[substring(ll,nchar(ll)-3)=="html"]
	printt(ll)
	for(i in seq_along(ll)){
		printt(i)
		setwd(dir)
		s=html2mediawiki(ll[i])
		thisfile=gsub(".html",".wiki",ll[i])
		setwd(outdir)
		writeLines(s,thisfile)
		}
	length(ll)
	}
