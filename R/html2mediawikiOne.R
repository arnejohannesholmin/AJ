#*********************************************
#*********************************************
#' ***DECRIPTION MISSING***
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom tools file_ext
#'
#' @export
#' @rdname html2mediawikiOne
#'
html2mediawikiOne<-function(x,dir=NULL){
	s=html2mediawiki(x)
	if(length(dir)==0){
		wikifile=gsub(paste0(".", file_ext(x)), ".wiki", x)
		}
	else{
		wikifile=file.path(dir,gsub(paste0(".", file_ext(x)), ".wiki", basename(x)))
		}
	f = file(wikifile)
	writeLines(s, f)
	close(f)
	}
