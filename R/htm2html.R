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
#' @rdname htm2html
#'
htm2html<-function(f, dir=NULL, rewrite=FALSE){
	
	# Read the tournament:
	Sys.setlocale(locale="C")
	l=readLines(f)
	
	# Html spaces:
	l=gsub("&nbsp;"," ",l,ignore.case=TRUE)
	
	# Convert to upper case:
	l=gsub("<strong>","<STRONG>",l,ignore.case=FALSE)
	
	# Replace headers with strong:
	l=gsub("<H3>","<STRONG>",l,ignore.case=TRUE)
	l=gsub("<H4>","<STRONG>",l,ignore.case=TRUE)
	l=gsub("<H5>","<STRONG>",l,ignore.case=TRUE)
	l=gsub("<H6>","<STRONG>",l,ignore.case=TRUE)
	l=gsub("<B><U>","<STRONG>",l,ignore.case=TRUE)
	
	l=gsub("</STRONG>","",l,ignore.case=TRUE)
	
	l=gsub("</H3>","",l,ignore.case=TRUE)
	l=gsub("</H4>","",l,ignore.case=TRUE)
	l=gsub("</H5>","",l,ignore.case=TRUE)
	l=gsub("</H6>","",l,ignore.case=TRUE)
	
	l=gsub("<U>","",l,ignore.case=TRUE)
	l=gsub("<B>","",l,ignore.case=TRUE)
	l=gsub("</U>","",l,ignore.case=TRUE)
	l=gsub("</B>","",l,ignore.case=TRUE)
	
	# Remove links and stuff:
	l = l[setdiff(seq_along(l),grep("<!",l,ignore.case=TRUE))]
	l = l[setdiff(seq_along(l),grep("<A",l,ignore.case=TRUE))]
	l = l[setdiff(seq_along(l),grep("<L",l,ignore.case=TRUE))]
	
	# Remove background:
	l = l[setdiff(seq_along(l),grep("<BODY BACKGROUND",l,ignore.case=TRUE))]
	
	# Add strong at the start of table headers:
	TableHeader=intersect(grep(" S ",l,ignore.case=TRUE), grep(" U ",l,ignore.case=TRUE))
	l[TableHeader] = paste0("<STRONG>", l[TableHeader])
	
	# Add end of strong at all lines with strong:
	atStrong = grep("<STRONG>",l,ignore.case=TRUE)
	l[atStrong] = paste0(l[atStrong], "</STRONG>")
	
	# Delete lines starting with "---":
	atlll = grep("---",l,ignore.case=TRUE)
	if(length(atlll)>0){
		l = l[-atlll]
		}
		
	# Remove the following strings:
	str = c(" (io)", " io", "io", "(mf)", "(i.o)", "(m.f)", " (ib)", "(ib)")
	for(i in seq_along(str)){
		hasmatch = grep(str[i],substring(l,nchar(l)-nchar(str[i])+1), ignore.case=TRUE)
		l[hasmatch] = gsub(str[i], "", l[hasmatch], fixed=TRUE)
		}
	
	nordichtml = c("&oslash;", "&Oslash;", "&aelig;", "&AElig;", "&aring;", "&Aring;", "&ouml;", "&eacute;")
	nordic = c("ø", "Ø", "æ", "Æ", "å", "Å", "o", "e")
	nordic = c("o", "O", "a", "A", "a", "A", "o", "e")
	for(i in seq_along(nordichtml)){
		printt(grep(nordichtml[i], l))
		l = gsub(nordichtml[i], nordic[i], l)
		}
	
	# Split by tabs:
	#lsplitted = strsplit(l, "\t", fixed=TRUE)
	#lenlsplitted = sapply(lsplitted, length)
	#attable = lenlsplitted>2
	#hasplace = substr()
	#l[attable] = 
	
	l = gsub("*", "s", l, fixed=TRUE)
	
	if(length(dir) || rewrite){
		if(length(dir)==0){
			dir = dirname(f)
			}
		to = file.path(dir, paste0(basename(f),"l"))
		writeLines(l, to)
		}
	l	
	}
