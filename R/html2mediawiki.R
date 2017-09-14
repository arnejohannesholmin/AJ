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
#' @rdname html2mediawiki
#'
html2mediawiki<-function(f){
	
	
	
	# Read the tournament:
	#Sys.setlocale(locale="C")
	#l=readLines(f)
	l = htm2html(f)
	printt(79794)
	# Crop to the essential information:
	s1=grep("<PRE>",l,ignore.case=TRUE)
	#s2=min(grep("</PRE>",l,ignore.case=TRUE),grep("Endelig Rekk",l,ignore.case=TRUE))
	s2=min(grep("</PRE>",l,ignore.case=TRUE), grep("</BODY>",l,ignore.case=TRUE), grep("</HTML>",l,ignore.case=TRUE))
	s3=grep("<TITLE>",l,ignore.case=TRUE)
	s4=grep("Endelig Rekk",l,ignore.case=TRUE)
	if(length(s4)>0){
		startswithzero=substr(l[s4:length(l)],1,1)=="0"
		startswithone=substr(l[s4:length(l)],1,1)=="1"
		if(any(startswithzero) && any(startswithone)){
			l[s4:length(l)][startswithzero]=substring(l[s4:length(l)][startswithzero],2)
			}
		else if(any(startswithzero)){
			l[s4:length(l)][startswithzero]=substring(l[s4:length(l)][startswithzero],2)
			}
		}
	# Add indent:
	l[substr(l,1,2)=="\t\t"] = paste(":: ",l[substr(l,1,2)=="\t\t"])
	l[substr(l,1,1)=="\t"] = paste(": ",l[substr(l,1,1)=="\t"])
	printt(1243)
	
	tournament=substr(l[s3], regexpr("<TITLE>", l[s3], ignore.case=TRUE) + 7, regexpr("</TITLE>", l[s3], ignore.case=TRUE)-1)
	
	l=c("__TOC__",paste("<!--",tournament,"-->",sep=""),l[seq(s1[[1]]+2,s2[[1]]-1)])
	# l=c("__TOC__",paste("==",tournament,"==",sep=""),l[seq(s1[[1]]+2,s2[[1]]-1)])
	
	l=gsub("<STRONG>","'''",l,ignore.case=TRUE)
	l=gsub("</STRONG>","'''",l,ignore.case=TRUE)
	l=gsub("<HR>","----",l,ignore.case=TRUE)
	
	# Insert paragraphs at Tabell, Sluttspill, Endelig:
	#atTabell=grep("'''Tabell",substr(l,1,9),ignore.case=TRUE)
	atSlutts=grep("Sluttsp",substr(l,1,12),ignore.case=TRUE)
	atEndeli=c(grep("Endelig",substr(l,1,12),ignore.case=TRUE), grep("Sluttst",substr(l,1,12),ignore.case=TRUE))
	#l[atTabell]=gsub("'''","==",l[atTabell],ignore.case=TRUE)
	l[atSlutts]=gsub("'''","==",l[atSlutts],ignore.case=TRUE)
	l[atEndeli]=gsub("'''","==",l[atEndeli],ignore.case=TRUE)
	printt(8356)
	
	# Find tables and results:
	lclean = gsub("'", "", l)
	ncharclean = nchar(lclean)
	hask = regexpr(" k ", lclean, ignore.case=TRUE)
	hasp = regexpr(" p", lclean, ignore.case=TRUE)
	hastabk = regexpr("\tk\t", lclean, ignore.case=TRUE)
	hastabp = regexpr("\tp", lclean, ignore.case=TRUE)
	
	###TableHeader = which( (hasp>0 | hastabp>0) & (hasp==ncharclean-1 | hastabp==ncharclean-1) )
	
	TableHeader = which( (hask>0 & hasp>0) | (hastabk>0 & hastabp>0) | (hastabp>0 & hastabp>=ncharclean-2) ) 
	not=NULL
	
	printt(TableHeader)
	if(length(TableHeader)>0){
		
		#TableHeader = which( ((hask>0 & hasp>0) | (hastabk>0 & hastabp>0)) & (hasp==ncharclean-1 | hastabp==ncharclean-1) )
		#TableHeader = sort(c(intersect(hask>0, which(hasp>0)), intersect(which(hastabk>0), which(hastabp>0))))
		#TableHeader = sort(c(intersect(grep(" k ",l,ignore.case=TRUE), grep(" p",l,ignore.case=TRUE)), intersect(grep("\tk\t",l,ignore.case=TRUE), grep("\tp",l,ignore.case=TRUE))))
		empties=which(nchar(gsub(" ","",l))<2)
		nonempties=which(nchar(gsub(" ","",l))>2)
		empties = beforeafter(empties, TableHeader)
		TableEnd = empties[,3]
		nonempties = beforeafter(nonempties, TableHeader)
		
		TableLabels = c(" k ", " s ", " u ", " t ", " ms ", " mi ", "+/- ", " p")		
		TableLabelstab = c("\tk\t", "\ts\t", "\tu\t", "\tt\t", "\tms\t", "\tmi\t", "+/-", "\tp")		
		TableLabelsCase = c(" K ", " S ", " U ", " T ", " MS ", " MI ", " +/- ", " P")		
		
		# Move through the tables and convert to wiki table:
		for(i in seq_along(TableHeader)){
			l[nonempties[i,1]]=gsub("'''","==",l[nonempties[i,1]],ignore.case=TRUE)
			thisseq = seq(TableHeader[i]+1, TableEnd[i]-1)
			thisseq1 = seq(TableHeader[i], TableEnd[i])
			not = c(not, thisseq1)
			printt(thisseq)
			THIS = l[thisseq]
			# Split into pasts separated by " ":
			present = unlist(lapply(TableLabels, function(xx) isTRUE(grep(xx,tolower(l[TableHeader[i]]))>0))) | unlist(lapply(TableLabelstab, function(xx) isTRUE(grep(xx,tolower(l[TableHeader[i]]))>0)))
			npresent = sum(present)
			printt(present)
			presentKlubb = isTRUE(grep("klubb", tolower(l[TableHeader[i]]))>0)
			printt(THIS)
			if(presentKlubb){
				if(any(grep("\t", THIS, fixed=TRUE))){
					THIS = sub("\t",", ",THIS, fixed=TRUE)
					}
				else{
					THIS = sub("   ",", ",THIS, fixed=TRUE)
					}
				}
			presentPl = grep("1",substr(THIS[1],1,2))
			printt(substr(THIS[1],1,1))
			printt(presentPl)
			printt(THIS)
			if(!presentPl){
				THIS = paste0(numstr(seq_along(THIS)), ". ", THIS)
				}
			printt(THIS)
			TableParts = strsplit(THIS, "[[:space:]]")
			printt(TableParts)
			# Remove empty parts:
			TableParts = lapply(TableParts, function(xx) xx[nchar(xx)>0])
			printt(TableParts)
			Place = sapply(TableParts, head, 1)
			printt(Place)
			printt(TableParts)
			PlayerNames = lapply(TableParts, function(xx) xx[seq(2,length(xx)-npresent)])
			printt(PlayerNames)
			# Insert comma if "klubb" is given:
	 		PlayerNames = lapply(TableParts, function(xx) xx[seq(2,length(xx)-npresent)])
			printt(PlayerNames)
			PlayerNames = sapply(PlayerNames, paste, collapse=" ")
			printt(PlayerNames)
			TableParts = lapply(TableParts, tail, npresent)
			printt(TableParts)
			
			for(j in seq_along(THIS)){
				THIS[j] = paste("|", Place[j], "||", PlayerNames[j], "||", paste(TableParts[[j]], collapse="||"), if(j==length(THIS)) "" else "\n|-")
				}
			header = paste("Pl || Navn || ", paste(TableLabelsCase[present], collapse=" || "), "\n|-")
			THIS = c(header, THIS)
			
			# Add header and ending:
			THIS[1] = paste0("{| border=\"1\" cellpadding=\"4\" cellspacing=\"0\" style=\"margin: 1em 1em 1em 0; background: #f9f9f9; border: 1px #aaa solid; border-collapse: collapse;\"\n|", THIS[1])
			THIS[length(THIS)] = paste0(THIS[length(THIS)], "\n|}")
			
			l[seq(TableHeader[i], TableEnd[i]-1)] = THIS
			}
		}
	
	
	# Add spacing:
	if(length(atEndeli)>0){
		l = c(l, "")
		empties=which(nchar(gsub(" ","",l))<2)
		empties = beforeafter(empties, atEndeli+2)
		for(i in seq_along(atEndeli)){
			# First remove single space starting the lines:
			singlespace = which(substr(l[seq(atEndeli[i]+1,length(l))],1,1)==" ")
			l[seq(atEndeli[i]+1,length(l))][singlespace] = substring(l[seq(atEndeli[i]+1,length(l))][singlespace],2)
			verbatim = unique( c(grep("   ", l[seq(atEndeli[i]+1,length(l))], fixed=TRUE), grep("\t", l[seq(atEndeli[i]+1,length(l))], fixed=TRUE)))
			if(length(verbatim)>0){
				atSluttst = seq(atEndeli[i]+1,empties[i,3])
				not = c(not, atSluttst)
				l[atSluttst] = paste0(" ", l[atSluttst])
				}
			}
		}
		  
	addspace=seq_along(l)
	if(length(not)>0){
		addspace=addspace[-not]
		}
	l[addspace]=paste0(l[addspace],"\n")
	
	# Output:
	l
	}
