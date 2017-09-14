getBLref <- function(x, dir="~/Documents/Diverse/Bordhockey/BSI Bordhockey/Bergensligaen/Ligareferat/FromWiki"){
	cat(x, "\n")
	l <- getURI(x)
	at1 <- "hr />\n<p><br />"
	at2 <- "NewPP "
	athr <- c(
		gregexpr(at1, l, fixed=TRUE)[[1]] + nchar(at1), 
		gregexpr(at2, l, fixed=TRUE)[[1]] - nchar(at2) - 1)
	ll <- sapply(seq_len(length(athr)-1), function(i) substr(l, athr[i], athr[i+1]))
	hash2 <- grep("h2>", ll)
	if(length(hash2)){
		ll <- ll[-hash2]
	}
	if(length(ll)){
		ll <- tail(ll, 1)
		ll <- gsub("</p><p>", "\n", ll)
		ll <- gsub("</p>", "", ll)
	}
	f <- basename(x)
	year <- substr(f, 1, 4)
	thisdir <- file.path(dir, year)
	if(!file.exists(thisdir)){
		dir.create(thisdir)
	}
	f <- file.path(thisdir, paste(basename(x), "txt", sep="."))
	cat(f, "\n")
	write(ll, f)
}
#
#
#
#
#
#URL <- "http://puck.no/mediawiki126/index.php/2017.02.01,_Bergensliga_Spring_2017_nr_4"
#getBLref(URL)
#
#
#
#
#
#g <- getURI("http://puck.no/mediawiki126/index.php/Bergensligaen")
#str <- "/mediawiki126/index.php/"
#at1 <- gregexpr(str, g)[[1]]
#ll <- sapply(at1, function(i) substr(g, i, i+100))
#at2 <- gregexpr("\"", ll, fixed=TRUE)
#at2 <- sapply(at2, head, 1) - 1
#ll <- substr(ll, nchar(str)+1, at2)
#start <- as.numeric(substr(ll, 1, 4))
#start <- !is.na(start)
#ll <- ll[start]
#ll <- paste0("http://puck.no/mediawiki126/index.php/", ll)
#ll
#
#
#temp <- lapply(ll, getBLref)
#
#
#
#
#