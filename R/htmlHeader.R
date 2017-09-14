#' htmlHeader
#' @param x
#' @export
#'
htmlHeader=function(title, date){
	c(	"<HTML>",
		"<HEAD>",
		paste("<TITLE>",title,", ",date,"</TITLE>",sep=""),
		"",
		#"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">",
		"<META NAME=\"Generator\" CONTENT=\"Hockey\">",
		"<META NAME=\"Version\" CONTENT=\"3.8.0\">",
		"</HEAD>",
		"<BODY>",
		"<PRE>",
		paste("<H2><FONT COLOR=\"blue\"><STRONG>",title,", ",date,"</STRONG></FONT></H2>",sep="")
		)
	}
