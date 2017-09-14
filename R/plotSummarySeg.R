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
#' @rdname plotSummarySeg
#'
plotSummarySeg <- function(s, var="tons", x="anio", ylim=NULL, col=1, linecol=1, cex=1, spar=NULL, legpos="topright", smooth=TRUE, cex.leg=1.3, ...){
	if(identical(x,"anio")){
		x = s[[1]]$anio
		xlab = "Angle of incidence"
		}
	else if(identical(x,"anis")){
		x = s[[1]]$anis
		xlab = "Angle of incidence"
		}
	else{
		x = s[[1]]$indt
		xlab = "Ping"
		}
	labels = list(tons="Biomass [tons]", tonsE="Biomass inside ellipse [tons]", XtTS="Total TS [dB]", XaSv="Average Sv [dB]", Xtvl="Volume [cub m]", Xtha="Horizontal area [sq m]", XeBT="Total TS inside ellipsoid")
	tags = list(tons="W", tonsE="WE", XtTS="TS", XaSv="Sv", Xtvl="V", Xtha="A", XeBT="TS")
	
	if(is.function(col)){
		col = col(s[[1]], ...)
		}
	
	# Plot:
	if(length(ylim)==0){
		ylim = range(c(s[[1]][[var]],s[[2]][[var]]), na.rm=TRUE)
		}
	# Plot the non-missing data:
	notna1 = !is.na(s[[1]][[var]])
	
	plot(NULL, ylim=ylim, xlab=xlab, xlim=range(x[notna1]), ylab=labels[[var]], cex.lab=1.5, cex.axis=1.3, ...)
	if(length(s$ang)>1){
		abline(v=s$ang, col=4)
		#axis(1, at=s$ang, line=2.4)
		}
	tag1 = paste0(tags[[var]],"1")
	tag2 = paste0(tags[[var]],"2")
	abline(h=s$tab[3,tag1], lwd=1.5, lty=2)
	abline(h=s$tab[3,tag2], lwd=1.5, lty=3)
	# Add estimate:
	abline(h=pretty(ylim), col="grey", lty=3)
	
	legend(legpos, lwd=2, lty=2:3, paste0(c("Initial: ","Final: "), c(s$tab[3,tag1], s$tab[3,tag2])), cex=cex.leg, bg="white")
	
	points(x[notna1], s[[1]][[var]][notna1], col=col, cex=cex*1)
	lines(x[notna1], s[[1]][[var]][notna1], col=linecol)
	points(x[notna1], s[[2]][[var]][notna1], pch="*", col=col, cex=cex*1.8)
	lines(x[notna1], s[[2]][[var]][notna1], col=linecol)
	# Add the spline smoothed data:
	if(smooth){
		o1 = order(x[notna1])
		ss1 = smooth.spline(x[notna1][o1], s[[1]][[var]][notna1][o1], spar=spar)
		lines(ss1, lty=2)
		notna2 = !is.na(s[[2]][[var]])
		o2 = order(x[notna2])
		ss2 = smooth.spline(x[notna2][o2], s[[2]][[var]][notna2][o2], spar=spar)
		lines(ss2, lty=3)
		}
	}
