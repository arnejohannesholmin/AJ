#*********************************************
#*********************************************
#' (Internal) Calculates and writes the total horizontal area to file.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR circle read.event
#' @importFrom TSD NAs read.TSD write.TSD
#'
#' @export
#' @rdname writeTtha
#' 
writeTtha<-function(event, R=600){
	
	Tthafile = file.path(event,"Ttha.TSD")
	if(file.exists(Tthafile)){
		return(read.TSD(Tthafile, t="all", header=FALSE))
		}
	
	vv = read.event(event=event, var="vessel", t="all")
	bb = read.event(event=event, var="beams", t="all")
	if(length(R)==0){
		R = soundbeam_range(bb, pos="max")
		#R = max(bb$lenb) * bb$sint[1]*bb$asps[1]/2
		}
	ss = read.event(event=event, var=c("szxS","szyS","szzS","psxS","psyS"), t="all")
	
	# Move thorugh the pings and create indices for which schools are completely inside the sonar sampling volume:
	range = R * sin(bb$dire[1])
	
	npings=length(vv$psxv)
	schoolInd=vector("list",npings)
	schoolIntersection=vector("list",npings)
	sonarCircle = circle(r=range)
	Ttha = double(npings)
	
	#sss=read.event(event, t="all", var="school")
	#maxsize=apply(cbind(sss$szxS,sss$szyS,sss$szzS),1,max)
	maxsize=apply(cbind(ss$szxS,ss$szyS,ss$szzS),1,max)
	
	for(i in seq_len(npings)){
		if(i%%10 == 0){
			plotl(sonarCircle[,1]+vv$psxv[i], sonarCircle[,2]+vv$psyv[i], main=i, xlim=vv$psxv[i]+c(-750,750), ylim=vv$psyv[i]+c(-750,750))
			}
		# Get the schools that are inside the sonar volume:
		distToSchools=sqrt((ss$psxS-vv$psxv[i])^2 + (ss$psyS-vv$psyv[i])^2)
		inside=which(distToSchools < range+maxsize)
		
		if(length(inside)>0){
			schoolEllipse=NULL
			for(s in inside){
				schoolInd[[i]] = c(schoolInd[[i]], s)
				
				# Create an ellipse defining the outer margins of the school, and test whether all points are inside the sonar volume:
				schoolEllipse = ellipse(c(ss$psxS[s]-vv$psxv[i],ss$psyS[s]-vv$psyv[i]),ss$szxS[s]/2,ss$szyS[s]/2,n=1000)
				schoolR = sqrt(rowSums(schoolEllipse^2))
				fraqInside = mean(schoolR<range)
				# Area of the school ellipse:
				thisTtha = ss$szxS[s]/2 * ss$szyS[s]/2 * pi # There was a serious error in this formula, where szyS was replaced by szxS, leading to the area of a circle instead of the ellipse (fixed 2015-05-13)
				if(fraqInside==1){
					schoolIntersection[[i]] = c(schoolIntersection[[i]], 1)
					}
				else{
					# Create an ellipsoid to intesect with the sonar volume:
					maxR = max(ss$szxS[s],ss$szyS[s])
					schoolEllipsoid = seq(-maxR-1,maxR+1)
					schoolEllipsoid=as.matrix(expand.grid(ss$psxS[s]-vv$psxv[i]+schoolEllipsoid, ss$psyS[s]-vv$psyv[i]+schoolEllipsoid))
					schoolR = sqrt(rowSums(schoolEllipse^2))
					thisIntersection = mean(schoolR<range)
					thisTtha = thisTtha * thisIntersection
					schoolIntersection[[i]] = c(schoolIntersection[[i]], thisIntersection)
					}
				
				Ttha[i] = Ttha[i] + thisTtha
				
				if(i%%10 == 0){
					lines(schoolEllipse[,1]+vv$psxv[i], schoolEllipse[,2]+vv$psyv[i], col=2+as.numeric(fraqInside==1))
					text(ss$psxS[s],ss$psyS[s],s)
					text(ss$psxS[s],ss$psyS[s]-100,round(ss$szxS[s]))
					text(ss$psxS[s],ss$psyS[s]-150,round(ss$szyS[s]))
					lines(ss$psxS[s]+c(-50,50),rep(ss$psyS[s]+100,2))
					}
				}	
			}
		}
	
	numsShoolsInVolume = unlist(lapply(schoolIntersection,length))
	onlyOneSchool = numsShoolsInVolume==1
	if(all(numsShoolsInVolume<2)){
		# Collapse 'schoolIntersection' to a vector:
		schoolIntersectionVector=NAs(npings)
		schoolIntersectionVector[onlyOneSchool] = unlist(schoolIntersection[onlyOneSchool])
		# Collapse 'indS' to a vector:
		indS=NAs(npings)
		indS[onlyOneSchool] = unlist(schoolInd[onlyOneSchool])
		}
		
	out=list(utim=vv$utim, Ttha=Ttha, indS=indS, frcS=schoolIntersectionVector)
	write.TSD(out, Tthafile)
	out
	}
