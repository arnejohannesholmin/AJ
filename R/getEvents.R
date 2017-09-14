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
#' @rdname getEvents
#'
getEvents <- function(eventspec, dir="/Users/arnejh/Data/echoIBM", project="SX90_BlindZone", esnm=c("SU90", "EK60"), new=TRUE){
	projectDir = file.path(dir, project)
	if(new){
		# Create the project:
		suppressWarnings(dir.create(projectDir, recursive=TRUE))
		}
	scriptsDir = file.path(dir, project, paste("Scripts", project, sep="_"))
	if(new){
		# Create the directory of the scripts:
		suppressWarnings(dir.create(scriptsDir, recursive=TRUE))
		}
	eventsDir = file.path(dir, project, "Events")
	if(new){
		# Create the directory holding the event of the simulation projects:
		suppressWarnings(dir.create(eventsDir, recursive=TRUE))
		}
	eventname = paste(project, eventspec, sep="_")
	events = file.path(eventsDir, eventname, esnm, "tsd")
	if(new){
		# Create the events:
		for(i in seq_along(events)){
			suppressWarnings(dir.create(events[i], recursive=TRUE))
			}
		}
	events
	}
