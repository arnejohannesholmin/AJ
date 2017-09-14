##### Create and install the package stox in R: #####

##### Load devtools: #####
library("devtools")
##########

buildPackage <- function(
	pkgName = "AJ", 
	dir = "~/Documents/Produktivt/Prosjekt/R-packages", 
	pckDep = c("TSD","SimradRaw","sonR","echoIBM","cpplot3d"), 
	version = "1.0", 
	Rversion = "3.3.1", 
	title = "", 
	description = "", 
	depends = NULL, 
	check = FALSE, 
	onLoad = "", 
	onAttach = NULL, 
	backup = FALSE){
	
	# Hard coded list of Holmin packages ordered by increasing dependency:
	pckDep <- c("TSD","SimradRaw","sonR","echoIBM","cpplot3d")
	
	getImports <- function(dir){
		# Read the NAMESPACE file and get the package dependencies:
		dirList <- list.files(dir, recursive=TRUE, full.names=TRUE)
		imports <- NULL
		if(length(dirList)){
			NAMESPACE <- readLines(dirList[basename(dirList) == "NAMESPACE"])
			atImports <- grep("import", NAMESPACE)
			imports <- NAMESPACE[atImports]
			imports <- sapply(strsplit(imports, "(", fixed=TRUE), "[", 2)
			imports <- sapply(strsplit(imports, ")", fixed=TRUE), "[", 1)
			imports <- unique(sapply(strsplit(imports, ",", fixed=TRUE), "[", 1))
			inst <- installed.packages()
			Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
			imports <- sort(setdiff(imports, Base))
			#notBase <- inst[, "Package"][!(inst[,"Priority"]) %in% "base"]
			#imports <- sort(imports[!imports %in% notBase])
		}
		imports
	}

	# Directories:
	pckDir <- file.path(dir, pkgName, pkgName)
	if(backup){
		backupDir <- file.path(dir, "Backup")
		tempBackupDir <- file.path(backupDir, pkgName)
		thisBackupDir <- file.path(backupDir, paste(pkgName, "backup", format(Sys.time(), tz="UTC", "%Y-%m-%d_%H-%M-%S"), sep="_"))
		suppressWarnings(dir.create(backupDir, recursive=TRUE))
		file.copy(pckDir, backupDir, recursive=TRUE)
		file.rename(tempBackupDir, thisBackupDir)
	}
	manDir <- file.path(pckDir, "man")
	pckAll <- unique(c(pkgName, pckDep))
	cppDir <- file.path(dir, pckAll, pckAll, "src")
	exportDir <- file.path(dirname(pckDir), "bundle")
	thisExportDir <- file.path(exportDir, paste(pkgName, version, sep="_"))
	bugReports <- paste0("https://github.com/arnejohannesholmin/", pkgName, "/issues")
	
	# Files:
	DESCRIPTIONfile = file.path(pckDir, "DESCRIPTION")
	examplesfile <- file.path(pckDir, "examples.R")
	READMEfile <- file.path(pckDir, "README.md")
	READMEfileExport <- file.path(thisExportDir, "README.md")
	onLoadfile <- file.path(pckDir, "R", "onLoad.R")
	onAttachFile <- file.path(pckDir, "R", "onAttach.R")
	
	# Do the packages have c++ code:
	cpp <- file.exists(cppDir) & length(list.files(cppDir))
	names(cpp) <- pkgName
	
	# Clear the installed package:
	try(lapply(.libPaths(), function(xx) remove.packages(pkgName, xx)), silent=TRUE)
	
	
	##### Save the following content to the onLoad.R file in the "R" directory: #####
	if(length(onAttach)){
		onLoadtext = paste(
			".onLoad <- function(libname, pkgname){",
			onLoad,
			"}", sep="\n")
		write(onLoadtext, onLoadfile)
	}
	##########
	
	
	##### Save the following content to the onAttachR file in the "R" directory: #####
	if(length(onAttach)){
		onAttachText = paste(
			".onAttach <- function(libname, pkgname){",
			"	",
			onAttach,
			"}", sep="\n")
		write(onAttachText, onAttachFile)
	}
	##########
	
	
	##### Add required fields to the DESCRIPTION file (below is the full content of the DESCRIPTION file): #####
	Rdepends <- paste0("R (>= ", Rversion, ")")
	DESCRIPTIONtext = paste(
		paste0("Package: ", pkgName),
		paste0("Title: ", title),
		paste0("Version: ", version),
		"Authors@R: c(",
		"  person(\"Arne Johannes\", \"Holmin\", role = c(\"aut\",\"cre\"), email = \"arnejh@imr.no\"))",
		"Author: Arne Johannes Holmin [aut, cre],",
		"Maintainer: Arne Johannes Holmin <arnejh@imr.no>",
		paste0("Description: ", description),
		paste0("Depends: ", paste(c(Rdepends, depends), collapse=", ")), 
		paste0("BugReports: ", bugReports), 
		"License: LGPL-3",
		"LazyData: true",
		"", sep="\n")
	write(DESCRIPTIONtext, DESCRIPTIONfile)
	##########
	
	
	##### Create documentation: #####
	# Remove current documentation first:
	unlink(manDir, recursive=TRUE, force=TRUE)
	document(pckDir)
	
	# Add C++ compability:
	if(cpp[[pkgName]]){
		NAMESPACEfile = file.path(pckDir, "NAMESPACE")
		NAMESPACEfileContent = readLines(NAMESPACEfile)
		NAMESPACEfileContent = c(paste0(NAMESPACEfileContent[1], " (edited for C++ compability)"), c("exportPattern(\"^[^\\\\.]\")", paste0("useDynLib(", pkgName, ")")), NAMESPACEfileContent[-1])
		writeLines(NAMESPACEfileContent, NAMESPACEfile)
		
		# Also remove the shared objects from the src directory:
		sharedObjects <- list.files(cppDir, pattern = "\\.o|so$", full.names=TRUE)
		print("sharedObjects")
		print(sharedObjects)
		unlink(sharedObjects, recursive=TRUE, force=TRUE)
	}
	
	# Alter the DESCRIPTION file to contain the imports listed in the NAMESPACE file:
	#imports <- lapply(pkgName, function(xx) getImports(file.path(dir, xx)))
	imports <- getImports(pckDir)
	#imports <- unique(unlist(imports))
	CRAN <- sort(setdiff(imports, pckDep))
	GitHub <- intersect(imports, pckDep)
	GitHub <- GitHub[order(match(GitHub, pckDep))]
	notInstalledCRAN <- setdiff(CRAN, installed.packages()[,"Package"])
	imports <- list(CRAN=CRAN, GitHub=GitHub, notInstalledCRAN=notInstalledCRAN)
	
	DESCRIPTIONtext <- readLines(DESCRIPTIONfile)
	if(length(imports)){
		cat("Imports:\n		", file=DESCRIPTIONfile, append=TRUE)
		cat(paste(unlist(imports), collapse=",\n		"), file=DESCRIPTIONfile, append=TRUE)
	}
	##########
	
	
	##### Run R cmd check with devtools: #####
	if(check){
		devtools::check(pckDir)
	}
	##########
	
	
	##### Create platform independent bundle of source package: #####
	suppressWarnings(dir.create(thisExportDir, recursive=TRUE))
	pkgFileVer <- build(pckDir, path=thisExportDir)
	
	##### Write README file: #####
	### READMEtext <- c(
	### 	paste0("# Installation instructions for the package ", pkgName, "(", format(Sys.time(), "%Y-%m-%d"), ")"), 
	### 	paste0("# R version: ", Rversion), 
	### 	paste0("# Install the packages that ", pkgName, " depends on. Note that this updates all the specified packages to the latest (binary) version:"),
	### 	paste0("dep.pck <- c(\"", paste0(imports$CRAN, collapse="\", \""), "\")"),
	### 	"install.packages(dep.pck, repos=\"http://cran.us.r-project.org\")",
	### 	"",
	### 	paste0("# Install ", pkgName, " and also the packages that ", pkgName, " depends on which are on GitHub (by Holmin):"),
	### 	if(any(cpp)) "# On Windows you will need Rtools to complete the installations. Check if you have this by running Sys.getenv('PATH'), and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not.",
	### 	"",
	### 	paste0("dep.pck.git <- c(\"", paste0("arnejohannesholmin/", c(imports$GitHub, pkgName), collapse="\", \""), "\")"),
	### 	"install_github(dep.pck.git)",
	### 	"", 
	### 	paste0("# For changes log see ", "https://github.com/arnejohannesholmin/", pkgName, "/NEWS"),
	### 	"", 
	### 	paste0("# Examples:"),
	### 	"", 
	### 	readLines(examplesfile), 
	### 	"")
	# Clean any existing READMEfile:
	
	dependencytext <- function(x, pkgName){
		if(length(x)){
			paste(paste0("# Install the packages that ", pkgName, " depends on. Note that this updates all the specified packages to the latest (binary) version. To skip installing already installed packages, run install.packages(setdiff(dep.pck, installed.packages()[,\"Package\"]), repos=\"http://cran.us.r-project.org\") instead:"), paste0("dep.pck <- c(\"", paste0(x, collapse="\", \""), "\")"), "install.packages(dep.pck, repos=\"http://cran.us.r-project.org\")", sep="\n")
		}
		else{
			""
		}
	}
	print(dependencytext(imports$CRAN, pkgName=pkgName))
	print(dependencytext(imports$notInstalledCRAN, pkgName=pkgName))

	READMEtext <- c(
		paste(pkgName, "R package"),
		"=====",
		"",
		description,
		"",
		paste0("Version: ", version),
		paste0("Required R version: ", Rversion),
		"",
		"Installation",
		"=====",
		"",
		"``` r",
		if(length(imports$CRAN)) dependencytext(imports$CRAN, pkgName=pkgName), 
		"",
		paste0("# Install ", pkgName, " and also the packages that ", pkgName, " depends on which are on GitHub (by Holmin):"),
		if(any(cpp)) "# On Windows you will need Rtools to complete the installations. Check if you have this by running Sys.getenv('PATH'), and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not.",
		"",
		paste0("dep.pck.git <- c(\"", paste0("arnejohannesholmin/", c(imports$GitHub, pkgName), collapse="\", \""), "\")"),
		"devtools::install_github(dep.pck.git)",
		"",
		"```",
		"",
		paste0("# For changes log see ", "https://github.com/arnejohannesholmin/", pkgName, "/NEWS"),
		"",
		"Examples",
		"=====",
		"",
		"``` r",
		readLines(examplesfile), 
		"```",
		"",
		"License",
		"=====",
		"",
		paste0("The ", pkgName, " package is licensed under the LGPL-3.)"), 
		""
		)
	unlink(READMEfile, force=TRUE)
	write(READMEtext, READMEfile, append=TRUE)
	write(READMEtext, READMEfileExport, append=TRUE)
	##########
	
	
	
	
	##### Unload the package: #####
	unload(pckDir)
	##########
	
	
	##### Install local source package by utils (independent of dev-tools), and check that it loads: #####
	eval(parse(text=dependencytext(imports$notInstalledCRAN, pkgName=pkgName)))
	install.packages(pkgFileVer, repos=NULL, type="source", lib=.libPaths()[1])
	library(pkgName, character.only=TRUE)
	if(pkgName=="sonR"){
		Acoustics_datasets_directory("/Volumes/Acoustics")
	}
	if(pkgName=="echoIBM"){
		echoIBM_datasets_directory("~/Data/echoIBM")
	}
	##########
}



# Define the directory of the working copy:
dir <- "~/Documents/Produktivt/Prosjekt/R-packages"
check=FALSE

# Build TSD 1.1:
buildPackage("TSD", 
	dir=dir, 
	version="1.1", 
	Rversion="3.3.3", 
	check=check, 
	title="Read and write in the Time Step Data (TSD) format", 
	description="The `TSD` package provides functions for reading, writing and other processing of data in the Time Step Data (TSD) format (and other utilities used by the packages SimradRaw, sonR, cpplot3d and echoIBM).The TSD format was created for the purpose of storing data organized in time steps possibly with different dimensions at different time steps. It was created in 2010 for personal use by the package author, but the NetCDF 4 format or HDF5 format largely covers the functionality of the TSD format."
)

# Build SimradRaw 1.1:
buildPackage("SimradRaw", 
	dir=dir, 
	version="1.1", 
	Rversion="3.3.3", 
	check=check, 
	title="Read and write Simrad raw files", 
	description="This R package provides functions for reading, writing, splitting and other processing of Simrad raw files (raw0 (echosounder) and raw1 (fishery sonar)). The code is based on the Matlab library written by dr. Rick Towler, NOAA Alaska Fisheries Science Center. The functions use the TSD package for efficient reading of the data (writes to a temporary TSD file and reads afterwards to avoid appending in memory)."
)

# Build sonR 1.1:
buildPackage("sonR", 
	dir=dir, 
	version="1.1", 
	Rversion="3.3.3", 
	check=check, 
	title="Library for reading, processing and converting sonar data to the TSD format", 
	description="This package contains many functions used by the packages echoIBM and cpplot3d."
)

# Build echoIBM 1.1:
buildPackage("echoIBM", 
	dir=dir, 
	version="1.1", 
	Rversion="3.3.3", 
	check=check, 
	title="Simulation of multibeam sonar data", 
	description="This R package provides utilities for simulating multibeam sonar data (echosounder, fishery sonar, 3D-sonar). Writes simulated data in the TSD format"
)

# Build cpplot3d 1.1:
buildPackage("cpplot3d", 
	dir=dir, 
	version="1.1", 
	Rversion="3.3.3", 
	check=check, 
	title="3D-plotting and segmentation of sonar data", 
	description="This R package provides functions for creating 3D color and point plots using the rgl package, and segmentation of sonar data."
	#depends="TSD, SimradRaw, sonR, echoIBM"
)

# Build AJ 1.1:
buildPackage("AJ", 
	dir=dir, 
	version="1.1", 
	Rversion="3.3.3", 
	check=check, 
	title="Holmins functions, used also for loading all packages by library(AJ)", 
	description="All functions private to Holmin (not suited or intended for sharing with others) are included in this package, which is an extension of the previous package HolminUtils", 
	depends="TSD, SimradRaw, sonR, echoIBM, cpplot3d"
)




