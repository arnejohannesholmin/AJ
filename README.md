AJ R package
=====

All functions private to Holmin (not suited or intended for sharing with others) are included in this package, which is an extension of the previous package HolminUtils

Version: 1.1
Required R version: 3.3.3

Installation
=====

``` r
# Install the packages that AJ depends on. Note that this updates all the specified packages to the latest (binary) version. To skip installing already installed packages, run install.packages(setdiff(dep.pck, installed.packages()[,"Package"]), repos="http://cran.us.r-project.org") instead:
dep.pck <- c("fBasics", "fields", "gdata", "rgl")
install.packages(dep.pck, repos="http://cran.us.r-project.org")

# Install AJ and also the packages that AJ depends on which are on GitHub (by Holmin):
# On Windows you will need Rtools to complete the installations. Check if you have this by running Sys.getenv('PATH'), and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not.

dep.pck.git <- c("arnejohannesholmin/TSD", "arnejohannesholmin/SimradRaw", "arnejohannesholmin/sonR", "arnejohannesholmin/echoIBM", "arnejohannesholmin/cpplot3d", "arnejohannesholmin/AJ")
devtools::install_github(dep.pck.git)

```

# For changes log see https://github.com/arnejohannesholmin/AJ/NEWS

Examples
=====

``` r
# Write some data to a TSD file (all variable must have 4 character names):
dat <- list(
	var1=list(
		array(runif(2*3*4), dim=c(2,3,4)), 
		array(runif(7*4), dim=c(7,4))
		), 
	var2=list(
		
		c("Time step 1", "ebaerb"), 
		c("Last time step")
		), 
	var3=list(
		complex(real=1:12, imaginary=runif(12)), 
		NULL
		)
	)
TSDfile <- tempfile()
write.TSD(dat, TSDfile, numt=2)
datread <- read.TSD(TSDfile, t="all")

# Differs in precision of the first variable 'var1':
all.equal(dat, datread)

# Set the first varialbe to double precision:
write.TSD(dat, TSDfile, numt=2, header=list(dtyp=list(var1="doub")))
datread <- read.TSD(TSDfile, t="all")
all.equal(dat, datread)


```

License
=====

The AJ package is licensed under the LGPL-3.)

