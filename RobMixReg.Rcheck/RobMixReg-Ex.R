pkgname <- "RobMixReg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "RobMixReg-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RobMixReg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("TLE-methods")
### * TLE-methods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: TLE
### Title: TLE: robust mixture regression based on trimmed likelihood
###   estimation.
### Aliases: TLE TLE,formula,ANY,numeric,numeric,numeric-method
###   CTLE,formula,ANY,numeric,numeric,numeric-method

### ** Examples

library("RobMixReg")
formula01=as.formula("y~x")
x=(gaussData$x);y=as.numeric(gaussData$y);
example_data01=data.frame(x,y)
## No test: 
res = TLE(formula01,example_data01, nc=2,tRatio=0.05,MaxIt=200)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("TLE-methods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mixLp-methods")
### * mixLp-methods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mixLp
### Title: mixLp : mixLp_one estimates the mixture regression parameters
###   robustly using Laplace distribution based on multiply initial value..
### Aliases: mixLp mixLp,formula,ANY,numeric,numeric-method
###   CTLE,formula,ANY,numeric,numeric-method

### ** Examples

library("RobMixReg")
formula01=as.formula("y~x")
x=(gaussData$x);y=as.numeric(gaussData$y);
example_data01=data.frame(x,y)
res = mixLp(formula01, example_data01, nc=2, nit=20)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mixLp-methods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rmr")
### * rmr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rmr
### Title: The main function of Robust Mixture Regression using five
###   methods.
### Aliases: rmr

### ** Examples

library(RobMixReg)
#library(robust)
library(flexmix)
library(robustbase)
library(MASS)
library(gtools)
# gaussData
x=(gaussData$x);y=as.numeric(gaussData$y);
formula01=as.formula("y~x")
example_data01=data.frame(x,y)
res_rmr = rmr(lr.method='flexmix', formula=formula01, data=example_data01)
res_rmr = rmr(lr.method='CTLERob', formula=formula01, data=example_data01)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rmr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
