
#' The main function of Robust Mixture Regression using five methods.
#'
#' @param lr.method A robust mixture regression method to be used. Should be one of "flexmix", "TLE", "CTLERob", "mixbi","mixLp".
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the predictor and response variables, where the last column is the response varible.
#' @param nc Number of mixture components.
#' @param nit Number of iterations for CTLE, mixbi, mixLp.
#' @param tRatio Trimming proportion for TLE method.
#' @param MaxIt Maximum iteration for TLE method.
#' @return An S4 object about the regression result.
#' @examples
#' library(RobMixReg)
#' #library(robust)
#' library(flexmix)
#' library(robustbase)
#' library(MASS)
#' library(gtools)
#' # gaussData
#' x=(gaussData$x);y=as.numeric(gaussData$y);
#' formula01=as.formula("y~x")
#' example_data01=data.frame(x,y)
#' res_rmr = rmr(lr.method='flexmix', formula=formula01, data=example_data01)
#' res_rmr = rmr(lr.method='CTLERob', formula=formula01, data=example_data01)
#'
rmr <- function(lr.method="flexmix", formula=NULL, data=NULL, nc=2, nit=20, tRatio=0.05, MaxIt=200)
{
  if(is.null(formula)) stop('Please input formula!')
  if(is.null(data)) stop('data is null! Please input data')


  method.lib <- c('flexmix', 'TLE', 'CTLERob', 'mixbi','mixLp')
  if(!lr.method %in% method.lib) stop('lr.method must be chosen from "flexmix","TLE","CTLERob","mixbi","mixLp" !  ')

  if(lr.method=='flexmix')
  {
    res = flexmix(formula,data,k=nc)
  }

  if(lr.method=='CTLERob')
  {
    res = CTLERob(formula,data, nit,nc,rlr_method="ltsReg")
  }

  if(lr.method=='TLE')
  {
    res = TLE(formula,data, nc,tRatio,MaxIt)
  }

	if(lr.method=='mixbi')
	{
    res = mixlinrb_bi(formula,data,nc, nit)
	}

  if(lr.method=='mixLp')
  {
    res = mixLp(formula, data, nc, nit)
  }

  return(res)
}
