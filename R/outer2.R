

#' The main function of mining the latent relationship among variables.
#'
#' @param ml.method The option to select the four methods in vignette.
#' @param gene x variable.
#' @param outcome y variable .
#' @param b.formulaList The case b require the user provide the formula list. This enable the flexible mixture regression.
#' @param formula The linear relationship between two variables.
#' @param nc Number of mixture components.
#' @param nit Number of iterations for CTLE, mixbi, mixLp.
#' @param x The matrix x of the high dimension situation.
#' @param y The external outcome variable.
#' @param max_iter Maximum iteration for TLE method.
#' @return Main result object.
MLM <- function(ml.method="a", gene, outcome,
                b.formulaList=list(formula(outcome ~ gene),formula(outcome ~ 1)),
                formula=outcome~gene, nit=1,nc=2,
                x=NULL, y=NULL, max_iter=50)
{
  #if(is.null(formula)) stop('Please input formula!')

  method.lib <- c('a','b','c','d')
  if(!ml.method %in% method.lib) stop('ml.method must be chosen from the options "a","b","c","d" ! For more details please refer the package vignette.')


  if(ml.method=='a')
  {
    res = ltsReg(outcome, gene)
  }

  if(ml.method=='b')
  {
    res = mixtureReg(regData = data.frame(gene,outcome),formulaList = b.formulaList,mixingProb = "Constant")
  }

  if(ml.method=='c')
  {
    res = CTLERob(formula,data.frame(gene,outcome), nit,nc,rlr_method="ltsReg")
  }

  if(ml.method=='d')
  {
    res=CSMR(x,y,nit,nc,max_iter)
  }

  return(res)
}


#-----------------------------------------------------below are example code------------------------------------

#
# #-----------1
# library('RobMixReg')
# library("robustbase")
# data(heart)
# ## Default method works with 'x'-matrix and y-var:
# heart.x <- data.matrix(heart[, 1]) # the X-variables
# heart.y <- heart[,"clength"]
# e1 = ltsReg(heart.x, heart.y)
# class(e1)
# names(e1)
# e1$lts.wt
# e1$raw.weights
# e1$call
#
# mydata = data.frame(heart.x, heart.y)
# inds_in = which(e1$lts.wt == 1)
# par(mfrow=c(1,1))
# plot_CTLE(heart.y~heart.x,data=mydata,nc=1,inds_in=inds_in)
#
# set.seed(12345)
# gene = rnorm(n=300, mean=5, sd=1)
# inter = 1 ; slope = 0.8
# outcome = slope * gene + inter
# outcome = outcome + rnorm(n=length(outcome),mean=0, sd=0.3)
# id_outlier = sample(1:length(outcome), size = 0.1*length(outcome)) #10% of variable should be outliers
# outcome[id_outlier] = outcome[id_outlier] + rnorm(length(id_outlier), mean=0,sd=2)
# e2 = ltsReg(outcome, gene)
# inds_in = which(e2$lts.wt == 1)
# par(mfrow=c(1,2))
# plot_CTLE(outcome~gene,data=data.frame(gene,outcome),nc=1,inds_in=inds_in)
#
#
#
# ------------2
# library(mixtools)
# data("CO2data")
# head(CO2data)
# mx1 <- mixtureReg(
#   regData = CO2data,
#   formulaList = list(formula(CO2 ~ GNP),
#                      formula(CO2 ~ GNP)),
#   mixingProb = "Constant"
# )
#
# par(mfrow=c(1,1))
# plot_mixtureReg(mx1, which = 1)
# plot_mixtureReg(mx1, which = 2)
#
# ###
# set.seed(12345)
# gene = rnorm(n=300, mean=5, sd=1)
# outcome = rep(0, length(gene))
# slope1 = 0.8; inter1 = 0.2; slope2 = -0.01; inter2 = 4
# n <- length(gene)
# outcome[seq(n) %% 2 == 1] = slope1 * gene[seq(n) %% 2 == 1] + inter1
# outcome[seq(n) %% 2 == 1] = outcome[seq(n) %% 2 == 1] + rnorm(length(outcome)/2,mean=0,sd=0.1)
# outcome[seq(n) %% 2 == 0] = slope2 * gene[seq(n) %% 2 == 0] + inter2
# outcome[seq(n) %% 2 == 0] = outcome[seq(n) %% 2 == 0] + rnorm(length(outcome)/2,mean=0,sd=0.1)
# plot(gene, outcome)
#
# mx1 <- mixtureReg(
#   regData = data.frame(gene,outcome),
#   formulaList = list(formula(outcome ~ gene),
#                      formula(outcome ~ 1)),
#   mixingProb = "Constant"
# )
# par(mfrow=c(1,3))
# plot_mixtureReg(mx1, which = 1)
# plot_mixtureReg(mx1, which = 2)
#
#
#
#
#
# #------------3
# data(gaussData)
# dim(gaussData)
# head(gaussData)
# table(gaussData$c)
# inds_in=1:80
# formula=as.formula("y~x")
# x=runif(100)
# y=runif(100)
# data=gaussData[,-3]
# #data=data.frame(x,y)
# nc=2
#
# par(mfrow=c(1,1))
# res = CTLERob(formula,data, nit,nc,rlr_method="ltsReg")
# inds_in=res@inds_in
# plot_CTLE(y~x,data=data,nc=2,inds_in=inds_in)
#
#
#
# ###
# set.seed(12345)
# gene = rnorm(n=300, mean=5, sd=1)
# outcome = rep(0, length(gene))
# slope1 = 0.8; inter1 = 0.2; slope2 = -0.6; inter2 = 7
# n <- length(gene)
# outcome[seq(n) %% 2 == 1] = slope1 * gene[seq(n) %% 2 == 1] + inter1
# outcome[seq(n) %% 2 == 1] = outcome[seq(n) %% 2 == 1] + rnorm(length(outcome)/2,mean=0,sd=0.1)
# outcome[seq(n) %% 2 == 0] = slope2 * gene[seq(n) %% 2 == 0] + inter2
# outcome[seq(n) %% 2 == 0] = outcome[seq(n) %% 2 == 0] + rnorm(length(outcome)/2,mean=0,sd=0.1)
# #plot(gene, outcome)
# id_outlier = sample(1:length(outcome), size = 10) # Total number of outlier is 10
# outcome[id_outlier] = outcome[id_outlier] + rnorm(length(id_outlier), mean=0,sd=2)
# #plot(gene, outcome)
# res = CTLERob(outcome~gene,data.frame(gene,outcome), nit,nc,rlr_method="ltsReg")
# inds_in=res@inds_in
# plot_CTLE(outcome~gene,data.frame(gene,outcome),nc=2,inds_in=inds_in)
#
#
#
#
#
#
# #------------4
#
# n=400####need to loop through 100, 200 400
# bet1=bet2=rep(0,101)
# bet1[2:21]=sign(runif(20,-1,1))*runif(20,2,5)
# bet2[22:41]=sign(runif(20,-1,1))*runif(20,2,5)
# bet=rbind(bet1,bet2)
# pr=c(1,1)*0.5
# sigs=c(1,1)####need to loop through 0.5, 1, 2
# tmp_list = simu_data_sparse(n=n,bet=bet,pr=pr,sigma=sigs)
# nit=1
# nc=2
# max_iter=50
# x=tmp_list$x
# dim(x)
# y=tmp_list$y
# length(y)
# rrr=CSMR(x,y,nit,nc,max_iter)
# names(rrr)
#
# blockMap(rrr)
#
#




