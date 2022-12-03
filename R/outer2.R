

#' The main function of mining the latent relationship among variables.
#'
#' @param ml.method The option to select the four methods in vignette.
#' @param rmr.method The option to select the robust mixture regression method.
#' @param b.formulaList The case b require the user provide the formula list. This enable the flexible mixture regression.
#' @param formula The linear relationship between two variables.
#' @param nc Number of mixture components.
#' @param nit Number of iterations for CTLE, mixbi, mixLp.
#' @param x The matrix x of the high dimension situation.
#' @param y The external outcome variable.
#' @param max_iter Maximum iteration for TLE method.
#' @param tRatio The ratio of the outliers in the TLE robust mixture regression method.
#' @return Main result object.
MLM <- function(ml.method="rlr", rmr.method='cat',
                b.formulaList=list(formula(y ~ x),formula(y ~ 1)),
                formula=y~x, nc=2,nit=1,
                x=NULL, y=NULL, max_iter=50,  tRatio=0.05)
{
  #if(is.null(formula)) stop('Please input formula!')

  method.lib <- c('rlr','fmr','rmr','hrmr')
  if(!ml.method %in% method.lib) stop('ml.method must be chosen from the options "rlr","fmr","rmr","hrmr" ! For more details please refer the package vignette.')


  if(ml.method=='rlr')
  {
    res = ltsReg(x, y)
    coff = res$coefficients
    coff = matrix(coff,length(coff),1)
    rownames(coff) = c("Intercept",rep("coef.x",nrow(coff)-1))
    if(is.null(dim(x))) x = matrix(x,length(x),1)

    inter = res$coefficients[1]
    slope = res$coefficients[-1]
    slope = matrix(slope,1,length(slope))
    in.index = which(res$lts.wt==1)
    out.index = which(res$lts.wt==0)
    y_est = slope %*% t(x[in.index,,drop=FALSE]) + inter
    sig_square = fitdistr(y[in.index]-y_est,"normal")$estimate[2]
    sd = sig_square;
    names(sd) = NULL
    coff = rbind(coff,sd)

    like = (2*pi*sig_square)^(-length(x)/2) * exp(-1/(2*sig_square)*sum((y[in.index]-y_est)^2) )
    BIC = - 2*log(like) + (length(slope)+2) * log(length(x))
    names(BIC) = NULL

    result = list(coff=coff, cluMem=res$lts.wt, BIC=BIC)
    return(result)
  }

  if(ml.method=='fmr')
  {
    res = mixtureReg(regData = data.frame(x,y),formulaList = b.formulaList,mixingProb = "Constant")
    if(is.null(dim(x))) x = matrix(x,length(x),1)
    nVar = max( sapply(b.formulaList, function(cc){ length(attr(terms(cc),'term.labels')) }) )

    coff = matrix(0, nVar+1, length(b.formulaList))
    sig = matrix(0,1,length(b.formulaList))
    cluMem = rep(0, length(y))
    for(k in 1:length(b.formulaList)){
      #coff
      par = coefficients(res$lmList[[k]])
      length(par) = nVar+1
      coff[,k] = par
      coff[is.na(coff)] = 0

      #cluster
      loca = which(res$posterior[[k]] > res$prior[[k]])
      cluMem[loca] = k

      #sig
      y_fit = res$lmList[[k]]$fitted.values
      sig[1,k] = fitdistr(y[loca] - y_fit[loca],"normal")$estimate[2]
    }
    rownames(coff) = c("Intercept",rep('coff.x',nVar))

    dK = 2*length(b.formulaList) - 1 + sum(coff[-1,] != 0 )
    BIC = -2* res$logLik + dK * length(y)

    rownames(sig) = 'sd'
    coff = rbind(coff, sig)
    mx = table(cluMem) / length(y)
    coff = rbind(coff, mx)

    result = list(coff=coff, cluMem=cluMem, BIC=BIC,res=res)
    return(result)
  }

  if(ml.method=='rmr')
  {
    if(rmr.method=='cat'){
      res = rmr(lr.method='CTLERob', formula=formula, data=data.frame(x,y), nc)
      coff = res@compcoef
      cluMem=res@ctleclusters

      if(is.null(dim(x))) x = matrix(x,length(x),1)
      logLike = 0
      for(k in 1:length(table(cluMem)[-1])){
        loca = which(cluMem == k)
        beta = coff[2:(nrow(coff)-2),k]
        beta = matrix(beta,1,length(beta))
        tmp = length(loca)*log(coff[nrow(coff),k]) + sum(log(dnorm(y[loca]-beta %*% t(x[loca,1,drop=FALSE]) -coff[1,k], mean=0, sd=coff[nrow(coff)-1,k], log=FALSE)) )
        logLike = logLike + tmp
      }
      dK = 2*nc-1 + length( which(coff[2:(nrow(coff)-2),] != 0) )
      BIC = -2*logLike + log(length(which(res@inds_in != -1)))*dK # length, not sum.

      result = list(coff=coff, cluMem=cluMem, BIC=BIC)
      return(result)
    }
    if(rmr.method=='flexmix')
      res = rmr(lr.method='flexmix', formula=formula, data=data.frame(x,y), nit,nc)
    if(rmr.method=='TLE')
      res = rmr(lr.method='TLE', formula=formula, data=data.frame(x,y), nit,nc,tRatio)
    if(rmr.method=='mixLp')
      res = rmr(lr.method='mixLp', formula=formula, data=data.frame(x,y), nit,nc)
    if(rmr.method=='mixbi')
      res = rmr(lr.method='mixbi', formula=formula, data=data.frame(x,y), nit,nc)


    return(res)
  }

  if(ml.method=='hrmr')
  {
    res=CSMR_train(x,y,nit,nc,max_iter)
    coff = res$coffs

    sig = matrix(0,1,nc)
    for(k in 1:nc){
      y_fit = res$yhat
      loca = which(res$clus == k)
      sig[1,k] = fitdistr(y[loca] - y_fit[loca],"normal")$estimate[2]
    }
    rownames(sig) = 'sd'
    coff = rbind(coff, sig)

    cluMem = res$clus
    mx = table(cluMem) / length(y)
    coff = rbind(coff, mx)

    log_mix = res$infoAll$mx.model$logLik
    dK = 2*nc -1 + sum(res$coffs != 0 )
    BIC = -2*log_mix + dK*length(y)

    result = list(coff=coff, cluMem=cluMem, BIC=BIC, res=res)
    return(result)
  }

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




