
# vali_model <- function(y, x)
# {
#   #------------ model 1
#   res.a = MLM(ml.method='rlr', xlow=x, ylow=y)
#   inter = res.a$coefficients[1]
#   slope = res.a$coefficients[2]
#   in.index = which(res.a$lts.wt==1)
#   out.index = which(res.a$lts.wt==0)
#   y_est = slope*x[in.index] + inter
#   sig_square = fitdistr(y[in.index]-y_est,"normal")$estimate[2]
#
#   y_est2 = slope*x[out.index] + inter
#   sig_square2 = fitdistr(y[out.index]-y_est2,"normal")$estimate[2] + sig_square
#
#   p = matrix(0, 2, length(x))
#   p[1, in.index] = dnorm(y[in.index]-y_est, mean=0, sd=sig_square, log=FALSE)
#   p[2,out.index]= dnorm(y[out.index]-y_est2, mean=0, sd=sig_square2, log=FALSE)
#   p1 = mean(apply(p,2,sum))
#   # p1
#   BIC1 = 5 * log(length(x)) - 2*log(p1)
#   print(paste("BIC1=", BIC1, sep=""))
#
#
#   # plot(x,y)
#
#   #------------ model 2
#   # res.b = MLM(ml.method='b', gene, outcome,
#   #             b.formulaList = list(formula(outcome ~ gene),formula(outcome ~ 1)))
#   res.b = MLM(ml.method='fmr', xlow=x, ylow=y,
#               b.formulaList = list(formula(ylow ~ xlow),formula(ylow ~ xlow)))
#   slope1 = res.b$lmList[[1]]$coefficients[2]
#   inter1 = res.b$lmList[[1]]$coefficients[1]
#   slope2 = res.b$lmList[[2]]$coefficients[2]
#   inter2 = res.b$lmList[[2]]$coefficients[1]
#   index1 = which(res.b$posterior[[1]] > res.b$prior[[1]])
#   index2 = which(res.b$posterior[[2]] > res.b$prior[[2]])
#
#   y_est = slope1*x[index1] + inter1
#   sig_square = fitdistr(y[index1]-y_est,"normal")$estimate[2]
#
#   y_est2 = slope2*x[index2] + inter2
#   sig_square2 = fitdistr(y[index2]-y_est2,"normal")$estimate[2]
#
#   p_m2 = matrix(0, 2, length(x))
#   p_m2[1, index1] = dnorm(y[index1]-y_est, mean=0, sd=sig_square, log=FALSE)
#   p_m2[2, index2] = dnorm(y[index2]-y_est2, mean=0, sd=sig_square2, log=FALSE)
#   p2 = mean(apply(p_m2,2,sum))
#   # p2
#   BIC2 = 5 * log(length(x)) - 2*log(p2)
#   # plot_mixtureReg(res.b, which = 1)
#   # BIC2
#   print(paste("BIC2=", BIC2, sep=""))
#
#   #-----------model 3
#   result = tryCatch({
#                     res.c = MLM(ml.method="rmr", xlow=x, ylow=y)
#                     # res.c@inds_in
#                     # res.c@indout
#                     index.e = res.c@indout
#                     index.a = which(res.c@compwww[,1] > res.c@compwww[,2])
#                     index.b = which(res.c@compwww[,2] > res.c@compwww[,1])
#                     #res.c@compcoef
#                     slope.a = res.c@compcoef[2,1]
#                     slope.b = res.c@compcoef[2,2]
#                     inter.a = res.c@compcoef[1,1]
#                     inter.b = res.c@compcoef[1,2]
#
#                     y_est.a = slope.a*x[index.a] + inter.a
#                     sig_square.a = fitdistr(y[index.a]-y_est.a,"normal")$estimate[2]
#
#                     y_est.b = slope.b*x[index.b] + inter.b
#                     sig_square.b = fitdistr(y[index.b]-y_est.b,"normal")$estimate[2]
#
#                     y_est.e = y[index.e]
#                     sig_square.e = fitdistr(y_est.e,"normal")$estimate[2]
#                     u.e =  fitdistr(y_est.e,"normal")$estimate[1]
#
#                     p_m3 = matrix(0, 3, length(x))
#                     p_m3[1, index.a] = dnorm(y[index.a]-y_est.a, mean=0, sd=sig_square.a, log=FALSE)
#                     p_m3[2, index.b] = dnorm(y[index.b]-y_est.b, mean=0, sd=sig_square.b, log=FALSE)
#                     p_m3[3, index.e] = dnorm(y_est.e, mean=u.e, sd=sig_square.e, log=FALSE)
#                     p3 = mean(apply(p_m3,2,sum))
#                     # p3
#                     BIC3 = 5 * log(length(x)) - 2*log(p3)
#                     # BIC3
#                     print(paste("BIC3=", BIC3, sep=""))
#
#                     # inds_in=res.c@inds_in
#                     # par(mfrow=c(1,2),mar = c(4, 4, 6, 2))
#                     # plot_CTLE(outcome~gene,data.frame(gene,outcome),nc=2,inds_in=inds_in)
#                     }
#                     , error = function(e){ warning("Do NOT have the pattern as example 3!") })
#
# }


#' Model selection function for low dimension data.
#'
#' @param  ml.method The parameter to choose the fitted model for calculating the BIC
#' @param x x variable.
#' @param y y variable.
#' @param nc The component number for low dimensional feature
#' @param formulaList The list of target formular
#' @param K The component number for high dimensional feature
#' @return BIC value.
MLM_bic <- function(ml.method='rlr', x, y, nc=1, formulaList=NULL,K=2){

    # 1. rlr or rmr
    if(ml.method=='rlr' & nc==1){
      # y=y1;x=x1;
      # y=y1;x=cbind(x1,2*x1+rnorm(length(y),0,1))
      res = ltsReg(x, y)

      inter = res$coefficients[1]
      slope = res$coefficients[2]
      in.index = which(res$lts.wt==1)
      out.index = which(res$lts.wt==0)
      y_est = slope*x[in.index] + inter
      sig_square = fitdistr(y[in.index]-y_est,"normal")$estimate[2]

      like = (2*pi*sig_square)^(-length(x)/2) * exp(-1/(2*sig_square)*sum((y[in.index]-y_est)^2) )
      BIC = - 2*log(like) + 3 * log(length(x))
      names(BIC) = NULL
      return(BIC)
    }
    if(ml.method=='rmr' & nc==2){
      # y=y3;x=x3;nc=2
      # res = CTLERob(formula=y~x,data=data.frame(x,y), nit=1,nc,rlr_method="ltsReg")
      res = TLE(formula=y~x,data=data.frame(x,y), nc,tRatio=0.05,MaxIt=10)
      in.index = res@inds_in
      #res.c@indout
      ddd = data.frame(x[in.index],y[in.index]); colnames(ddd)=c('x','y')
      res = mixtureReg(regData = ddd,formulaList = list(formula(y ~ x),formula(y ~ x)),mixingProb = "Constant", silently = TRUE)
      L = -res$logLik
      BIC = -2*L + 5*log(length(x))
      return(BIC)
    }
    if(ml.method=='rmr' & nc==3){
      # y=y3;x=x3;nc=3
      # res = CTLERob(formula=y~x,data=data.frame(x,y), nit=1,nc,rlr_method="ltsReg")
      res = TLE(formula=y~x,data=data.frame(x,y), nc,tRatio=0.05,MaxIt=10)
      in.index = res@inds_in
      #res.c@indout
      ddd = data.frame(x[in.index],y[in.index]); colnames(ddd)=c('x','y')
      res = mixtureReg(regData = ddd,formulaList = list(formula(y ~ x),formula(y ~ x),formula(y ~ x)),mixingProb = "Constant", silently = TRUE)
      L = -res$logLik
      BIC = -2*L + 7*log(length(x))
      return(BIC)
    }
    # 2. fmr
    if(ml.method=='fmr'){
      # x=x2;y=y2;formulaList = list(formula(y ~ x),formula(y ~ x+ I(x^2)))
      res = mixtureReg(regData=data.frame(x,y),formulaList = formulaList,mixingProb = "Constant", silently = TRUE)
      L = -res$logLik
      # print(L)

      if(formulaList[[1]]==formula(y ~ x) & formulaList[[2]]==formula(y ~ 1)) k=4
      if(formulaList[[1]]==formula(y ~ x) & formulaList[[2]]==formula(y ~ x)) k=5
      if(formulaList[[1]]==formula(y ~ x) & formulaList[[2]]==formula(y ~ x+ I(x^2))){k=6}
      if(length(formulaList)==3){
        if(formulaList[[1]]==formula(y ~ x) & formulaList[[2]]==formula(y ~ x) & formulaList[[3]]==formula(y ~ x)) k=7
      }


      BIC = -2*L + k*log(length(x))
      # print(-2*L)
      # print(k*log(length(x)))
      # print(BIC)
      return(BIC)
    }
    # 3.high dimensional
    if(ml.method=='hrmr'){
      # x=hx;y=hy
      CSMR_res=CSMR_train(x, y , nit=1, nc=K, max_iter=10)
      log_mix = CSMR_res$infoAll$mx.model$logLik
      dK = 2*K-1 + sum(CSMR_res$coffs != 0 )
      BIC = -2*log_mix + dK*length(y)
      return(BIC)
    }


}



#' Cross validation (fold-5) function for high dimension data.
#'
#' @param x x variable.
#' @param y y variable.
#' @param nit Iteration number.
#' @param nc The number of component.
#' @param max_iter Maximum iteration.
#' @return The correlation between y and y_hat based on five fold cross validation.
MLM_cv <- function(x=NULL, y=NULL, nit=1, nc=2, max_iter=50){

  # x = hx;y=hy
  Nsample = length(y)
  Ntest = Nsample / 5
  if(Nsample %% 5 != 0) stop('The total sample number does not allow 5 fold CV!')

  cor_storage <- matrix(0,1,5)
  rmse_storage <- matrix(0,1,5)
  for(myk in 1:5){
    test_id = ((myk-1)*Ntest + 1) : ( myk * Ntest )
    # print(test_id)
    # print('----')

    x.train<-x[-test_id,]
    y.train<-y[-test_id]
    x.test<-x[test_id,]
    y.test<-y[test_id]

    CSMR_res=CSMR_train(x.train,y.train,nit=1,nc=3,max_iter=5)
    c_model=CSMR_res$CSMR.model
    c_coffs=CSMR_res$coffs
    yhat_k2=CSMR_predict(c_coffs,c_model,xnew=x.test,ynew=y.test)
    # yhat_k2=CSMR(x.test,y.test, nit, nc, max_iter)$yhat

    cor_storage[1, myk] = cor(yhat_k2, y.test,use="complete.obs")
    rmse_storage[1,myk] = sqrt( sum((yhat_k2 - y.test)^2) )

    print(paste("Fold ",myk,' done.',sep=''))
  }
  score_cor = apply(cor_storage,1, mean) # correlation based on 5-fold CV.
  score_rmse = apply(rmse_storage,1,mean)
  return(list(ycor=score_cor, RMSE=score_rmse))
}



#
# high_likeli <- function(x=NULL, y=NULL, nit=1, nc=2, max_iter=50){
#
#   CSMR_res=CSMR_train(x, y , nit, nc, max_iter)
#   log_mix = CSMR_res$infoAll$mx.model$logLik
#
#   return(log_mix)
# }

