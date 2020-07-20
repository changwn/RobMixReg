#' Model selection function for low dimension data.
#'
#' @param gene x variable.
#' @param outcome y variable.
# library(MASS)
vali_model <- function(outcome, gene)
{
  #------------ model 1
  res.a = MLM(ml.method='a', outcome, gene)
  inter = res.a$coefficients[1]
  slope = res.a$coefficients[2]
  in.index = which(res.a$lts.wt==1)
  out.index = which(res.a$lts.wt==0)
  y_est = slope*gene[in.index] + inter
  sig_square = fitdistr(outcome[in.index]-y_est,"normal")$estimate[2]

  y_est2 = slope*gene[out.index] + inter
  sig_square2 = fitdistr(outcome[out.index]-y_est2,"normal")$estimate[2] + sig_square

  p = matrix(0, 2, length(gene))
  p[1, in.index] = dnorm(outcome[in.index]-y_est, mean=0, sd=sig_square, log=FALSE)
  p[2,out.index]= dnorm(outcome[out.index]-y_est2, mean=0, sd=sig_square2, log=FALSE)
  p1 = mean(apply(p,2,sum))
  # p1
  BIC1 = 5 * log(length(gene)) - 2*log(p1)
  print(paste("BIC1=", BIC1, sep=""))


  # plot(gene,outcome)

  #------------ model 2
  # res.b = MLM(ml.method='b', gene, outcome,
  #             b.formulaList = list(formula(outcome ~ gene),formula(outcome ~ 1)))
  res.b = MLM(ml.method='b', gene, outcome,
              b.formulaList = list(formula(outcome ~ gene),formula(outcome ~ gene)))
  slope1 = res.b$lmList[[1]]$coefficients[2]
  inter1 = res.b$lmList[[1]]$coefficients[1]
  slope2 = res.b$lmList[[2]]$coefficients[2]
  inter2 = res.b$lmList[[2]]$coefficients[1]
  index1 = which(res.b$posterior[[1]] > res.b$prior[[1]])
  index2 = which(res.b$posterior[[2]] > res.b$prior[[2]])

  y_est = slope1*gene[index1] + inter1
  sig_square = fitdistr(outcome[index1]-y_est,"normal")$estimate[2]

  y_est2 = slope2*gene[index2] + inter2
  sig_square2 = fitdistr(outcome[index2]-y_est2,"normal")$estimate[2]

  p_m2 = matrix(0, 2, length(gene))
  p_m2[1, index1] = dnorm(outcome[index1]-y_est, mean=0, sd=sig_square, log=FALSE)
  p_m2[2, index2] = dnorm(outcome[index2]-y_est2, mean=0, sd=sig_square2, log=FALSE)
  p2 = mean(apply(p_m2,2,sum))
  # p2
  BIC2 = 5 * log(length(gene)) - 2*log(p2)
  # plot_mixtureReg(res.b, which = 1)
  # BIC2
  print(paste("BIC2=", BIC2, sep=""))

  #-----------model 3
  result = tryCatch({
                    res.c = MLM(ml.method="c", gene, outcome)
                    # res.c@inds_in
                    # res.c@indout
                    index.e = res.c@indout
                    index.a = which(res.c@compwww[,1] > res.c@compwww[,2])
                    index.b = which(res.c@compwww[,2] > res.c@compwww[,1])
                    #res.c@compcoef
                    slope.a = res.c@compcoef[2,1]
                    slope.b = res.c@compcoef[2,2]
                    inter.a = res.c@compcoef[1,1]
                    inter.b = res.c@compcoef[1,2]

                    y_est.a = slope.a*gene[index.a] + inter.a
                    sig_square.a = fitdistr(outcome[index.a]-y_est.a,"normal")$estimate[2]

                    y_est.b = slope.b*gene[index.b] + inter.b
                    sig_square.b = fitdistr(outcome[index.b]-y_est.b,"normal")$estimate[2]

                    y_est.e = outcome[index.e]
                    sig_square.e = fitdistr(y_est.e,"normal")$estimate[2]
                    u.e =  fitdistr(y_est.e,"normal")$estimate[1]

                    p_m3 = matrix(0, 3, length(gene))
                    p_m3[1, index.a] = dnorm(outcome[index.a]-y_est.a, mean=0, sd=sig_square.a, log=FALSE)
                    p_m3[2, index.b] = dnorm(outcome[index.b]-y_est.b, mean=0, sd=sig_square.b, log=FALSE)
                    p_m3[3, index.e] = dnorm(y_est.e, mean=u.e, sd=sig_square.e, log=FALSE)
                    p3 = mean(apply(p_m3,2,sum))
                    # p3
                    BIC3 = 5 * log(length(gene)) - 2*log(p3)
                    # BIC3
                    print(paste("BIC3=", BIC3, sep=""))

                    # inds_in=res.c@inds_in
                    # par(mfrow=c(1,2),mar = c(4, 4, 6, 2))
                    # plot_CTLE(outcome~gene,data.frame(gene,outcome),nc=2,inds_in=inds_in)
                    }
                    , error = function(e){ warning("Do NOT have the pattern as example 3!") })

}


