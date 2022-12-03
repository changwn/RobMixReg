
#' The simulation function for low dimensional space.
#'
#' @param beta The slope vector.
#' @param inter The intercept vector.
#' @param alpha The parameter to control the number of outliers.
#' @return A list object consists of the x variable in low dimensional space and the external y variable.
simu_low <- function(beta, inter, alpha=NULL)
{
  if(length(beta) != length(inter)) stop("The input of beta and inter have different length!")

  if(length(beta) == 1 & !is.null(alpha)){ #case 1
    # set the seed for randomization
    set.seed(12345)
    x = rnorm(n=300, mean=5, sd=1)
    # inter = 1 ; beta = 0.8
    y = beta * x + inter
    y = y + rnorm(n=length(y),mean=0, sd=0.3)
    # add the outliers
    # 10% of variable should be outliers, user can customize the percentage
    id_outlier = sample(1:length(y), size = alpha * length(y))
    y[id_outlier] = y[id_outlier] + rnorm(length(id_outlier), mean=0,sd=2)
    # x.case1 = x; y.case1 = y;

    return(list(x=x,y=y))
  }
  if(length(beta) == 2 & is.null(alpha)){ #case 2
    set.seed(12345)
    x = rnorm(n=300, mean=5, sd=1)
    y = rep(0, length(x))
    # beta1 = 0.8; inter1 = 0.2; beta2 = -0.01; inter2 = 4
    beta1 = beta[1]; beta2 = beta[2];
    inter1 = inter[1]; inter2 = inter[2]
    n <- length(x)
    y[seq(n) %% 2 == 1] = beta1 * x[seq(n) %% 2 == 1] + inter1
    y[seq(n) %% 2 == 1] = y[seq(n) %% 2 == 1] + rnorm(length(y)/2,mean=0,sd=0.1)
    y[seq(n) %% 2 == 0] = beta2 * x[seq(n) %% 2 == 0] + inter2
    y[seq(n) %% 2 == 0] = y[seq(n) %% 2 == 0] + rnorm(length(y)/2,mean=0,sd=0.1)
    # x.case2 = x; y.case2 = y;

    return(list(x=x,y=y))
  }
  if(length(beta) == 2 & !is.null(alpha)){ # case 3

    set.seed(12345)
    x = rnorm(n=300, mean=5, sd=1)
    y = rep(0, length(x))
    # slope1 = 0.8; inter1 = 0.2; slope2 = -0.6; inter2 = 7
    beta1 = beta[1]; beta2 = beta[2];
    inter1 = inter[1]; inter2 = inter[2]
    n <- length(x)
    y[seq(n) %% 2 == 1] = beta1 * x[seq(n) %% 2 == 1] + inter1
    y[seq(n) %% 2 == 1] = y[seq(n) %% 2 == 1] + rnorm(length(y)/2,mean=0,sd=0.1)
    y[seq(n) %% 2 == 0] = beta2 * x[seq(n) %% 2 == 0] + inter2
    y[seq(n) %% 2 == 0] = y[seq(n) %% 2 == 0] + rnorm(length(y)/2,mean=0,sd=0.1)
    # The size parameter control the number of outliers during simulation.
    id_outlier = sample(1:length(y), size = alpha * length(y))
    y[id_outlier] = y[id_outlier] + rnorm(length(id_outlier), mean=0,sd=2)
    x3=x ;y3=y
    # x.case3 = x; y.case3 = y;

    return(list(x=x,y=y))
  }

}


#' The simulation function for low/high dimensional space.
#'
#' @param beta The slope vector for low dimensional space or matrix for high dimensional space.
#' @param sigma A vector whose k-th element is the standard deviation for the k-th regression component.
#' @param alpha The parameter to control the number of outliers for low dimensional space.
#' @param n The sample number for high dimensional data.
#' @return A list object.
simu_func <- function(beta, sigma, alpha=NULL, n=400){

  if(is.null(dim(beta))){
    # print('simu1')
    inter = beta[1]
    beta = beta[2]
    obj = simu_low(beta, inter, alpha)
    return(obj)
  }
  if(!is.null(dim(beta)) & ncol(beta) <= 2){
    # print('simu2')
    inter = beta[, 1]
    beta = beta[,2]
    obj = simu_low(beta, inter, alpha)
    return(obj)
  }
  if((!is.null(dim(beta)))  & (ncol(beta) > 2) ){
    # print('simu high')
    set.seed(12345)
    # n
    # bet1=bet2=rep(0,101)
    # bet1[2:21]=sign(runif(20,-1,1))*runif(20,2,5)
    # bet2[22:41]=sign(runif(20,-1,1))*runif(20,2,5)
    # bet=rbind(bet1,bet2)
    pr=c(1,1)*0.5
    sigs=c(1,1)
    obj = simu_data_sparse(n=n,bet=beta,pr=pr,sigma=sigs)
    return(obj)
  }


}
