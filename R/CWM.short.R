# library(ibdreg)  ### this library contains the Ginv function to calculate g-inverse of a matrix
setClass("RobMixReg",
         representation(inds_in="ANY",
                        indout="ANY",
                        ctleclusters="ANY",
                        compcoef="ANY",
                        comppvals="ANY",
                        compwww="ANY",
                        call="call"))

#' TreatSingularity: see original CWM method
#' @description see original CWM method.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return see original CWM method.
TreatSingularity <- function (iter, pa)
{
  warning ("points in the data set are concentrated in k subspaces after trimming ")
  iter$code = 2
  return (iter)
}

#' calcobj_i: it calculates the objective function
#' @description function to calculate the objective function.
#' @param X independent variable.
#' @param Y response variable.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return see original CWM method.
calcobj_i <- function (X,Y, iter, pa)   ### it calculates the objective function
{
  ww=matrix(0,nrow=pa$n,ncol=1)
  one=matrix(1,nrow=pa$n,ncol=1)
  for (k in 1:pa$K) {
    w=iter$cw[k]*dmnorm(X,iter$center[k,],ssclmat (iter$sigma,k)) * dnorm(Y,cbind(one,X)%*%cbind(iter$b[k,]),sd=sqrt(iter$v[k]))
    w=w*(w>=0)
    ww=w+ww
  }

  iter$obj <- mean(log(ww[iter$assig>0]))

  return (iter)
}

#' findClustAssig_i: it obtains the assigment corresponding to each point to the K populations and the set of trimmed points
#' @description see original CWM method.
#' @param X independent variable.
#' @param Y response variable.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return see original CWM method.
findClustAssig_i <- function (X,Y, iter, pa)  ### it obtains the assigment corresponding to each point to the K populations and the set of trimmed points
{

  ll = matrix (NA, pa$n, pa$K)
  one=matrix(1,nrow=pa$n,ncol=1)

  for (k in 1:pa$K)
    ll[,k] <- iter$cw[k]*dmnorm(X,iter$center[k,],ssclmat (iter$sigma,k)) * dnorm(Y,cbind(one,X)%*%cbind(iter$b[k,]),sd=sqrt(iter$v[k]))
  ll <- ll*(ll>=0)

  iter$z_ij<- matrix (0, ncol = pa$K, nrow = pa$n)
  pre.z=apply(ll,1,'sum')

  pre.z_=matrix(pre.z, nrow=pa$n, ncol=pa$K,byrow=FALSE)

  iter$z_ij=ll/(pre.z_ + (pre.z_ ==0))+(pre.z ==0)%*%matrix(rep(0,pa$K),nrow=1)*t(rmultinom(pa$n, 1, iter$cw))

  dsf=pre.z
  yy=order(dsf,decreasing = FALSE)
  zz=rep(1,pa$n)
  if (round(pa$alpha*pa$n)>=1 )  zz[yy[1:round(pa$alpha*pa$n)]]=0
  zzm=matrix(zz,nrow=pa$n, ncol=pa$K,byrow=FALSE)
  iter$z_ij=iter$z_ij*zzm

  iter$assig <- apply(iter$z_ij,1,which.max)*zz

  iter$csize <- colSums (iter$z_ij)
  iter$cw <- iter$csize / sum(zz)

  return (iter)
}

#' dmnorm: multivariate normal density function
#' @description see original CWM method.
#' @param X see original CWM method.
#' @param mu see original CWM method.
#' @param sigma see original CWM method.
#' @return see original CWM method.
dmnorm <- function (X,mu,sigma) ((2 * pi)^(-length(mu) / 2)) * (det(sigma)^(-1/ 2)) * exp (-0.5 * mahalanobis (X, mu, sigma)) ### multivariate normal density function


#' ssclmat: see original CWM method
#' @description see original CWM method.
#' @param x see original CWM method.
#' @param k see original CWM method.
#' @return see original CWM method.
ssclmat <- function (x, k) as.matrix (x[,,k])

#' getini: see original CWM method
#' @description see original CWM method.
#' @param K see original CWM method.
#' @param n see original CWM method.
#' @return see original CWM method.
getini <- function (K, n)   ### it is used inside function InitClusters_i
{
  if (K == 1)
    return (n)

  pi.ini <- runif(K)
  ni.ini <- sample(x = K, size = n, replace = T, prob = pi.ini / sum (pi.ini))
  return (tabulate(ni.ini, nbins = K))
}


#' InitClusters_i: see original CWM method
#' @description see original CWM method.
#' @param X independent variable.
#' @param Y response variable.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return see original CWM method.
InitClusters_i <- function (X, Y, iter, pa)  ### it obtains random initial solution
{
  dMaxVar = 0
  for (k in 1:pa$K)
  {
    idx <- sample (1:pa$n, pa$init+1)
    X.ini = X [drop = F,idx,]
    cc <- (pa$init/(pa$init+1))*cov (X.ini)
    iter$sigma[,,k] <- cc
    iter $center [k,] <- colMeans (X.ini)

    Y.ini = Y [drop = F,idx,]
    one=matrix(1,nrow=pa$init+1,ncol=1)
    X.ini=cbind(one,X.ini)


    iter $b [k,] <- t(Ginv(t(X.ini)%*%X.ini)$Ginv%*%t(X.ini)%*%Y.ini)
    e=Y.ini-X.ini%*%cbind(iter $b [k,])
    iter$v[k] <- (pa$init/(pa$init+1))*cov (e)

  }

  iter$csize = getini (pa$K, pa$n)
  iter$cw <- iter$csize / pa$n

  return (iter)
}


#' estimClustPar_i: see original CWM method
#' @description see original CWM method.
#' @param X independent variables.
#' @param Y response variable.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return xsee original CWM method.
estimClustPar_i <- function (X, Y, iter, pa) ### it obtains estimations for the model parameters
{
  for (k in 1:pa$K){
    if (iter$csize[k] > pa$zero.tol){
      iter$center[k,] = (t(iter$z_ij[,k]) %*% X) / iter$csize[k]
      X.c <- (X - matrix (iter$center[k,], ncol = pa$p, nrow = pa$n, byrow = TRUE))
      iter$sigma[,,k] <- (t(X.c * iter$z_ij[,k]) %*% X.c) / iter$csize[k]
      one=matrix(1,nrow=pa$n,ncol=1)
      X_=cbind(one,X)
      iter $b [k,] <- t(Ginv(  t(X_*iter$z_ij[,k])%*%X_  )$Ginv%*%t(X_*iter$z_ij[,k])%*%Y)
      e=Y-X_%*%cbind(iter $b [k,])
      iter$v[k] <- sum (iter$z_ij[,k]*e*e)/ iter$csize[k]
    }
    else
      iter$sigma[,,k] <- 0
  }
  return (iter)
}


#' restr.diffax_i1: see original CWM method
#' @description see original CWM method.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return see original CWM method.
restr.diffax_i1 <- function (iter, pa)  ### it applies restrictions to the eigenvalues of covariance matrices of X
{
  u = array (NA, c(pa$p, pa$p, pa$K))
  d = array (NA, c(pa$p, pa$K))
  for (k in 1:pa$K){
    ev = eigen (iter$sigma[,,k])
    u [,,k] <- ev$vectors
    d [,k] <- ev$values
  }
  d [d < 0] <- 0
  d=restr2_eigenv (d, iter$csize, pa$maxfactx, pa$zero.tol)
  iter$code = max(d) > pa$zero.tol1
  if (!iter$code)
    return (iter)

  for (k in 1:pa$K)
    iter$sigma[,,k] <- u[,,k] %*% diag (d[,k], nrow = pa$p) %*% t(u[,,k])
  return (iter)
}

#' restr.diffax_i2: see original CWM method
#' @description see original CWM method.
#' @param iter see original CWM method.
#' @param pa see original CWM method.
#' @return see original CWM method.
restr.diffax_i2 <- function (iter, pa) ### it applies restrictions to the variances of regression residuals
{
  u = array (NA, c(1, 1, pa$K))
  d = array (NA, c(1 , pa$K))
  for (k in 1:pa$K){
    d [,k] <- iter$v[k]
  }

  d [d < 0] <- 0
  d=restr2_eigenv (d, iter$csize, pa$maxfacty, pa$zero.tol2)
  iter$code = max(d) > pa$zero.tol2
  if (!iter$code)
    return (iter)
  for (k in 1:pa$K)
    iter$v[k] <- d[k]
  return (iter)
}


#' restr2_eigenv: see original CWM method
#' @description see original CWM method.
#' @param autovalues see original CWM method.
#' @param ni.ini see original CWM method.
#' @param factor see original CWM method.
#' @param zero.tol see original CWM method.
#' @return see original CWM method.
restr2_eigenv <- function(autovalues, ni.ini, factor, zero.tol)   ### it is used in restr.diffax_i1 and restr.diffax_i2     It is the basic function for obtaining restrincions
{
  c=factor
  d=t(autovalues)
  p = nrow (autovalues)
  K = ncol (autovalues)
  n=sum(ni.ini)
  nis = matrix(data=ni.ini,nrow=K,ncol=p)

  d_=sort(c(d,d/c))
  dim=length(d_)
  d_1=d_
  d_1[dim+1]=d_[dim]*2
  d_2=c(0,d_)
  ed=(d_1+d_2)/2
  dim=dim+1;


  if ((max(d[nis>0]) <= zero.tol))
    return (matrix (0, nrow = p, ncol = K))


  if (max(d[nis>0])/min(d[nis>0])<=c){
    d[nis==0]=mean(d[nis>0])
    return (t (d))
  }

  t <- s <- r <- array(0,c(K,dim))
  sol <- sal <- array(0,c(dim))

  for (mp_ in 1:dim){
    for (i in 1:K)
    {
      r[i,mp_]=sum((d[i,]<ed[mp_]))+sum((d[i,]>ed[mp_]*c))
      s[i,mp_]=sum(d[i,]*(d[i,]<ed[mp_]))
      t[i,mp_]=sum(d[i,]*(d[i,]>ed[mp_]*c))
    }

    sol[mp_]=sum(ni.ini/n*(s[,mp_]+t[,mp_]/c))/(sum(ni.ini/n*(r[,mp_])))

    e = sol[mp_]*(d<sol[mp_])+d*(d>=sol[mp_])*(d<=c*sol[mp_])+(c*sol[mp_])*(d>c*sol[mp_])
    o=-1/2*nis/n*(log(e)+d/e)

    sal[mp_]=sum(o)
  }

  eo=which.max(c(sal))
  m=sol[eo]


  t (m*(d<m)+d*(d>=m)*(d<=c*m)+(c*m)*(d>c*m))
}


#' trim.cwm: main function
#' @description main function adapted from the original developer of CWM method.
#' @param X independent variables.
#' @param Y response variable.
#' @param K number of clusters.
#' @param niter see original CWM method.
#' @param Ksteps see original CWM method
#' @param maxfactx see original CWM method
#' @param maxfacty see original CWM method
#' @param zero.tol see original CWM method
#' @param zero.tol1 see original CWM method
#' @param zero.tol2  see original CWM method
#' @param trace see original CWM method
#' @param alpha see original CWM method
#' @param init number of initializations
#' @return an RobMixReg object.
### main function
trim.cwm <- function(X,Y,K,niter = 20, Ksteps=10, maxfactx=5, maxfacty=5,  zero.tol = 1e-16, zero.tol1 = 1e-16,zero.tol2 = 1e-90, trace = 0, alpha=0.1,init=5)
{

  if (!is.numeric (X))
    stop ("parameter x: numeric matrix/vector expected")
  if (!is.numeric (Y))
    stop ("parameter y: numeric matrix/vector expected")
  if( !is.matrix (X))
    X <- matrix (X, ncol = 1)
  if( !is.matrix (Y))
    Y <- matrix (Y, ncol = 1)
  n <- nrow (X)
  p <- ncol (X)

  if (!((nrow (Y)==n)&(ncol (Y)==1)) )
    stop ("parameter y: erroneo")

  # preparing lot's of lists
  pa <- list (
    n = n,		                        # number of points
    p = p,					# number of points
    K = K,					# number of points
    zero.tol = zero.tol,
    zero.tol1 = zero.tol1,
    zero.tol2 = zero.tol2,
    trace = trace,
    maxfactx=maxfactx,                      # restriction level for X
    maxfacty=maxfacty,                      # restriction level for regression residuals
    alpha=alpha,                            # level of trimming
    init=init   	                        # number of observations (for each population) for build random initial solutions
  )



  iter <- list (
    obj = NA,	                        # objective function value
    assig = array (0, n),			# vector with populations assigments for each point     0 corresponds to triimmed points
    csize = array (NA, K),		        # vector with number of observations assigned to each population
    cw = rep (NA, K),			# vector of estimated population proportions
    sigma = array (NA, c (p, p, K)),	# covariance matrices of X variables (for all populations)
    center = array (NA, c(K, p)),		# means of X variables	(for all populations)
    b = array (NA, c(K, p+1)),	        # regression parameters (for all populations)
    v = array (NA, c(1, p)),		# variance of regression residuals  (for all populations)
    code = NA,
    z_ij = matrix (0, nrow=n, ncol=K )	# matrix with posterior probabilities of pertenence to each population (for each point)
  )

  best.iter <- list (obj = -Inf)

  for (j in 1:niter)
  {


    iter <- InitClusters_i (X,Y ,iter, pa)            #for obtaining initial solution

    for (i in 0:Ksteps)
    {

      iter <- restr.diffax_i1 (iter = iter, pa = pa)	  #for appliying restrictions to eigenvalues of variability in X
      iter <- restr.diffax_i2 (iter = iter, pa = pa)    #for appliying restrictions to variability  in Y/X


      if (!iter$code)		{
        if (i)
          return (TreatSingularity (calcobj_i (X, Y,iter, pa), pa))
        else

          iter$sigma [,,] = diag (pa$p)

      }

      iter <- findClustAssig_i (X,Y, iter, pa)	  #for obtaining assigments and trimmed points

      if (!iter$code ||
          i == Ksteps)
        break

      iter <- estimClustPar_i (X,Y, iter, pa)	 #for obtaining estimations

    }

    iter <- calcobj_i (X,Y, iter, pa)	         #for obtaining the objetive function value
    iter$code = as.numeric (i == Ksteps)

    if (iter$obj > best.iter$obj)
      best.iter = iter
  }

  result = new("RobMixReg", inds_in=setdiff(1:n,which(iter$assig==0)),indout=which(iter$assig==0),ctleclusters=iter$assig,compcoef=t(iter$b),comppvals=NA,compwww=iter$z_ij)
  return (result)
}



