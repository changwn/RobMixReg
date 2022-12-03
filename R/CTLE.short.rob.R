# library(robust)
# library(flexmix)
# library(robustbase)



#' flexmix_2: Multiple runs of MLE based mixture regression to stabilize the output.
#' @description Mixture regression based on MLE could be unstable when assuming unequal variance. Multiple runs of flexmix is performed to stabilize the results.
#' @param formula A symbolic description of the model to be fit.
#' @param data1 A data frame containing the predictor and response variables, where the last column is the response varible.
#' @param k Number of mixture components.
#' @param mprior A numeric number in (0,1) that specifies the minimum proportion of samples in each mixing components.
#' @return A S4 object of flexmix class. xxx
flexmix_2<-function(formula,data1,k,mprior){
  liks=NULL
  mix_list=list()
  niter=10
  ii=1
  flag=0
  while(ii<niter & flag==0){
    tmp_mix = try(flexmix(formula,data1,k=k,control = list(minprior = mprior)),silent=TRUE)
    if(!inherits(tmp_mix, "try-error")){
      if(tmp_mix@k==k){
        flag=1
      }
    }
    ii=ii+1
  }
  return(tmp_mix)
}


#' DeOut : Detect outlier observations.
#' @description Detect outlier observations from a vector.
#' @param daData A numerical vector.
#' @param method Choose from '3sigma','hampel' and 'boxplot'.
#' @return indices of outlier observations.
DeOut<-function(daData,method){
  ################################################################################################################################################################
  ## Old 3 sigma, or "normal", Rule
  if(method=="3sigma"){
    cParmSig  <- 3                                                # This is where the "3" comes from in "3 Sigma"
    daMean    <- mean(daData)                                     # The "center" of our "non-outlier" interval
    daSD      <- sd(daData)                                       # The "radius" of our "non-outlier" interval
    cutOffSig <- cParmSig*c(-1, 1)*daSD+daMean                    # Lower and upper limits of our "non-outlier" interval
    ooo = which(daData>cutOffSig[2] | daData<cutOffSig[1])
  }
  ################################################################################################################################################################
  ## Hampel identifier
  if(method=="hampel"){
    cParmHem  <- 3                                                # This is the most common value used today
    daMAD     <- mad(daData)                                      # The "center" of our "non-outlier" interval
    daMedian  <- median(daData)                                   # The "radius" of our "non-outlier" interval
    cutOffHem <- cParmHem*c(-1, 1)*daMAD+daMedian                 # Lower and upper limits of our "non-outlier" interval
    ooo = which(daData>cutOffHem[2] | daData<cutOffHem[1])
  }
  ################################################################################################################################################################
  ## boxplot rule
  if(method=="boxplot"){
    #cParmBox  <- 2                                              # This is the most common value used today
    cParmBox  <- 1.5                                              # This is the most common value used today
    daQUAR    <- quantile(daData, c(.25, .75))                    # The first and third quartiles (Q1 & Q3)
    cutOffBox <- daQUAR+cParmBox*c(-1,1)*(daQUAR[2]-daQUAR[1])    # Lower and upper limits of our "non-outlier" interval
    ooo = which(daData>cutOffBox[2] | daData<cutOffBox[1])
  }
  return(ooo)
}


#' Class RobMixReg.
#'
#' Class \code{RobMixReg} defines a robust mixture regression class as a S4 object.
#'
#' @name RobMixReg-class
#' @rdname RobMixReg-class
#' @exportClass RobMixReg
#' @slot inds_in The indices of observations used in the parameter estimation.
#' @slot indout The indices of outlier samples, not used in the parameter estimation.
#' @slot ctleclusters The cluster membership of each observation.
#' @slot compcoef Regression coefficients for each component.
#' @slot comppvals Component p values.
#' @slot compwww The posterior of the clustering.
#' @slot call Call function.
setClass("RobMixReg",
	representation(inds_in="ANY",
	indout="ANY",
	ctleclusters="ANY",
	compcoef="ANY",
	comppvals="ANY",
	compwww="ANY",
	call="call"))


#' CTLERob: Robust mixture regression based on component-wise adaptive trimming likelihood estimation.
#' @description CTLERob performes robust linear regression with high breakdown point and high efficiency in each mixing components and adaptively remove the outlier samples.
#' @name CTLERob
#' @rdname CTLERob-methods
#' @exportMethod CTLERob
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the predictor and response variables, where the last column is the response varible.
#' @param nit Number of iterations.
#' @param nc Number of mixture components.
#' @param rlr_method The regression methods, default is 'ltsReg'.
##########################################################################################
##### setGeneric function CTLERob
##########################################################################################
setGeneric("CTLERob",
	function(formula,data, nit=20,nc=2,rlr_method="ltsReg")
	standardGeneric("CTLERob"))




#' @rdname CTLERob-methods
#' @aliases CTLERob,formula,ANY,ANY,numeric,ANY-method
### #######################################################################################
### #######################################################################################
### #######################################################################################
### ## setMethod CTLERob
### #######################################################################################
### if called with existing CTLERob object (as result=), then restart estimate...
### #######################################################################################
setMethod("CTLERob",
	signature(formula="formula",data="ANY",nit="ANY",nc="numeric",rlr_method="ANY"),
	function(formula,data, nit=20,nc=2,rlr_method="ltsReg")
	{
		mycall = match.call();
		res_list=vector("list",nit)
		ooo_list=vector("list",nit)
		nx=ncol(data)-1; nobs = nrow(data)            # number of observations
		for(jj in 1:nit){
			flag=0; outliers=c(); ccc=0
			www=NULL
			for(ii in 1:nc){
				inds_random=sample(1:nobs,((nx+1)*2*5))
				if(rlr_method =="lmRob"){
					mod_tmp = try(lmRob(formula=formula, data=data[inds_random,],control = lmRob.control(weight=c("Bisquare", "Bisquare"))),silent=TRUE)
				}
				if(rlr_method =="lmrob"){
					mod_tmp = try(lmrob(formula=formula, data=data[inds_random,]),silent=TRUE)
				}
				if(rlr_method =="ltsReg"){
					mod_tmp = try(ltsReg(formula=formula, data=data[inds_random,]),silent=TRUE)
				}
				#www=cbind(www,dnorm(predict(mod_tmp, newdata=data),mean=0,sd=summary(mod_tmp)$sigma))
				www=cbind(www,dnorm(abs(cbind(1,as.matrix(data)) %*% matrix(c(mod_tmp$coefficients,-1),ncol=1))[,1],mean=0,sd=summary(mod_tmp)$sigma))
			}
			nc_tmp=nc
			while(flag==0 ){
				inds_in=1:nobs; outliers.old=outliers
				ccc=ccc+1
				outlier_list1=outlier_list=vector("list", nc_tmp)
				for(j in 1:nc_tmp){
					inds_in_tmp=which(apply(www, 1, which.max)==j)
					#ltsres = lm(formula=formula, data=data[inds_in_tmp,])
					if(rlr_method =="lmRob"){
						ltsres = try(lmRob(formula=formula, data=data[inds_in_tmp,],control = lmRob.control(weight=c("Bisquare", "Bisquare"))),silent=TRUE)
					}
					if(rlr_method =="lmrob"){
						ltsres = try(lmrob(formula=formula, data=data[inds_in_tmp,]),silent=TRUE)
					}
					if(rlr_method =="ltsReg"){
						ltsres = try(ltsReg(formula=formula, data=data[inds_in_tmp,]),silent=TRUE)
					}
					if(!inherits(ltsres, "try-error")){
		                                res12 = abs(cbind(1,as.matrix(data)) %*% matrix(c(ltsres$coefficients,-1),ncol=1))[,1] ##could we change it to nx!!!.
						oo1=inds_in_tmp[DeOut(res12[inds_in_tmp],"hampel")]
						if(rlr_method=="lmRob"){
							#oo2=inds_in_tmp[which(ltsres$T.M.weights==0)]
							oo2=inds_in_tmp[which(ltsres$M.weights==0)]
						}
						if(rlr_method=="ltsReg"){
							oo2=inds_in_tmp[which(ltsres$lts.wt==0)]
						}
						if(rlr_method=="lmrob"){
							oo2=inds_in_tmp[which(ltsres$rweights==0)]
						}
						#outlier_list[[j]] = oo1##i
						#outlier_list[[j]] = oo2##ii
						outlier_list[[j]] = intersect(oo1,oo2)##iii
						#outlier_list[[j]] = unique(c(oo1,oo2))##c
					}
				}
				outliers=Reduce(c,outlier_list)
				if(length(outliers)>0){inds_in=c(1:nobs)[-outliers]}
				fres = flexmix_2(formula,data1=data[inds_in,],k=nc,mprior=0.1)            #
				nc_tmp=fres@k
				www = posterior(fres, newdata=data, unscale=TRUE)
				if(ccc>10  ){flag=1}
				if(length(outliers)==length(outliers.old)& ccc>1){
					if(length(outliers)==sum(outliers==outliers.old)){
						flag=1;
					}
				}
			}
			#print(sort(unique(outliers)))
			if(fres@k == nc & ccc<11){
				res_list[[jj]]=fres
				ooo_list[[jj]]=sort(unique(outliers))
			}
			#print(paste("The number of iteraction is", ccc))
		}
		ooo_list=ooo_list[][which(sapply(res_list, length)>0)]
		res_list=res_list[][which(sapply(res_list, length)>0)]
		llik=sapply(res_list, function(x)x@logLik)
		res_list=res_list[][order(llik,decreasing=TRUE)]
		ooo_list=ooo_list[][order(llik,decreasing=TRUE)]

		list1=sapply(ooo_list, function(x)list(1*((1:nobs)%in%x)))
		opt.ind=which.min(sapply(list1, function(x)sum((x-Reduce("+",list1)/length(list1))^2)))
		opt.fres=res_list[[opt.ind]]

		outliers=ooo_list[[opt.ind]]
		inds_in=setdiff(1:nobs,outliers)
                coffs_final=rbind(parameters(opt.fres),apply(opt.fres@posterior$scaled,2,sum)/length(inds_in))
                cates=vector("numeric",nobs)
                cates[inds_in]=opt.fres@cluster
                cates[outliers]=-1

                wij=matrix(NA,nrow=nobs,ncol=nc)
                wij[inds_in,]=opt.fres@posterior$unscaled

		pvals_final=sapply(refit(opt.fres)@components[[1]], function(x)(x[-1,4]))
                result = new("RobMixReg", inds_in=inds_in,indout=outliers,ctleclusters=cates,compcoef=coffs_final,comppvals=pvals_final,compwww=wij)
                result@call <- mycall
		result
	}
)
