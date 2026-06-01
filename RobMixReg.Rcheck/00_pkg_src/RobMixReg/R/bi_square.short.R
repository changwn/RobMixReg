
#' bisquare : Robust estimates for mean.
#' @description Tukey's bisquare family of functions.
#' @param t Numerical input, usually residuals.
#' @param k A constant tuning parameter, default is 4.685.
#' @return A bi-square weight for mean.
bisquare<-function(t,k=4.685){out=t*pmax(0,(1-(t/k)^2))^2;out}

#' biscalew :Robust M-estimates for scale.
#' @description Tukey's bisquare family of functions.
#' @param t Numerical input, usually residuals.
#' @return bisquare weight for scale.
biscalew<-function(t){ t[which(t==0)]=min(t[which(t!=0)])/10; out=pmin(1-(1-t^2/1.56^2)^3,1)/t^2;out}

#' mixlinrb_bione : mixlinrb_bione estimates the mixture regression parameters robustly using bisquare function based on one initial value.
#' @description  An EM-type of parameter estimation by replacing the least square estimation in the M-step with a robust criterior.
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the predictor and response variables, where the last column is the response varible.
#' @param nc Number of mixture components.
#' @return Estimated coefficients of all components.
mixlinrb_bione<-function(formula,data,nc=2){
	nx=ncol(data)-1; n = nrow(data)
	p=nx+1; n1=2*p;
	bet= matrix(rep(0,nc*p),nrow=nc);
	sig=0;
	for(j in seq(nc)){
		ind1= sample(1:n,n1);
		tmp_mod=lm(formula,data=data[ind1,])
		bet[j,]=coefficients(tmp_mod)
		sig=sig+sum((resid(tmp_mod))^2)
	}
	pr=rep(1/nc,nc);sig=rep(sig/n1/nc,nc);

	run=0;acc=10^(-4)*max(abs(c(bet,sig,pr)));
	r=matrix(rep(0,nc*n),nrow=n);pk=r;
	for(j in seq(nc)){
		r[,j]= (cbind(1,as.matrix(data)) %*% matrix(c(bet[j,],-1),ncol=1)[,1])/sig[j]
	}
#E-steps
	repeat{
		prest=c(sig,bet,pr);run=run+1;
		for(j in seq(nc)){
			pk[,j]=pr[j]*pmax(10^(-300),dnorm(r[,j],0,1))/sig[j]
		}
		pk=pk/matrix(rep(apply(pk,1,sum),nc),nrow=n);
#M-step
		np=apply(pk,2,sum);pr=np/n;
		r[which(r==0)]=min(r[which(r!=0)])/10;
		for(j in seq(nc)){
			w <- pk[,j]*bisquare(r[,j])/r[,j];
			tmp_mod=lm(formula,data=data,weights=w)
			bet[j,]= coefficients(tmp_mod)
			r[,j]= resid(tmp_mod)/sig[j];
			sig[j]=sqrt(sum(r[,j]^2*sig[j]^2*pk[,j]*biscalew(r[,j]))/np[j]/0.5);
		}
	#	print(head(r))
		dif=max(abs(c(sig,bet,pr)-prest))
		if(dif<acc|run>500){break}
	}
	theta=matrix(c(bet,sig,pr),nrow=nc);
	est=list(theta=theta,difpar=dif,run=run)
	return(est)
}


#' mixlinrb_bi: mixlinrb_bione estimates the mixture regression parameters robustly using bisquare function based on multiply initial value.
#' @description An EM-type of parameter estimation by replacing the least square estimation in the M-step with a robust criterior.
#' @name mixlinrb_bi
#' @rdname mixlinrb_bi-methods
#' @exportMethod mixlinrb
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the predictor and response variables, where the last column is the response varible.
#' @param nc Number of mixture components.
#' @param nit Number of iterations for biSauqre method.
#' @return Estimated coefficients of all components.
##########################################################################################
##### setGeneric function mixlinrb_bi
##########################################################################################
setGeneric("mixlinrb_bi",
	function(formula,data, nc=2,nit=200)
	standardGeneric("mixlinrb_bi"))

#' @rdname mixlinrb_bi-methods
#' @aliases mixlinrb_bi,formula,ANY,numeric,numeric-method
### #######################################################################################
### #######################################################################################
### #######################################################################################
### ## setMethod mixlinrb_bi
### #######################################################################################
### if called with existing mixlinrb_bi object (as result=), then restart estimate...
### #######################################################################################
setMethod("mixlinrb_bi",
	signature(formula="formula",data="ANY",nc="numeric",nit="numeric"),
	function(formula,data, nc=2,nit=20)
	{
		mycall = match.call();

		nx=ncol(data)-1; n = nrow(data)
		p=nx+1; n1=2*p;
		perm=permutations(nc,nc);
		est=mixlinrb_bione(formula,data,nc);
		lenpar=length(c(est$theta));
		theta= matrix(rep(0,lenpar*(nit)),ncol=lenpar);
		theta[1,]=c(est$theta);
		trimbet=matrix(theta[1,1:(p*nc)],nrow=nc);
		trimbet=matrix(rep(matrix(t(trimbet),ncol=p*nc,byrow=T),gamma(nc+1)),ncol=p*nc,byrow=T);
		ind=matrix(rep(0,nit),nrow=1);ind[1]=1;numsol=1;solindex=1; sol=matrix(theta[1,],nrow=1);
		for(i in 2:nit){
			est=mixlinrb_bione(formula,data,nc)
			theta[i,]=est$theta;
			temp= matrix(theta[i,1:(p*nc)],nrow=nc);
			temp=matrix(t(temp[t(perm),]),ncol=p*nc,byrow=T);
			dif=apply((trimbet-temp )^2,1,sum);
			temp1=which(dif==min(dif));
			theta[i,]=c(c(matrix(temp[temp1[1],],nrow=nc,byrow=T)),theta[i,p*nc+perm[temp1[1],]],theta[i,p*nc+nc+perm[temp1[1],]]);
			dif= apply((matrix(rep(theta[i,1:(p*nc)],numsol),nrow=numsol,byrow=T)-sol[,1:(p*nc)])^2,1,sum);
			if(min(dif)>0.1){
				sol=rbind(sol,theta[i,]);
				numsol=numsol+1;
				solindex=c(solindex,i);
				ind=rbind(ind,rep(0,nit));
				ind[numsol,i]=1
			}else{
				ind1=which(dif==min(dif));
				ind[ind1,i]=1;
			}
		}
		num=apply(ind,1,sum);
		ind1=order(-num);
		bestindex=ind1;
		for(j in seq(numsol)){
			if(min(sol[ind1[j], (p*nc+nc+1):(p*nc+2*nc)])>0.05){
				index=1;
				est=matrix(sol[ind1[j],],nrow=nc);
				for(l in seq(nc-1)){
					temp=matrix(rep(est[l,1:p],nc-l),nrow=nc-l,byrow=T)-est[(l+1):nc,1:p];
					temp=matrix(temp,nrow=nc-l);
					dif=apply(temp^2,1,sum);
					if(min(dif)<0.1){
						index=0;break
					}
				}
				if(index==1){
					bestindex=ind1[j];break
				}
			}
		}
		est= sol[bestindex[1],];
		coffs_final = t(matrix(est,nrow=nc))
		cates = NA

                result = new("RobMixReg", inds_in=NA,indout=NA,ctleclusters=cates,compcoef=coffs_final,comppvals=NA)
                result@call <- mycall
                result
	}
)
