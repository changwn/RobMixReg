
#' plot_CTLE: Plot the mixture/single regression line(s) in a simply function.
#' @description CTLERob performes robust linear regression with high breakdown point and high efficiency in each mixing components and adaptively remove the outlier samples.
#' @name plot_CTLE
#' @rdname plot_CTLE-methods
#' @exportMethod plot_CTLE
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the predictor and response variables, where the last column is the response varible.
#' @param nc Number of mixture components.
#' @param inds_in The index of the point which belongs to the current regression line.
setGeneric("plot_CTLE",
           function(formula,data,nc=2,inds_in)
             standardGeneric("plot_CTLE"))







#######################!!!!!!!!!!!!!!!!!!!!the response variable must be in the last column!


#' @rdname plot_CTLE-methods
#' @aliases CTLERob,formula,ANY,numeric,ANY
setMethod("plot_CTLE",
          signature(formula="formula",data="ANY",nc="numeric",inds_in="ANY"),
          function(formula,data,nc=2,inds_in)
          {
            #par(mfrow=c(1,4))
            #		colors = pal_npg("nrc")(nc+1)
            #colors=brewer.pal(nc+1,"Set3")  #Accent
            colors = c("red","dodgerblue","gold","greenyellow","darkorchid1","darkorange")
            aaa1 = flexmix(formula,data=data[inds_in,],k=nc)            #
            #cols=rep("pink",nrow(data))
            #cols[inds_in]="lightblue"
            # color candidate : darkslategrey, tomato, deepskyblue1
            cols=rep("tomato",nrow(data))
            cols[inds_in]="deepskyblue1"
            x=data[,1]
            y=data[,2]
            #plot(x,y,pch=19, main='Input data point')
            #plot(x,y,col=cols,pch=19, main='Identify outliers', xlab='Gene', ylab='Outcome')
            all_variable = as.character(attr(terms(formula), "variables"))[-1L]
            yname = all_variable[1]; xname = all_variable[2];
            #plot(x,y,col=cols,pch=19, main='Identify outliers', xlab=xname, ylab=yname)
            for(j in 1:nc){
              inds_in_component = inds_in[which(aaa1@cluster==j)]
              if(length(inds_in_component)>4){
                ltsres = ltsReg(formula=formula, data=data[inds_in_component,])
                res1 = cbind(1,as.matrix(data)) %*% matrix(c(ltsres$coefficients,-1),ncol=1)
             #   inds_in_component = unique(c(inds_in_component,which(abs(res1) <= max(abs(res1[inds_in_component]))))) #rm overlap points!!!
                #inds_in_component = unique(c(inds_in_component,which(abs(res1) <= max(abs(res1[inds_in_component][ltsres$best])))))
                lm_mod = lm(formula=formula, data=data[inds_in_component,])
                #ppp = summary(lm_mod)$coefficients[-1,4]
                #plot(x[inds_in_component],y[inds_in_component],main=paste("Comp",j,":",paste("p=", round(ppp,3),sep=""), paste("coef=", round(lm_mod$coefficients[2],3),sep="")), xlim=c(min(x),max(x)), ylim=c(min(y),max(y)),col=colors[j+1],pch=19,xlab=xname,ylab=yname)
                plot(x[inds_in_component],y[inds_in_component],main='Robust Linear Regression', xlim=c(min(x),max(x)), ylim=c(min(y),max(y)),col=colors[j+1],pch=19,xlab=xname,ylab=yname)
                abline(lm_mod,col="darkslategrey")
                points(x[-inds_in_component],y[-inds_in_component],col="gray60",pch=19)
                # points(x[-inds_in_component],y[-inds_in_component],col="gold",pch=19)
                points(x[-inds_in],y[-inds_in],col="firebrick1",pch=19)
              }else{
                plot(0)
              }
            }

          })



