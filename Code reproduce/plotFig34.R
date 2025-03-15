
###Figure 4
rm(list=ls())
load("bic_mat.at20.RData")
par(mfcol=c(1,8),mar=c(2.4,2.0,1.5,1.0))
labs = c("Intact",paste0("At x=",c(0, 0.1, 0.2, 0.7, 0.8, 0.9, 1.0)))
cccc=0
for(k in c(1,2,3,4,9,10,11,12)){
cccc=cccc+1
#plot(bic_mat[,k], type="l")
plot(bic_mat[,k], type="l",lwd=2,col="coral1",main=labs[cccc]);points(x=1:5,y=bic_mat[,k],pch=19,col="blue")
}



###Figure 5
rm(list=ls())
load("Figure4_res_list.RData")
	par(mfrow=c(4,12),mar=c(1.8,2.0,1.5,1.0))
for(k in c(1,3,4,9)){
		x=x_list[[k]]
		y=y_list[[k]]
		plot(x,y,main="",xlab="cg16012690",ylab="CREB3L1",pch=20,col = ifelse(y > 18, "red", rgb(190/255, 190/255, 190/255, alpha = 0.8)),xlim=c(0,1),ylim=c(4,24),cex.main=1.5,xaxt = "n",yaxt = "n",bty="n" );axis(1);axis(2)
for(j in 1:length(res_list_list_ff1[[1]])){
	aa_tmp = sapply(res_list_list_ff1, function(x)list(x[[j]]))
		if(length(aa_tmp[[k]])>1){
			if(k==1){
			plot(x,y,main=names(res_list_list_ff1[[1]])[j],pch=20,col = rgb(190/255, 190/255, 190/255, alpha = 0.8),xlim=c(0,1),ylim=c(4,24),cex.main=1.5,xaxt = "n",yaxt = "n",bty="n" );axis(1, labels = FALSE);axis(2, labels = FALSE)
			}else{
			plot(x,y,main="",pch=20,col = rgb(190/255, 190/255, 190/255, alpha = 0.8),xlim=c(0,1),ylim=c(4,24),cex.main=1.5,xaxt = "n",yaxt = "n",bty="n" );axis(1, labels = FALSE);axis(2, labels = FALSE)
			}
			if(j %in% c(6,7,8)){
				abline(res_list_list_ff1[[1]][["MLE"]][1:2,1],col="black",lwd=2.5)
				abline(res_list_list_ff1[[1]][["MLE"]][1:2,2],col="black",lwd=2.5)
			}else{
				abline(aa_tmp[[1]][1:2,1],col="black",lwd=2.5)
				abline(aa_tmp[[1]][1:2,2],col="black",lwd=2.5)
			}
			abline(aa_tmp[[k]][1:2,1],col="deepskyblue1",lwd=2.5, lty=2)
			abline(aa_tmp[[k]][1:2,2],col="deepskyblue1",lwd=2.5, lty=2)
		}
#	}
}
}

	par(mfrow=c(4,12),mar=c(1.8,2.0,1.5,1.0))
for(k in c(2,10,11,12)){
		x=x_list[[k]]
		y=y_list[[k]]
		plot(x,y,main="",xlab="cg16012690",ylab="CREB3L1",pch=20,col = ifelse(y > 18, "red", rgb(190/255, 190/255, 190/255, alpha = 0.8)),xlim=c(0,1),ylim=c(4,24),cex.main=1.5,xaxt = "n",yaxt = "n",bty="n" );axis(1);axis(2)
for(j in 1:length(res_list_list_ff1[[1]])){
	aa_tmp = sapply(res_list_list_ff1, function(x)list(x[[j]]))
		if(length(aa_tmp[[k]])>1){
			if(k==2){
			plot(x,y,main=names(res_list_list_ff1[[1]])[j],pch=20,col = rgb(190/255, 190/255, 190/255, alpha = 0.8),xlim=c(0,1),ylim=c(4,24),cex.main=1.5,xaxt = "n",yaxt = "n",bty="n" );axis(1, labels = FALSE);axis(2, labels = FALSE)
			}else{
			plot(x,y,main="",pch=20,col = rgb(190/255, 190/255, 190/255, alpha = 0.8),xlim=c(0,1),ylim=c(4,24),cex.main=1.5,xaxt = "n",yaxt = "n",bty="n" );axis(1, labels = FALSE);axis(2, labels = FALSE)
			}
			if(j %in% c(6,7,8)){
				abline(res_list_list_ff1[[1]][["MLE"]][1:2,1],col="black",lwd=2.5)
				abline(res_list_list_ff1[[1]][["MLE"]][1:2,2],col="black",lwd=2.5)
			}else{
				abline(aa_tmp[[1]][1:2,1],col="black",lwd=2.5)
				abline(aa_tmp[[1]][1:2,2],col="black",lwd=2.5)
			}
			abline(aa_tmp[[k]][1:2,1],col="deepskyblue1",lwd=2.5, lty=2)
			abline(aa_tmp[[k]][1:2,2],col="deepskyblue1",lwd=2.5, lty=2)
		}
}
}



