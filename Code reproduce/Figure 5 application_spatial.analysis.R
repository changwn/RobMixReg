rm(list = ls())
library(RobMixReg)
library(dplyr)
library(patchwork)
library('R3port')
library(ggplot2)
library(palettes)
library(ggpubr)
library(RColorBrewer)
load("Spatial_data_list.app2.RData")
plots_list=vector("list",4)
res_list = vector("list",4)
for(ii in 1:length(Spatial_data_list)){
	print(ii)
	print("#########")
	x1=Spatial_data_list[[ii]][[1]]##deconvoluted macrophage proportion by RCTD
	x2=Spatial_data_list[[ii]][[2]]##deconvoluted dendritic cell proportion by RCTD
	y=Spatial_data_list[[ii]][[3]]##APC gene expression average
	coords=Spatial_data_list[[ii]][[4]]##Spatial coordinates
	formula = as.formula("y~x1+x2")
	ddd = data.frame(x1,x2,y)
	nc=2
	res_ctle2 = CTLERob(formula, data=ddd,nc=nc)
	res_list[[ii]]=res_ctle2
}

for(ii in 1:length(Spatial_data_list)){
	print(ii)
	print("#########")
	x1=Spatial_data_list[[ii]][[1]]##deconvoluted macrophage proportion by RCTD
	x2=Spatial_data_list[[ii]][[2]]##deconvoluted dendritic cell proportion by RCTD
	y=Spatial_data_list[[ii]][[3]]##APC gene expression average
	coords=Spatial_data_list[[ii]][[4]]##Spatial coordinates
	formula = as.formula("y~x1+x2")
	ddd = data.frame(x1,x2,y)
	nc=2
	res_ctle2 = res_list[[ii]]
	dfall = data.frame(coords)
	dfall$CATclus=as.factor(res_ctle2@ctleclusters)
	library(reshape)
	reshaped_df <- melt(dfall, id.vars = c("imagerow","imagecol"))
	
	if(ii==2){
	p0=ggplot(reshaped_df, aes(x = imagerow, y = imagecol, color = value)) +ggtitle(paste(names(Spatial_data_list)[ii],"CAT Clustering"))+geom_point(size = 1, alpha = 0.7) +
	scale_color_manual(values = c("grey", "skyblue","lightpink"))+theme_minimal() +labs(x = "", y = "", color = "Cluster") +
	#scale_color_manual(values = c("grey", "palevioletred2", "steelblue2"))+theme_minimal() +labs(x = "", y = "", color = "Cluster") +
	theme(axis.ticks = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text = element_blank(),legend.position = "right")
	}else{
	p0=ggplot(reshaped_df, aes(x = imagerow, y = imagecol, color = value)) +ggtitle(paste(names(Spatial_data_list)[ii],"CAT Clustering"))+geom_point(size = 1, alpha = 0.7) +
	scale_color_manual(values = c("grey", "lightpink","skyblue"))+theme_minimal() +labs(x = "", y = "", color = "Cluster") +
	#scale_color_manual(values = c("grey", "palevioletred2", "steelblue2"))+theme_minimal() +labs(x = "", y = "", color = "Cluster") +
	theme(axis.ticks = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text = element_blank(),legend.position = "right")
	}
	
	dfall1 = dfall
	dfall1$x1 = x1
	dfall1$x2 = x2
	p1 = ggplot(dfall1, aes(x = imagerow, y = imagecol, color = x1)) +ggtitle("Macrophage")+geom_point(size = 1, alpha = 0.7) +theme_minimal() +labs(x = "", y = "", color = "") +
	theme(axis.ticks = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text = element_blank(),legend.position = "right")+
	scale_colour_gradientn(colours = brewer.pal(9, "Blues"))
	p2 = ggplot(dfall1, aes(x = imagerow, y = imagecol, color = x2)) +ggtitle("Dendritic cells")+geom_point(size = 1, alpha = 0.7) +theme_minimal() +labs(x = "", y = "", color = "") +
	theme(axis.ticks = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text = element_blank(),legend.position = "right")+
	scale_colour_gradientn(colours = brewer.pal(9, "Blues"))
	plots_list[[ii]] = (p0+p1+p2)
}

pdf("APC_final_main.pdf",width=10,height=6)
#ggarrange(plotlist = list(plots_list[[1]]+ggtitle("Pustule_479"),plots_list[[4]]+ggtitle("Pustule_483")), ncol=1)
ggarrange(plotlist = plots_list[][c(1,4)], ncol=1)
dev.off()

pdf("APC_final_supp.pdf",width=10,height=6)
#ggarrange(plotlist = list(plots_list[[2]]+ggtitle("Pustule_481"),plots_list[[3]]+ggtitle("Pustule_482")), ncol=1)
ggarrange(plotlist = plots_list[][c(2,3)], ncol=1)
dev.off()
