rm(list=ls())
library(reshape)
library(ggplot2)
library(ggpubr)
load("Fig2.TPR.TNR.RData")
p1 = ggplot(dfall_TPR, aes(x=X1, y=value))+geom_boxplot(fill="skyblue")+labs(x = NULL, y = "TPR")+theme_minimal() +  theme(
    panel.grid = element_blank() # Remove grid lines
  )
p2 = ggplot(dfall_TNR, aes(x=X1, y=value))+geom_boxplot(fill="tomato")+labs(x = NULL, y = "TNR")+theme_minimal() +  theme(
    panel.grid = element_blank()  # Remove grid lines
  )
ggarrange(p1,p2,ncol=2)



