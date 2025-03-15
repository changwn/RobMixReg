rm(list=ls())
load("Figure1.RData")

##################################Figure 1 
p1=ggplot(df11, aes(x = Method, y = Bias,  color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.8) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white")) 
p2=ggplot(df12, aes(x = Method, y = Bias, color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p3=ggplot(df13, aes(x = Method, y = Bias,  color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  

p4=ggplot(df14, aes(x = Method, y = Bias,color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
##################################Table 1 
pdf("Fig_1.pdf",width=9,height=8)
ggarrange(p1+ggtitle("Model 1, N=200"),p2+ggtitle("Model 1, N=400"),p3+ggtitle("Model 2, N=200"),p4+ggtitle("Model 2, N=400"), ncol=4)
dev.off()


##################################Fig Supp S1
p1=ggplot(df21, aes(x = Method, y = Bias,  color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.8) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p2=ggplot(df22, aes(x = Method, y = Bias, color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p3=ggplot(df23, aes(x = Method, y = Bias,  color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p4=ggplot(df24, aes(x = Method, y = Bias,color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
##################################Table 2 
pdf("FigS1.pdf",width=9,height=8)
ggarrange(p1+ggtitle("Model 1, N=200"),p2+ggtitle("Model 1, N=400"),p3+ggtitle("Model 2, N=200"),p4+ggtitle("Model 2, N=400"), ncol=4)
dev.off()



##################################Fig supp S2 
p1=ggplot(df31, aes(x = Method, y = Bias,  color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.8) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p2=ggplot(df32, aes(x = Method, y = Bias, color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p3=ggplot(df33, aes(x = Method, y = Bias,  color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
p4=ggplot(df34, aes(x = Method, y = Bias,color = Method)) +
  geom_jitter(width = 0.2, height = 0,alpha = 0.6) +  # Use semi-transparent dots
  facet_wrap(~Scenario1, scales = "free_y",ncol=1) +  # Each scenario has a separate y-axis
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank(),axis.title.y = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.position = "none",panel.border = element_rect(color = "black", fill = NA, size = 1),strip.background = element_rect(color = "black", fill = "white"))  
##################################Table 3 
pdf("FigS2.pdf",width=9,height=8)
ggarrange(p1+ggtitle("Model 1, N=200"),p2+ggtitle("Model 1, N=400"),p3+ggtitle("Model 2, N=200"),p4+ggtitle("Model 2, N=400"), ncol=4)
dev.off()

