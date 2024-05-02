library(vegan)
library(ggfortify)
library(ggrepel)


ordiplot(ord)
autoplot(ord)


#########################################################################Invertebrates-CCA1-Sites

df_sites1  <- data.frame(summary(ord)$sites[,1:2])

df_sites1$complex_trt <- row.names(df_sites1)

df_sites1 <- df_sites1 %>% 
  mutate(complex_trt=ifelse(complex_trt=="mormon_e_relict","mormone_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="mormon_w_relict","mormonw_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="skinny_island_relict","skinnyisland_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="rowe_n_relict","rowen_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="schumaker_w_relict","schumakerw_relict",complex_trt)) %>% 
  separate(complex_trt,into = c("complex","trt"),sep = "_")

scaling_factor <- 5

centroid <- df_sites1 %>% 
  group_by(trt) %>% 
  reframe(CCA1=mean(CCA1),
          CCA2=mean(CCA2))

cca1_plot_sites <- ggplot(df_sites1, 
                          aes(x=CCA1, y=CCA2)) + 
  #Draw lines on x = 0 and y = 0
  geom_hline(yintercept=0, 
             linetype="dashed") +
  geom_vline(xintercept=0, 
             linetype="dashed") +
  coord_fixed()+
  #Add species text
  geom_point(data=df_sites1, 
             aes(x=CCA1,#Score in CCA1 to add species text
                 y=CCA2,#Score in CCA2 to add species text
                 color = trt),#Set the text vertical alignment according to its position in the CCA plot
             size=8)+
  #Add environmental vars arrows
  geom_segment(data=df_environ1, 
               aes(x=0, #Starting coordinate in CCA1 = 0 
                   xend=CCA1*scaling_factor,#Ending coordinate in CCA1  
                   y=0, #Start in CCA2 = 0
                   yend=CCA2*scaling_factor), #Ending coordinate in CCA2 
               linewidth = 2,
               # color="firebrick1", #set color
               arrow=arrow(length=unit(0.01,"npc"))#Set the size of the lines that form the tip of the arrow
  )+
  #Add environmental vars text
  geom_text_repel(data=df_environ1, 
                  aes(x=CCA1*scaling_factor, 
                      y=CCA2*scaling_factor,
                      label=rownames(df_environ1),
                      hjust=0.75*(1-sign(CCA1)),#Add the text of each environmental var at the end of the arrow
                      vjust=0.75*(1-sign(CCA2))),#Add the text of each environmental var at the end of the arrow 
                  # color="firebrick1",
                  size=12)+
  geom_point(data=centroid,aes(x=CCA1,y=CCA2,color=trt), size=12,shape=15,show.legend = FALSE)+
  stat_ellipse(data=df_sites1,aes(x=CCA1, y=CCA2,color=trt),type = "norm",lwd=2)+
  #Set bw theme
  theme_classic()+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  #Set x and y axis titles
  labs(x=paste0("CCA1 (",cca1_varex1," %)"),
       y=paste0("CCA2 (",cca2_varex1," %)"))+
  # xlim(-4,4)+
  # ylim(-4,4)+
  labs(colour= "Wetland",
       shape="Wetland")+
  scale_color_grey(labels=c("relict"="Native","restored"="Restored"))+
  scale_color_grey(labels=c("relict"="Native","restored"="Restored"))

cca1_plot_sites

ggsave(cca1_plot_sites, file="cca1_plot_sites.jpg", dpi=600, width=20, height=15, units="in")





#########################################################################Veg-CCA2-Sites


#Get CCA scores
df_species2  <- data.frame(summary(ord2)$species[,1:2])# get the species CC1 and CC2 scores
df_environ2  <- scores(ord2, display = 'bp') #get the environment vars CC1 and CC2 scores
df_sites2 <- data.frame(summary(ord2)$sites[,1:2])

cca1_varex2<-round(summary(ord2)$cont$importance[2,1]*100,2) #Get percentage of variance explained by first axis
cca2_varex2<-round(summary(ord2)$cont$importance[2,2]*100,2) #Get percentage of variance explained by second axis



df_sites2  <- data.frame(summary(ord2)$sites[,1:2])

df_sites2$complex_trt <- row.names(df_sites2)

df_sites2 <- df_sites2 %>% 
  mutate(complex_trt=ifelse(complex_trt=="mormon_e_relict","mormone_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="mormon_w_relict","mormonw_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="skinny_island_relict","skinnyisland_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="rowe_n_relict","rowen_relict",complex_trt),
         complex_trt=ifelse(complex_trt=="schumaker_w_relict","schumakerw_relict",complex_trt)) %>% 
  separate(complex_trt,into = c("trt","complex"),sep = "_")

scaling_factor <- 5

centroid2 <- df_sites2 %>% 
  ungroup() %>% 
  group_by(trt) %>% 
  reframe(CCA1=mean(CCA1),
          CCA2=mean(CCA2))

cca2_plot_sites <- ggplot(df_sites2, 
                          aes(x=CCA1, y=CCA2)) + 
  #Draw lines on x = 0 and y = 0
  geom_hline(yintercept=0, 
             linetype="dashed") +
  geom_vline(xintercept=0, 
             linetype="dashed") +
  coord_fixed()+
  #Add species text
  geom_point(data=df_sites2, 
             aes(x=CCA1,#Score in CCA1 to add species text
                 y=CCA2,#Score in CCA2 to add species text
                 color = trt),#Set the text vertical alignment according to its position in the CCA plot
             size=8)+
  #Add environmental vars arrows
  geom_segment(data=df_environ2, 
               aes(x=0, #Starting coordinate in CCA1 = 0 
                   xend=CCA1*scaling_factor,#Ending coordinate in CCA1  
                   y=0, #Start in CCA2 = 0
                   yend=CCA2*scaling_factor), #Ending coordinate in CCA2 
               linewidth = 2,
               # color="firebrick1", #set color
               arrow=arrow(length=unit(0.01,"npc"))#Set the size of the lines that form the tip of the arrow
  )+
  #Add environmental vars text
  geom_text_repel(data=df_environ2, 
                  aes(x=CCA1*scaling_factor, 
                      y=CCA2*scaling_factor,
                      label=rownames(df_environ2),
                      hjust=0.75*(1-sign(CCA1)),#Add the text of each environmental var at the end of the arrow
                      vjust=0.75*(1-sign(CCA2))),#Add the text of each environmental var at the end of the arrow 
                  # color="firebrick1",
                  size=12)+
  geom_point(data=centroid2,aes(x=CCA1,y=CCA2,color=trt), size=12,shape=15,show.legend = FALSE)+
  stat_ellipse(data=df_sites2,aes(x=CCA1, y=CCA2,color=trt),type = "norm",lwd=2)+
  #Set bw theme
  theme_classic()+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  #Set x and y axis titles
  labs(x=paste0("CCA1 (",cca1_varex2," %)"),
       y=paste0("CCA2 (",cca2_varex2," %)"))+
  # xlim(-4,4)+
  # ylim(-4,4)+
  labs(colour= "Wetland",
       shape="Wetland")+
  scale_color_grey(labels=c("relict"="Native","restored"="Restored"))+
  scale_color_grey(labels=c("relict"="Native","restored"="Restored"))

cca2_plot_sites

ggsave(cca2_plot_sites, file="cca2_plot_sites.jpg", dpi=600, width=20, height=15, units="in")

