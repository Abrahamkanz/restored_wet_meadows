library(tidyverse)
library(lubridate)
library(BiodiversityR) # also loads vegan
library(ggplot2)
library(readxl)
library(ggsci)
library(ggrepel)
library(ggforce)
library(ggfortify)
library(tidyverse)
library(HDInterval)
library(ggridges)
library(ordr)


comm_bio_m3_mod <- data_doc %>%
  select(complex,trt,diplo,apor,arach,asil,tipu,scara,
         canth,chrys,heteroc,elat,carab,arma,lepi,
         curc,staph,taban,meloi,myri,penta,
         form,planthopper,strat) %>%
  mutate(complex_trt=paste(complex,trt,sep = "_")) %>% 
  column_to_rownames("complex_trt") %>% 
  select(-trt,-complex)




comm_bio_m3_mod_ex <- data_doc %>%
  select(complex,trt,diplo,apor,arach,asil,tipu,scara,
         canth,chrys,heteroc,elat,carab,arma,lepi,
         curc,staph,taban,meloi,myri,penta,
         form,planthopper,strat) %>%
  mutate(complex_trt=paste(complex,trt,sep = "_"))

comm_bio_m3_mod[is.na(comm_bio_m3_mod)] <- 0

comm_bio_m3_mod_ex[is.na(comm_bio_m3_mod_ex)] <- 0

write.csv(comm_bio_m3_mod_ex, "comm_bio_m3_mod_ex.csv",row.names = FALSE)

complex_dist <- vegdist(comm_bio_m3_mod,method = "bray",na.rm = TRUE)

dist_matrix <- as.matrix(complex_dist)

dist_tbl <- as_tibble(dist_matrix, rownames="samples")


dist_tbl %>%
  pivot_longer(cols=-samples, names_to="b", values_to="distances")

set.seed(19940614)
nmds <- metaMDS(dist_matrix)

centroid <- scores(nmds) %>% 
  as.tibble(rownames="sample") %>% 
  separate(sample,c("complex","trt")) %>% 
  group_by(trt) %>% 
  summarise(NMDS1=mean(NMDS1),
            NMDS2=mean(NMDS2))

nmds_fig <- scores(nmds) %>% 
  as.tibble(rownames="sample") %>% 
  separate(sample,c("complex","trt")) %>% 
  ggplot(aes(x=NMDS1,y=NMDS2,color=trt))+
  geom_point(size=12)+
  geom_point(data=centroid,aes(x=NMDS1,y=NMDS2,color=trt),shape=15,size=20,show.legend = FALSE)+
  stat_ellipse(lwd=2)+
  theme_classic()+
  guides(colour = guide_legend(override.aes = list(size = 5)))+
  labs(colour= "Treatment")+
  scale_color_grey(labels=c("relict"="Native","restored"="Restored"))+
  theme(axis.text.x=element_text(size=40),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40))+
  expand_limits(y = 0.5)

ggsave(nmds_fig, file="nmds_fig.jpg", dpi=600, width=20, height=15, units="in")

####Stats for NMDS

library(cluster)

mrpp_data <- data_doc %>%
  select(complex,trt,diplo,apor,tipu,scara,
         elat,carab,arma,lepi,
         curc,staph,taban,meloi,
         form) %>%
  select(-diplo,-apor, -tipu,-scara,
         -elat,-carab,-arma,-lepi,
         -curc,-staph,-taban,-meloi,
         -form)


invert_all_adon <- adonis2(as.dist(complex_dist)~data_doc$trt,permutations = 10000)

str(invert_all_adon)

invert_all_adon$`Pr(>F)`[1]

envfit(complex_dist~trt,data_doc,
       permutations = 9999)

test <- dist_matrix %>% 
  as.data.frame() %>% 
  rownames_to_column(.,"loc_trt")

goodness(nmds)
stressplot(nmds)


test <- comm_bio_m3_mod_ex %>% 
  select(-complex,-complex_trt)

mrpp(dist_matrix,grouping = data_doc$trt, distance = "bray")

decorana(dist_matrix)
plot(nmds)