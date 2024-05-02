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



################################################Veg Community


veg_quads_mod <- veg_quads_bind %>%
  pivot_longer(cols=c(2:265), names_to = "id", values_to = "cover") %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  merge(veg_master22_clean_id) %>% 
  group_by(loc,id) %>% 
  summarise(coverage=sum(cover,na.rm = TRUE)) %>% 
  ungroup() %>% 
  # group_by(loc) %>% 
  # mutate(tot_cover=sum(coverage),
  #        rel_cover=coverage/tot_cover) %>% 
  merge(veg_master22_clean_id) %>% 
  # filter(wis<3) %>%
  select(-wis) %>% 
  pivot_wider(names_from = id, values_from = coverage) %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>%  
  group_by(trt,complex) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  unite(trt_complex, c(trt, complex)) %>% 
  column_to_rownames(var="trt_complex") %>% 
  select(-sample_num)

write.csv(veg_quads_mod, "veg_quads_mod2.csv",row.names = FALSE)


test <- veg_quads_mod %>% 
  rownames_to_column("loc") %>% 
  group_by(loc) %>% 
  mutate(tot=sum(across(where(is.numeric)))) %>% 
  select(tot)


veg_quads_mod_ex <- veg_quads_bind %>%
  pivot_longer(cols=c(2:265), names_to = "id", values_to = "cover") %>%  
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  merge(veg_master22_clean_id) %>% 
  mutate(id = case_when(startsWith(as.character(id), "car") ~ "carex sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "ele") ~ "eleocharis sp",
                        TRUE ~ id)) %>% 
  group_by(loc,id) %>% 
  summarise(coverage=sum(cover)) %>% 
  ungroup() %>% 
  merge(veg_master22_clean_id) %>% 
  filter(wis<=2) %>%
  select(-wis) %>% 
  pivot_wider(names_from = id, values_from = coverage) %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>%  
  group_by(trt,complex) %>% 
  summarise(across(where(is.numeric), mean))

write.csv(veg_quads_mod_ex, "veg_quads_mod_ex.csv",row.names = FALSE)

veg_dist <- vegdist(veg_quads_mod,method = "bray",na.rm = TRUE)

veg_matrix <- as.matrix(veg_dist)

veg_tbl <- as_tibble(veg_matrix, rownames="samples")


veg_tbl %>%
  pivot_longer(cols=-samples, names_to="b", values_to="distances")

set.seed(19970206)
nmds_veg <- metaMDS(veg_matrix)

centroid_veg <- scores(nmds_veg) %>% 
  as.tibble(rownames="sample") %>% 
  separate(sample,c("trt","complex")) %>% 
  group_by(trt) %>% 
  summarise(NMDS1=mean(NMDS1),
            NMDS2=mean(NMDS2))

nmds_veg_fig <- scores(nmds_veg) %>% 
  as.tibble(rownames="sample") %>% 
  separate(sample,c("trt","complex")) %>% 
  ggplot(aes(x=NMDS1,y=NMDS2,color=trt))+
  geom_point(size=12)+
  geom_point(data=centroid_veg,aes(x=NMDS1,y=NMDS2,color=trt),shape=15,size=20,show.legend = FALSE)+
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
  expand_limits(y = c(-0.5,0.5))


nmds_veg_fig

ggsave(nmds_veg_fig, file="nmds_veg_fig.jpg", dpi=600, width=20, height=15, units="in")

####Stats for NMDS

library(cluster)

mrpp_data_veg <- veg_quads_bind %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  select(-transect_id) %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  select(complex, trt) %>% 
  distinct()

veg_all_adon <- adonis2(as.dist(veg_dist)~data_doc$trt,permutations = 10000)

veg_all_adon$`Pr(>F)`[1]

envfit(nmds_veg~trt,mrpp_data,
       permutations = 9999)

goodness(nmds_veg)
stressplot_veg <- stressplot(nmds_veg)


mrpp(veg_matrix,grouping = data_doc$trt, distance = "bray")

plot(nmds_veg)
scores(nmds_veg)


meandist(veg_matrix,grouping = data_doc$trt, distance = "bray")