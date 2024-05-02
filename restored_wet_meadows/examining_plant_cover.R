library(FSA)
library(dunn.test)
library(tidyverse)

#Examining plant cover

############################################Examining most abundant native


veg_cover_new <- veg_quads_bind %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:265), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  merge(veg_master22_clean_ne) %>% 
  mutate(id = case_when(startsWith(as.character(id), "caryo sp.") ~ "not_carex",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "car") ~ "carex sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "eleery") ~ "eleocharis sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "elepal") ~ "eleocharis sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "elemac") ~ "eleocharis sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "eleocharis sp.") ~ "eleocharis sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "eleocharis spp") ~ "eleocharis sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "eleocharis spp.") ~ "eleocharis sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "not_carex") ~ "caryo sp.",
                        TRUE ~ id)) %>% 
  group_by(id,trt) %>% 
  summarise(coverage=mean(cover)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = trt, values_from = coverage) %>% 
  filter(id!="bg",
         id!="ld")


veg_cover_new_non_trt <- veg_quads_bind %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:265), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>%
  merge(veg_master22_clean_id) %>%
  mutate(id = ifelse(id == "eleind","indian_goosegrass",id)) %>% 
  mutate(id = case_when(startsWith(as.character(id), "car") ~ "carex sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "ele") ~ "eleocharis sp",
                        TRUE ~ id)) %>%
  mutate(id = ifelse(id == "indian_goosegrass","eleind",id)) %>% 
  group_by(id) %>% 
  summarise(coverage=mean(cover)) 


test <- veg_quads_bind_nomix %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:295), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>%
  merge(veg_master22_clean_id) %>%
  mutate(id = ifelse(id == "eleind","indian_goosegrass",id)) %>% 
  mutate(id = case_when(startsWith(as.character(id), "car") ~ "carex sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "ele") ~ "eleocharis sp",
                        TRUE ~ id)) %>%
  mutate(id = ifelse(id == "indian_goosegrass","eleind",id)) %>% 
  group_by(loc,id) %>% 
  summarise(coverage=mean(cover)) %>% 
  ungroup() %>% 
  merge(veg_master22_clean_id) %>% 
  select(-wis) %>% 
  pivot_wider(names_from = id, values_from = coverage) %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>%  
  group_by(trt,complex) %>% 
  summarise(across(where(is.numeric), mean)) %>%  
  mutate(trt_n=ifelse(trt=="relict",21,13))



test2 <- veg_quads_bind_nomix %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:295), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>%
  merge(veg_master22_clean_id) %>% 
  mutate(id = ifelse(id == "eleind","indian_goosegrass",id))
mutate(id = case_when(startsWith(as.character(id), "car") ~ "carex sp",
                      TRUE ~ id),
       id = case_when(startsWith(as.character(id), "ele") ~ "eleocharis sp",
                      TRUE ~ id)) 


test2 %>% 
  filter(id== "indian_goosegrass")




carex_cover <- veg_quads_bind_nomix %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:295), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  merge(veg_master22_clean_ne) %>% 
  filter(startsWith(as.character(id), "car")) %>% 
  group_by(id) %>% 
  summarise(coverage=mean(cover)) 

eleocharis_cover <- veg_quads_bind_nomix %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:295), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  merge(veg_master22_clean_ne) %>% 
  filter(startsWith(as.character(id), "ele")) %>% 
  group_by(id) %>% 
  summarise(coverage=mean(cover)) 

###################################Ten most common native veg taxa

trt_veg_comp <- veg_quads_bind %>% 
  mutate(loc=tolower(transect_id),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc)) %>% 
  pivot_longer(cols=c(2:265), names_to = "id", values_to = "cover") %>% 
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  merge(veg_master22_clean_ne) %>% 
  mutate(id = case_when(startsWith(as.character(id), "car") ~ "carex sp",
                        TRUE ~ id),
         id = case_when(startsWith(as.character(id), "ele") ~ "eleocharis sp",
                        TRUE ~ id)) %>% 
  filter(native_exotic=="N") %>% 
  group_by(complex,id,trt) %>% 
  summarise(coverage=mean(cover)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id, values_from = coverage) %>%  
  mutate(trt_n=ifelse(trt=="relict",21,13))




kruskal.test(trt_veg_comp$panvir,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(panvir),
          se=sd(panvir)/sqrt(trt_n),
          sd=sd(panvir)) %>% 
  distinct()



kruskal.test(trt_veg_comp$andger,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(andger),
          se=sd(andger)/sqrt(trt_n),
          sd=sd(andger)) %>% 
  distinct()



kruskal.test(trt_veg_comp$schpun,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(schpun),
          se=sd(schpun)/sqrt(trt_n),
          sd=sd(schpun)) %>% 
  distinct()



kruskal.test(trt_veg_comp$spapec,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(spapec),
          se=sd(spapec)/sqrt(trt_n),
          sd=sd(spapec)) %>% 
  distinct()




kruskal.test(trt_veg_comp$sornut,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(sornut),
          se=sd(sornut)/sqrt(trt_n),
          sd=sd(sornut)) %>% 
  distinct()



kruskal.test(trt_veg_comp$ambpsi,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(ambpsi),
          se=sd(ambpsi)/sqrt(trt_n),
          sd=sd(ambpsi)) %>% 
  distinct()



kruskal.test(trt_veg_comp$asteri,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(asteri),
          se=sd(asteri)/sqrt(trt_n),
          sd=sd(asteri)) %>% 
  distinct()




kruskal.test(trt_veg_comp$equlae,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(equlae),
          se=sd(equlae)/sqrt(trt_n),
          sd=sd(equlae)) %>% 
  distinct()


kruskal.test(trt_veg_comp$apocan,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(apocan),
          se=sd(apocan)/sqrt(trt_n),
          sd=sd(apocan)) %>% 
  distinct()



kruskal.test(trt_veg_comp$elytra,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(elytra),
          se=sd(elytra)/sqrt(trt_n),
          sd=sd(elytra)) %>% 
  distinct()



kruskal.test(trt_veg_comp$`carex sp`,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(`carex sp`),
          se=sd(`carex sp`)/sqrt(trt_n),
          sd=sd(`carex sp`)) %>% 
  distinct()


kruskal.test(trt_veg_comp$`eleocharis sp`,trt_veg_comp$trt)

trt_veg_comp %>% 
  group_by(trt) %>% 
  reframe(mean=mean(`eleocharis sp`),
          se=sd(`eleocharis sp`)/sqrt(trt_n),
          sd=sd(`eleocharis sp`)) %>% 
  distinct()


trt_veg_comp %>% 
  filter(trt=="relict") %>% 
  summarise(mean=mean(`eleocharis sp`))


trt_veg_comp %>% 
  select(complex,trt,elytra)
