library(tidyverse)
library(janitor)

getmode <- function(x) {
  ux <- unique(na.omit(x))
  tx <- tabulate(match(x, ux))
  if(length(ux) != 1 & sum(max(tx) == tx) > 1) {
    if (is.character(ux)) return(NA_character_) else return(NA_real_)
  }
  max_tx <- tx == max(tx)
  return(ux[max_tx])
}


complexes <- complexes %>% 
  filter(loc!="op1") %>% 
  filter(loc!="op2")

test <- soil22 %>%
  rbind(soil_20_21_mod) %>% 
  mutate(som = tolower(som),
         gley = tolower(gley),
         redox = tolower(redox),
         loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "si1","si",loc)) %>% 
  merge(complexes) %>% 
  full_join(site_type_all)

test2 <- as.data.frame(unique(test$loc))

# site_type_all <- site_type_all %>%
#   mutate(trt_all=trt,
#          trt=ifelse(trt=="relict","relict","restored"))

soil_summaries_all <- soil22 %>%
  rbind(soil_20_21_mod) %>%  
  select(-trt) %>% 
  mutate(som = tolower(som),
         gley = tolower(gley),
         redox = tolower(redox),
         loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "si1","si",loc)) %>% 
  merge(complexes) %>%
  full_join(site_type_all) %>%
  mutate(dirt_mass=as.numeric(dirt_mass),
         elevation1=as.numeric(elevation1),
         elevation2=as.numeric(elevation2),
         soil_moist=as.numeric(soil_moist),
         dtw_cm=as.numeric(dtw_cm),
         root_g=as.numeric(root_g),
         ribbon_mm=as.numeric(ribbon_mm),
         cond=as.numeric(cond),
         celcius=as.numeric(celcius),
         dirt_mass = ifelse(dirt_mass == 0, NA, dirt_mass)) %>% 
  mutate(dirt_vol = 1647.4072755,
         bulk_dens = dirt_mass/dirt_vol) %>%
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3",loc!="sw1") %>%  
  group_by(complex,trt) %>% 
  summarise(cond_mean = mean(cond,na.rm = TRUE),
            cond_min = min(cond,na.rm = TRUE),
            cond_max = max(cond,na.rm = TRUE),
            cond_sd = sd(cond,na.rm = TRUE),
            ribbon_mm_mean = mean(ribbon_mm,na.rm = TRUE),
            ribbon_mm_min = min(ribbon_mm,na.rm = TRUE),
            ribbon_mm_max = max(ribbon_mm,na.rm = TRUE),
            ribbon_mm_sd = sd(ribbon_mm,na.rm = TRUE),
            root_g_mean = mean(root_g,na.rm = TRUE),
            root_g_min = min(root_g,na.rm = TRUE),
            root_g_max = max(root_g,na.rm = TRUE),
            root_g_sd = sd(root_g,na.rm = TRUE),
            soil_moist_mean = mean(soil_moist,na.rm = TRUE),
            soil_moist_min = min(soil_moist,na.rm = TRUE),
            soil_moist_max = max(soil_moist,na.rm = TRUE),
            soil_moist_sd = sd(soil_moist,na.rm = TRUE),
            dtw_cm_mean = mean(dtw_cm,na.rm = TRUE),
            dtw_cm_min = min(dtw_cm,na.rm = TRUE),
            dtw_cm_max = max(dtw_cm,na.rm = TRUE),
            dtw_cm_sd = sd(dtw_cm,na.rm = TRUE),
            bulk_dens_mean = mean(bulk_dens,na.rm = TRUE),
            bulk_dens_min = min(bulk_dens,na.rm = TRUE),
            bulk_dens_max = max(bulk_dens,na.rm = TRUE),
            bulk_dens_sd = sd(bulk_dens,na.rm = TRUE),
            celcius_mean = mean(celcius,na.rm = TRUE),
            celcius_min = min(celcius,na.rm = TRUE),
            celcius_max = max(celcius,na.rm = TRUE),
            celcius_sd = sd(celcius,na.rm = TRUE),
            elevation_mean = mean((elevation1+elevation2)/2,na.rm = TRUE),
            elevation_min = min((elevation1+elevation2)/2,na.rm = TRUE),
            elevation_max = max((elevation1+elevation2)/2,na.rm = TRUE),
            elevation_sd = sd((elevation1+elevation2)/2,na.rm = TRUE))


spring_depths21_summary2 <- spring_depths21 %>% 
  mutate(loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "prs1","prs",loc),
         loc = ifelse(loc == "si1","si",loc),
         loc = ifelse(loc == "urep1","urep",loc)) %>% 
  filter(loc != "mm2",
         loc != "dnr1") %>% 
  merge(complexes) %>%  
  merge(site_type_all) %>% 
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3") %>%  
  group_by(complex,trt) %>% 
  select(complex, depth_cm) %>% 
  summarise(spring_depth21_cm_mean = mean(depth_cm,na.rm = TRUE),
            spring_depth21_cm_min = min(depth_cm,na.rm = TRUE),
            spring_depth21_cm_max = max(depth_cm,na.rm = TRUE),
            spring_depth21_cm_sd = sd(depth_cm,na.rm = TRUE))



spring_depths22_summary2 <- spring_depths22 %>% 
  mutate(loc = site) %>%  
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  group_by(complex,trt) %>% 
  filter(loc != "dnr") %>%
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3") %>%  
  select(complex,depth_cm)  %>% 
  summarise(spring_depth22_cm_mean = mean(depth_cm,na.rm = TRUE),
            spring_depth22_cm_min = min(depth_cm,na.rm = TRUE),
            spring_depth22_cm_max = max(depth_cm,na.rm = TRUE),
            spring_depth22_cm_sd = sd(depth_cm,na.rm = TRUE))


spring_depths21_mod <- spring_depths21 %>% 
  select(date,loc,sample,depth_cm) %>% 
  mutate(year="21")

spring_depths_all_summary2 <- spring_depths22 %>% 
  mutate(loc=site,
         sample=s,
         year="22") %>% 
  select(date,loc,sample,depth_cm,year) %>% 
  rbind(spring_depths21_mod) %>% 
  mutate(loc = ifelse(loc == "phm1","phm",loc),
         loc = ifelse(loc == "prs1","prs",loc),
         loc = ifelse(loc == "si1","si",loc),
         loc = ifelse(loc == "urep1","urep",loc)) %>% 
  filter(loc != "mm2",
         loc != "dnr1") %>% 
  merge(complexes) %>%  
  merge(site_type_all) %>% 
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3") %>% 
  select(-trt_all,-year) %>% 
  group_by(complex,trt) %>% 
  summarise(spring_depth_cm_mean = mean(depth_cm,na.rm = TRUE),
            spring_depth_cm_min = min(depth_cm,na.rm = TRUE),
            spring_depth_cm_max = max(depth_cm,na.rm = TRUE),
            spring_depth_cm_sd = sd(depth_cm,na.rm = TRUE))





soil_lab <- soil_lab_all %>%
  merge(complexes) %>% 
  merge(site_type_all) %>% 
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3",loc!="sw1") %>%  
  select(-sample,-s) %>% 
  mutate(top_n=as.numeric(top_n),
         om=as.numeric(om),
         tn=as.numeric(tn),
         sand=as.numeric(ifelse(sand=="NA","0",sand)),
         silt=as.numeric(silt),
         clay=as.numeric(clay)) %>% 
  group_by(complex,trt) %>% 
  summarise(top_nmean = mean(top_n,na.rm = TRUE),
            top_nmin = min(top_n,na.rm = TRUE),
            top_nmax = max(top_n,na.rm = TRUE),
            top_nsd = sd(top_n,na.rm = TRUE),
            om_mean = mean(om,na.rm = TRUE),
            om_min = min(om,na.rm = TRUE),
            om_max = max(om,na.rm = TRUE),
            om_sd = sd(om,na.rm = TRUE),
            nh4_mean = mean(nh4,na.rm = TRUE),
            nh4_min = min(nh4,na.rm = TRUE),
            nh4_max = max(nh4,na.rm = TRUE),
            nh4_sd = sd(nh4,na.rm = TRUE),
            sand_mean = mean(sand,na.rm = TRUE),
            sand_min = min(sand,na.rm = TRUE),
            sand_max = max(sand,na.rm = TRUE),
            sand_sd = sd(sand,na.rm = TRUE),
            silt_mean = mean(silt,na.rm = TRUE),
            silt_min = min(silt,na.rm = TRUE),
            silt_max = max(silt,na.rm = TRUE),
            silt_sd = sd(silt,na.rm = TRUE),
            clay_mean = mean(clay,na.rm = TRUE),
            clay_min = min(clay,na.rm = TRUE),
            clay_max = max(clay,na.rm = TRUE),
            clay_sd = sd(clay,na.rm = TRUE))



complex_wwis <- wwis_all %>% 
  merge(complexes) %>%
  merge(site_type_all) %>%  
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3") %>%  
  group_by(complex,trt) %>% 
  summarise(wwis_mean=mean(wwis,na.rm = TRUE),
            wwis_min=min(wwis,na.rm = TRUE),
            wwis_max = max(wwis,na.rm = TRUE),
            wwis_sd = sd(wwis,na.rm = TRUE))


complex_lon <- site_trt_flood_all %>% 
  mutate(trt_all=trt,
         trt=ifelse(trt=="relict","relict","restored")) %>% 
  merge(complexes) %>% 
  filter(loc!="wm2",loc!="mr1",loc!="wm4",loc!="wr1",loc!="sem4",loc!="sm1",
         loc!="n1",loc!="yps",loc!="pd2",loc!="swm1",loc!="swm2",loc!="sem4",
         loc!="sem3",loc!="sw1") %>%  
  group_by(complex,trt) %>% 
  summarise(lon_mean=mean(Longitude,na.rm = TRUE),
            lon_min=min(Longitude,na.rm = TRUE),
            lon_max = max(Longitude,na.rm = TRUE),
            lon_sd = sd(Longitude,na.rm = TRUE))


data_summary_all <-  soil_summaries_all%>% 
  left_join(worm_summary_all_wide2) %>% 
  merge(arth_summary_all_wide2) %>% 
  merge(spring_depths_all_summary2) %>% 
  mutate_at(c(35:70), ~replace_na(.,0))




data_doc <- data_summary_all %>% 
  merge(complex_lon) %>% 
  merge(complex_wwis) %>%  
  merge(soil_lab) %>% 
  mutate(elevation_km=elevation_mean/1000) %>% 
  mutate(dtw_cm_mean=-dtw_cm_mean) %>% 
  mutate(wetness = ifelse(wwis_mean<3.0,"wet","non")) %>% 
  mutate(n_trt=as.numeric(ifelse(trt=="relict","22","12"))) %>% 
  merge(veg_summary_merge) %>% 
  mutate(n_trt=ifelse(trt=="relict",21,12)) %>% 
  merge(invert_richness_complex) %>% 
  merge(invert_bio_tot)





data_doc_mod_re <- restored_recon_time %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  summarise(time_mean=mean(time_since),
            time_min=min(time_since)) %>% 
  left_join(data_doc) %>% 
  filter(trt!="relict") %>% 
  mutate(time_cat_comp=ifelse(time_mean<10,"new","old"))




