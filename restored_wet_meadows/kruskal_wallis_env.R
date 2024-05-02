library(dunn.test)
library(FSA)



kruskal.test(data_doc$wwis_mean,data_doc$trt,method = "bonferroni")


test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(wwis_mean),
          se=sd(wwis_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()




kruskal.test(data_doc$dtw_cm_mean,data_doc$trt,method = "bonferroni")



test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(dtw_cm_mean),
          se=sd(dtw_cm_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()




kruskal.test(data_doc$spring_depth21_cm_mean,data_doc$trt,method = "bonferroni")



test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(spring_depth21_cm_mean),
          se=sd(spring_depth21_cm_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()





kruskal.test(data_doc$spring_depth22_cm_mean,data_doc$trt,method = "bonferroni")


test <- data_doc %>%
  group_by(trt) %>% 
  summarise(mean=mean(spring_depth22_cm_mean),
            se=sd(spring_depth22_cm_mean)/sqrt(n_trt)) %>%
  ungroup()





kruskal.test(data_doc$bulk_dens_mean,data_doc$trt,method = "bonferroni")


test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(bulk_dens_mean),
          se=sd(bulk_dens_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()




kruskal.test(data_doc$elevation_mean,data_doc$trt,method = "bonferroni")

test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(elevation_mean),
          se=sd(elevation_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()



kruskal.test(data_doc$om_mean,data_doc$trt,method = "bonferroni")

test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(om_mean),
          se=sd(om_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()




kruskal.test(data_doc$nh4_mean,data_doc$trt,method = "bonferroni")


test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(nh4_mean),
          se=sd(nh4_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()




kruskal.test(data_doc$top_nmean,data_doc$trt,method = "bonferroni")
kruskal.test(data_doc$top_nmean,data_doc$trt)


test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(top_nmean),
          se=sd(top_nmean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()


kruskal.test(data_doc$soil_moist_mean,data_doc$trt)

test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(soil_moist_mean),
          se=sd(soil_moist_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()


kruskal.test(data_doc$sand_mean,data_doc$trt)

test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(sand_mean),
          se=sd(sand_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()



kruskal.test(data_doc$silt_mean,data_doc$trt)

test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(silt_mean),
          se=sd(silt_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()




kruskal.test(data_doc$clay_mean,data_doc$trt)

test <- data_doc %>%
  group_by(trt) %>% 
  reframe(mean=mean(clay_mean),
          se=sd(clay_mean)/sqrt(n_trt)) %>%
  ungroup() %>% 
  distinct()


test <- veg_summary_all %>% 
  merge(trt_number)


kruskal.test(veg_summary_all$bg_mean,veg_summary_all$trt)

test2 <- test %>%
  group_by(trt) %>% 
  reframe(mean=mean(bg_mean),
          se=sd(bg_mean)/sqrt(trt_n)) %>%
  ungroup() %>% 
  distinct()


kruskal.test(veg_summary_all$ld_mean,veg_summary_all$trt)

test2<-test %>%
  group_by(trt) %>% 
  reframe(mean=mean(ld_mean),
          se=sd(ld_mean)/sqrt(trt_n)) %>%
  ungroup() %>% 
  distinct()



kruskal.test(data_doc$root_g_mean,data_doc$trt)

test %>%
  group_by(trt) %>% 
  reframe(mean=mean(ld_mean),
          se=sd(ld_mean)/sqrt(trt_n)) %>%
  ungroup() %>% 
  distinct()
