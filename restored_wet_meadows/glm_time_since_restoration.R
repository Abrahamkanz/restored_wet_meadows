

############################################################Linear modeling
#Time since restoration

##########################################################GLMs for time since restoration

complex_restore_time <- complexes %>% 
  merge(restored_recon_time) %>% 
  group_by(complex) %>% 
  reframe(time_mean=mean(time_since))

data_doc_restored_time <- data_doc %>% 
  merge(complex_restore_time) %>% 
  filter(trt=="restored") %>% 
  merge(veg_quads_for_time)

wwis <- lm(wwis_mean~time_mean, data = data_doc_mod_re)
summary(wwis)
confint(wwis)

om <- lm(om_mean~time_mean, data = data_doc_mod_re)
summary(om)
confint(om)

spring21 <- lm(spring_depth_cm_mean ~ time_mean,data = data_doc_mod_re)
summary(spring21)
confint(spring21)

spring22 <- lm(spring_depth22_cm_mean ~ time_mean,data = data_doc_mod_re)
summary(spring22)
confint(spring22)

dtw <- lm(dtw_cm_mean ~ time_mean,data = data_doc_mod_re)
summary(dtw)
confint(dtw)


cond <- lm(cond_mean ~ time_mean,data = data_doc_mod_re)
summary(cond)
confint(cond)

soil_moist <- lm(soil_moist_mean ~ time_mean,data = data_doc_mod_re)
summary(soil_moist)
confint(soil_moist)

bulk_dens <- lm(bulk_dens_mean ~ time_mean,data = data_doc_mod_re)
summary(bulk_dens)
confint(bulk_dens)

nh4 <- lm(nh4_mean ~ time_mean,data = data_doc_mod_re)
summary(nh4)
confint(nh4)


top_nmean <- lm(top_nmean ~ time_mean,data = data_doc_mod_re)
summary(top_nmean)
confint(top_nmean)


ld <- lm(ld_mean~time_mean, data = data_doc_mod_re)
summary(ld)

graminoid <- lm(graminoid~time_mean, data = data_doc_mod_re)
summary(graminoid)

sedge <- lm(sedge~time_mean, data = data_doc_mod_re)
summary(sedge)





#####################################################Inverts- Frequentist GLM


test <- lm(arth_abun_tipu ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_scara ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_elat ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_curc ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_form ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_strat ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_staph ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_lepi ~ time_since, data = temp)
summary(test)

test <- lm(worm_abun_diplo ~ time_since, data = temp)
summary(test)

test <- lm(worm_abun_apor ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_heteroc ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_doli ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_meloi ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_myri ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_canth ~ time_since, data = temp)
summary(test)

test <- lm(arth_abun_asil ~ time_since, data = temp)
summary(test)