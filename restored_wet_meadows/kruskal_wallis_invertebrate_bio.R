library(dunn.test)
library(FSA)


#####################################################################


invert_biomass_complex <- worm_summary_all_wide2 %>% 
  full_join(arth_summary_all_wide2)

invert_biomass_complex[is.na(invert_biomass_complex)] <- 0


kruskal.test(invert_biomass_complex$diplo,invert_biomass_complex$trt)

kruskal.test(invert_biomass_complex$tipu,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$taban,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$diplo,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$apor,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$strat,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$scara,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$carab,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$lepi,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$arma,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$elat,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$curc,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$staph,invert_biomass_complex$trt)

kruskal.test(invert_biomass_complex$anob,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$arach,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$asil,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$canth,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$chrys,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$doli,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$form,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$heteroc,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$juve,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$meloi,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$myri,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$penta,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$phalangida,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$planthopper,invert_biomass_complex$trt)

kruskal.test(data_doc$tipu,data_doc$trt)


kruskal.test(data_doc$tot_invert_bio,data_doc$trt)



kruskal.test(invert_biomass_complex$bruc,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$byrrh,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$chiro,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$eisen,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$gryl,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$hydro,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$lamp,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$ptil,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$ptin,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$sarc,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$teneb,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$there,invert_biomass_complex$trt)
kruskal.test(invert_biomass_complex$trog,invert_biomass_complex$trt)



