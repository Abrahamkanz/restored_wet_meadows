library(tidyverse)
library(BiodiversityR)
library(CCP)

########################################################PCA

pca_env <- data_doc[c(900,896,11,19,23,920,15,912)]

pca1 <- prcomp(x=pca_env,
               center = TRUE,
               scale. = FALSE)

pca1
plot(pca1)
summary(pca1)
scores(pca1)


autoplot(pca1, data = data_doc, colour = 'trt')

#######################################################Invertebrate CCA

ord <- cca(comm_bio_m3_mod ~ ., data=env_all)


ord
anova.cca(ord)
anova.cca(ord, by="axis",permutations = 10000)
plot(ord)
summary(ord)
scores(ord)

test <- adonis2(dist_matrix~trt,data_doc,permutations = 10000)
test

test$`Pr(>F)`[1]

plot(ord, type = "n", display = "spec")
points(ord, display = "spec")
points(ord, display = "bp")
ordilabel(ord, dis="sp")
text(ord,display = "bp")


p.perm(env_all,comm_bio_m3, nboot = 999, rhostart = 1, type = "Wilks")




##############################################################Veg CCA

ord2 <- cca(veg_quads_mod ~ ., data=env_all)

ord2
anova.cca(ord2)
anova.cca(ord2, by="axis")
plot(ord2)
summary(ord2)
scores(ord2)

test <- adonis2(veg_matrix~trt,data_doc,permutations = 10000)
test
