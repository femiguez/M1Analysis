## The purpose of this file is to try to understand the statistical analysis of
## the prediction for each site

library(sf)
library(ggplot2)
library(mgcv)
library(nlraa)

#### SOYBEAN ####
#### Read in yield monitor data
#### This data are only for 2018 where there was soybean
ym <- st_read("data/out_files/accola/shape_files/accola_soybean_2018_full/accola_soybean_2018_full.shp")

## Spatial figures
ggplot(data = ym) + 
  geom_sf(aes(fill = s_ydMgHaMn), linetype = 0) +
  scale_fill_viridis_c() + 
  theme(legend.position = "top") 

## Values of TWI greater than 7.5 are the low areas of the field
ggplot(data = ym) + 
  geom_sf(aes(fill = mean_twi), linetype = 0) +
  scale_fill_viridis_c() + 
  theme(legend.position = "top") 

## What is the relationship between TWI And NDVI?
ggplot(data = ym, aes(x = mean_twi, y = jun_ndvi)) + 
  geom_point()

## What is s_ydMgHaMn? 
## Why does TWI sort of saturate at values of around 7.7?
## Is it because this is about 
ggplot(data = ym, aes(x = mean_twi, y = s_ydMgHaMn)) + 
  geom_point() + 
  geom_smooth()

ym.ss <- subset(ym, mean_twi < 7)
ggplot(data = ym.ss, aes(x = mean_twi, y = s_ydMgHaMn)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = ym, aes(x = jun_ndvi, y = s_ydMgHaMn)) + 
  geom_point() + 
  geom_smooth()

ym$logit_jun_ndvi <- log(ym$jun_ndvi/(1 - ym$jun_ndvi)) + 1
ggplot(data = ym, aes(x = logit_jun_ndvi, y = s_ydMgHaMn)) + 
  geom_point() + 
  geom_smooth()

## What is soybean_yi?
ggplot(data = ym, aes(x = mean_twi, y = soybean_yi)) + 
  geom_point()

## For TWI > 7.5 is there a relationship between yield and NDVI
ym.ss2 <- subset(ym, mean_twi > 7.5)
## ym.ss2$logit_jun_ndvi <- log(ym.ss2$jun_ndvi/(1 - ym.ss2$jun_ndvi)) + 1
ggplot(data = ym.ss2, aes(x = jun_ndvi, y = s_ydMgHaMn)) + 
  geom_point() + 
  geom_smooth(method = "gam")

ggplot(data = ym.ss2, aes(x = s_ydMgHaMn)) + 
  geom_density() + 
  geom_vline(aes(color = "mean yield for TWI > 7.5", xintercept = mean(s_ydMgHaMn))) +
  geom_vline(aes(color = "mean yield for whole field", xintercept = mean(ym$s_ydMgHaMn))) + 
  theme(legend.title = element_blank())
   
## For an intercept model Root MSE is the same as the standard deviation
sd(ym$s_ydMgHaMn) ## Root MSE is 1.39
fm1 <- lm(s_ydMgHaMn ~ mean_twi + jun_ndvi, data = ym)
summary(fm1)$r.squared ## 36% R-squared
IA_tab(object = fm1)$IA_tab ## Root MSE is 1.11
gm1 <- gam(s_ydMgHaMn ~ s(mean_twi) + s(jun_ndvi), data = ym)
IA_tab(object = gm1)$IA_tab ## Root MSE is 1.02
summary(gm1) ## 46% variance explained 
gm2 <- gam(s_ydMgHaMn ~ te(mean_twi, jun_ndvi), data = ym) ## The interaction is not that important
summary(gm2) ## 46% variance explained (interaction is not that important)

## gm2 is the best model according to AIC and not different from gm1
## according to BIC
IC_tab(fm1, gm1, gm2)

## Assessing asumptions
plot(gm1)

#### CORN ####
ymm <- st_read("data/out_files/accola/shape_files/accola_maize_2019_full/accola_maize_2019_full.shp")

## Spatial figures
ggplot(data = ymm) + 
  geom_sf(aes(fill = s_ydMgHaMn), linetype = 0) +
  scale_fill_viridis_c() + 
  theme(legend.position = "top") 

## Values of TWI greater than 7.5 are the low areas of the field
ggplot(data = ymm) + 
  geom_sf(aes(fill = mean_twi), linetype = 0) +
  scale_fill_viridis_c() + 
  theme(legend.position = "top") 

ggplot(data = ymm, aes(x = mean_twi, y = s_ydMgHaMn)) + 
  geom_point() + 
  geom_smooth()

## Analysis for corn
sd(ymm$s_ydMgHaMn) ## Root MSE is 3.3977
fmm1 <- lm(s_ydMgHaMn ~ mean_twi + jun_ndvi, data = ymm)
summary(fmm1)$r.squared ## 47% R-squared
IA_tab(object = fmm1)$IA_tab ## Root MSE is 2.47
gmm1 <- gam(s_ydMgHaMn ~ s(mean_twi) + s(jun_ndvi), data = ymm)
IA_tab(object = gmm1)$IA_tab ## Root MSE is 2.37
summary(gm1) ## 46% variance explained 
gmm2 <- gam(s_ydMgHaMn ~ te(mean_twi, jun_ndvi), data = ymm) ## The interaction is not that important
summary(gmm2) ## 52% variance explained (interaction is important)

IC_tab(fmm1, gmm1, gmm2)


