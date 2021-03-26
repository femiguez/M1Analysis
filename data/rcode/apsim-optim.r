#### Optimizing APSIM runs against suboptimal yields
####
#### Date: 2021-03-25
#### 
#### APSIM Classic has a bug in which simulations run at 
#### the command line can only be run from the current directory so 
#### I need to copy files here

library(apsimx) ### Need 1.979 for this script (from github)
library(sf)
library(ggplot2)

apsim_options(warn.versions = FALSE)

sfcs <- "../apsim_files/Accola/2018/sfc"
lsf <- list.files(sfcs)
apsim.files <- grep("apsim$", lsf, value = TRUE)
file.copy(from = paste0(sfcs, "/", apsim.files), to = ".")

## Let's start with "Accola_2765537_sfc.apsim"
inspect_apsim("Accola_2765537_sfc.apsim",
              node = "Weather")

edit_apsim("Accola_2765537_sfc.apsim", 
           node = "Weather",
           value = "../met/accola-1990-2020.met",
           overwrite = TRUE)

sim0 <- apsim("Accola_2765537_sfc.apsim", value = "report")

sim0[which.max(sim0$soybean_yield),]
## The harvest date was 2018-09-01

#### Read in yield monitor data
#### This data are only for 2018 where there was soybean
ym <- st_read("../shapefiles/accola_soybean_2018_full/accola_soybean_2018.shp")

names(ym)

ym$soy_yield_cat <- cut(ym$s_ydMgHaMn, 5)

ggplot(data = ym, aes(x = s_ydMgHaMn, fill = soy_yield_cat)) + 
  geom_histogram()

### So second category is in a yield level of about 4 Mg/ha
pp.kl <- inspect_apsim_xml("Accola_2765537_sfc.apsim", parm = "KL")
## Only the second one is relevant to soybeans
pp.xf <- inspect_apsim_xml("Accola_2765537_sfc.apsim", parm = "XF")

## Create the yield data frame
yld.dat <- data.frame(Date = as.Date("2018-09-01"),
                      soybean_yield = 3000)

## I'm trying different optimizations
## First KL alone
op.kl <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.dat,
                     parm.paths = pp.kl[2],
                     method = "Brent",
                     lower = 0.001, upper = 1.001)

sim1 <- apsim("Accola_2765537_sfc.apsim", value = "report")

op.kl <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.dat,
                     parm.paths = c(pp.kl[2], pp.xf[2]),
                     method = "L-BFGS-B",
                     lower = c(0.01, 0.01), upper = c(1.001, 1.001))




yld.dat2 <- data.frame(Date = as.Date("2018-09-01"),
                       soybean_yield = 2000)

op2 <- optim_apsim("Accola_2765537_sfc.apsim", 
                  data = yld.dat2,
                  parm.paths = pp.kl[2],
                  method = "Brent",
                  lower = 0.0001, 
                  upper = 0.5)

sim2 <- apsim("Accola_2765537_sfc.apsim", value = "report")
