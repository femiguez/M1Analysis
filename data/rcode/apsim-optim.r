#### Optimizing APSIM runs against suboptimal yields
####
#### Date: 2021-04-01
#### 
#### APSIM Classic has a bug in which simulations run at 
#### the command line can only be run from the current directory so 
#### I need to copy files here

library(apsimx) ### Need 1.980 for this script (from github)
library(sf)
library(ggplot2)
library(here)

hr.path <- here()
setwd(file.path(hr.path, "data", "rcode"))

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

names(sim0)

sim0[which.max(sim0$soybean_yield),]
## The harvest date was 2018-09-01

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.out",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_high.out")

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.sum",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_high.sum")

### Calculate yield and nitrate leaching for the default simulation
sim0.2018 <- subset(sim0, Date > as.Date("2018-01-01") & Date < as.Date("2018-12-31"))
(max.yield <- max(sim0.2018$soybean_yield))
(n.leach <- sum(sim0.2018$leach_no3))

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

## Create the yield data frame for medium
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

sim1.2018 <- subset(sim1, Date > as.Date("2018-01-01") & Date < as.Date("2018-12-31"))
(max.yield <- max(sim1.2018$soybean_yield))
(n.leach <- sum(sim1.2018$leach_no3))

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.out",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_medium_kl.out")

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.sum",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_medium_kl.sum")

inspect_apsim("Accola_2765537_sfc.apsim",
              node = "Soil",
              soil.child = "Water",
              parm = "KL")

kl.vals <- c(0.08, 0.079, 0.078, 0.077, 0.076, 0.075, 0.073, 0.07, 
             0.068, 0.066, 0.062, 0.058, 0.054, 0.044, 0.036, 0.03)

edit_apsim("Accola_2765537_sfc.apsim",
           node = "Other",
           parm.path = pp.kl[2],
           value = kl.vals,
           overwrite = TRUE)

inspect_apsim("Accola_2765537_sfc.apsim",
              node = "Soil",
              soil.child = "Water",
              parm = "KL")

## Trying optimizing XF
op.xf <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.dat,
                     parm.paths = pp.xf[2],
                     method = "Brent",
                     lower = 0.01, 
                     upper = 1.001)

## So far: default 53, KL 49, XF 107
sim2 <- apsim("Accola_2765537_sfc.apsim", value = "report")

sim2.2018 <- subset(sim2, Date > as.Date("2018-01-01") & Date < as.Date("2018-12-31"))
(max.yield <- max(sim2.2018$soybean_yield))
(n.leach <- sum(sim2.2018$leach_no3))

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.out",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_medium_xf.out")

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.sum",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_medium_xf.sum")

## Restore XF values
xf.vals <- rep(1, 16)

edit_apsim("Accola_2765537_sfc.apsim",
           node = "Other",
           parm.path = pp.xf[2],
           value = xf.vals,
           overwrite = TRUE)

#### Trying both KL and XF??? ----
op.kl.xf <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.dat,
                     parm.paths = c(pp.kl[2], pp.xf[2]),
                     method = "L-BFGS-B",
                     lower = 0.2, 
                     upper = 1.001)
#### As expected this does not work

## Restore
xf.vals <- rep(1, 16)

edit_apsim("Accola_2765537_sfc.apsim",
           node = "Other",
           parm.path = pp.xf[2],
           value = xf.vals,
           overwrite = TRUE)


## Create the yield data frame for low
yld.datL <- data.frame(Date = as.Date("2018-09-01"),
                      soybean_yield = 2000)

## First KL alone
op.kl <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.datL,
                     parm.paths = pp.kl[2],
                     method = "Brent",
                     lower = 0.001, upper = 1.001)




### Plot
ggplot() + 
  geom_line(data = sim0, aes(x = Date, y = soybean_yield, color = "default"), size = 2) + 
  geom_line(data = sim1, aes(x = Date, y = soybean_yield, color = "KL optimized"), size = 2) + 
  geom_line(data = sim2, aes(x = Date, y = soybean_yield, color = "XF optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + ylab("Yield (kg/ha)") + 
  ggtitle("Yield optimization")
ggsave("optim-yield.png")

ggplot() + 
  geom_line(data = sim0, aes(x = Date, y = leach_no3, color = "default"), size = 2) + 
  geom_line(data = sim1, aes(x = Date, y = leach_no3, color = "KL optimized"), size = 2) + 
  geom_line(data = sim2, aes(x = Date, y = leach_no3, color = "XF optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + ylab("Nitrate leaching (kg/ha)") + 
  ggtitle("Impact on NO3 leaching")
ggsave("optim-yield-leach.png")


ggplot() + 
  geom_line(data = sim0, aes(x = Date, y = cumsum(leach_no3), color = "default"), size = 2) + 
  geom_line(data = sim1, aes(x = Date, y = cumsum(leach_no3), color = "KL optimized"), size = 2) + 
  geom_line(data = sim2, aes(x = Date, y = cumsum(leach_no3), color = "XF optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + 
  ylab("Cumulative nitrate leaching (kg/ha)") + 
  ggtitle("Impact on cumulative NO3 leaching")
ggsave("optim-cumulative-leach.png")
