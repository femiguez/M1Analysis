#### Optimizing APSIM runs against suboptimal yields
####
#### Date: 2021-04-01
#### Edited: 2021-05-19
#### 
#### APSIM Classic has a bug in which simulations run at 
#### the command line can only be run from the current directory so 
#### I need to copy files here

library(apsimx) ### Need 1.983 for this script (from github)
library(sf)
library(ggplot2)
library(here)

hr.path <- here()
setwd(file.path(hr.path, "data", "rcode"))

apsim_options(warn.versions = FALSE)

yr <- "2018"
sfcs <- paste0("../apsim_files/AccolaDefault/", yr, "/sfc")
lsf <- list.files(sfcs)
apsim.files <- grep("apsim$", lsf, value = TRUE)
file.copy(from = paste0(sfcs, "/", apsim.files), to = ".")

i <- "AccolaDefault_2765537_sfc.apsim"

for(i in list.files(pattern = ".apsim$")){
  
  site <- strsplit(i, "_")[[1]][1]
  mukey <- strsplit(i, "_")[[1]][2]
  
  site.met <- tolower(strsplit(site, "Default")[[1]][1])

  edit_apsim(file = i,
             node = "Weather",
             value = paste0(sfcs, "/met_files/",site.met,"-1990-2020.met"),
             overwrite = TRUE)
  
  sim0 <- apsim(file = i, value = "report")
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_sfc_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_rot_sfc_sim_high.out")
  file.copy(from = frm,
            to = paste0("./sims_high/", tu))

  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_sfc_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_rot_sfc_sim_high.sum")
  file.copy(from = frm,
            to = paste0("./sims_high/", tu))
  
  ## Calculating yield and nitrate leaching requires subsetting
  sim0s <- subset(sim0, year == yr)
  ## We are either growing corn or soybean
  max.yield <- max(sim0s$soybean_yield + sim0s$maize_yield)
  n.leach <- sum(sim0s$leach_no3)
  
  dat0 <- data.frame(file = i, max_yield = max.yield, 
                     N_leach = n.leach)

  ## Which crop are we growing?
  crop <- ifelse(max(sim0s$soybean_yield) > 0, "soybean", "maize")[1]
  ## inspect_apsim(i, node = "Soil", soil.child = "Water")
  ## The crops are maize soybean and wheat
  
  ### When crop is maize the index needs to be 1
  ### otherwise 2
  crop.index <- ifelse(crop == "maize", 1, 2)
  pp.kl <- inspect_apsim_xml(i, parm = paste0("SoilCrop[", crop.index,"]/KL"))
  ## Only the second one is relevant to soybeans
  pp.xf <- inspect_apsim_xml(i, parm = paste0("SoilCrop[", crop.index,"]/XF"))

  ## Create the yield data frame for medium
  harvest.date <- sim0s[which.max(sim0s[,paste0(crop,"_yield")]),"Date"]
  ## Retrieve yield data
  yld.cat <- read.csv("../lohi_centers.csv")
  yld.cat.s <- subset(yld.cat, year == yr & 
                      field == strsplit(site, "Default")[[1]][1])
  
  yld.dat <- data.frame(Date = as.Date(harvest.date),
                        crop_yield = yld.cat.s[,"Mid"])
  names(yld.dat) <- c("Date", paste0(crop, "_yield"))
  
  op.kl <- optim_apsim(file = i, 
                       data = yld.dat,
                       parm.paths = pp.kl,
                       method = "Brent",
                       lower = 0.001, upper = 1.001)
  
  sim1 <- apsim("Accola_2765537_sfc.apsim", value = "report")
  
  
}





## I'm trying different optimizations
## First KL alone
op.kl <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.dat,
                     parm.paths = pp.kl[2],
                     method = "Brent",
                     lower = 0.001, upper = 1.001)


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

sim3 <- apsim("Accola_2765537_sfc.apsim", value = "report")

sim3.2018 <- subset(sim3, Date > as.Date("2018-01-01") & Date < as.Date("2018-12-31"))
(max.yield <- max(sim3.2018$soybean_yield))
(n.leach <- sum(sim3.2018$leach_no3))

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.out",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_low_kl.out")

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.sum",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_low_kl.sum")

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

### Now for XF
## Trying optimizing XF
op.xf <- optim_apsim("Accola_2765537_sfc.apsim", 
                     data = yld.datL,
                     parm.paths = pp.xf[2],
                     method = "Brent",
                     lower = 0.01, 
                     upper = 1.001)

sim4 <- apsim("Accola_2765537_sfc.apsim", value = "report")

sim4.2018 <- subset(sim4, Date > as.Date("2018-01-01") & Date < as.Date("2018-12-31"))
(max.yield <- max(sim4.2018$soybean_yield))
(n.leach <- sum(sim4.2018$leach_no3))

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.out",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_low_xf.out")

file.copy(from = "name_Accola_mukey_2765537_rot_sfc_sim.sum",
          to = "name_Accola_mukey_2765537_rot_sfc_sim_low_xf.sum")



