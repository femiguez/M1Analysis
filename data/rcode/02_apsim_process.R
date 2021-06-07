#### Processing output from the apsim optimizations
#### Author: Fernando E. Miguez
#### Created: 2021-04-01
####
#### Goal: process output from APSIM runs and make tables and figures

library(apsimx)
library(ggplot2)
library(here)

hr.path <- here()
setwd(file.path(hr.path, "data", "rcode"))

### Predicted values
prds <- read.csv("../lohi_centers.csv")

### Read in the low, medium and high for Accola 2018 - mukey 2765537
prds.accola.2018 <- subset(prds, field == "Accola" & year == 2018)

sim.medium.KL <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_medium_KL.out",
                    src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_medium")
sim.medium.XF <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_medium_XF.out",
                            src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_medium")
sim.medium.DUL <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_medium_DUL.out",
                            src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_medium")
sim.low.KL <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_low_KL.out",
                     src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_low")
sim.low.XF <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_low_XF.out",
                     src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_low")
sim.low.DUL <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_low_DUL.out",
                      src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_low")
sim.high.HI <- read_apsim("name_AccolaDefault_mukey_2765537_soybean_2018_rot_sfc_sim_high_HI.out",
                          src.dir = "./results/AccolaDefault/soybean/mukey_2765537/sims_high")
### Plot
ggplot() + 
  geom_point(data = prds.accola.2018, aes(x = as.Date("2018-11-01"), y = Low*1e3, color = "Low")) + 
  geom_point(data = prds.accola.2018, aes(x = as.Date("2018-11-01"), y = Mid*1e3, color = "Medium")) + 
  geom_point(data = prds.accola.2018, aes(x = as.Date("2018-11-01"), y = High*1e3, color = "High")) + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = soybean_yield, color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = soybean_yield, color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = soybean_yield, color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = soybean_yield, color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = soybean_yield, color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = soybean_yield, color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = soybean_yield, color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + ylab("Yield (kg/ha)") + 
  ggtitle("Yield optimization")
ggsave("./figs/Accola-soybean-2018-optim-yield.png")

ggplot() + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = leach_no3, color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = leach_no3, color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = leach_no3, color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = leach_no3, color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = leach_no3, color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = leach_no3, color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = leach_no3, color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + ylab("Nitrate (NO3) leaching (kg/ha)") + 
  ggtitle("Impact on NO3 leaching")
ggsave("./figs/Accola-soybean-2018-optim-yield-NO3-leach.png")

ggplot() + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = lch_no3n, color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = lch_no3n, color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = lch_no3n, color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = lch_no3n, color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = lch_no3n, color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = lch_no3n, color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = lch_no3n, color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + ylab("Nitrogen-Nitrate (N-NO3) leaching (kg/ha)") + 
  ggtitle("Impact on N-NO3 leaching")
ggsave("./figs/Accola-soybean-2018-optim-yield-N-NO3-leach.png")

ggplot() + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = cumsum(lch_no3n), color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = cumsum(lch_no3n), color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = cumsum(lch_no3n), color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = cumsum(lch_no3n), color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = cumsum(lch_no3n), color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = cumsum(lch_no3n), color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = cumsum(lch_no3n), color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2018-06-01", "2018-11-15"))) + 
  ylab("Cumulative nitrogen-nitrate leaching (kg/ha)") + 
  ggtitle("Impact on cumulative N-NO3 leaching")
ggsave("./figs/Accola-soybean-2018-optim-cumulative-leach.png")

#### Accola Maize 2019 mukey 2765537####
mukey <- 2765537
crop <- "maize"
yr <- 2019
ext <- "cfs"
prds.accola.2019 <- subset(prds, field == "Accola" & year == 2019)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_medium_KL.out")
src.dir.pth <- paste0("./results/AccolaDefault/", crop, "/mukey_", mukey, "/sims_medium")
sim.medium.KL <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_medium_XF.out")
sim.medium.XF <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_medium_DUL.out")
sim.medium.DUL <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_low_KL.out")
src.dir.pth <- paste0("./results/AccolaDefault/", crop, "/mukey_", mukey, "/sims_low")
sim.low.KL <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_low_XF.out")
sim.low.XF <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_low_DUL.out")
sim.low.DUL <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

fl.pth <- paste0("name_AccolaDefault_mukey_", mukey, "_", crop, "_", yr, "_rot_", ext, "_sim_high_HI.out")
src.dir.pth <- paste0("./results/AccolaDefault/", crop, "/mukey_", mukey, "/sims_high")
sim.high.HI <- read_apsim(file = fl.pth, src.dir = src.dir.pth)

### Plot
ggplot() + 
  geom_point(data = prds.accola.2019, aes(x = as.Date("2019-11-01"), y = Low*1e3, color = "Low")) + 
  geom_point(data = prds.accola.2019, aes(x = as.Date("2019-11-01"), y = Mid*1e3, color = "Medium")) + 
  geom_point(data = prds.accola.2019, aes(x = as.Date("2019-11-01"), y = High*1e3, color = "High")) + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = maize_yield, color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = maize_yield, color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = maize_yield, color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = maize_yield, color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = maize_yield, color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = maize_yield, color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = maize_yield, color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2019-06-01", "2019-11-15"))) + ylab("Yield (kg/ha)") + xlab("Date") + 
  ggtitle("Yield optimization: Accola - soybean - 2018 - mukey 2765537")
ggsave("./figs/Accola-maize-2019-optim-yield.png")

ggplot() + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = leach_no3, color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = leach_no3, color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = leach_no3, color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = leach_no3, color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = leach_no3, color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = leach_no3, color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = leach_no3, color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2019-06-01", "2019-11-15"))) + ylab("Nitrate (NO3) leaching (kg/ha)") + xlab("Date") + 
  ggtitle("Impact on NO3 leaching: Accola - soybean - 2018 - mukey 2765537")
ggsave("./figs/Accola-maize-2019-optim-yield-NO3-leach.png")

ggplot() + 
  geom_line(data = sim.medium.KL, aes(x = Date, y = cumsum(lch_no3n), color = "KL medium optimized"), size = 2) + 
  geom_line(data = sim.medium.XF, aes(x = Date, y = cumsum(lch_no3n), color = "XF medium optimized"), size = 2) + 
  geom_line(data = sim.medium.DUL, aes(x = Date, y = cumsum(lch_no3n), color = "DUL medium optimized"), size = 2) + 
  geom_line(data = sim.low.KL, aes(x = Date, y = cumsum(lch_no3n), color = "KL low optimized"), size = 2) + 
  geom_line(data = sim.low.XF, aes(x = Date, y = cumsum(lch_no3n), color = "XF low optimized"), size = 2) + 
  geom_line(data = sim.low.DUL, aes(x = Date, y = cumsum(lch_no3n), color = "DUL low optimized"), size = 2) + 
  geom_line(data = sim.high.HI, aes(x = Date, y = cumsum(lch_no3n), color = "HI high optimized"), size = 2) + 
  xlim(as.Date(c("2019-06-01", "2019-11-15"))) + xlab("Date") + 
  ylab("Cumulative nitrogen-nitrate leaching (kg/ha)") + 
  ggtitle("Impact on cumulative N-NO3 leaching: Accola - soybean - 2018 - mukey 2765537")
ggsave("./figs/Accola-maize-2019-optim-cumulative-leach.png")
