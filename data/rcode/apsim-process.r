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

### Read in the high and medium for Accola 2018 - mukey 2765537

sim0 <- read_apsim("name_Accola_mukey_2765537_rot_sfc_sim_high.out")
sim1 <- read_apsim("name_Accola_mukey_2765537_rot_sfc_sim_medium_kl.out")
sim2 <- read_apsim("name_Accola_mukey_2765537_rot_sfc_sim_medium_xf.out")

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