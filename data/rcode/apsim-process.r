#### Processing output from the apsim optimizations
#### Author: Fernando E. Miguez
#### Created: 2021-04-01
####
#### Goal: process output from APSIM runs and make tables and figures

library(apsimx)
library(ggplot2)
library(here)

hr.path <- here()
setwd(file.path(hr.path, "data/rcode"))

### Read in the high and medium for Accola 2018 - mukey 2765537

sim.high <- read_apsim("name_Accola_mukey_2765537_rot_sfc_sim_high.out")
