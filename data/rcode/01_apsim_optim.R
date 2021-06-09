#### Optimizing APSIM runs against suboptimal, default and supraoptimal yields
####
#### Date: 2021-04-01
#### Edited: 2021-06-07
#### 
#### APSIM Classic has a bug in which simulations run at 
#### the command line can only be run from the current directory so 
#### I need to copy files here

library(apsimx) ### Need 1.984
library(sf)
library(ggplot2)
library(here)

hr.path <- here()
setwd(file.path(hr.path, "data", "rcode"))

apsim_options(warn.versions = FALSE)

yr <- "2016"
site.default <- "BasswoodDefault"
## This is for soybean
## sfcs <- file.path("../apsim_files", site.default, yr, "sfc")
## This is for corn
sfcs <- file.path("../apsim_files", site.default, yr, "cfs")
lsf <- list.files(sfcs)
apsim.files <- grep("apsim$", lsf, value = TRUE)
file.copy(from = paste0(sfcs, "/", apsim.files), to = ".")

## Copy xml files to here
## Remove them if necessary
file.remove(from = "MaizeM1.xml")
file.remove(from = "SoybeanM1.xml")

file.copy(from = "../crop_xml/MaizeM1.xml", to = ".")
file.copy(from = "../crop_xml/SoybeanM1.xml", to = ".")

j <- 1

for(i in list.files(pattern = ".apsim$")){
  
  site <- strsplit(i, "_")[[1]][1]
  mukey <- strsplit(i, "_")[[1]][2]
  
  wrt.dir0 <- file.path(".", "results", site)
  wrt.dir1 <- file.path(".", "results", site, paste0("mukey_", mukey))
  if(j == 1) dir.create(wrt.dir0)
  dir.create(wrt.dir1)
  
  site.met <- tolower(strsplit(site, "Default")[[1]][1])

  edit_apsim(file = i,
             node = "Weather",
             value = paste0(sfcs, "/met_files/",site.met,"-1990-2020.met"),
             overwrite = TRUE)

  ppm <- inspect_apsim_xml(file = i, parm = "maize/ini/filename")
  edit_apsim(file = i, node = "Other", parm.path = ppm, value = "MaizeM1.xml", overwrite = TRUE)
  
  pps <- inspect_apsim_xml(file = i, parm = "soybean")
  edit_apsim(file = i, node = "Other", parm.path = pps, value = "SoybeanM1.xml", overwrite = TRUE)
    
  sim0 <- apsim(file = i, value = "report")
  
  sim0s <- subset(sim0, year == yr)

  ## Which crop are we growing?
  crop <- ifelse(max(sim0s$soybean_yield) > 0, "soybean", "maize")[1]
  file_ext <- ifelse(crop == "soybean", "sfc", "cfs")
  
  dir.create(file.path(wrt.dir1, "sims_default"))
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext ,"_sim_default.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_default", tu))

  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_default.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_default", tu))
  
  ### When crop is maize the index needs to be 1
  ### otherwise 2
  crop.index <- ifelse(crop == "maize", 1, 2)
  pp.kl <- inspect_apsim_xml(i, parm = paste0("SoilCrop[", crop.index,"]/KL"))
  pp.xf <- inspect_apsim_xml(i, parm = paste0("SoilCrop[", crop.index,"]/XF"))
  pp.dul <- inspect_apsim_xml(i, parm = ".//Soil/Water/DUL")
  pp.ll <- inspect_apsim_xml(i, parm = paste0("SoilCrop[", crop.index,"]/LL"))

  ## Create the yield data frame for default
  harvest.date <- sim0s[which.max(sim0s[,paste0(crop,"_yield")]),"Date"]
  ## Retrieve yield data
  yld.cat <- read.csv("../lohi_centers.csv")
  yld.cat.s <- subset(yld.cat, year == yr & 
                      field == strsplit(site, "Default")[[1]][1])
  
  yld.dat <- data.frame(Date = as.Date(harvest.date),
                        crop_yield = yld.cat.s[,"Low"] * 1e3)
  names(yld.dat) <- c("Date", paste0(crop, "_yield"))
  
  op.kl <- optim_apsim(file = i, 
                       data = yld.dat,
                       parm.paths = pp.kl,
                       method = "Brent",
                       lower = 0.01, upper = 1.001)
  
  sim1 <- apsim(file = i, value = "report")
  
  dir.create(file.path(wrt.dir1, "sims_low"))
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_low_KL.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_low", tu))

  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_low_KL.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_low", tu))

  ## Restore KL values
  kl.vals <- c(0.08, 0.079, 0.078, 0.077, 0.076, 0.075, 0.073, 0.07, 
               0.068, 0.066, 0.062, 0.058, 0.054, 0.044, 0.036, 0.03)
  
  edit_apsim(file = i,
             node = "Other",
             parm.path = pp.kl,
             value = kl.vals,
             overwrite = TRUE)
  
  ## Try now for XF
  op.xf <- optim_apsim(file = i, 
                       data = yld.dat,
                       parm.paths = pp.xf,
                       method = "Brent",
                       lower = 0.01, 
                       upper = 1.001)
  
  sim2 <- apsim(file = i, value = "report")
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_low_XF.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_low", tu))
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_low_XF.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_low", tu))

  ## Restore XF values
  xf.vals <- rep(1, 16)
  
  edit_apsim(file = i,
             node = "Other",
             parm.path = pp.xf,
             value = xf.vals,
             overwrite = TRUE)
   
  ## What about changing DUL?
  op.dul <- optim_apsim(file = i, 
                       data = yld.dat,
                       parm.paths = pp.dul,
                       method = "Brent",
                       lower = 0.01, 
                       upper = 1.001)
  
  sim3 <- apsim(file = i, value = "report")
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_low_DUL.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_low", tu))
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_low_DUL.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_low", tu))
  
  ## Restore DUL values
  dul.vals <- c(0.286, 0.286, 0.286, 0.286, 0.286, 0.286, 0.286,
            0.284, 0.281, 0.276, 0.27, 0.27, 0.265, 0.246, 0.246, 0.246) 
  
  edit_apsim(file = i,
             node = "Other",
             parm.path = pp.dul,
             value = dul.vals,
             overwrite = TRUE)

  ## For medium
  yld.dat <- data.frame(Date = as.Date(harvest.date),
                        crop_yield = yld.cat.s[,"Mid"] * 1e3)
  names(yld.dat) <- c("Date", paste0(crop, "_yield"))
  
  op.kl <- optim_apsim(file = i, 
                       data = yld.dat,
                       parm.paths = pp.kl,
                       method = "Brent",
                       lower = 0.01, upper = 1.001)
  
  sim4 <- apsim(file = i, value = "report")
  
  dir.create(file.path(wrt.dir1, "sims_medium"))
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_medium_KL.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_medium", tu))
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_medium_KL.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_medium", tu))
  
  ## Restore KL values
  kl.vals <- c(0.08, 0.079, 0.078, 0.077, 0.076, 0.075, 0.073, 0.07, 
               0.068, 0.066, 0.062, 0.058, 0.054, 0.044, 0.036, 0.03)
  
  edit_apsim(file = i,
             node = "Other",
             parm.path = pp.kl,
             value = kl.vals,
             overwrite = TRUE)
  
  ## Try now for XF
  op.xf <- optim_apsim(file = i, 
                       data = yld.dat,
                       parm.paths = pp.xf,
                       method = "Brent",
                       lower = 0.01, 
                       upper = 1.001)
  
  sim5 <- apsim(file = i, value = "report")
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_medium_XF.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_medium", tu))
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_medium_XF.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_medium", tu))
  
  ## Restore XF values
  xf.vals <- rep(1, 16)
  
  edit_apsim(file = i,
             node = "Other",
             parm.path = pp.xf,
             value = xf.vals,
             overwrite = TRUE)
  
  ## What about changing DUL?
  op.dul <- optim_apsim(file = i, 
                        data = yld.dat,
                        parm.paths = pp.dul,
                        method = "Brent",
                        lower = 0.01, 
                        upper = 1.001)
  
  sim6 <- apsim(file = i, value = "report")
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_medium_DUL.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_medium", tu))
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_medium_DUL.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_medium", tu))
  
  ## Restore DUL values
  dul.vals <- c(0.286, 0.286, 0.286, 0.286, 0.286, 0.286, 0.286,
                0.284, 0.281, 0.276, 0.27, 0.27, 0.265, 0.246, 0.246, 0.246) 
  
  edit_apsim(file = i,
             node = "Other",
             parm.path = pp.dul,
             value = dul.vals,
             overwrite = TRUE)
  
  ## In order to produce higher yields, we need to edit the xml file
  json.mngr <- jsonlite::fromJSON(paste0("../mgmt_jsons/", site.met, "_", file_ext, "_", yr, ".json"))
  cltvr <- json.mngr$cultivar
  if(crop == "soybean"){
    pph <- inspect_apsim_xml(file = "SoybeanM1.xml", parm = paste0(cltvr, "/y_hi_incr"))
    cxmlf <- "SoybeanM1.xml"
  }else{
    pph <- inspect_apsim_xml(file = "MaizeM1.xml", parm = paste0(cltvr, "/GNmaxCoef"))
    cxmlf <- "MaizeM1.xml"
  }
  
  dir.create(file.path(wrt.dir1, "sims_high"))

  yld.dat <- data.frame(Date = as.Date(harvest.date),
                        crop_yield = yld.cat.s[,"High"] * 1e3)
  names(yld.dat) <- c("Date", paste0(crop, "_yield"))
  
  op.HI <- optim_apsim(file = i, 
                       crop.file = cxmlf,
                       data = yld.dat,
                       parm.paths = pph,
                       method = "Brent",
                       lower = 0.5, 
                       upper = 10)
  
  sim4 <- apsim(file = i, value = "report")

  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.out")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_high_HI.out")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_high", tu))
  
  frm <- paste0("name_", site, "_mukey_", mukey, "_rot_", file_ext, "_sim.sum")
  tu <- paste0("name_", site, "_mukey_", mukey, "_", crop, "_", yr, "_rot_", file_ext, "_sim_high_HI.sum")
  file.copy(from = frm,
            to = file.path(wrt.dir1, "sims_high", tu))

  j <- j + 1
}






