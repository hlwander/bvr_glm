# lake output file plots for different scenarios

# Load packages, set sim folder, load nml file ####
pacman::p_load(GLMr,glmtools,tidyverse,lubridate,ncdf4)

#setwd("../BVR-GLM") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
sim_folder <- getwd()

#"norm","plus1C","plus2C","plus3C","plus5C"
scenario <- "norm" 

#pull pars + files associated with the correct scenario
if(scenario %in% "plus5C"){
  file.copy('4Sep24_tempcal_glm3_plus5C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus0.5C"){
  file.copy('4Sep24_tempcal_glm3_plus0.5C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus1C"){
  file.copy('4Sep24_tempcal_glm3_plus1C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus1.5C"){
  file.copy('4Sep24_tempcal_glm3_plus1.5C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus2C"){
  file.copy('4Sep24_tempcal_glm3_plus2C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus2.5C"){
  file.copy('4Sep24_tempcal_glm3_plus2.5C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus3C"){
  file.copy('4Sep24_tempcal_glm3_plus3C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus4C"){
  file.copy('4Sep24_tempcal_glm3_plus4C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus10C"){
  file.copy('4Sep24_tempcal_glm3_plus10C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus6C"){
  file.copy('22Aug24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
} else(
  file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)  
)

file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed2_phyto_pars_12Jul2024.csv', # 16sep = rqt pars, 12jul = hlw pars
          'aed/aed_phyto_pars.csv', overwrite = TRUE)

if(scenario %in% c("plus5C", "plus10C", "norm","plus2C", 
                   "plus0.5C", "plus1C", "plus3C",
                   "plus1.5C", "plus2.5C", "plus4C")){
  file.copy('aed/aed_zoop_pars_3groups_4Sep2024.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)  
} else if(scenario %in% c("topt1", "plus6C")){
  file.copy('aed/aed_zoop_pars_3groups_20Aug2024_topt1.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)  
} else if(scenario %in% "topt1_plus5") {
  file.copy('aed/aed_zoop_pars_3groups_20Aug2024_topt1_plus5.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)  
} else if(scenario %in% "topt2") {
  file.copy('aed/aed_zoop_pars_3groups_20Aug2024_topt2.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)  
} else if(scenario %in% "topt2_plus5") {
  file.copy('aed/aed_zoop_pars_3groups_20Aug2024_topt2_plus5.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)  
} else if (scenario %in% "topt3") {
  file.copy('aed/aed_zoop_pars_3groups_20Aug2024_topt3.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)
} else (
  file.copy('aed/aed_zoop_pars_3groups_20Aug2024_topt3_plus5.csv', 
            'aed/aed_zoop_pars.csv', overwrite = TRUE)
)

#run the model!
system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
        stdout = TRUE, stderr = TRUE, 
        env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
                     "/glm.app/Contents/MacOS"))

#define output file
nc_file <- file.path(sim_folder, 'output/output.nc')


norm_out <- read.csv("output/lake.csv") |> mutate(DateTime = as.Date(time))
plus1C_out <- read.csv("output/lake.csv") |> mutate(DateTime = as.Date(time))
plus2C_out <- read.csv("output/lake.csv") |> mutate(DateTime = as.Date(time))
plus3C_out <- read.csv("output/lake.csv") |> mutate(DateTime = as.Date(time))
plus5C_out <- read.csv("output/lake.csv") |> mutate(DateTime = as.Date(time))

#plot various vars
jpeg('figures/lakenum_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$LakeNumber, type = "l", ylim = c(0,10000))
points(plus1C_out$DateTime, plus1C_out$LakeNumber, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$LakeNumber, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$LakeNumber, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$LakeNumber, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off() #way more mixing in +2 and 5C scenarios...

jpeg('figures/wave_period_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Surface.Wave.Period, type = "l")
points(plus1C_out$DateTime, plus1C_out$Surface.Wave.Period, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Surface.Wave.Period, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Surface.Wave.Period, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Surface.Wave.Period, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/surf_temp_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Surface.Temp, type = "l")
points(plus1C_out$DateTime, plus1C_out$Surface.Temp, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Surface.Temp, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Surface.Temp, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Surface.Temp, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/min_temp_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Min.Temp, type = "l")
points(plus1C_out$DateTime, plus1C_out$Min.Temp, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Min.Temp, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Min.Temp, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Min.Temp, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/albedo_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Albedo, type = "l")
points(plus1C_out$DateTime, plus1C_out$Albedo, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Albedo, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Albedo, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Albedo, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/blueice_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Blue.Ice.Thickness, type = "l", ylim = c(0,2))
points(plus1C_out$DateTime, plus1C_out$Blue.Ice.Thickness, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Blue.Ice.Thickness, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Blue.Ice.Thickness, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Blue.Ice.Thickness, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/snow_dens_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Snow.Density, type = "l")
points(plus1C_out$DateTime, plus1C_out$Snow.Density, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Snow.Density, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Snow.Density, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Snow.Density, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/sa_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Surface.Area, type = "l", ylim = c(250000,400000))
points(plus1C_out$DateTime, plus1C_out$Surface.Area, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Surface.Area, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Surface.Area, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Surface.Area, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n')
dev.off()

jpeg('figures/depth_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Lake.Level, type = "l", ylim = c(9, 14))
points(plus1C_out$DateTime, plus1C_out$Lake.Level, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Lake.Level, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Lake.Level, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Lake.Level, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/rain_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Rain, type = "l", ylim = c(0, 100000))
points(plus1C_out$DateTime, plus1C_out$Rain, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Rain, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Rain, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Rain, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/evap_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Evaporation, type = "l")
points(plus1C_out$DateTime, plus1C_out$Evaporation, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Evaporation, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Evaporation, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Evaporation, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/overflow_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(norm_out$DateTime, norm_out$Overflow.Vol, type = "l")
points(plus1C_out$DateTime, plus1C_out$Overflow.Vol, col="#5B8E7D", type="l")
points(plus2C_out$DateTime, plus2C_out$Overflow.Vol, col="#F4E285", type="l")
points(plus3C_out$DateTime, plus3C_out$Overflow.Vol, col="#F4A259", type="l")
points(plus5C_out$DateTime, plus5C_out$Overflow.Vol, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

#confirm met precip/other vars do not change!!
#read in met files
met <- read.csv("inputs/met.csv") |> 
  mutate(time = as.Date(time)) |> 
  group_by(time) |> 
  summarise(across(AirTemp:Snow, ~mean(.x, na.rm=T)))
met_1C <- read.csv("inputs/met_plus1C.csv") |> 
  mutate(time = as.Date(time)) |> 
  group_by(time) |> 
  summarise(across(AirTemp:Snow, ~mean(.x, na.rm=T)))
met_2C <- read.csv("inputs/met_plus2C.csv") |> 
  mutate(time = as.Date(time)) |> 
  group_by(time) |> 
  summarise(across(AirTemp:Snow, ~mean(.x, na.rm=T)))
met_3C <- read.csv("inputs/met_plus3C.csv") |> 
  mutate(time = as.Date(time)) |> 
  group_by(time) |> 
  summarise(across(AirTemp:Snow, ~mean(.x, na.rm=T)))
met_5C <- read.csv("inputs/met_plus5C.csv") |> 
  mutate(time = as.Date(time)) |> 
  group_by(time) |> 
  summarise(across(AirTemp:Snow, ~mean(.x, na.rm=T)))

#plots - all met drivers are the same for each scenario besides air temp
plot(met$time, met$Rain, type = "l")
points(met_1C$time, met_1C$Rain, col="#5B8E7D", type="l")
points(met_2C$time, met_2C$Rain, col="#F4E285", type="l")
points(met_3C$time, met_3C$Rain, col="#F4A259", type="l")
points(met_5C$time, met_5C$Rain, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
