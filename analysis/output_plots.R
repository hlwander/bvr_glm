# lake.csv output file plots for different scenarios

# Load packages, set sim folder, load nml file ####
pacman::p_load(tidyverse,lubridate,ncdf4,glmtools)

#"norm","plus1C","plus2C","plus3C","plus5C"
scenario <- "plus3C" 

#pull pars + files associated with the correct scenario
if(scenario %in% "plus5C"){
  file.copy('4Sep24_tempcal_glm3_plus5C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus1C"){
  file.copy('4Sep24_tempcal_glm3_plus1C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus2C"){
  file.copy('4Sep24_tempcal_glm3_plus2C.nml', 'glm3.nml', overwrite = TRUE)
} else if(scenario %in% "plus3C"){
  file.copy('4Sep24_tempcal_glm3_plus3C.nml', 'glm3.nml', overwrite = TRUE)
} else(
  file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)  
)

file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed2_phyto_pars_12Jul2024.csv', 
          'aed/aed_phyto_pars.csv', overwrite = TRUE)
file.copy('aed/aed_zoop_pars_3groups_4Sep2024.csv', 
          'aed/aed_zoop_pars.csv', overwrite = TRUE)

#run the model!
GLM3r::run_glm()

#define output file
nc_file <- file.path('./output/output.nc')

#plotting vars in output nc file (UPDATE - same as lake.csv)
#nc <- ncdf4::nc_open(nc_file)
#names(nc$var)

#baseline
ln <- get_var(nc_file, "lake_number", reference="surface")
temp <- get_var(nc_file, "temp", reference="surface")
surf_temp <- get_var(nc_file, "surface_temp", reference="surface")
min_temp <- get_var(nc_file, "min_temp", reference="surface")
max_temp <- get_var(nc_file, "max_temp", reference="surface")
albedo <- get_var(nc_file, "albedo", reference="surface")
snow_dens <- get_var(nc_file, "snow_density", reference="surface")
lake_level <- get_var(nc_file, "lake_level", reference="surface")
rain <- get_var(nc_file, "rain", reference="surface")
evap <- get_var(nc_file, "evaporation", reference="surface")
sa <-  get_var(nc_file, "surface_area", reference="surface")
lake_vol <-  get_var(nc_file, "lake_volume", reference="surface")
wind <-  get_var(nc_file, "wind", reference="surface")
solar <-  get_var(nc_file, "solar", reference="surface")
precip <-  get_var(nc_file, "precipitation", reference="surface")
white_ice_thick <-  get_var(nc_file, "white_ice_thickness", reference="surface")
blue_ice_thick <-  get_var(nc_file, "blue_ice_thickness", reference="surface")

#plus 1C
ln_plus1C <- get_var(nc_file, "lake_number", reference="surface")
temp_plus1C <- get_var(nc_file, "temp", reference="surface")
surf_temp_plus1C <- get_var(nc_file, "surface_temp", reference="surface")
min_temp_plus1C <- get_var(nc_file, "min_temp", reference="surface")
max_temp_plus1C <- get_var(nc_file, "max_temp", reference="surface")
albedo_plus1C <- get_var(nc_file, "albedo", reference="surface")
snow_dens_plus1C <- get_var(nc_file, "snow_density", reference="surface")
lake_level_plus1C <- get_var(nc_file, "lake_level", reference="surface")
rain_plus1C <- get_var(nc_file, "rain", reference="surface")
evap_plus1C <- get_var(nc_file, "evaporation", reference="surface")
sa_plus1C <-  get_var(nc_file, "surface_area", reference="surface")
lake_vol_plus1C <-  get_var(nc_file, "lake_volume", reference="surface")
wind_plus1C <-  get_var(nc_file, "wind", reference="surface")
solar_plus1C <-  get_var(nc_file, "solar", reference="surface")
precip_plus1C <-  get_var(nc_file, "precipitation", reference="surface")
white_ice_thick_plus1C <-  get_var(nc_file, "white_ice_thickness", reference="surface")
blue_ice_thick_plus1C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")

#plus 2C
ln_plus2C <- get_var(nc_file, "lake_number", reference="surface")
temp_plus2C <- get_var(nc_file, "temp", reference="surface")
surf_temp_plus2C <- get_var(nc_file, "surface_temp", reference="surface")
min_temp_plus2C <- get_var(nc_file, "min_temp", reference="surface")
max_temp_plus2C <- get_var(nc_file, "max_temp", reference="surface")
albedo_plus2C <- get_var(nc_file, "albedo", reference="surface")
snow_dens_plus2C <- get_var(nc_file, "snow_density", reference="surface")
lake_level_plus2C <- get_var(nc_file, "lake_level", reference="surface")
rain_plus2C <- get_var(nc_file, "rain", reference="surface")
evap_plus2C <- get_var(nc_file, "evaporation", reference="surface")
sa_plus2C <-  get_var(nc_file, "surface_area", reference="surface")
lake_vol_plus2C <-  get_var(nc_file, "lake_volume", reference="surface")
wind_plus2C <-  get_var(nc_file, "wind", reference="surface")
solar_plus2C <-  get_var(nc_file, "solar", reference="surface")
precip_plus2C <-  get_var(nc_file, "precipitation", reference="surface")
white_ice_thick_plus2C <-  get_var(nc_file, "white_ice_thickness", reference="surface")
blue_ice_thick_plus2C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")

#plus 3C
ln_plus3C <- get_var(nc_file, "lake_number", reference="surface")
temp_plus3C <- get_var(nc_file, "temp", reference="surface")
surf_temp_plus3C <- get_var(nc_file, "surface_temp", reference="surface")
min_temp_plus3C <- get_var(nc_file, "min_temp", reference="surface")
max_temp_plus3C <- get_var(nc_file, "max_temp", reference="surface")
albedo_plus3C <- get_var(nc_file, "albedo", reference="surface")
snow_dens_plus3C <- get_var(nc_file, "snow_density", reference="surface")
lake_level_plus3C <- get_var(nc_file, "lake_level", reference="surface")
rain_plus3C <- get_var(nc_file, "rain", reference="surface")
evap_plus3C <- get_var(nc_file, "evaporation", reference="surface")
sa_plus3C <-  get_var(nc_file, "surface_area", reference="surface")
lake_vol_plus3C <-  get_var(nc_file, "lake_volume", reference="surface")
wind_plus3C <-  get_var(nc_file, "wind", reference="surface")
solar_plus3C <-  get_var(nc_file, "solar", reference="surface")
precip_plus3C <-  get_var(nc_file, "precipitation", reference="surface")
white_ice_thick_plus3C <-  get_var(nc_file, "white_ice_thickness", reference="surface")
blue_ice_thick_plus3C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")

#plus 5C
ln_plus5C <- get_var(nc_file, "lake_number", reference="surface")
temp_plus5C <- get_var(nc_file, "temp", reference="surface")
surf_temp_plus5C <- get_var(nc_file, "surface_temp", reference="surface")
min_temp_plus5C <- get_var(nc_file, "min_temp", reference="surface")
max_temp_plus5C <- get_var(nc_file, "max_temp", reference="surface")
albedo_plus5C <- get_var(nc_file, "albedo", reference="surface")
snow_dens_plus5C <- get_var(nc_file, "snow_density", reference="surface")
lake_level_plus5C <- get_var(nc_file, "lake_level", reference="surface")
rain_plus5C <- get_var(nc_file, "rain", reference="surface")
evap_plus5C <- get_var(nc_file, "evaporation", reference="surface")
sa_plus5C <-  get_var(nc_file, "surface_area", reference="surface")
lake_vol_plus5C <-  get_var(nc_file, "lake_volume", reference="surface")
wind_plus5C <-  get_var(nc_file, "wind", reference="surface")
solar_plus5C <-  get_var(nc_file, "solar", reference="surface")
precip_plus5C <-  get_var(nc_file, "precipitation", reference="surface")
white_ice_thick_plus5C <-  get_var(nc_file, "white_ice_thickness", reference="surface")
blue_ice_thick_plus5C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")

#plots
plot(ln$DateTime, ln$lake_number, type = "l", ylim = c(0, 10000))
points(ln_plus1C$DateTime, ln_plus1C$lake_number, col="#5B8E7D", type="l")
points(ln_plus2C$DateTime, ln_plus2C$lake_number, col="#F4E285", type="l")
points(ln_plus3C$DateTime, ln_plus3C$lake_number, col="#F4A259", type="l")
points(ln_plus5C$DateTime, ln_plus5C$lake_number, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(surf_temp$DateTime, surf_temp$avg_surf_temp, type = "l")
points(surf_temp_plus1C$DateTime, surf_temp_plus1C$avg_surf_temp, col="#5B8E7D", type="l")
points(surf_temp_plus2C$DateTime, surf_temp_plus2C$avg_surf_temp, col="#F4E285", type="l")
points(surf_temp_plus3C$DateTime, surf_temp_plus3C$avg_surf_temp, col="#F4A259", type="l")
points(surf_temp_plus5C$DateTime, surf_temp_plus5C$avg_surf_temp, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(albedo$DateTime, albedo$albedo, type = "l")
points(albedo_plus1C$DateTime, albedo_plus1C$albedo, col="#5B8E7D", type="l")
points(albedo_plus2C$DateTime, albedo_plus2C$albedo, col="#F4E285", type="l")
points(albedo_plus3C$DateTime, albedo_plus3C$albedo, col="#F4A259", type="l")
points(albedo_plus5C$DateTime, albedo_plus5C$albedo, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(blue_ice_thick$DateTime, blue_ice_thick$blue_ice_thickness, type = "l", ylim = c(0,2))
points(blue_ice_thick_plus1C$DateTime, blue_ice_thick_plus1C$blue_ice_thickness, col="#5B8E7D", type="l")
points(blue_ice_thick_plus2C$DateTime, blue_ice_thick_plus2C$blue_ice_thickness, col="#F4E285", type="l")
points(blue_ice_thick_plus3C$DateTime, blue_ice_thick_plus3C$blue_ice_thickness, col="#F4A259", type="l")
points(blue_ice_thick_plus5C$DateTime, blue_ice_thick_plus5C$blue_ice_thickness, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(sa$DateTime, sa$surface_area, type = "l")
points(sa_plus1C$DateTime, sa_plus1C$surface_area, col="#5B8E7D", type="l")
points(sa_plus2C$DateTime, sa_plus2C$surface_area, col="#F4E285", type="l")
points(sa_plus3C$DateTime, sa_plus3C$surface_area, col="#F4A259", type="l")
points(sa_plus5C$DateTime, sa_plus5C$surface_area, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(precip$DateTime, precip$precipitation, type = "l")
points(precip_plus1C$DateTime, precip_plus1C$precipitation, col="#5B8E7D", type="l")
points(precip_plus2C$DateTime, precip_plus2C$precipitation, col="#F4E285", type="l")
points(precip_plus3C$DateTime, precip_plus3C$precipitation, col="#F4A259", type="l")
points(precip_plus5C$DateTime, precip_plus5C$precipitation, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(evap$DateTime, evap$evaporation, type = "l")
points(evap_plus1C$DateTime, evap_plus1C$evaporation, col="#5B8E7D", type="l")
points(evap_plus2C$DateTime, evap_plus2C$evaporation, col="#F4E285", type="l")
points(evap_plus3C$DateTime, evap_plus3C$evaporation, col="#F4A259", type="l")
points(evap_plus5C$DateTime, evap_plus5C$evaporation, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(wind$DateTime, wind$wind, type = "l")
points(wind_plus1C$DateTime, wind_plus1C$wind, col="#5B8E7D", type="l")
points(wind_plus2C$DateTime, wind_plus2C$wind, col="#F4E285", type="l")
points(wind_plus3C$DateTime, wind_plus3C$wind, col="#F4A259", type="l")
points(wind_plus5C$DateTime, wind_plus5C$wind, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

#-----------------------------------------------------------------------------#
#plotting vars in lake.csv file
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
plot(met$time, met$AirTemp, type = "l")
points(met_1C$time, met_1C$AirTemp, col="#5B8E7D", type="l")
points(met_2C$time, met_2C$AirTemp, col="#F4E285", type="l")
points(met_3C$time, met_3C$AirTemp, col="#F4A259", type="l")
points(met_5C$time, met_5C$AirTemp, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

