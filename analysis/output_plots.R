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

#plotting vars in output nc file (UPDATE - same as lake.csv)
#nc <- ncdf4::nc_open(nc_file)
#names(nc$var)

#baseline
ln <- get_var(nc_file, "lake_number", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
taub <- get_var(nc_file, "taub", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
uorb <-  get_var(nc_file, "uorb", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
umean <- get_var(nc_file, "umean", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
extc <- get_var(nc_file, "extc", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
radn <- get_var(nc_file, "radn", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
dens <- get_var(nc_file, "dens", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
temp <- get_var(nc_file, "temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
v <- get_var(nc_file, "V", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
h <- get_var(nc_file, "H", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
z_l <- get_var(nc_file, "z_L", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
che <- get_var(nc_file, "CHE", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
cd <- get_var(nc_file, "CD", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_dT_dz <- get_var(nc_file, "max_dT_dz", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_height <- get_var(nc_file, "surface_wave_height", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_length <- get_var(nc_file, "surface_wave_length", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_period <- get_var(nc_file, "surface_wave_period", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
benthic_light <-  get_var(nc_file, "benthic_light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
light <-  get_var(nc_file, "light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qlw <- get_var(nc_file, "daily_qlw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qh <- get_var(nc_file, "daily_qh", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qe <- get_var(nc_file, "daily_qe", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qsw <- get_var(nc_file, "daily_qsw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp <- get_var(nc_file, "surface_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
min_temp <- get_var(nc_file, "min_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_temp <- get_var(nc_file, "max_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
albedo <- get_var(nc_file, "albedo", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_dens <- get_var(nc_file, "snow_density", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_level <- get_var(nc_file, "lake_level", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snowfall <-get_var(nc_file, "snowfall", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
runoff <- get_var(nc_file, "local_runoff", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
rain <- get_var(nc_file, "rain", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap <- get_var(nc_file, "evaporation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
overflow <- get_var(nc_file, "overflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_outflow <- get_var(nc_file, "tot_outflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_inflow <- get_var(nc_file, "tot_inflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice <-  get_var(nc_file, "vol_white_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice <-  get_var(nc_file, "vol_blue_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
vol_snow <-  get_var(nc_file, "vol_snow", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sa <-  get_var(nc_file, "surface_area", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_vol <-  get_var(nc_file, "lake_volume", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
wind <-  get_var(nc_file, "wind", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
solar <-  get_var(nc_file, "solar", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_mass_flux <-  get_var(nc_file, "evap_mass_flux", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
precip <-  get_var(nc_file, "precipitation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp <-  get_var(nc_file, "avg_surf_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_layer <-  get_var(nc_file, "surface_layer", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_thick <-  get_var(nc_file, "white_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_thick <-  get_var(nc_file, "blue_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_thick <-  get_var(nc_file, "snow_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
  
#plus 1C
ln_plus1C <- get_var(nc_file, "lake_number", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
taub_plus1C <- get_var(nc_file, "taub", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
uorb_plus1C <-  get_var(nc_file, "uorb", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
umean_plus1C <- get_var(nc_file, "umean", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
extc_plus1C <- get_var(nc_file, "extc", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
radn_plus1C <- get_var(nc_file, "radn", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
dens_plus1C <- get_var(nc_file, "dens", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
temp_plus1C <- get_var(nc_file, "temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
v_plus1C <- get_var(nc_file, "V", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
h_plus1C <- get_var(nc_file, "H", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
z_l_plus1C <- get_var(nc_file, "z_L", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
che_plus1C <- get_var(nc_file, "CHE", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
cd_plus1C <- get_var(nc_file, "CD", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_dT_dz_plus1C <- get_var(nc_file, "max_dT_dz", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_height_plus1C <- get_var(nc_file, "surface_wave_height", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_length_plus1C <- get_var(nc_file, "surface_wave_length", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_period_plus1C <- get_var(nc_file, "surface_wave_period", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
benthic_light_plus1C <-  get_var(nc_file, "benthic_light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
light_plus1C <-  get_var(nc_file, "light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qlw_plus1C <- get_var(nc_file, "daily_qlw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qh_plus1C <- get_var(nc_file, "daily_qh", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qe_plus1C <- get_var(nc_file, "daily_qe", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qsw_plus1C <- get_var(nc_file, "daily_qsw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus1C <- get_var(nc_file, "surface_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
min_temp_plus1C <- get_var(nc_file, "min_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_temp_plus1C <- get_var(nc_file, "max_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
albedo_plus1C <- get_var(nc_file, "albedo", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_dens_plus1C <- get_var(nc_file, "snow_density", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_level_plus1C <- get_var(nc_file, "lake_level", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snowfall_plus1C <-get_var(nc_file, "snowfall", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
runoff_plus1C <- get_var(nc_file, "local_runoff", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
rain_plus1C <- get_var(nc_file, "rain", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_plus1C <- get_var(nc_file, "evaporation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
overflow_plus1C <- get_var(nc_file, "overflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_outflow_plus1C <- get_var(nc_file, "tot_outflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_inflow_plus1C <- get_var(nc_file, "tot_inflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_plus1C <-  get_var(nc_file, "vol_white_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_plus1C <-  get_var(nc_file, "vol_blue_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
vol_snow_plus1C <-  get_var(nc_file, "vol_snow", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sa_plus1C <-  get_var(nc_file, "surface_area", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_vol_plus1C <-  get_var(nc_file, "lake_volume", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
wind_plus1C <-  get_var(nc_file, "wind", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
solar_plus1C <-  get_var(nc_file, "solar", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_mass_flux_plus1C <-  get_var(nc_file, "evap_mass_flux", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
precip_plus1C <-  get_var(nc_file, "precipitation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus1C <-  get_var(nc_file, "avg_surf_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_layer_plus1C <-  get_var(nc_file, "surface_layer", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_thick_plus1C <-  get_var(nc_file, "white_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_thick_plus1C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_thick_plus1C <-  get_var(nc_file, "snow_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)

#plus 2C
ln_plus2C <- get_var(nc_file, "lake_number", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
taub_plus2C <- get_var(nc_file, "taub", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
uorb_plus2C <-  get_var(nc_file, "uorb", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
umean_plus2C <- get_var(nc_file, "umean", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
extc_plus2C <- get_var(nc_file, "extc", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
radn_plus2C <- get_var(nc_file, "radn", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
dens_plus2C <- get_var(nc_file, "dens", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
temp_plus2C <- get_var(nc_file, "temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
v_plus2C <- get_var(nc_file, "V", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
h_plus2C <- get_var(nc_file, "H", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
z_l_plus2C <- get_var(nc_file, "z_L", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
che_plus2C <- get_var(nc_file, "CHE", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
cd_plus2C <- get_var(nc_file, "CD", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_dT_dz_plus2C <- get_var(nc_file, "max_dT_dz", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_height_plus2C <- get_var(nc_file, "surface_wave_height", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_length_plus2C <- get_var(nc_file, "surface_wave_length", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_period_plus2C <- get_var(nc_file, "surface_wave_period", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
benthic_light_plus2C <-  get_var(nc_file, "benthic_light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
light_plus2C <-  get_var(nc_file, "light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qlw_plus2C <- get_var(nc_file, "daily_qlw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qh_plus2C <- get_var(nc_file, "daily_qh", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qe_plus2C <- get_var(nc_file, "daily_qe", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qsw_plus2C <- get_var(nc_file, "daily_qsw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus2C <- get_var(nc_file, "surface_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
min_temp_plus2C <- get_var(nc_file, "min_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_temp_plus2C <- get_var(nc_file, "max_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
albedo_plus2C <- get_var(nc_file, "albedo", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_dens_plus2C <- get_var(nc_file, "snow_density", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_level_plus2C <- get_var(nc_file, "lake_level", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snowfall_plus2C <-get_var(nc_file, "snowfall", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
runoff_plus2C <- get_var(nc_file, "local_runoff", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
rain_plus2C <- get_var(nc_file, "rain", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_plus2C <- get_var(nc_file, "evaporation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
overflow_plus2C <- get_var(nc_file, "overflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_outflow_plus2C <- get_var(nc_file, "tot_outflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_inflow_plus2C <- get_var(nc_file, "tot_inflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_plus2C <-  get_var(nc_file, "vol_white_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_plus2C <-  get_var(nc_file, "vol_blue_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
vol_snow_plus2C <-  get_var(nc_file, "vol_snow", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sa_plus2C <-  get_var(nc_file, "surface_area", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_vol_plus2C <-  get_var(nc_file, "lake_volume", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
wind_plus2C <-  get_var(nc_file, "wind", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
solar_plus2C <-  get_var(nc_file, "solar", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_mass_flux_plus2C <-  get_var(nc_file, "evap_mass_flux", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
precip_plus2C <-  get_var(nc_file, "precipitation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus2C <-  get_var(nc_file, "avg_surf_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_layer_plus2C <-  get_var(nc_file, "surface_layer", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_thick_plus2C <-  get_var(nc_file, "white_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_thick_plus2C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_thick_plus2C <-  get_var(nc_file, "snow_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)

#plus 3C
ln_plus3C <- get_var(nc_file, "lake_number", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
taub_plus3C <- get_var(nc_file, "taub", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
uorb_plus3C <-  get_var(nc_file, "uorb", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
umean_plus3C <- get_var(nc_file, "umean", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
extc_plus3C <- get_var(nc_file, "extc", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
radn_plus3C <- get_var(nc_file, "radn", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
dens_plus3C <- get_var(nc_file, "dens", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
temp_plus3C <- get_var(nc_file, "temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
v_plus3C <- get_var(nc_file, "V", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
h_plus3C <- get_var(nc_file, "H", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
z_l_plus3C <- get_var(nc_file, "z_L", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
che_plus3C <- get_var(nc_file, "CHE", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
cd_plus3C <- get_var(nc_file, "CD", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_dT_dz_plus3C <- get_var(nc_file, "max_dT_dz", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_height_plus3C <- get_var(nc_file, "surface_wave_height", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_length_plus3C <- get_var(nc_file, "surface_wave_length", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_period_plus3C <- get_var(nc_file, "surface_wave_period", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
benthic_light_plus3C <-  get_var(nc_file, "benthic_light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
light_plus3C <-  get_var(nc_file, "light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qlw_plus3C <- get_var(nc_file, "daily_qlw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qh_plus3C <- get_var(nc_file, "daily_qh", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qe_plus3C <- get_var(nc_file, "daily_qe", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qsw_plus3C <- get_var(nc_file, "daily_qsw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus3C <- get_var(nc_file, "surface_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
min_temp_plus3C <- get_var(nc_file, "min_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_temp_plus3C <- get_var(nc_file, "max_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
albedo_plus3C <- get_var(nc_file, "albedo", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_dens_plus3C <- get_var(nc_file, "snow_density", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_level_plus3C <- get_var(nc_file, "lake_level", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snowfall_plus3C <-get_var(nc_file, "snowfall", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
runoff_plus3C <- get_var(nc_file, "local_runoff", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
rain_plus3C <- get_var(nc_file, "rain", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_plus3C <- get_var(nc_file, "evaporation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
overflow_plus3C <- get_var(nc_file, "overflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_outflow_plus3C <- get_var(nc_file, "tot_outflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_inflow_plus3C <- get_var(nc_file, "tot_inflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_plus3C <-  get_var(nc_file, "vol_white_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_plus3C <-  get_var(nc_file, "vol_blue_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
vol_snow_plus3C <-  get_var(nc_file, "vol_snow", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sa_plus3C <-  get_var(nc_file, "surface_area", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_vol_plus3C <-  get_var(nc_file, "lake_volume", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
wind_plus3C <-  get_var(nc_file, "wind", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
solar_plus3C <-  get_var(nc_file, "solar", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_mass_flux_plus3C <-  get_var(nc_file, "evap_mass_flux", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
precip_plus3C <-  get_var(nc_file, "precipitation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus3C <-  get_var(nc_file, "avg_surf_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_layer_plus3C <-  get_var(nc_file, "surface_layer", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_thick_plus3C <-  get_var(nc_file, "white_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_thick_plus3C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_thick_plus3C <-  get_var(nc_file, "snow_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)

#plus 5C
ln_plus5C <- get_var(nc_file, "lake_number", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
taub_plus5C <- get_var(nc_file, "taub", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
uorb_plus5C <-  get_var(nc_file, "uorb", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
umean_plus5C <- get_var(nc_file, "umean", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
extc_plus5C <- get_var(nc_file, "extc", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
radn_plus5C <- get_var(nc_file, "radn", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
dens_plus5C <- get_var(nc_file, "dens", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
temp_plus5C <- get_var(nc_file, "temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
v_plus5C <- get_var(nc_file, "V", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
h_plus5C <- get_var(nc_file, "H", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
z_l_plus5C <- get_var(nc_file, "z_L", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
che_plus5C <- get_var(nc_file, "CHE", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
cd_plus5C <- get_var(nc_file, "CD", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_dT_dz_plus5C <- get_var(nc_file, "max_dT_dz", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_height_plus5C <- get_var(nc_file, "surface_wave_height", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_length_plus5C <- get_var(nc_file, "surface_wave_length", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sw_period_plus5C <- get_var(nc_file, "surface_wave_period", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
benthic_light_plus5C <-  get_var(nc_file, "benthic_light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
light_plus5C <-  get_var(nc_file, "light", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qlw_plus5C <- get_var(nc_file, "daily_qlw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qh_plus5C <- get_var(nc_file, "daily_qh", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qe_plus5C <- get_var(nc_file, "daily_qe", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
daily_qsw_plus5C <- get_var(nc_file, "daily_qsw", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus5C <- get_var(nc_file, "surface_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
min_temp_plus5C <- get_var(nc_file, "min_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
max_temp_plus5C <- get_var(nc_file, "max_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
albedo_plus5C <- get_var(nc_file, "albedo", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_dens_plus5C <- get_var(nc_file, "snow_density", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_level_plus5C <- get_var(nc_file, "lake_level", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snowfall_plus5C <-get_var(nc_file, "snowfall", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
runoff_plus5C <- get_var(nc_file, "local_runoff", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
rain_plus5C <- get_var(nc_file, "rain", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_plus5C <- get_var(nc_file, "evaporation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
overflow_plus5C <- get_var(nc_file, "overflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_outflow_plus5C <- get_var(nc_file, "tot_outflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
tot_inflow_plus5C <- get_var(nc_file, "tot_inflow_vol", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_plus5C <-  get_var(nc_file, "vol_white_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_plus5C <-  get_var(nc_file, "vol_blue_ice", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
vol_snow_plus5C <-  get_var(nc_file, "vol_snow", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
sa_plus5C <-  get_var(nc_file, "surface_area", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
lake_vol_plus5C <-  get_var(nc_file, "lake_volume", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
wind_plus5C <-  get_var(nc_file, "wind", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
solar_plus5C <-  get_var(nc_file, "solar", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
evap_mass_flux_plus5C <-  get_var(nc_file, "evap_mass_flux", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
precip_plus5C <-  get_var(nc_file, "precipitation", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_temp_plus5C <-  get_var(nc_file, "avg_surf_temp", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
surf_layer_plus5C <-  get_var(nc_file, "surface_layer", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
white_ice_thick_plus5C <-  get_var(nc_file, "white_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
blue_ice_thick_plus5C <-  get_var(nc_file, "blue_ice_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)
snow_thick_plus5C <-  get_var(nc_file, "snow_thickness", reference="surface")|> 
  filter(hour(DateTime) %in% 00)

#plots
plot(ln$DateTime, ln$lake_number, type = "l", ylim = c(0, 10000))
points(ln_plus1C$DateTime, ln_plus1C$lake_number, col="#5B8E7D", type="l")
points(ln_plus2C$DateTime, ln_plus2C$lake_number, col="#F4E285", type="l")
points(ln_plus3C$DateTime, ln_plus3C$lake_number, col="#F4A259", type="l")
points(ln_plus5C$DateTime, ln_plus5C$lake_number, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(sw_period$DateTime, sw_period$surface_wave_period, type = "l")
points(sw_period_plus1C$DateTime, sw_period_plus1C$surface_wave_period, col="#5B8E7D", type="l")
points(sw_period_plus2C$DateTime, sw_period_plus2C$surface_wave_period, col="#F4E285", type="l")
points(sw_period_plus3C$DateTime, sw_period_plus3C$surface_wave_period, col="#F4A259", type="l")
points(sw_period_plus5C$DateTime, sw_period_plus5C$surface_wave_period, col="#BC4B51", type="l")
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

plot(snow_dens$DateTime, snow_dens$snow_density, type = "l")
points(snow_dens_plus1C$DateTime, snow_dens_plus1C$snow_density, col="#5B8E7D", type="l")
points(snow_dens_plus2C$DateTime, snow_dens_plus2C$snow_density, col="#F4E285", type="l")
points(snow_dens_plus3C$DateTime, snow_dens_plus3C$snow_density, col="#F4A259", type="l")
points(snow_dens_plus5C$DateTime, snow_dens_plus5C$snow_density, col="#BC4B51", type="l")
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

plot(uorb$DateTime, uorb$uorb_0, type = "l")
points(uorb_plus1C$DateTime, uorb_plus1C$uorb_0, col="#5B8E7D", type="l")
points(uorb_plus2C$DateTime, uorb_plus2C$uorb_0, col="#F4E285", type="l")
points(uorb_plus3C$DateTime, uorb_plus3C$uorb_0, col="#F4A259", type="l")
points(uorb_plus5C$DateTime, uorb_plus5C$uorb_0, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)

plot(light$DateTime, light$light, type = "l")
points(light_plus1C$DateTime, light_plus1C$light, col="#5B8E7D", type="l")
points(light_plus2C$DateTime, light_plus2C$light, col="#F4E285", type="l")
points(light_plus3C$DateTime, light_plus3C$light, col="#F4A259", type="l")
points(light_plus5C$DateTime, light_plus5C$light, col="#BC4B51", type="l")
legend("topleft", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)


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
plot(met$time, met$Rain, type = "l")
points(met_1C$time, met_1C$Rain, col="#5B8E7D", type="l")
points(met_2C$time, met_2C$Rain, col="#F4E285", type="l")
points(met_3C$time, met_3C$Rain, col="#F4A259", type="l")
points(met_5C$time, met_5C$Rain, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
