#run baseline scenario
GLM3r::run_glm(sim_folder = "./sims/baseline")

# set nml file
nc_file <- file.path('sims/baseline/output/output.nc') 

# install glmtools
library(devtools)
devtools::install_github("rqthomas/glmtools", force = TRUE)

# access and plot temperature
current_temp <- glmtools::get_var(nc_file, var_name = "temp")
glmtools::plot_temp(nc_file, reference = "surface")

# assign names to scenarios
scenario_folder_names <- c("plus1","plus2","plus3","plus5")

# create a folder for each scenarios and populate with sim files
glm_files = list.files("./sims/baseline", full.names = TRUE)[1:3]

for (j in 1:length(scenario_folder_names)){
  subdirName <- paste0("./sims/",scenario_folder_names[j])
  folder<-dir.create(subdirName)
  file.copy(from = glm_files, to = subdirName, recursive = TRUE)
  outputdirName <- paste0(subdirName,"/output")
  output_folder<-dir.create(outputdirName)
}

# add corresponding degrees C to met Temp_C column for each scenario
# set temperature increments 
temp_increments <- c(1,2,3,5)

for (j in 1:length(scenario_folder_names)){
  
  # get met data filepath and read in met data
  met_filepath <- paste0("./sims/",scenario_folder_names[j],"/inputs/met.csv")
  met <- read.csv(met_filepath)
  
  # HERE THE SCENARIO IS APPLIED - THIS WILL NEED TO BE EDITED!
  met$AirTemp <- met$AirTemp + temp_increments[j]
  #met$time <-as.POSIXct(strptime(met$time, "%Y-%m-%d %H:%M:%S", tz="EST")) 
  
  # write to file
  new_met_filepath <- paste0("./sims/",scenario_folder_names[j],"/inputs/met_",scenario_folder_names[j],".csv")
  write.csv(met, new_met_filepath, row.names = FALSE, quote = FALSE)
  
  # set nml to use scenario met data
  scenario_nml_file <- file.path(paste0("./sims/",scenario_folder_names[j],"/glm3.nml"))
  scenario_nml <- glmtools::read_nml(nml_file = scenario_nml_file)
  scenario_nml <- glmtools::set_nml(scenario_nml, arg_name = "meteo_fl", arg_val = paste0("inputs/met_",scenario_folder_names[j],".csv"))
  glmtools::write_nml(scenario_nml, file = scenario_nml_file)
}


# run and plot each scenario

for (j in 1:length(scenario_folder_names)){
  
  # run the model
  sim_folder = paste0("./sims/",scenario_folder_names[j])
  GLM3r::run_glm(sim_folder)
  
  # set nml file
  nc_file <- file.path(paste0("sims/",scenario_folder_names[j],"/output/output.nc")) 
  
  # access and plot temperature
  current_temp <- glmtools::get_var(nc_file, var_name = "temp")
  p <- glmtools::plot_var(nc_file, var_name = "temp", reference = "surface", 
                          plot.title = scenario_folder_names[j])
  plot_filename <- paste0("./figures/waterTemp_",scenario_folder_names[j],".png")
  ggplot2::ggsave(p, filename = plot_filename, device = "png",
                  height = 6, width = 8, units = "in")
  
}
