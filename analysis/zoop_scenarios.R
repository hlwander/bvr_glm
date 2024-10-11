# Plankton air temp scenarios
# 22 August 2024

# Instructions: run all code up until plot generation using each scenario
# rerun lines 14-488 to generate each scenario dataframe

#create met file for 1, 2, 3, and 5C warming temp scenario
#met <- read.csv("inputs/met.csv")
#met_1C_warmer <- met |> 
#  mutate(AirTemp = AirTemp + 1)
#write.csv(met_1C_warmer,"inputs/met_plus1C.csv", row.names = F)

#"norm", "plus1C", "plus2C", "plus3C", "plus5C"
scenario <- "plus5C" 

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

#save zoop output
var="ZOO_cladoceran"
clad_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                     reference = 'surface') 
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                     reference = 'surface') |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                   reference = 'surface') |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                     reference = 'surface') |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                   reference = 'surface') |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                     reference = 'surface') |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                   reference = 'surface') |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                     reference = 'surface') |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                   reference = 'surface') |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                     reference = 'surface') |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                   reference = 'surface') |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                     reference = 'surface') |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                   reference = 'surface') |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                     reference = 'surface') |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                   reference = 'surface') |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                     reference = 'surface') |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                   reference = 'surface') |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                     reference = 'surface') |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                   reference = 'surface') |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                     reference = 'surface') |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                    reference = 'surface') |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                      reference = 'surface') |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                    reference = 'surface') |> select(-DateTime)

clads_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                           zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                           zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                           zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                           zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                           zoops_10,zoops_10.5,zoops_11)

#sum all depths
clads_full_wc <- clads_full_wc |> 
  mutate(ZOO_clad = rowSums(across(where(is.numeric)),na.rm=T)) 

clad<- clads_full_wc |>
  select(DateTime, ZOO_clad) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_cladoceran = ZOO_clad)

var="ZOO_copepod"
cope_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |>
  na.omit()

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                     reference = 'surface')
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                     reference = 'surface') |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                   reference = 'surface') |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                     reference = 'surface') |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                   reference = 'surface') |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                     reference = 'surface') |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                   reference = 'surface') |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                     reference = 'surface') |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                   reference = 'surface') |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                     reference = 'surface') |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                   reference = 'surface') |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                     reference = 'surface') |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                   reference = 'surface') |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                     reference = 'surface') |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                   reference = 'surface') |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                     reference = 'surface') |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                   reference = 'surface') |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                     reference = 'surface') |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                   reference = 'surface') |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                     reference = 'surface') |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                    reference = 'surface') |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                      reference = 'surface') |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                    reference = 'surface') |> select(-DateTime)

cope_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                          zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                          zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                          zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                          zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                          zoops_10,zoops_10.5,zoops_11)

#sum all depths
cope_full_wc <- cope_full_wc |> 
  mutate(ZOO_cope = rowSums(across(where(is.numeric)),na.rm=T))

cope<- cope_full_wc |>
  select(DateTime, ZOO_cope) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_copepod = ZOO_cope)


var="ZOO_rotifer"
rot_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) 

zoops_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                     reference = 'surface') 
zoops_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                     reference = 'surface') |> select(-DateTime)
zoops_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                   reference = 'surface') |> select(-DateTime)
zoops_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                     reference = 'surface') |> select(-DateTime)
zoops_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                   reference = 'surface') |> select(-DateTime)
zoops_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                     reference = 'surface') |> select(-DateTime)
zoops_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                   reference = 'surface') |> select(-DateTime)
zoops_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                     reference = 'surface') |> select(-DateTime)
zoops_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                   reference = 'surface') |> select(-DateTime)
zoops_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                     reference = 'surface') |> select(-DateTime)
zoops_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                   reference = 'surface') |> select(-DateTime)
zoops_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                     reference = 'surface') |> select(-DateTime)
zoops_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                   reference = 'surface') |> select(-DateTime)
zoops_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                     reference = 'surface') |> select(-DateTime)
zoops_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                   reference = 'surface') |> select(-DateTime)
zoops_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                     reference = 'surface') |> select(-DateTime)
zoops_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                   reference = 'surface') |> select(-DateTime)
zoops_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                     reference = 'surface') |> select(-DateTime)
zoops_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                   reference = 'surface') |> select(-DateTime)
zoops_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                     reference = 'surface') |> select(-DateTime)
zoops_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                    reference = 'surface') |> select(-DateTime)
zoops_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                      reference = 'surface') |> select(-DateTime)
zoops_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                    reference = 'surface') |> select(-DateTime)

rot_full_wc <- bind_cols(zoops_0.1,zoops_0.5,zoops_1,zoops_1.5,
                         zoops_2,zoops_2.5,zoops_3,zoops_3.5,
                         zoops_4,zoops_4.5,zoops_5,zoops_5.5,
                         zoops_6,zoops_6.5,zoops_7,zoops_7.5,
                         zoops_8,zoops_8.5,zoops_9,zoops_9.5,
                         zoops_10,zoops_10.5,zoops_11)

#sum all depths
rot_full_wc <- rot_full_wc |> 
  mutate(ZOO_rot = rowSums(across(where(is.numeric)),na.rm=T))

rot<- rot_full_wc |>
  select(DateTime, ZOO_rot) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_rotifer = ZOO_rot)

#combine into one df 
all_zoops <- reduce(list(clad, cope, rot), full_join) 

all_zoops_obs <- reduce(list(clad_obs, cope_obs, rot_obs), full_join) |> 
  pivot_longer(cols = -c(DateTime), 
               names_pattern = "(...)_(...*)$",
               names_to = c("mod", "taxon")) |> 
  na.omit() |> 
  filter(value < 500) |> 
  mutate(DateTime = as.Date(DateTime))

#convert from wide to long for plotting
all_zoops_final <- all_zoops |> 
  pivot_longer(cols = -c(DateTime), 
               names_pattern = "(...)_(...*)$",
               names_to = c("mod", "taxon")) |> 
  na.omit()

#now create a dynamic df name
assign(paste0("all_zoops_", scenario), all_zoops_final)

#------------------------------------------------------------------#
# same for phytos
var="PHY_cyano"
phytos_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                      reference = 'surface') 
phytos_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                      reference = 'surface') |> select(-DateTime)
phytos_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                    reference = 'surface') |> select(-DateTime)
phytos_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                      reference = 'surface') |> select(-DateTime)
phytos_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                    reference = 'surface') |> select(-DateTime)
phytos_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                      reference = 'surface') |> select(-DateTime)
phytos_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                    reference = 'surface') |> select(-DateTime)
phytos_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                      reference = 'surface') |> select(-DateTime)
phytos_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                    reference = 'surface') |> select(-DateTime)
phytos_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                      reference = 'surface') |> select(-DateTime)
phytos_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                    reference = 'surface') |> select(-DateTime)
phytos_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                      reference = 'surface') |> select(-DateTime)
phytos_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                    reference = 'surface') |> select(-DateTime)
phytos_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                      reference = 'surface') |> select(-DateTime)
phytos_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                    reference = 'surface') |> select(-DateTime)
phytos_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                      reference = 'surface') |> select(-DateTime)
phytos_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                    reference = 'surface') |> select(-DateTime)
phytos_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                      reference = 'surface') |> select(-DateTime)
phytos_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                    reference = 'surface') |> select(-DateTime)
phytos_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                      reference = 'surface') |> select(-DateTime)
phytos_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                     reference = 'surface') |> select(-DateTime)
phytos_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                       reference = 'surface') |> select(-DateTime)
phytos_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                     reference = 'surface') |> select(-DateTime)

cyano_full_wc <- bind_cols(phytos_0.1,phytos_0.5,phytos_1,phytos_1.5,
                           phytos_2,phytos_2.5,phytos_3,phytos_3.5,
                           phytos_4,phytos_4.5,phytos_5,phytos_5.5,
                           phytos_6,phytos_6.5,phytos_7,phytos_7.5,
                           phytos_8,phytos_8.5,phytos_9,phytos_9.5,
                           phytos_10,phytos_10.5,phytos_11)

#sum all depths
cyano_full_wc <- cyano_full_wc |> 
  mutate(PHY_cyano = rowSums(across(where(is.numeric)),na.rm=T))

cyano<- cyano_full_wc |>
  select(DateTime, PHY_cyano) |> 
  mutate(DateTime = as.Date(DateTime)) 

var="PHY_green"
phytos_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                      reference = 'surface') 
phytos_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                      reference = 'surface') |> select(-DateTime)
phytos_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                    reference = 'surface') |> select(-DateTime)
phytos_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                      reference = 'surface') |> select(-DateTime)
phytos_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                    reference = 'surface') |> select(-DateTime)
phytos_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                      reference = 'surface') |> select(-DateTime)
phytos_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                    reference = 'surface') |> select(-DateTime)
phytos_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                      reference = 'surface') |> select(-DateTime)
phytos_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                    reference = 'surface') |> select(-DateTime)
phytos_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                      reference = 'surface') |> select(-DateTime)
phytos_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                    reference = 'surface') |> select(-DateTime)
phytos_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                      reference = 'surface') |> select(-DateTime)
phytos_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                    reference = 'surface') |> select(-DateTime)
phytos_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                      reference = 'surface') |> select(-DateTime)
phytos_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                    reference = 'surface') |> select(-DateTime)
phytos_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                      reference = 'surface') |> select(-DateTime)
phytos_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                    reference = 'surface') |> select(-DateTime)
phytos_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                      reference = 'surface') |> select(-DateTime)
phytos_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                    reference = 'surface') |> select(-DateTime)
phytos_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                      reference = 'surface') |> select(-DateTime)
phytos_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                     reference = 'surface') |> select(-DateTime)
phytos_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                       reference = 'surface') |> select(-DateTime)
phytos_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                     reference = 'surface') |> select(-DateTime)

green_full_wc <- bind_cols(phytos_0.1,phytos_0.5,phytos_1,phytos_1.5,
                           phytos_2,phytos_2.5,phytos_3,phytos_3.5,
                           phytos_4,phytos_4.5,phytos_5,phytos_5.5,
                           phytos_6,phytos_6.5,phytos_7,phytos_7.5,
                           phytos_8,phytos_8.5,phytos_9,phytos_9.5,
                           phytos_10,phytos_10.5,phytos_11)

#sum all depths
green_full_wc <- green_full_wc |> 
  mutate(PHY_green = rowSums(across(where(is.numeric)),na.rm=T))

green <- green_full_wc |>
  select(DateTime, PHY_green) |> 
  mutate(DateTime = as.Date(DateTime)) 

var="PHY_diatom"
phytos_0.1 <- get_var(file=nc_file,var_name = var,z_out=0.1,
                      reference = 'surface') 
phytos_0.5 <- get_var(file=nc_file,var_name = var,z_out=0.5,
                      reference = 'surface') |> select(-DateTime)
phytos_1 <- get_var(file=nc_file,var_name = var,z_out=1,
                    reference = 'surface') |> select(-DateTime)
phytos_1.5 <- get_var(file=nc_file,var_name = var,z_out=1.5,
                      reference = 'surface') |> select(-DateTime)
phytos_2 <- get_var(file=nc_file,var_name = var,z_out=2,
                    reference = 'surface') |> select(-DateTime)
phytos_2.5 <- get_var(file=nc_file,var_name = var,z_out=2.5,
                      reference = 'surface') |> select(-DateTime)
phytos_3 <- get_var(file=nc_file,var_name = var,z_out=3,
                    reference = 'surface') |> select(-DateTime)
phytos_3.5 <- get_var(file=nc_file,var_name = var,z_out=3.5,
                      reference = 'surface') |> select(-DateTime)
phytos_4 <- get_var(file=nc_file,var_name = var,z_out=4,
                    reference = 'surface') |> select(-DateTime)
phytos_4.5 <- get_var(file=nc_file,var_name = var,z_out=4.5,
                      reference = 'surface') |> select(-DateTime)
phytos_5 <- get_var(file=nc_file,var_name = var,z_out=5,
                    reference = 'surface') |> select(-DateTime)
phytos_5.5 <- get_var(file=nc_file,var_name = var,z_out=5.5,
                      reference = 'surface') |> select(-DateTime)
phytos_6 <- get_var(file=nc_file,var_name = var,z_out=6,
                    reference = 'surface') |> select(-DateTime)
phytos_6.5 <- get_var(file=nc_file,var_name = var,z_out=6.5,
                      reference = 'surface') |> select(-DateTime)
phytos_7 <- get_var(file=nc_file,var_name = var,z_out=7,
                    reference = 'surface') |> select(-DateTime)
phytos_7.5 <- get_var(file=nc_file,var_name = var,z_out=7.5,
                      reference = 'surface') |> select(-DateTime)
phytos_8 <- get_var(file=nc_file,var_name = var,z_out=8,
                    reference = 'surface') |> select(-DateTime)
phytos_8.5 <- get_var(file=nc_file,var_name = var,z_out=8.5,
                      reference = 'surface') |> select(-DateTime)
phytos_9 <- get_var(file=nc_file,var_name = var,z_out=9,
                    reference = 'surface') |> select(-DateTime)
phytos_9.5 <- get_var(file=nc_file,var_name = var,z_out=9.5,
                      reference = 'surface') |> select(-DateTime)
phytos_10 <- get_var(file=nc_file,var_name = var,z_out=10,
                     reference = 'surface') |> select(-DateTime)
phytos_10.5 <- get_var(file=nc_file,var_name = var,z_out=10.5,
                       reference = 'surface') |> select(-DateTime)
phytos_11 <- get_var(file=nc_file,var_name = var,z_out=11,
                     reference = 'surface') |> select(-DateTime)

diatom_full_wc <- bind_cols(phytos_0.1,phytos_0.5,phytos_1,phytos_1.5,
                            phytos_2,phytos_2.5,phytos_3,phytos_3.5,
                            phytos_4,phytos_4.5,phytos_5,phytos_5.5,
                            phytos_6,phytos_6.5,phytos_7,phytos_7.5,
                            phytos_8,phytos_8.5,phytos_9,phytos_9.5,
                            phytos_10,phytos_10.5,phytos_11)

#sum all depths
diatom_full_wc <- diatom_full_wc |> 
  mutate(PHY_diatom = rowSums(across(where(is.numeric)),na.rm=T))

diatom <- diatom_full_wc |>
  select(DateTime, PHY_diatom) |> 
  mutate(DateTime = as.Date(DateTime)) 

#combine into one df 
all_phytos <- reduce(list(cyano, green, diatom), full_join) 

#convert from wide to long for plotting
all_phytos_final <- all_phytos |> 
  pivot_longer(cols = -c(DateTime), 
               names_pattern = "(...)_(...*)$",
               names_to = c("mod", "taxon")) |> 
  na.omit()

#now create a dynamic df name
assign(paste0("all_phytos_", scenario), all_phytos_final)

# And lastly chla 
var="PHY_tchla"
chla_obs <- read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |> 
  na.omit()

#read mod chla
chla_mod<- get_var(nc_file, var, reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  mutate(Depth=as.numeric(Depth)) |> 
  na.omit()

#now create a dynamic df name
assign(paste0("chla_", scenario), chla_mod)

#------------------------------------------------------------------#
# plot zoops
ggplot() +
  geom_line(data=all_zoops_norm,
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=all_zoops_plus1C,
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=all_zoops_plus3C,
            aes(DateTime, value, color = "+3C")) +
  geom_line(data=all_zoops_plus5C,
            aes(DateTime, value, color = "+5C")) +
  geom_point(data=all_zoops_obs,
             aes(DateTime, value, color="observed")) + 
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51","red"),
                     breaks = c("+0C","+1C","+3C","+5C","observed")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.32,0.98),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/zoop_scenario_airtemp_1_3_5.jpg", width=6, height=6)

# plot phytos
ggplot() +
  geom_line(data=all_phytos_norm,
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=all_phytos_plus1C,
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=all_phytos_plus3C,
            aes(DateTime, value, color = "+3C")) +
  geom_line(data=all_phytos_plus5C,
            aes(DateTime, value, color = "+5C")) +
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51"),
                     breaks = c("+0C","+1C","+3C","+5C")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.28,0.98),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/phytos_scenario_airtemp_1_3_5.jpg", width=6, height=6)

# plot chla
ggplot() +
  geom_line(data=subset(chla_norm, Depth %in% 0.1),
            aes(DateTime, PHY_tchla, color = "+0C")) +
  geom_line(data=subset(chla_plus1C, Depth %in% 0.1),
            aes(DateTime, PHY_tchla, color = "+1C")) +
  geom_line(data=subset(chla_plus3C, Depth %in% 0.1),
            aes(DateTime, PHY_tchla, color = "+3C")) +
  geom_line(data=subset(chla_plus5C, Depth %in% 0.1),
            aes(DateTime, PHY_tchla, color = "+5C")) +
  geom_point(data=subset(chla_obs, Depth %in% 0.1),
             aes(DateTime, PHY_tchla, color = "observed")) +
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("#5B8E7D","#F4E285","#F4A259","#BC4B51","red"),
                     breaks = c("+0C","+1C","+3C","+5C","observed")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.32,0.98),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.direction = "horizontal",
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/chla_0.1m_scenario_airtemp.jpg", width=6, height=6)