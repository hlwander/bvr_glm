# Zoop scenarios - Topt and air temp manipulation
# 22 August 2024

# Instructions: run all code up until plot generation using desired scenarios
# If multiple scenarios are desired, rerun lines 17-309 to generate each dataframe

#topt1 (median values), topt2 (Scavia et al. 1976)
#topt3 (Arhonditsis and Brett 2005), plus6C (+6C to daily air temp in met file)

#create met file for 6C warming temp scenario
#met <- read.csv("inputs/met.csv")
#met_6C_warmer <- met |> 
#  mutate(AirTemp = AirTemp + 6)
#write.csv(met_6C_warmer,"inputs/met_plus6C.csv", row.names = F)

#"topt1","topt2","topt3","topt1_plus5","topt2_plus5","topt3_plus5","plus6C"
scenario <- "topt1" 

#pull pars + files associated with the correct scenario
if(scenario %in% "plus6C"){
  file.copy('22Aug24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
} else(
  file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)  
)

file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed2_phyto_pars_12Jul2024.csv', 
        'aed/aed_phyto_pars.csv', overwrite = TRUE)

if(scenario %in% c("topt1", "plus6C")){
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
  
#save zoop output
var="ZOO_cladoceran"

obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
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
  mutate(ZOO_clad = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime) %in% 12)

mod<- clads_full_wc |>
  select(DateTime, ZOO_clad) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_cladoceran = ZOO_clad)

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime"))

#calculate RMSE
clad <- compare |> 
  rename(Mod_cladoceran = ZOO_cladoceran.x,
         Obs_cladoceran = ZOO_cladoceran.y)

var="ZOO_copepod"

obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  dplyr::select(DateTime, var) %>%
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
  mutate(ZOO_cope = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime) %in% 12)

mod<- cope_full_wc |>
  select(DateTime, ZOO_cope) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_copepod = ZOO_cope)

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime"))

#calculate RMSE
cope <- compare |> 
  rename(Mod_copepod = ZOO_copepod.x,
         Obs_copepod = ZOO_copepod.y)


var="ZOO_rotifer"

obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
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
  mutate(ZOO_rot = rowSums(across(where(is.numeric)),na.rm=T)) |> 
  filter(hour(DateTime) %in% 12)

mod<- rot_full_wc |>
  select(DateTime, ZOO_rot) |> 
  mutate(DateTime = as.Date(DateTime)) |> 
  rename(ZOO_rotifer = ZOO_rot)

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime"))

#calculate RMSE
rot <- compare |> 
  rename(Mod_rotifer = ZOO_rotifer.x,
         Obs_rotifer = ZOO_rotifer.y)

#combine into one df 
all_zoops <- reduce(list(clad, cope, rot), full_join) 

#convert from wide to long for plotting
all_zoops_final <- all_zoops |> 
  pivot_longer(cols = -c(DateTime), 
               names_pattern = "(...)_(...*)$",
               names_to = c("type", "taxon")) |> 
  na.omit() |> 
  filter(value < 500)

#now create a dynamic df name
assign(paste0("all_zoops_", scenario), all_zoops_final)

#------------------------------------------------------------------#
# plot zoops

#color scheme for each different scenario (n=7)
colors <- c("#143c80","#0377a8","#bcf8ec","#aed9e0","#9fa0c3","#8b687f","#7b435b")

# note - make sure to change df and color names in function below!
ggplot() +
  geom_line(data=subset(all_zoops_topt1, type %in% "Mod"),
            aes(DateTime, value, color = "topt1")) +
  geom_line(data=subset(all_zoops_topt1_plus5, type %in% "Mod"),
            aes(DateTime, value, color = "topt1_plus5"), linetype = "dashed") +
  geom_point(data=subset(all_zoops_topt1, type %in% "Obs"),
             aes(DateTime, value, color="observed")) + 
  facet_wrap(~taxon, scales="free_y", nrow=3, strip.position = "right") + 
  theme_bw() + xlab("") +
  scale_color_manual("", values = c("red","#143c80","#0377a8"),
                     guide = guide_legend(override.aes = list(
                       #color = c("red","#143c80","#7b435b"),
                       linetype = c("blank", "solid","dashed"),
                       shape = c(16, NA,NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.25,0.98),
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
#ggsave("figures/zoop_scenario_topt1.jpg", width=6, height=6)
#ggsave("figures/zoop_scenario_topt2.jpg", width=6, height=6)
#ggsave("figures/zoop_scenario_topt3.jpg", width=6, height=6)
#ggsave("figures/zoop_scenario_plus6C.jpg", width=6, height=6)
