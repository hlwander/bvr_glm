# Plankton air temp scenarios
# 22 August 2024

library(ggplot2)

scenario <- c("baseline","plus1","plus2","plus3","plus5")

for (i in 1:length(scenario)){

nc_file = paste0("sims/",scenario[i],"/output/output.nc")  
  
#save zoop output
var="ZOO_cladoceran"
clad_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

# Function to get zoop data for varying depths
get_zoops <- function(depths, nc_file, var) {
  lapply(depths, function(z) {
    if (z == 0) {
      glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface')
    } else {
      glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface') |>
        dplyr::select(-DateTime)
    }
  }) |> 
    dplyr::bind_cols()
  
}

# Define depth range and call the function
depths <- seq(0, 11, by = 0.5)
clad_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
clad <- clad_full_wc |> 
  dplyr::mutate(ZOO_cladoceran = rowSums(dplyr::across(where(is.numeric)),na.rm=TRUE)) |>
  dplyr::select(DateTime, ZOO_cladoceran) |> 
  dplyr::mutate(DateTime = as.Date(DateTime)) 

var="ZOO_copepod"
cope_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

cope_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
cope <- cope_full_wc |> 
  dplyr::mutate(ZOO_copepod = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
  dplyr::select(DateTime, ZOO_copepod) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))


var="ZOO_rotifer"
rot_obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::select(DateTime, var) |> 
  na.omit() 

rot_full_wc <- get_zoops(depths, nc_file, var)

#sum all depths
rot <- rot_full_wc |> 
  dplyr::mutate(ZOO_rotifer = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
  dplyr::select(DateTime, ZOO_rotifer) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))


#combine into one df 
all_zoops <- purrr::reduce(list(clad, cope, rot), dplyr::full_join) 

all_zoops_obs <- purrr::reduce(list(clad_obs, cope_obs, rot_obs), dplyr::full_join) |> 
  tidyr::pivot_longer(cols = -c(DateTime), 
               names_pattern = "(...)_(...*)$",
               names_to = c("mod", "taxon")) |> 
  na.omit() |> 
  dplyr::filter(value < 500) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))

#convert from wide to long for plotting
all_zoops_final <- all_zoops |> 
  tidyr::pivot_longer(cols = -c(DateTime), 
               names_pattern = "(...)_(...*)$",
               names_to = c("mod", "taxon")) |> 
  na.omit()

#now create a dynamic df name
assign(paste0("all_zoops_", scenario[i]), all_zoops_final)
}

#------------------------------------------------------------------#
# same for phytos
for (i in 1:length(scenario)){
  
  nc_file = paste0("sims/",scenario[i],"/output/output.nc")  
  
  var="PHY_cyano"
  # Function to get phyto data for varying depths
  get_zoops <- function(depths, nc_file, var) {
    lapply(depths, function(z) {
      if (z == 0) {
        glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface')
      } else {
        glmtools::get_var(file = nc_file, var_name = var, z_out = z, reference = 'surface') |>
          dplyr::select(-DateTime)
      }
    }) |> 
      dplyr::bind_cols()
    
  }
  
  # Define depth range and call the function
  depths <- seq(0, 11, by = 0.5)
  cyano_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  cyano <- cyano_full_wc |> 
    dplyr::mutate(PHY_cyano = rowSums(dplyr::across(where(is.numeric)),na.rm=TRUE)) |>
    dplyr::select(DateTime, PHY_cyano) |> 
    dplyr::mutate(DateTime = as.Date(DateTime)) 
  
  var="PHY_green"
  green_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  green <- green_full_wc |> 
    dplyr::mutate(PHY_green = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
    dplyr::select(DateTime, PHY_green) |> 
    dplyr::mutate(DateTime = as.Date(DateTime))
  
  var="PHY_diatom"
  diatom_full_wc <- get_zoops(depths, nc_file, var)
  
  #sum all depths
  diatom <- diatom_full_wc |> 
    dplyr::mutate(PHY_diatom = rowSums(dplyr::across(where(is.numeric)),na.rm=T)) |>
    dplyr::select(DateTime, PHY_diatom) |> 
    dplyr::mutate(DateTime = as.Date(DateTime))
  
  #combine into one df 
  all_phytos <- purrr::reduce(list(cyano, green, diatom), dplyr::full_join) 
  
  #convert from wide to long for plotting
  all_phytos_final <- all_phytos |> 
    tidyr::pivot_longer(cols = -c(DateTime), 
                        names_pattern = "(...)_(...*)$",
                        names_to = c("mod", "taxon")) |> 
    na.omit()
  
  #now create a dynamic df name
  assign(paste0("all_phytos_", scenario[i]), all_phytos_final)
}

#-------------------------------------------------------------------------#
# And lastly chla 

for (i in 1:length(scenario)){
  
  nc_file = paste0("sims/",scenario[i],"/output/output.nc")  
  
var="PHY_tchla"
chla_obs <- read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.Date(DateTime))  |> 
  na.omit()

#read mod chla
chla_mod<- glmtools::get_var(nc_file, var, reference="surface", z_out=depths) |> 
  tidyr::pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) |> 
  dplyr::mutate(DateTime = as.Date(DateTime)) |> 
  dplyr::mutate(Depth=as.numeric(Depth)) |> 
  na.omit()

#now create a dynamic df name
assign(paste0("chla_", scenario[i]), chla_mod)
}

#------------------------------------------------------------------#
# plot zoops
ggplot() +
  geom_line(data=all_zoops_baseline,
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=all_zoops_plus1,
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=all_zoops_plus3,
            aes(DateTime, value, color = "+3C")) +
  geom_line(data=all_zoops_plus5,
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
  geom_line(data=all_phytos_baseline,
            aes(DateTime, value, color = "+0C")) +
  geom_line(data=all_phytos_plus1,
            aes(DateTime, value, color = "+1C")) +
  geom_line(data=all_phytos_plus3,
            aes(DateTime, value, color = "+3C")) +
  geom_line(data=all_phytos_plus5,
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
  geom_line(data=subset(chla_baseline, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+0C")) +
  geom_line(data=subset(chla_plus1, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+1C")) +
  geom_line(data=subset(chla_plus3, Depth %in% 0),
            aes(DateTime, PHY_tchla, color = "+3C")) +
  geom_line(data=subset(chla_plus5, Depth %in% 0),
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