#miscellaneous code for checking on plankton dynamics

pacman::p_load(scales, ggplot2)

file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)

#run the model!
GLM3r::run_glm()

#define the output.nc file 
nc_file <- file.path('./output/output.nc') 

#--------------------------------------------------------------------------#
#phyto and zoop stacked line plot

cyano <- get_var(file=nc_file,var_name = "PHY_cyano",
                 z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

green <- get_var(file=nc_file,var_name = "PHY_green",
                 z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

diatom <- get_var(file=nc_file,var_name = "PHY_diatom",
                  z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

#combine taxon dfs
phytos_long <- bind_cols(cyano, green[!colnames(green) %in% "DateTime"], 
                         diatom[!colnames(diatom) %in% "DateTime"]) |> 
  rename(cyano = PHY_cyano_0.1,
         green = PHY_green_0.1,
         diatom = PHY_diatom_0.1) |> 
  pivot_longer(cols = cyano:diatom, 
               names_to = "variable")

#reorder taxa
phytos_long$variable <- factor(phytos_long$variable, 
                               levels=c("diatom","cyano","green"))

#phyto plot
ggplot(phytos_long, aes(x = DateTime, y = value)) + 
  geom_area(aes(color = variable, fill = variable),
            position = "stack", stat="identity",
            linewidth=1) +
  scale_color_manual(values = NatParksPalettes::
                       natparks.pals("RockyMtn", 7,  direction = -1))+
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("RockyMtn", 7, direction = -1))+
  ylab("Biomass (mmol/m3 C)")+
  #labs(fill = "Taxon", color = "Taxon")+
  scale_x_datetime(expand = c(0,0),labels = 
                     date_format("%Y",tz="EST5EDT")) +
  #date_format("%b",tz="EST5EDT")) +
  scale_y_continuous(expand = c(0,0))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.88,0.83),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_stacked_phyto_composition_0.1m.jpg", width=5, height=4) 

clad <- get_var(file=nc_file,var_name = "ZOO_cladoceran",
                z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

cope <- get_var(file=nc_file,var_name = "ZOO_copepod",
                z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

rot <- get_var(file=nc_file,var_name = "ZOO_rotifer",
               z_out=0.1,reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

#combine taxon dfs
zoops_long <- bind_cols(clad, cope[!colnames(cope) %in% "DateTime"], 
                        rot[!colnames(rot) %in% "DateTime"]) |> 
  rename(cladoceran = ZOO_cladoceran_0.1,
         copepod = ZOO_copepod_0.1,
         rotifer = ZOO_rotifer_0.1) |> 
  pivot_longer(cols = cladoceran:rotifer, 
               names_to = "variable")

#zoop plot
ggplot(zoops_long, aes(x = DateTime, y = value)) + 
  geom_area(aes(color = variable, fill = variable),
            position = "stack", stat="identity",
            linewidth=1) +
  scale_color_manual(values = NatParksPalettes::
                       natparks.pals("KingsCanyon", 3,  direction = -1))+
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("KingsCanyon", 3, direction = -1))+
  ylab("Biomass (mmol/m3 C)")+
  #labs(fill = "Taxon", color = "Taxon")+
  scale_x_datetime(expand = c(0,0),labels = 
                     date_format("%Y",tz="EST5EDT")) +
  #date_format("%b",tz="EST5EDT")) +
  scale_y_continuous(expand = c(0,0))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.88,0.83),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_stacked_zoop_composition_0.1m.jpg", width=5, height=4) 

#--------------------------------------------------------------------------#
#stacked plot for zoop diagnostics

grz <- get_var(file=nc_file,var_name = 'ZOO_grz',z_out=1,
               reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31"))
#filter(DateTime < as.POSIXct("2017-07-08"))

resp <- get_var(file=nc_file,var_name = 'ZOO_resp',z_out=1,
                reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |> 
  #filter(DateTime < as.POSIXct("2017-07-08")) 
  mutate(ZOO_resp_1 = ZOO_resp_1 * -1)

mort <- get_var(file=nc_file,var_name = 'ZOO_mort',z_out=1,
                reference = 'surface') |> 
  filter(DateTime < as.POSIXct("2020-12-31")) |>
  #filter(DateTime < as.POSIXct("2017-07-08"))  
  mutate(ZOO_mort_1 = ZOO_mort_1 * -1)

#combine diagnostics into 1 df
diag_long <- bind_cols(grz, resp[!colnames(resp) %in% "DateTime"],
                       mort[!colnames(mort) %in% "DateTime"]) |> 
  rename(grz = ZOO_grz_1,
         resp = ZOO_resp_1,
         mort = ZOO_mort_1) |> 
  pivot_longer(cols = grz:mort,
               names_to = "variable")

ggplot(diag_long, aes(x = DateTime, y = value)) + 
  geom_area(aes(color = variable, fill = variable),
            position = "stack", stat="identity",
            linewidth=1) +
  scale_color_manual(values = NatParksPalettes::
                       natparks.pals("Volcanoes", 5,  direction = -1))+
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("Volcanoes", 5, direction = -1))+
  ylab("diagnostics (mmolC/m3/day)")+ xlab("") +
  #labs(fill = "Taxon", color = "Taxon")+
  scale_x_datetime(expand = c(0,0),labels = 
                     date_format("%Y",tz="EST5EDT")) +
  #date_format("%b",tz="EST5EDT")) +
  scale_y_continuous(expand = c(0,0))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.88,0.83),
        text = element_text(size=8), 
        axis.text.y = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(face = "bold",hjust = 0),
        axis.text.x = element_text(angle=0),
        strip.background.x = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.margin = unit(c(0, 1, 0, 0), "cm"),
        panel.spacing = unit(0.5, "lines"))
#ggsave("figures/BVR_stacked_zoop_diag.jpg", width=5, height=4) 
