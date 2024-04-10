#miscellaneous code for checking on zoop dynamics

pacman::p_load(scales, ggplot2)

file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/22Jan24_po4cal_aed2.nml', 'aed/aed2.nml', overwrite = TRUE)

#run the model!
system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
        stdout = TRUE, stderr = TRUE, 
        env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
                     "/glm.app/Contents/MacOS"))

#sometimes, you'll get an error that says "Error in file, 'Time(Date)' is not first column!
#in this case, open the input file in Excel, set the column in Custom ("YYYY-MM-DD") format, resave, and close the file
nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 

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
            linewidth=3) +
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
#ggsave("figures/BVR_stacked_phyto_composition.jpg", width=5, height=4) 

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
            linewidth=3) +
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
#ggsave("figures/BVR_stacked_zoop_composition.jpg", width=5, height=4) 

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
            linewidth=3) +
  scale_color_manual(values = NatParksPalettes::
                       natparks.pals("Volcanoes", 5,  direction = -1))+
  scale_fill_manual(values = NatParksPalettes::
                      natparks.pals("Volcanoes", 5, direction = -1))+
  ylab("diagnostics (mmolC/m3/day")+
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


#--------------------------------------------------------------------------#
#respiration as a function of temperature
f_resp <- function(x, Rresp, theta_resp){
  y = Rresp_zoo*theta_resp_zoo^(x-20)
}
Rresp_zoo = 0.08 
theta_resp_zoo = 1.08

#jpeg("./plot_output/resp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f_resp(x, Rresp = Rresp_zoo, theta_resp = theta_resp_zoo),from=4, 
      to=30,ylab='f(T)',xlab = 'Water temperature (°C)', main = "respiration")
#dev.off()

#grazing as a function of temperature
f_grz <- function(x, Rgrz, theta_grz, Kgrz, C){
  y = Rgrz_zoo*theta_grz_zoo^(x-20) * (C /Kgrz_zoo +  C)
}

Rgrz_zoo = 1.6
theta_grz_zoo = 1.08
Kgrz_zoo = 7
C = 100 #this varies a lot

#jpeg("./plot_output/resp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f_grz(x, Rgrz = Rgrz_zoo, theta_grz = theta_grz_zoo,
            Kgrz = Kgrz_zoo, C = C),from=4, 
      to=30,ylab='f(T)',xlab = 'Water temperature (°C)', main = "grazing")
#dev.off()

#grazing = data%zoops(zoop_i)%Rgrz_zoo * fGrazing_Limitation * f_T


#mortality as a function of temperature
f_mort <- function(x, Rmort, theta){
  y = Rmort_zoo*theta_grz_zoo^(x-20) * 1 + ((DOmin_zoo - oxy) / DOmin_zoo) 
} # the DO part is technically only used when DO < DOmin_zoo, so not sure how to do that here
#or what to set oxy to given that it changes over the course of the year

Rmort_zoo = 0.1 
theta_grz_zoo = 1.08
DOmin_zoo = 5
oxy = 5

#jpeg("./plot_output/resp_eq.jpeg", res = 300, width = 5, height = 3.5, units = "in")
par(cex.lab = 1.5, mgp = c(2.7,1,0))
curve(f_mort(x, Rmort = Rmort_zoo, theta = theta_grz_zoo),from=4, 
      to=30,ylab='f(T)',xlab = 'Water temperature (°C)', main = "mortality")
#dev.off()


#mortality = data%zoops(zoop_i)%Rmort_zoo * f_T * f_DO
