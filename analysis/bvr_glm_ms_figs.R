# Script for generating BVR glm aed ms figures and tables
# 20 Aug 2024

#load packages
pacman::p_load(GLMr,glmtools,tidyverse,lubridate,purr, hydroGOF)

#set wd
sim_folder <- getwd()

#overwrite files so calibrated pars are used in model
file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/aed2_4zones.nml', 'aed/aed2.nml', overwrite = TRUE)
file.copy('aed/aed2_phyto_pars_12Jul2024.csv', 
          'aed/aed_phyto_pars.csv', overwrite = TRUE)
file.copy('aed/aed_zoop_pars_3groups_27jun2024_ps1.csv', #ps1, ps2
          'aed/aed_zoop_pars.csv', overwrite = TRUE)

#run the model!
system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
        stdout = TRUE, stderr = TRUE, 
        env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
                     "/glm.app/Contents/MacOS"))

#define the output.nc file 
nc_file <- file.path(sim_folder, 'output/output.nc') 

#----------------------------------------------------------------------#
# create modeled vs. observed df for each variable

#focal depths
depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

# temp
obstemp<-read_csv('field_data/CleanedObsTemp.csv') |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

modtemp <- get_temp(nc_file, reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth")) |> 
  rename(mod_temp = temp.x, obs_temp = temp.y)

# DO
obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') |> 
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))

mod_oxy <- get_var(nc_file, "OXY_oxy", reference='surface', z_out=depths) |> 
  pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy")  |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) |> 
  rename(mod_oxy = OXY_oxy.x, obs_oxy = OXY_oxy.y)

# DIC
obs_dic <-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, CAR_dic) 

mod_dic <- get_var(nc_file, "CAR_dic", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("CAR_dic_"), names_to="Depth", names_prefix="CAR_dic_", values_to = "CAR_dic")  |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

dic_compare<-merge(mod_dic, obs_dic, by=c("DateTime","Depth")) |> 
  rename(mod_dic = CAR_dic.x, obs_dic = CAR_dic.y)

# CH4
obs_ch4 <-read.csv('field_data/field_gases.csv', header=TRUE) |>  
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, CAR_ch4) 

mod_ch4 <- get_var(nc_file, "CAR_ch4", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("CAR_ch4_"), names_to="Depth", names_prefix="CAR_ch4_", values_to = "CAR_ch4") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

ch4_compare<-merge(mod_ch4, obs_ch4, by=c("DateTime","Depth")) |> 
  rename(mod_ch4 = CAR_ch4.x, obs_ch4 = CAR_ch4.y)

# NH4
obs_nh4 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, NIT_amm) 

mod_nh4 <- get_var(nc_file, "NIT_amm", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("NIT_amm_"), names_to="Depth", names_prefix="NIT_amm_", values_to = "NIT_amm") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

nh4_compare<-merge(mod_nh4, obs_nh4, by=c("DateTime","Depth")) |> 
  rename(mod_nh4 = NIT_amm.x, obs_nh4 = NIT_amm.y)

# NO3
obs_no3 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, NIT_nit) 

mod_no3 <- get_var(nc_file, "NIT_nit", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("NIT_nit_"), names_to="Depth", names_prefix="NIT_nit_", values_to = "NIT_nit") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

no3_compare<-merge(mod_no3, obs_no3, by=c("DateTime","Depth")) |> 
  rename(mod_no3 = NIT_nit.x, obs_no3 = NIT_nit.y)

# PO4
obs_po4 <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, PHS_frp) 

mod_po4 <- get_var(nc_file, "PHS_frp", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("PHS_frp_"), names_to="Depth", names_prefix="PHS_frp_", values_to = "PHS_frp") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

po4_compare<-merge(mod_po4, obs_po4, by=c("DateTime","Depth")) |> 
  rename(mod_po4 = PHS_frp.x, obs_po4 = PHS_frp.y)

# docr
obs_docr <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, OGM_docr) 

mod_docr <- get_var(nc_file, "OGM_docr", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("OGM_docr_"), names_to="Depth", names_prefix="OGM_docr_", values_to = "OGM_docr") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

docr_compare<-merge(mod_docr, obs_docr, by=c("DateTime","Depth")) |> 
  rename(mod_docr = OGM_docr.x, obs_docr = OGM_docr.y)

# doc labile
obs_doc <- read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, OGM_doc) 

mod_doc <- get_var(nc_file, "OGM_doc", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("OGM_doc_"), names_to="Depth", names_prefix="OGM_doc_", values_to = "OGM_doc") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

doc_compare<-merge(mod_doc, obs_doc, by=c("DateTime","Depth")) |> 
  rename(mod_doc = OGM_doc.x, obs_doc = OGM_doc.y)

# chl a
obs_chla <- read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
  select(DateTime, Depth, PHY_tchla) 

mod_chla <- get_var(nc_file, "PHY_tchla", reference="surface", z_out=depths) |> 
  pivot_longer(cols=starts_with("PHY_tchla_"), names_to="Depth", names_prefix="PHY_tchla_", values_to = "PHY_tchla") |> 
  filter(hour(DateTime) %in% 0) |> #make sure only selecting midnight value
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 

chla_compare<-merge(mod_chla, obs_chla, by=c("DateTime","Depth")) |> 
  rename(mod_chla = PHY_tchla.x, obs_chla = PHY_tchla.y)

#-------------------------------------------------------------#
#merge all dfs
all_vars <- reduce(list(watertemp, oxy_compare, dic_compare,
                           ch4_compare, nh4_compare, no3_compare,
                           po4_compare, docr_compare, doc_compare,
                           chla_compare), full_join)

#add col for calib vs. valid period (2020-12-31)
all_vars$period <- ifelse(all_vars$DateTime <= "2020-12-31",
                        "calib", "valid")

#convert from wide to long for plotting
all_vars_final <- all_vars |> 
  filter(Depth %in% c(0.1,9)) |> 
  pivot_longer(cols = -c(DateTime,Depth,period), 
               names_pattern = "(...)_(...*)$",
               names_to = c("type", "var")) |> 
  na.omit()

#-------------------------------------------------------------#
#df for text annotations
ann_text <- data.frame(y_val = c(30, 370, 90, 18, 7.5, 0.3, 0.23, 458, 63, 14.5),
                       y_val2 = c(12.6, 360, 135, 1100, 250, 0.36, 0.65, 530, 60, 11),
                       y_valid1 = c(26.3, 425, 68, 15, 4.1, 0.24, 0.119, 345, 57, 6.3),
                       y_valid2 = c(10.5, 370, 85, 650, 85, 0.27, 0.155, 440, 49, 9.8),
                       var = c("temp", "oxy", "dic", "ch4", "nh4",
                               "no3", "po4", "docr", "doc", "chla"),
                       label = c("Temp","DO","DIC","CH[4]","NH[4]",
                                 "NO[3]","PO[4]","DOC[R]","DOC[L]",
                                 "Chl~italic('a')"))

#ginormous plot with all vars for 0.1m
ggplot() +
  geom_line(data=subset(all_vars_final, type %in% "mod" & 
                          period %in% "calib" & Depth %in% 0.1), aes(DateTime, value, color = "modeled")) +
  geom_point(data=subset(all_vars_final, type %in% "obs" &
                           period %in% "calib" & Depth %in% 0.1), aes(DateTime, value, color="observed")) + 
  facet_wrap(~factor(var,levels = c("temp", "oxy", "dic", "ch4", "nh4",
                                   "no3", "po4", "docr", "doc", "chla")), 
             scales = "free_y", nrow=5) + theme_bw() + xlab("") +
  geom_text(data    = ann_text,
            mapping = aes(x = as.POSIXct("2015-05-01"), 
                  y = y_val, label = label),
            hjust   = -0.1, parse=T) +
  scale_color_manual("", values = c("black","red"),
                     guide = guide_legend(override.aes = list(
                       color = c("red","black"),
                         linetype = c("blank", "solid"),
                         shape = c(16, NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.9, 0.17),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/allvars_calib_mod_vs_obs_0.1m.jpg", width=6, height=6)

#ginormous plot with all vars for 9m
ggplot() +
  geom_line(data=subset(all_vars_final, type %in% "mod" & 
                          period %in% "calib" & Depth %in% 9), aes(DateTime, value, color = "modeled")) +
  geom_point(data=subset(all_vars_final, type %in% "obs" &
                           period %in% "calib" & Depth %in% 9), aes(DateTime, value, color="observed")) + 
  facet_wrap(~factor(var,levels = c("temp", "oxy", "dic", "ch4", "nh4",
                                    "no3", "po4", "docr", "doc", "chla")), 
             scales = "free_y", nrow=5) + theme_bw() + xlab("") +
  geom_text(data    = ann_text,
            mapping = aes(x = as.POSIXct("2015-05-01"), 
                          y = y_val2, label = label),
            hjust   = -0.1, parse=T) +
  scale_color_manual("", values = c("black","red"),
                     guide = guide_legend(override.aes = list(
                       color = c("red","black"),
                       linetype = c("blank", "solid"),
                       shape = c(16, NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.9, 0.17),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/allvars_calib_mod_vs_obs_9m.jpg", width=6, height=6)

# validation period 0.1m
ggplot() +
  geom_line(data=subset(all_vars_final, type %in% "mod" & 
                          period %in% "valid" & Depth %in% 0.1), aes(DateTime, value, color = "modeled")) +
  geom_point(data=subset(all_vars_final, type %in% "obs" &
                           period %in% "valid" & Depth %in% 0.1), aes(DateTime, value, color="observed")) + 
  facet_wrap(~factor(var,levels = c("temp", "oxy", "dic", "ch4", "nh4",
                                    "no3", "po4", "docr", "doc", "chla")), 
             scales = "free_y", nrow=5) + theme_bw() + xlab("") +
  geom_text(data    = ann_text,
            mapping = aes(x = as.POSIXct("2021-01-01"), 
                          y = y_valid1, label = label),
            hjust   = -0.1, parse=T) +
  scale_color_manual("", values = c("black","red"),
                     guide = guide_legend(override.aes = list(
                       color = c("red","black"),
                       linetype = c("blank", "solid"),
                       shape = c(16, NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.9, 0.15),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/allvars_valid_mod_vs_obs_0.1m.jpg", width=6, height=6)

# validation period 9m
ggplot() +
  geom_line(data=subset(all_vars_final, type %in% "mod" & 
                          period %in% "valid" & Depth %in% 9), aes(DateTime, value, color = "modeled")) +
  geom_point(data=subset(all_vars_final, type %in% "obs" &
                           period %in% "valid" & Depth %in% 9), aes(DateTime, value, color="observed")) + 
  facet_wrap(~factor(var,levels = c("temp", "oxy", "dic", "ch4", "nh4",
                                    "no3", "po4", "docr", "doc", "chla")), 
             scales = "free_y", nrow=5) + theme_bw() + xlab("") +
  geom_text(data    = ann_text,
            mapping = aes(x = as.POSIXct("2021-01-01"), 
                          y = y_valid2, label = label),
            hjust   = -0.1, parse=T) +
  scale_color_manual("", values = c("black","red"),
                     guide = guide_legend(override.aes = list(
                       color = c("red","black"),
                       linetype = c("blank", "solid"),
                       shape = c(16, NA)))) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.position = c(0.85, 0.15),
        text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        plot.margin = unit(c(0.2, 0.1, 0, 0), "cm"),
        panel.spacing.x = unit(0.1, "in"),
        panel.background = element_rect(
          fill = "white"),
        panel.spacing.y = unit(0, "lines"))
#ggsave("figures/allvars_valid_mod_vs_obs_9m.jpg", width=6, height=6)

#-----------------------------------------------------------------------#
# GOF table for calib vs. valid periods

# Full water column, full period (2015-2022)
all_gof <- setNames(data.frame(matrix(ncol=2,nrow=22)),c("Parameter","Temp"))
all_gof$Parameter <- c("ME_all","MAE_all","MSE_all","RMSE_all","NRMSE%_all","PBIAS%_all","RSR_all","rSD_all","NSE_all","mNSE_all","rNSE_all","d_all","md_all","rd_all","cp_all","r_all",
                       "R2_all","bR2_all","KGE_all","VE_all","r.Spearman","nonparamR2")

# Full water column, calibration period (2015-2020)
all_gof_cal <- setNames(data.frame(matrix(ncol=2,nrow=22)),c("Parameter","Temp"))
all_gof_cal$Parameter <- c("ME_cal","MAE_cal","MSE_cal","RMSE_cal","NRMSE%_cal","PBIAS%_cal","RSR_cal","rSD_cal","NSE_cal","mNSE_cal","rNSE_cal","d_cal","md_cal","rd_cal","cp_cal","r_cal",
                           "R2_cal","bR2_cal","KGE_cal","VE_cal","r.Spearman","nonparamR2")

# Full water column, validation period (2021-2022)
all_gof_val <- setNames(data.frame(matrix(ncol=2,nrow=22)),c("Parameter","Temp"))
all_gof_val$Parameter <- c("ME_val","MAE_val","MSE_val","RMSE_val","NRMSE%_val","PBIAS%_val","RSR_val","rSD_val","NSE_val","mNSE_val","rNSE_val","d_val","md_val","rd_val","cp_val","r_val",
                           "R2_val","bR2_val","KGE_val","VE_val","r.Spearman","nonparamR2")

# calculate all gof metrics for full period + all different vars
all_gof$Temp <- c(gof(watertemp$mod_temp,watertemp$obs_temp,do.spearman = TRUE), NA)
all_gof$DO <- c(gof(oxy_compare$mod_oxy,oxy_compare$obs_oxy,do.spearman = TRUE), NA)
all_gof$DIC <- c(gof(dic_compare$mod_dic,dic_compare$obs_dic,do.spearman = TRUE), NA)
all_gof$CH4 <- c(gof(ch4_compare$mod_ch4,ch4_compare$obs_ch4,do.spearman = TRUE), NA)
all_gof$NH4 <- c(gof(nh4_compare$mod_nh4,nh4_compare$obs_nh4,do.spearman = TRUE), NA)
all_gof$NO3 <- c(gof(no3_compare$mod_no3,no3_compare$obs_no3,do.spearman = TRUE), NA)
all_gof$PO4 <- c(gof(po4_compare$mod_po4,po4_compare$obs_po4,do.spearman = TRUE), NA)
all_gof$DOCr <- c(gof(docr_compare$mod_docr,docr_compare$obs_docr,do.spearman = TRUE), NA)
all_gof$DOC <- c(gof(doc_compare$mod_doc,doc_compare$obs_doc,do.spearman = TRUE), NA)
all_gof$Chla <- c(gof(chla_compare$mod_chla,chla_compare$obs_chla,do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_temp_rank <- watertemp |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_dic_rank <- dic_compare |> 
  mutate(rank_obs = rank(obs_dic),
         rank_mod = rank(mod_dic)) 
comb_ch4_rank <- ch4_compare |> 
  mutate(rank_obs = rank(obs_ch4),
         rank_mod = rank(mod_ch4)) 
comb_nh4_rank <- nh4_compare |>  
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_docr_rank <- docr_compare |> 
  mutate(rank_obs = rank(obs_docr),
         rank_mod = rank(mod_docr)) 
comb_doc_rank <- doc_compare |> 
  mutate(rank_obs = rank(obs_doc),
         rank_mod = rank(mod_doc)) 
comb_chla_rank <- chla_compare |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof$Temp[22] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof$DO[22] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof$DIC[22] <- summary(lm(comb_dic_rank$rank_obs ~ comb_dic_rank$rank_mod))$r.squared
all_gof$CH4[22] <- summary(lm(comb_ch4_rank$rank_obs ~ comb_ch4_rank$rank_mod))$r.squared
all_gof$NH4[22] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof$NO3[22] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof$PO4[22] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof$DOCr[22] <- summary(lm(comb_docr_rank$rank_obs ~ comb_docr_rank$rank_mod))$r.squared
all_gof$DOC[22] <- summary(lm(comb_doc_rank$rank_obs ~ comb_doc_rank$rank_mod))$r.squared
all_gof$Chla[22] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

# same as above but for CALIBRATION period
all_gof_cal$Temp <- c(gof(watertemp$mod_temp[watertemp$DateTime< "2021-01-01"],
                          watertemp$obs_temp[watertemp$DateTime< "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_cal$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$DateTime< "2021-01-01"],
                        oxy_compare$obs_oxy[oxy_compare$DateTime< "2021-01-01"],
                        do.spearman = TRUE), NA)
all_gof_cal$DIC <- c(gof(dic_compare$mod_dic[dic_compare$DateTime< "2021-01-01"],
                         dic_compare$obs_dic[dic_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$CH4 <- c(gof(ch4_compare$mod_ch4[ch4_compare$DateTime< "2021-01-01"],
                         ch4_compare$obs_ch4[ch4_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$DateTime< "2021-01-01"],
                         nh4_compare$obs_nh4[nh4_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$DateTime< "2021-01-01"],
                         no3_compare$obs_no3[no3_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$DateTime< "2021-01-01"],
                         po4_compare$obs_po4[po4_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$DOCr <- c(gof(docr_compare$mod_docr[docr_compare$DateTime< "2021-01-01"],
                          docr_compare$obs_docr[docr_compare$DateTime< "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_cal$DOC <- c(gof(doc_compare$mod_doc[doc_compare$DateTime< "2021-01-01"],
                         doc_compare$obs_doc[doc_compare$DateTime< "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_cal$Chla <- c(gof(chla_compare$mod_chla[chla_compare$DateTime< "2021-01-01"],
                          chla_compare$obs_chla[chla_compare$DateTime< "2021-01-01"],
                          do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_temp_rank <- watertemp |>  
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_dic_rank <- dic_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_dic),
         rank_mod = rank(mod_dic)) 
comb_ch4_rank <- ch4_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_ch4),
         rank_mod = rank(mod_ch4)) 
comb_nh4_rank <- nh4_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_docr_rank <- docr_compare |>  
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_docr),
         rank_mod = rank(mod_docr)) 
comb_doc_rank <- doc_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_doc),
         rank_mod = rank(mod_doc)) 
comb_chla_rank <- chla_compare |> 
  filter(DateTime < "2021-01-01") |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof_cal$Temp[22] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_cal$DO[22] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof_cal$DIC[22] <- summary(lm(comb_dic_rank$rank_obs ~ comb_dic_rank$rank_mod))$r.squared
all_gof_cal$CH4[22] <- summary(lm(comb_ch4_rank$rank_obs ~ comb_ch4_rank$rank_mod))$r.squared
all_gof_cal$NH4[22] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof_cal$NO3[22] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof_cal$PO4[22] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof_cal$DOCr[22] <- summary(lm(comb_docr_rank$rank_obs ~ comb_docr_rank$rank_mod))$r.squared
all_gof_cal$DOC[22] <- summary(lm(comb_doc_rank$rank_obs ~ comb_doc_rank$rank_mod))$r.squared
all_gof_cal$Chla[22] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

# now VALIDATION period
all_gof_val$Temp <- c(gof(watertemp$mod_temp[watertemp$DateTime >= "2021-01-01"],
                          watertemp$obs_temp[watertemp$DateTime >= "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_val$DO <- c(gof(oxy_compare$mod_oxy[oxy_compare$DateTime >= "2021-01-01"],
                        oxy_compare$obs_oxy[oxy_compare$DateTime >= "2021-01-01"],
                        do.spearman = TRUE), NA)
all_gof_val$DIC <- c(gof(dic_compare$mod_dic[dic_compare$DateTime >= "2021-01-01"],
                         dic_compare$obs_dic[dic_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$CH4 <- c(gof(ch4_compare$mod_ch4[ch4_compare$DateTime >= "2021-01-01"],
                         ch4_compare$obs_ch4[ch4_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$NH4 <- c(gof(nh4_compare$mod_nh4[nh4_compare$DateTime >= "2021-01-01"],
                         nh4_compare$obs_nh4[nh4_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$NO3 <- c(gof(no3_compare$mod_no3[no3_compare$DateTime >= "2021-01-01"],
                         no3_compare$obs_no3[no3_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$PO4 <- c(gof(po4_compare$mod_po4[po4_compare$DateTime >= "2021-01-01"],
                         po4_compare$obs_po4[po4_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$DOCr <- c(gof(docr_compare$mod_docr[docr_compare$DateTime >= "2021-01-01"],
                          docr_compare$obs_docr[docr_compare$DateTime >= "2021-01-01"],
                          do.spearman = TRUE), NA)
all_gof_val$DOC <- c(gof(doc_compare$mod_doc[doc_compare$DateTime >= "2021-01-01"],
                         doc_compare$obs_doc[doc_compare$DateTime >= "2021-01-01"],
                         do.spearman = TRUE), NA)
all_gof_val$Chla <- c(gof(chla_compare$mod_chla[chla_compare$DateTime >= "2021-01-01"],
                          chla_compare$obs_chla[chla_compare$DateTime >= "2021-01-01"],
                          do.spearman = TRUE), NA)

#create ranked dfs for nonparametric R2 calcs
comb_temp_rank <- watertemp |>  
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_temp),
         rank_mod = rank(mod_temp)) 
comb_oxy_rank <- oxy_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_oxy),
         rank_mod = rank(mod_oxy)) 
comb_dic_rank <- dic_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_dic),
         rank_mod = rank(mod_dic)) 
comb_ch4_rank <- ch4_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_ch4),
         rank_mod = rank(mod_ch4)) 
comb_nh4_rank <- nh4_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_nh4),
         rank_mod = rank(mod_nh4)) 
comb_no3_rank <- no3_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_no3),
         rank_mod = rank(mod_no3)) 
comb_po4_rank <- po4_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_po4),
         rank_mod = rank(mod_po4)) 
comb_docr_rank <- docr_compare |>  
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_docr),
         rank_mod = rank(mod_docr)) 
comb_doc_rank <- doc_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_doc),
         rank_mod = rank(mod_doc)) 
comb_chla_rank <- chla_compare |> 
  filter(DateTime >= "2021-01-01") |> 
  mutate(rank_obs = rank(obs_chla),
         rank_mod = rank(mod_chla)) 

# calculate non-parametric (ranked) R2, following Brett et al. 2016
all_gof_val$Temp[22] <- summary(lm(comb_temp_rank$rank_obs ~ comb_temp_rank$rank_mod))$r.squared
all_gof_val$DO[22] <- summary(lm(comb_oxy_rank$rank_obs ~ comb_oxy_rank$rank_mod))$r.squared
all_gof_val$DIC[22] <- summary(lm(comb_dic_rank$rank_obs ~ comb_dic_rank$rank_mod))$r.squared
all_gof_val$CH4[22] <- summary(lm(comb_ch4_rank$rank_obs ~ comb_ch4_rank$rank_mod))$r.squared
all_gof_val$NH4[22] <- summary(lm(comb_nh4_rank$rank_obs ~ comb_nh4_rank$rank_mod))$r.squared
all_gof_val$NO3[22] <- summary(lm(comb_no3_rank$rank_obs ~ comb_no3_rank$rank_mod))$r.squared
all_gof_val$PO4[22] <- summary(lm(comb_po4_rank$rank_obs ~ comb_po4_rank$rank_mod))$r.squared
all_gof_val$DOCr[22] <- summary(lm(comb_docr_rank$rank_obs ~ comb_docr_rank$rank_mod))$r.squared
all_gof_val$DOC[22] <- summary(lm(comb_doc_rank$rank_obs ~ comb_doc_rank$rank_mod))$r.squared
all_gof_val$Chla[22] <- summary(lm(comb_chla_rank$rank_obs ~ comb_chla_rank$rank_mod))$r.squared

####Cleaning up table ####
## Add NMAE calculation for all parameters
# all_gof
all_gof[nrow(all_gof)+1,] <- NA
all_gof[23,1] <- "NMAE_all"
all_gof$Parameter[21] <- "r.Spearman_all"
all_gof$Temp[23] <- round(all_gof$Temp[2]/mean(watertemp$obs_temp, na.rm=T),digits = 2)
all_gof$DO[23] <- round(all_gof$DO[2]/mean(oxy_compare$obs_oxy, na.rm=T),digits = 2)
all_gof$DIC[23] <- round(all_gof$DIC[2]/mean(dic_compare$obs_dic, na.rm=T),digits = 2)
all_gof$CH4[23] <- round(all_gof$CH4[2]/mean(ch4_compare$obs_ch4, na.rm=T),digits = 2)
all_gof$NH4[23] <- round(all_gof$NH4[2]/mean(nh4_compare$obs_nh4, na.rm=T),digits = 2)
all_gof$NO3[23] <- round(all_gof$NO3[2]/mean(no3_compare$obs_no3, na.rm=T),digits = 2)
all_gof$PO4[23] <- round(all_gof$PO4[2]/mean(po4_compare$obs_po4, na.rm=T),digits = 2)
all_gof$DOCr[23] <- round(all_gof$DOCr[2]/mean(docr_compare$obs_docr, na.rm=T),digits = 2)
all_gof$DOC[23] <- round(all_gof$DOC[2]/mean(doc_compare$obs_doc, na.rm=T),digits = 2)
all_gof$Chla[23] <- round(all_gof$Chla[2]/mean(chla_compare$obs_chla, na.rm=T),digits = 2)

# all_gof_cal
all_gof_cal[nrow(all_gof_cal)+1,] <- NA
all_gof_cal[23,1] <- "NMAE_cal"
all_gof_cal$Parameter[21] <- "r.Spearman_cal"
all_gof_cal$Temp[23] <- round(all_gof_cal$Temp[2]/mean(
  watertemp$obs_temp[watertemp$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$DO[23] <- round(all_gof_cal$DO[2]/mean(
  oxy_compare$obs_oxy[oxy_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$DIC[23] <- round(all_gof_cal$DIC[2]/mean(
  dic_compare$obs_dic[dic_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$CH4[23] <- round(all_gof_cal$CH4[2]/mean(
  ch4_compare$obs_ch4[ch4_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$NH4[23] <- round(all_gof_cal$NH4[2]/mean(
  nh4_compare$obs_nh4[nh4_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$NO3[23] <- round(all_gof_cal$NO3[2]/mean(
  no3_compare$obs_no3[no3_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$PO4[23] <- round(all_gof_cal$PO4[2]/mean(
  po4_compare$obs_po4[po4_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$DOCr[23] <- round(all_gof_cal$DOCr[2]/mean(
  docr_compare$obs_docr[docr_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$DOC[23] <- round(all_gof_cal$DOC[2]/mean(
  doc_compare$obs_doc[doc_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)
all_gof_cal$Chla[23] <- round(all_gof_cal$Chla[2]/mean(
  chla_compare$obs_chla[chla_compare$DateTime < "2021-01-01"], na.rm=T),digits = 2)

# all_gof_val
all_gof_val[nrow(all_gof_val)+1,] <- NA
all_gof_val[23,1] <- "NMAE_val"
all_gof_val$Parameter[21] <- "r.Spearman_val"
all_gof_val$Temp[23] <- round(all_gof_val$Temp[2]/mean(
  watertemp$obs_temp[watertemp$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$DO[23] <- round(all_gof_val$DO[2]/mean(
  oxy_compare$obs_oxy[oxy_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$DIC[23] <- round(all_gof_val$DIC[2]/mean(
  dic_compare$obs_dic[dic_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$CH4[23] <- round(all_gof_val$CH4[2]/mean(
  ch4_compare$obs_ch4[ch4_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$NH4[23] <- round(all_gof_val$NH4[2]/mean(
  nh4_compare$obs_nh4[nh4_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$NO3[23] <- round(all_gof_val$NO3[2]/mean(
  no3_compare$obs_no3[no3_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$PO4[23] <- round(all_gof_val$PO4[2]/mean(
  po4_compare$obs_po4[po4_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$DOCr[23] <- round(all_gof_val$DOCr[2]/mean(
  docr_compare$obs_docr[docr_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$DOC[23] <- round(all_gof_val$DOC[2]/mean(
  doc_compare$obs_doc[doc_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)
all_gof_val$Chla[23] <- round(all_gof_val$Chla[2]/mean(
  chla_compare$obs_chla[chla_compare$DateTime >= "2021-01-01"], na.rm=T),digits = 2)

# Select GOF variables for the full year
full_n_all <- c("n_all",length(obstemp$temp), length(obs_oxy$OXY_oxy),
                length(obs_dic$CAR_dic), length(obs_ch4$CAR_ch4),
                length(obs_nh4$NIT_amm), length(obs_no3$NIT_nit),
                length(obs_po4$PHS_frp), length(obs_docr$OGM_docr),
                length(obs_doc$OGM_doc), length(obs_chla$PHY_tchla))

full_n_cal <- c("n_cal",length(obstemp$DateTime[which(obstemp$DateTime < "2021-01-01")]),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime < "2021-01-01")]),
                length(obs_dic$DateTime[which(obs_dic$DateTime < "2021-01-01")]),
                length(obs_ch4$DateTime[which(obs_ch4$DateTime < "2021-01-01")]),
                length(obs_nh4$DateTime[which(obs_nh4$DateTime < "2021-01-01")]),
                length(obs_no3$DateTime[which(obs_no3$DateTime < "2021-01-01")]),
                length(obs_po4$DateTime[which(obs_po4$DateTime < "2021-01-01")]),
                length(obs_docr$DateTime[which(obs_docr$DateTime < "2021-01-01")]),
                length(obs_doc$DateTime[which(obs_doc$DateTime < "2021-01-01")]),
                length(obs_chla$DateTime[which(obs_chla$DateTime < "2021-01-01")]))

full_n_val <- c("n_val",length(obstemp$DateTime[which(obstemp$DateTime >= "2021-01-01")]),
                length(obs_oxy$DateTime[which(obs_oxy$DateTime >= "2021-01-01")]),
                length(obs_dic$DateTime[which(obs_dic$DateTime >= "2021-01-01")]),
                length(obs_ch4$DateTime[which(obs_ch4$DateTime >= "2021-01-01")]),
                length(obs_nh4$DateTime[which(obs_nh4$DateTime >= "2021-01-01")]),
                length(obs_no3$DateTime[which(obs_no3$DateTime >= "2021-01-01")]),
                length(obs_po4$DateTime[which(obs_po4$DateTime >= "2021-01-01")]),
                length(obs_docr$DateTime[which(obs_docr$DateTime >= "2021-01-01")]),
                length(obs_doc$DateTime[which(obs_doc$DateTime >= "2021-01-01")]),
                length(obs_chla$DateTime[which(obs_chla$DateTime >= "2021-01-01")]))

full_gof_all_table <- all_gof %>% 
  filter(Parameter == "r.Spearman_all" | Parameter == "R2_all" | Parameter == "RMSE_all" | Parameter == "PBIAS%_all" | Parameter == "NMAE_all")

full_gof_cal_table <- all_gof_cal %>% 
  filter(Parameter == "r.Spearman_cal" | Parameter == "R2_cal" | Parameter == "RMSE_cal" | Parameter == "PBIAS%_cal" | Parameter == "NMAE_cal")

full_gof_val_table <- all_gof_val %>% 
  filter(Parameter == "r.Spearman_val" | Parameter == "R2_val" | Parameter == "RMSE_val" | Parameter == "PBIAS%_val" | Parameter == "NMAE_val")

full_gof_table <- rbind(full_n_all,full_gof_all_table,full_n_cal,full_gof_cal_table,full_n_val,full_gof_val_table)

write_csv(full_gof_table,'figures/table_gof_watercol_bvr_2015-2022.csv')
