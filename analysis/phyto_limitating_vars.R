# Script to identify limiting vars for phyto groups
# 17 Sep 2024 - adapted from CCC

#### phyto succession ####

scenario <- c("baseline","plus1","plus3","plus5")

for(j in 1:length(scenario)) {
  
  nc_file = paste0("sims/",scenario[j],"/output/output.nc")  
  
phytos <- c('cyano','green','diatom')
limits <- c("fNit","fI","fPho","fSil","fT","fSal")
vars <- expand.grid(phytos,limits) |> 
  rename(phyto = Var1, limit = Var2)
mod <- NULL
for(i in 1:nrow(vars)){
  var <- paste("PHY",vars[i,1],vars[i,2],sep="_")
  tmp<- get_var(nc_file, var, reference="surface", z_out=depths) |> 
    pivot_longer(cols=starts_with(paste0(var,"_")), 
                 names_to="Depth", names_prefix=paste0(var,"_"), 
                 values_to = "value") |> 
    mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
    mutate(Depth=as.numeric(Depth)) |> 
    na.omit() |> 
    mutate(phyto = vars[i,1],
           limit = vars[i,2])
  mod <- rbind(mod, tmp)
}

assign(paste0("mod_", scenario[j]), mod)

#phyto_biomass <- NULL
#for(i in 1:length(phytos)){
#  var <- paste("PHY",phytos[i],sep="_")
#  tmp<- get_var(nc_file, var, reference="surface", z_out=depths) |> 
#    pivot_longer(cols=starts_with(paste0(var,"_")), 
#                 names_to="Depth", names_prefix=paste0(var,"_"), 
#                 values_to = "value") |> 
#    mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#    mutate(Depth=as.numeric(Depth)) |> 
#    na.omit() |>  
#    mutate(phyto = phytos[i])
#  phyto_biomass <- rbind(phyto_biomass, tmp)
#}
#
#assign(paste0("phyto_biomass_", scenario[j]), phyto_biomass)
#
#
#phytos <- c('cyano_gpp_c','green_gpp_c','diatom_gpp_c')
#phyto_prod <- NULL
#for(i in 1:length(phytos)){
#  var <- paste("PHY",phytos[i],sep="_")
#  tmp<- get_var(nc_file, var, reference="surface", z_out=depths) |> 
#    pivot_longer(cols=starts_with(paste0(var,"_")), 
#                 names_to="Depth", names_prefix=paste0(var,"_"), 
#                 values_to = "value") |> 
#    mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#    mutate(Depth=as.numeric(Depth)) |> 
#    na.omit() |> 
#    mutate(phyto = phytos[i])
#  phyto_prod <- rbind(phyto_prod, tmp)
#}
#
#assign(paste0("phyto_prod_", scenario[j]), phyto_prod)

#var="PHY_tchla"
#obs<-read.csv('field_data/CleanedObsChla.csv', header=TRUE) |> 
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#  rename(PHY_tchla = PHY_tchla) |> 
#  dplyr::select(DateTime, Depth, var) |> 
#  na.omit() |>  
#  dplyr::filter(Depth == 1.0)
#
#assign(paste0("obs_", scenario[j]), obs)
#
#modeled <- get_var(nc_file, var, reference="surface", z_out=depths) |> 
#  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", 
#               names_prefix=paste0(var,"_"), values_to = var) |> 
#  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#  mutate(Depth=as.numeric(Depth)) |> 
#  na.omit() |> 
#  dplyr::filter(Depth == 1.0)
#
#assign(paste0("modeled_", scenario[j]), modeled)

mod2 <- mod |>  pivot_wider(names_from = limit, values_from = value) |>  
  mutate(R_growth = ifelse(phyto == "cyano", 1.0, 1.4),
         R_growth = ifelse(phyto == "green", 3.0, R_growth)) |> 
  rowwise() |> 
  mutate(nutrients = min(c(fI,fPho,fSil,fNit)),
         combined = fT * nutrients,
         prod = combined * R_growth) |> 
  select(-fSal) |> 
  pivot_longer(-c("DateTime","Depth", "phyto"), 
               names_to = "limit", values_to = "value")

assign(paste0("mod2_", scenario[j]), mod2)
}

#plot limiting phyto var for each scenario
mod2_baseline |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0) |> 
  ggplot() + xlab("Baseline") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

mod2_plus1 |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0) |> 
  ggplot() + xlab("Plus 1C") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

mod2_plus3 |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0) |> 
  ggplot() + xlab("Plus 3C") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

mod2_plus5 |> 
  dplyr::filter(limit %in% c("fI","fPho","fSil","fNit","fT")) |> 
  dplyr::filter(Depth == 1.0) |> 
  ggplot() + xlab("Plus 5C") +
  geom_line(aes(x = DateTime, y = value, color = limit)) +
  facet_wrap(~phyto, nrow = 3) +
  theme_bw()

#baseline
#ggplot() + 
#  geom_line() +
#  theme_bw() +
#  geom_point(data = obs_baseline, aes(x = DateTime, y = PHY_tchla), color = "gray") +
#  geom_line(data = modeled_baseline, aes(x = DateTime, y = PHY_tchla))

