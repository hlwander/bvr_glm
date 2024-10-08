# Scenario diagnostics - why is +2C stimulating algal growth to the point of light limitation???

secchi_obs <- read.csv("./field_data/field_secchi.csv")

#plot Secchi depth & light extinction
lec_norm <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')
lec_1C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')
lec_2C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')
lec_3C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')
lec_5C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')

jpeg('figures/secchi_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(lec_norm$DateTime, 1.7/lec_norm$extc_1, ylim=c(0.5,5),type="l", 
     ylab="Secchi (m)", xlab="")
points(as.POSIXct(secchi_obs$DateTime), secchi_obs$Secchi_m, col="red", type="p",pch=16)
points(as.POSIXct(lec_1C$DateTime), 1.7/lec_1C$extc_1, col="#5B8E7D", type="l")
points(as.POSIXct(lec_2C$DateTime), 1.7/lec_2C$extc_1, col="#F4E285", type="l")
points(as.POSIXct(lec_3C$DateTime), 1.7/lec_3C$extc_1, col="#F4A259", type="l")
points(as.POSIXct(lec_5C$DateTime), 1.7/lec_5C$extc_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

#plot zooming in on the scenarios
jpeg('figures/secchi_scenarios_no_obs.jpg',width=5, height=4, units="in", res=500)
plot(as.POSIXct(lec_1C$DateTime), 1.7/lec_1C$extc_1, col="#5B8E7D", type="l",
     ylab="Secchi (m)", xlab="")
points(as.POSIXct(lec_2C$DateTime), 1.7/lec_2C$extc_1, col="#F4E285", type="l")
points(as.POSIXct(lec_3C$DateTime), 1.7/lec_3C$extc_1, col="#F4A259", type="l")
points(as.POSIXct(lec_5C$DateTime), 1.7/lec_5C$extc_1, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

#see what vars are available for diagnostics
sim_vars(file = nc_file)

#plot light vars (daily_qe, daily_qh, daily_qlw, daily_qsw, extc, light, 
#par, radn, solar, TOT_light, TOT_par, TOT_uv)

#daily qe
qe_norm <- get_var(file=nc_file,var_name = 'daily_qe',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qe_1C <- get_var(file=nc_file,var_name = 'daily_qe',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qe_2C <- get_var(file=nc_file,var_name = 'daily_qe',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qe_3C <- get_var(file=nc_file,var_name = 'daily_qe',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qe_5C <- get_var(file=nc_file,var_name = 'daily_qe',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#daily_qh
qh_norm <- get_var(file=nc_file,var_name = 'daily_qh',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qh_1C <- get_var(file=nc_file,var_name = 'daily_qh',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qh_2C <- get_var(file=nc_file,var_name = 'daily_qh',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qh_3C <- get_var(file=nc_file,var_name = 'daily_qh',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qh_5C <- get_var(file=nc_file,var_name = 'daily_qh',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#daily qlw
qlw_norm <- get_var(file=nc_file,var_name = 'daily_qlw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qlw_1C <- get_var(file=nc_file,var_name = 'daily_qlw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qlw_2C <- get_var(file=nc_file,var_name = 'daily_qlw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qlw_3C <- get_var(file=nc_file,var_name = 'daily_qlw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qlw_5C <- get_var(file=nc_file,var_name = 'daily_qlw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#daily qsw
qsw_norm <- get_var(file=nc_file,var_name = 'daily_qsw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qsw_1C <- get_var(file=nc_file,var_name = 'daily_qsw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qsw_2C <- get_var(file=nc_file,var_name = 'daily_qsw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qsw_3C <- get_var(file=nc_file,var_name = 'daily_qsw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
qsw_5C <- get_var(file=nc_file,var_name = 'daily_qsw',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#extc
extc_norm <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
extc_1C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
extc_2C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
extc_3C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
extc_5C <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#light
light_norm <- get_var(file=nc_file,var_name = 'light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
light_1C <- get_var(file=nc_file,var_name = 'light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
light_2C <- get_var(file=nc_file,var_name = 'light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
light_3C <- get_var(file=nc_file,var_name = 'light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
light_5C <- get_var(file=nc_file,var_name = 'light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#par
par_norm <- get_var(file=nc_file,var_name = 'PHY_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
par_1C <- get_var(file=nc_file,var_name = 'PHY_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
par_2C <- get_var(file=nc_file,var_name = 'PHY_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
par_3C <- get_var(file=nc_file,var_name = 'PHY_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
par_5C <- get_var(file=nc_file,var_name = 'PHY_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#radn
radn_norm <- get_var(file=nc_file,var_name = 'radn',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
radn_1C <- get_var(file=nc_file,var_name = 'radn',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
radn_2C <- get_var(file=nc_file,var_name = 'radn',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
radn_3C <- get_var(file=nc_file,var_name = 'radn',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
radn_5C <- get_var(file=nc_file,var_name = 'radn',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#solar
solar_norm <- get_var(file=nc_file,var_name = 'solar',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
solar_1C <- get_var(file=nc_file,var_name = 'solar',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
solar_2C <- get_var(file=nc_file,var_name = 'solar',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
solar_3C <- get_var(file=nc_file,var_name = 'solar',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
solar_5C <- get_var(file=nc_file,var_name = 'solar',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#TOT_light
tot_light_norm <- get_var(file=nc_file,var_name = 'TOT_light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
tot_light_1C <- get_var(file=nc_file,var_name = 'TOT_light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
tot_light_2C <- get_var(file=nc_file,var_name = 'TOT_light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
tot_light_3C <- get_var(file=nc_file,var_name = 'TOT_light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
tot_light_5C <- get_var(file=nc_file,var_name = 'TOT_light',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#TOT_par
TOT_par_norm <- get_var(file=nc_file,var_name = 'TOT_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_par_1C <- get_var(file=nc_file,var_name = 'TOT_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_par_2C <- get_var(file=nc_file,var_name = 'TOT_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_par_3C <- get_var(file=nc_file,var_name = 'TOT_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_par_5C <- get_var(file=nc_file,var_name = 'TOT_par',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)

#TOT_uv
TOT_uv_norm <- get_var(file=nc_file,var_name = 'TOT_uv',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_uv_1C <- get_var(file=nc_file,var_name = 'TOT_uv',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_uv_2C <- get_var(file=nc_file,var_name = 'TOT_uv',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_uv_3C <- get_var(file=nc_file,var_name = 'TOT_uv',z_out=1,reference = 'surface')|> 
  filter(hour(DateTime) %in% 00)
TOT_uv_5C <- get_var(file=nc_file,var_name = 'TOT_uv',z_out=1,reference = 'surface') |> 
  filter(hour(DateTime) %in% 00)

#----------------------------------------------------------------#
# now many light-related plots

jpeg('figures/daily_qe_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(qe_norm$DateTime, qe_norm$daily_qe, type="l", 
     ylab="daily qe", xlab="")
points(as.POSIXct(qe_1C$DateTime), qe_1C$daily_qe, col="#5B8E7D", type="l")
points(as.POSIXct(qe_2C$DateTime), qe_2C$daily_qe, col="#F4E285", type="l")
points(as.POSIXct(qe_3C$DateTime), qe_3C$daily_qe, col="#F4A259", type="l")
points(as.POSIXct(qe_5C$DateTime), qe_5C$daily_qe, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/daily_qh_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(qh_norm$DateTime, qh_norm$daily_qh, type="l", 
     ylab="daily qh", xlab="")
points(as.POSIXct(qh_1C$DateTime), qh_1C$daily_qh, col="#5B8E7D", type="l")
points(as.POSIXct(qh_2C$DateTime), qh_2C$daily_qh, col="#F4E285", type="l")
points(as.POSIXct(qh_3C$DateTime), qh_3C$daily_qh, col="#F4A259", type="l")
points(as.POSIXct(qh_5C$DateTime), qh_5C$daily_qh, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/daily_qlw_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(qlw_norm$DateTime, qlw_norm$daily_qlw,type="l", 
     ylab="daily qlw", xlab="")
points(as.POSIXct(qlw_1C$DateTime), qlw_1C$daily_qlw, col="#5B8E7D", type="l")
points(as.POSIXct(qlw_2C$DateTime), qlw_2C$daily_qlw, col="#F4E285", type="l")
points(as.POSIXct(qlw_3C$DateTime), qlw_3C$daily_qlw, col="#F4A259", type="l")
points(as.POSIXct(qlw_5C$DateTime), qlw_5C$daily_qlw, col="#BC4B51", type="l")
legend("bottom", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/daily_qsw_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(qsw_norm$DateTime, qsw_norm$daily_qsw,type="l", 
     ylab="daily qsw", xlab="")
points(as.POSIXct(qsw_1C$DateTime), qsw_1C$daily_qsw, col="#5B8E7D", type="l")
points(as.POSIXct(qsw_2C$DateTime), qsw_2C$daily_qsw, col="#F4E285", type="l")
points(as.POSIXct(qsw_3C$DateTime), qsw_3C$daily_qsw, col="#F4A259", type="l")
points(as.POSIXct(qsw_5C$DateTime), qsw_5C$daily_qsw, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/extc_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(extc_norm$DateTime, extc_norm$extc_1,type="l", 
     ylab="extc", xlab="")
points(as.POSIXct(extc_1C$DateTime), extc_1C$extc_1, col="#5B8E7D", type="l")
points(as.POSIXct(extc_2C$DateTime), extc_2C$extc_1, col="#F4E285", type="l")
points(as.POSIXct(extc_3C$DateTime), extc_3C$extc_1, col="#F4A259", type="l")
points(as.POSIXct(extc_5C$DateTime), extc_5C$extc_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/light_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(light_norm$DateTime, light_norm$light, type="l", 
     ylab="light", xlab="")
points(as.POSIXct(light_1C$DateTime), light_1C$light, col="#5B8E7D", type="l")
points(as.POSIXct(light_2C$DateTime), light_2C$light, col="#F4E285", type="l")
points(as.POSIXct(light_3C$DateTime), light_3C$light, col="#F4A259", type="l")
points(as.POSIXct(light_5C$DateTime), light_5C$light, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/phy_par_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(par_norm$DateTime, par_norm$PHY_par_1, type="l", 
     ylab="PHY_par", xlab="")
points(as.POSIXct(par_1C$DateTime), par_1C$PHY_par_1, col="#5B8E7D", type="l")
points(as.POSIXct(par_2C$DateTime), par_2C$PHY_par_1, col="#F4E285", type="l")
points(as.POSIXct(par_3C$DateTime), par_3C$PHY_par_1, col="#F4A259", type="l")
points(as.POSIXct(par_5C$DateTime), par_5C$PHY_par_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/radn_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(radn_norm$DateTime, radn_norm$radn_1, type="l", 
     ylab="radn", xlab="")
points(as.POSIXct(radn_1C$DateTime), radn_1C$radn_1, col="#5B8E7D", type="l")
points(as.POSIXct(radn_2C$DateTime), radn_2C$radn_1, col="#F4E285", type="l")
points(as.POSIXct(radn_3C$DateTime), radn_3C$radn_1, col="#F4A259", type="l")
points(as.POSIXct(radn_5C$DateTime), radn_5C$radn_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/solar_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(solar_norm$DateTime, solar_norm$solar, type="l", 
     ylab="solar", xlab="")
points(as.POSIXct(solar_1C$DateTime), solar_1C$solar, col="#5B8E7D", type="l")
points(as.POSIXct(solar_2C$DateTime), solar_2C$solar, col="#F4E285", type="l")
points(as.POSIXct(solar_3C$DateTime), solar_3C$solar, col="#F4A259", type="l")
points(as.POSIXct(solar_5C$DateTime), solar_5C$solar, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/tot_par_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(TOT_par_norm$DateTime, TOT_par_norm$TOT_par_1, type="l", 
     ylab="TOT_par", xlab="")
points(as.POSIXct(TOT_par_1C$DateTime), TOT_par_1C$TOT_par_1, col="#5B8E7D", type="l")
points(as.POSIXct(TOT_par_2C$DateTime), TOT_par_2C$TOT_par_1, col="#F4E285", type="l")
points(as.POSIXct(TOT_par_3C$DateTime), TOT_par_3C$TOT_par_1, col="#F4A259", type="l")
points(as.POSIXct(TOT_par_5C$DateTime), TOT_par_5C$TOT_par_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/tot_light_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(tot_light_norm$DateTime, tot_light_norm$TOT_light_1, type="l", 
     ylab="TOT_light", xlab="")
points(as.POSIXct(tot_light_1C$DateTime), tot_light_1C$TOT_light_1, col="#5B8E7D", type="l")
points(as.POSIXct(tot_light_2C$DateTime), tot_light_2C$TOT_light_1, col="#F4E285", type="l")
points(as.POSIXct(tot_light_3C$DateTime), tot_light_3C$TOT_light_1, col="#F4A259", type="l")
points(as.POSIXct(tot_light_5C$DateTime), tot_light_5C$TOT_light_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()

jpeg('figures/tot_uv_scenarios.jpg',width=5, height=4, units="in", res=500)
plot(TOT_uv_norm$DateTime, TOT_uv_norm$TOT_uv_1, type="l", 
     ylab="TOT_uv", xlab="")
points(as.POSIXct(TOT_uv_1C$DateTime), TOT_uv_1C$TOT_uv_1, col="#5B8E7D", type="l")
points(as.POSIXct(TOT_uv_2C$DateTime), TOT_uv_2C$TOT_uv_1, col="#F4E285", type="l")
points(as.POSIXct(TOT_uv_3C$DateTime), TOT_uv_3C$TOT_uv_1, col="#F4A259", type="l")
points(as.POSIXct(TOT_uv_5C$DateTime), TOT_uv_5C$TOT_uv_1, col="#BC4B51", type="l")
legend("top", legend=c("plus1C", "plus2C","plus3C","plus5C"),
       col=c("#5B8E7D", "#F4E285","#F4A259","#BC4B51"), 
       lty=1, cex=0.8, bty='n', horiz=T)
dev.off()
