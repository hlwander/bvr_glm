#trying to hand-tune ch4

#look at glm and aed nml files
nml_file <- paste0(sim_folder,"/16Mar23_ch4cal_glm3.nml")
aed_file <- paste0(sim_folder,"/aed/16Mar23_ch4cal_aed2.nml")
aed_phytos_file <- paste0(sim_folder,"/aed/aed2_phyto_pars_2May2022_RQT.nml")
nml <- read_nml(nml_file) 
aed <- read_nml(aed_file) #you may get a warning about an incomplete final line but it doesn't matter
aed_phytos <- read_nml(aed_phytos_file)
print(nml)
print(aed)
print(aed_phytos)

#file.copy('22Mar23_ch4cal_glm3.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed/22Mar23_ch4cal_aed2.nml', 'aed/aed2.nml', overwrite = TRUE)

#file.copy('Hipsey_glm3_rqt.nml', 'glm3.nml', overwrite = TRUE)
#file.copy('aed/Hipsey_aed2_rqt.nml', 'aed/aed2.nml', overwrite = TRUE)

file.copy('15Jul23_diccal_glm3.nml', 'glm3.nml', overwrite = TRUE)
file.copy('aed/15Jul23_diccal_aed2.nml', 'aed/aed2.nml', overwrite = TRUE)

#run the model!
system2(paste0(sim_folder,"/glm+.app/Contents/MacOS/glm+"), stdout = TRUE, stderr = TRUE, env = paste0("DYLD_LIBRARY_PATH=",sim_folder, "/glm+.app/Contents/MacOS"))
# Above from CCC

#sometimes, you'll get an error that says "Error in file, 'Time(Date)' is not first column!
#in this case, open the input file in Excel, set the column in Custom ("YYYY-MM-DD") format, resave, and close the file
nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 


#######################################################
var='CAR_ch4'

obs<-read.csv('field_data/field_gases.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,50)) #compare obs vs modeled

#get modeled concentrations for focal depths
depths<- sort(as.numeric(unique(obs$Depth)))

mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  mutate(Depth=as.numeric(Depth)) %>%
  na.omit()

#obs_o2 <- read.csv('field_data/CleanedObsOxy.csv', header=TRUE) %>% #read in observed chemistry data
#  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#  select(DateTime, Depth, "OXY_oxy") %>%
#  na.omit()

#ggplot(subset(mod, Depth==9), aes(DateTime, OGM_docr)) + geom_point() + theme_bw() + ggtitle("9 m")
#ggplot(subset(mod, Depth==4), aes(DateTime, SIL_rsi)) + geom_point() + theme_bw() + ggtitle("4 m")
#ggplot(subset(mod, Depth==8), aes(DateTime, SIL_rsi)) + geom_point() + theme_bw() + ggtitle("8 m")
#ggplot(subset(mod, Depth==12), aes(DateTime, SIL_rsi)) + geom_point() + theme_bw() + ggtitle("12 m")

#lets do depth by depth comparisons of the sims
compare<-merge(mod, obs, by=c("DateTime","Depth"))
compare<-na.omit(compare)
for(i in 1:length(depths)){
  tempdf<-subset(compare, compare$Depth==depths[i])
  if(nrow(tempdf)>1){
    plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
         ylab=var, xlab='time', pch=19,
         main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
    points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
  }
}

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

field_file<-file.path(sim_folder,'field_data/field_gases.csv')

temps <- resample_to_field(nc_file, field_file, precision="days", method='interp',
                           var_name=var)
temps<-temps[complete.cases(temps),]

RMSE(temps[temps$Depth==c(9),4],
     temps[temps$Depth==c(9),3])

RMSE(temps[temps$Depth==c(0.1),4],
     temps[temps$Depth==c(0.1),3])

RMSE(temps$Modeled_CAR_ch4, temps$Observed_CAR_ch4)

#------------------------------------------------------------------------------#
#fit Michaelis-Menten function to data
#library(renz)
#
#df_comb <- data.frame("x"=obs_o2$OXY_oxy[obs_o2$Depth==9][1:114], "y"=obs$PHS_frp[obs$Depth==9])
#
#df_comb_test <- data.frame("x"=obs_o2$OXY_oxy[obs_o2$Depth==9][c(9:114)], "y"=obs$PHS_frp[obs$Depth==9][c(9:114)])
#
#mm <- dir.MM(df_comb)
#
#ggplot(df_comb_test, aes(x = x, y = y)) +
#  theme_bw() +
#  xlab("Hypo DO") +
#  ylab("Hypo P") +
#  ggtitle("Michaelis-Menten kinetics") +
#  geom_point(alpha = 0.5) +
#  xlim(0,10) +
#  geom_line(data = mm$data, aes(x = S, y = fitted_v), colour = "red")
#ksed_po4 range is proba between 1.7-2.4

#------------------------------------------------------------------------------#
#plot distribution of 9m temp obs

var='temp'

obs<-read.csv('field_data/CleanedObsTemp.csv', header=TRUE) %>% #read in observed chemistry data
  dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
  select(DateTime, Depth, var) %>%
  na.omit()

#add in a year col
obs$year <- year(obs$DateTime)

x <- hist(obs$temp[obs$Depth==9])
median(obs$temp[obs$Depth==9])
range(obs$temp[obs$Depth==9])
mean(obs$temp[obs$Depth==9])

plot(obs$temp[obs$Depth==9]~ obs$DateTime[obs$Depth==9])

#create new df of 9m temp
temp_9m <- obs[obs$Depth==9,]

temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2015])]    # 23 Oct 2015
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2016])]    # 11 Nov 2016
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2017])][1] # 7 Nov 2017
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2018])]    # 2 Nov 2018
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2019])]    # 6 Jun 2019
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2020])][1] # 4 Jun 2020
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2021])]    # 9 Aug 2021
temp_9m$DateTime[temp_9m$temp == max(temp_9m$temp[temp_9m$year==2022])]

#range of temps each year --> avg range = 10.9
range(temp_9m$temp[temp_9m$year==2015]) # 1.5
range(temp_9m$temp[temp_9m$year==2016]) # 6.2
range(temp_9m$temp[temp_9m$year==2017]) # 6.4
range(temp_9m$temp[temp_9m$year==2018]) # 6.5
range(temp_9m$temp[temp_9m$year==2019]) # 18.7
range(temp_9m$temp[temp_9m$year==2020]) # 15.5
range(temp_9m$temp[temp_9m$year==2021]) # 21.4

#----------------------------------------------------------------------#
#look at inflow data to see why modeled temp peak looks so weird

inflow <- read.csv('inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.99X.csv')
outflow <- read.csv('inputs/BVR_spillway_outflow_2015_2022_metInflow_0.9917X.csv')

plot(as.Date(inflow$time), inflow$FLOW)
points(as.Date(outflow$time), outflow$FLOW, col="blue")

plot(as.Date(inflow$time), inflow$TEMP)

met <- read.csv('inputs/met.csv')

plot(as.Date(met$time),met$AirTemp)
plot(as.Date(met$time),met$ShortWave)
plot(as.Date(met$time),met$LongWave)
plot(as.Date(met$time),met$RelHum)
plot(as.Date(met$time),met$WindSpeed)
plot(as.Date(met$time),met$Rain)

#looking at water temp obs just to make sure things are okay
temp <- read.csv('field_data/CleanedObsTemp.csv')

ggplot(temp, aes(DateTime, temp, color=as.factor(Depth))) + geom_point() +
  theme_bw() + facet_wrap(~Depth)

