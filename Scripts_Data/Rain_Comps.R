### Script to compare precipitation: USGS Roanoke Gage, FCR Precip gage, NLDAS data
### A Hounshell, 27 Aug 2020

# Load libraries
pacman::p_load(tidyverse,ggplot2,zoo)

# Load data: USGS Roanoke Gage (from Heather); downloaded NLDAS data; FCR precip (from EDI)
usgs <- read_csv("./Raw_Data/USGS_precip_2013-2019.csv")
usgs$mdate <- as.POSIXct(strptime(usgs$mdate, "%m/%d/%Y", tz = "EST"))

nldas <- read_csv('./inputs/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv')
nldas$time <- as.POSIXct(strptime(nldas$time, "%Y-%m-%d", tz = "EST"))

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/4/c1db8816742823eba86696b29f106d0f" 
infile1 <- paste0(getwd(),"/Met_final_2015_2019.csv")
download.file(inUrl1,infile1,method="curl")

fcr <- read.csv("Met_final_2015_2019.csv", header=T)
fcr$DateTime <- as.POSIXct(strptime(fcr$DateTime, "%Y-%m-%d", tz = "EST"))

# Average nldas data across days (mm/d)
# NOTE: Needed to convert NLDAS data to hourly rain by multiplying by 'uncoverting' from m/day to mm/d
nldas_rain <- nldas %>% select(time,Rain) %>% mutate(Rain_hr = Rain*1000/24) %>% 
  group_by(time) %>% summarise_all(funs(sum))

nldas_avg <- nldas %>% select(time,AirTemp,ShortWave,LongWave) %>%
  group_by(time) %>% summarise_all(funs(mean))

# Average FCR data across days (mm/d)
fcr_rain <- fcr %>% select(DateTime,Rain_Total_mm) %>% group_by(DateTime) %>% summarise_all(funs(sum))
fcr_avg <- fcr %>% select(DateTime, AirTemp_Average_C,ShortwaveRadiationUp_Average_W_m2,InfaredRadiationUp_Average_W_m2) %>%
                   group_by(DateTime) %>% summarise_all(funs(mean))

#NLDAS vs fcr met
nldas_rain_sub <- nldas_rain$Rain[match(fcr_rain$DateTime,nldas_rain$time)]
nldas_airtemp <- nldas_avg$AirTemp[match(fcr_avg$DateTime,nldas_avg$time)]
nldas_shortwave <- nldas_avg$ShortWave[match(fcr_avg$DateTime,nldas_avg$time)]
nldas_longwave <- nldas_avg$LongWave[match(fcr_avg$DateTime,nldas_avg$time)]

plot(nldas_rain_sub, fcr_rain$Rain_Total_mm)
plot(nldas_airtemp, fcr_avg$AirTemp_Average_C)
plot(nldas_shortwave, fcr_avg$ShortwaveRadiationUp_Average_W_m2)
plot(nldas_longwave, fcr_avg$InfaredRadiationUp_Average_W_m2)

cor.test(nldas_rain_sub,fcr_rain$Rain_Total_mm)
cor.test(nldas_airtemp, fcr_avg$AirTemp_Average_C)
cor.test(nldas_shortwave, fcr_avg$ShortwaveRadiationUp_Average_W_m2)
cor.test(nldas_longwave, fcr_avg$InfaredRadiationUp_Average_W_m2)

# Plot them all
ggplot()+
  geom_line(usgs,mapping=aes(x=mdate,y=P_inperd,color="USGS"))+
  geom_line(nldas_rain,mapping=aes(x=time,y=Rain_hr,color="NLDAS"))+
  geom_line(fcr_rain,mapping=aes(x=DateTime,y=Rain_Total_mm,color="FCR Met"))+
  theme_classic(base_size = 15)

# Merge all by date
usgs_date <- usgs %>% select(mdate,P_inperd) %>% rename(date=mdate,usgs_mm=P_inperd)
nldas_date <- nldas_rain %>% select(time,Rain_hr) %>% rename(date=time,nldas_mm=Rain_hr)
fcr_date <- fcr_rain %>% rename(date=DateTime,fcr_mm=Rain_Total_mm)

all <- merge(usgs_date,nldas_date,by="date")
all <- merge(all,fcr_date,by="date")

ggplot()+
  geom_line(all,mapping=aes(x=date,y=usgs_mm,color="USGS"))+
  geom_line(all,mapping=aes(x=date,y=nldas_mm,color="NLDAS"))+
  geom_line(all,mapping=aes(x=date,y=fcr_mm,color="FCR"))+
  theme_classic(base_size=15)

ggplot(all,mapping=aes(x=fcr_mm,y=usgs_mm))+
  geom_point()+
  theme_classic(base_size=15)

ggplot(all,mapping=aes(x=fcr_mm,y=nldas_mm))+
  geom_point()+
  theme_classic(base_size=15)
