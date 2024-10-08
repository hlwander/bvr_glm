#visualize projected temp in Roanoke/vinton
#CCSM4 max temp projection under RCP 8.5 and 4.5

pacman::p_load(tidyverse)

future_temp <- read.csv("temp_scenarios/maxtemp_rcp8.5.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_rcp85.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 51.3C in 2098

#max temp for just 2020-2050 = 44.6C
max(future_temp$temp[future_temp$date>="2020-01-01" &
                          future_temp$date<="2051-01-01"])

future_temp_rcp4.5 <- read.csv("temp_scenarios/maxtemp_rcp4.5.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_rcp45.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_rcp4.5, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 46.2C in 2057

#vs historical max temp from 1950-2005
historical_temp <- read.csv("temp_scenarios/historical_temp.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_CCSM4_historical.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#plot historical max temps
ggplot(historical_temp, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 39.9C in 1955

#-----------------------------------------------------------------------------#
# Looking at a different model to confirm results (37.313, -79.816)

# GFDL-ESM2M (USA)

future_temp_esm2m <- read.csv("temp_scenarios/maxtemp_rcp8.5_GFDL-ESM2M.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2M_rcp85.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_esm2m, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 49.4C in 2072

#max temp for just 2020-2050 = 41.8C
max(future_temp_esm2m$temp[future_temp_esm2m$date>="2020-01-01" &
                             future_temp_esm2m$date<="2051-01-01"])

future_temp_rcp4.5_esm2m <- read.csv("temp_scenarios/maxtemp_rcp4.5_GFDL-ESM2M.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2M_rcp45.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_rcp4.5_esm2m, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 46.1C in 2069


# GFDL-ESM2G (USA)

future_temp_esm2g <- read.csv("temp_scenarios/maxtemp_rcp8.5_GFDL-ESM2G.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2G_rcp85.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_esm2g, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 48.3C in 2093

#max temp for just 2020-2050 = 42.3C
max(future_temp_esm2g$temp[future_temp_esm2g$date>="2020-01-01" &
                             future_temp_esm2g$date<="2051-01-01"])

future_temp_rcp4.5_esm2g <- read.csv("temp_scenarios/maxtemp_rcp4.5_GFDL-ESM2G.csv", skip=8) |> 
  rename(date = yyyy.mm.dd,
         temp = tasmax_GFDL.ESM2G_rcp45.K.) |> 
  mutate(temp = temp - 273.15,
         date = as.Date(date))

#quick plot of max temps from 2006-2099
ggplot(future_temp_rcp4.5_esm2g, aes(date, temp)) + geom_line() +
  theme_bw()
#max temp is 46.8C in 2037



