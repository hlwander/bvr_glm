#visualize projected temp in Roanoke/vinton
#CCSM4 max temp projection under RCP 8.5 and 6.5

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
