# code to visualize annual trend in air temperature at Roanoke weather station and FCR met station
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series/USW00013741/tavg/12/12/1948-2025?filter=true&filterType=loess 

noaa_roa_temp <- read_csv("analysis/data/ROA_annual_temp.csv",skip=3) |>
  mutate(Date = as.numeric(substr(Date,1,4)),
         Value = (Value - 32) * 5/9) |> # to convert to deg C
  rename(airtemp = Value) 
  
ggplot(noaa_roa_temp, aes(x = Date, y = airtemp)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x="", y = "Mean Air Temperature (°C)") +
  theme_minimal()
ggsave("figures/summer_airtemp.jpg", width=8, height=6)

model <- lm(airtemp ~ Date, data = noaa_roa_temp)
summary(model)


#quick look at fcr air temp

fcr_met1 <- read_csv("sims/spinup/baseline/inputs/met.csv") |>
  filter(as.Date(time) >= "2017-10-01" & as.Date(time) <= "2018-02-28") |>
  select(time, AirTemp) |>
  mutate(month_day = if_else(month(time) %in% c(10,11,12), 
                             as.Date(format(time, "1999-%m-%d")),
                             as.Date(format(time, "2000-%m-%d"))))

fcr_met2 <- read_csv("sims/spinup/baseline/inputs/met.csv") |>
  filter(as.Date(time) >= "2019-10-01" & as.Date(time) <= "2020-02-28") |>
  select(time, AirTemp) |>
  mutate(month_day = if_else(month(time) %in% c(10,11,12), 
                             as.Date(format(time, "1999-%m-%d")),
                             as.Date(format(time, "2000-%m-%d"))))
  
  ggplot() +
  geom_point(data=fcr_met1, aes(month_day,AirTemp,color="2017-2018")) +
  geom_point(data=fcr_met2, aes(month_day, AirTemp,color="2019-2020")) +
  labs(x="", y = "Mean Air Temperature (°C)") +
  scale_color_manual(values = c("2017-2018" = "red", "2019-2020" = "blue")) +
  scale_x_date(date_labels = "%b") + 
  theme_minimal()
ggsave("figures/winter_airtemp.jpg", width=8, height=6)
  
mean(fcr_met2$AirTemp[month(fcr_met2$time) %in% c(12,1,2)])
mean(fcr_met1$AirTemp[month(fcr_met1$time) %in% c(12,1,2)])
