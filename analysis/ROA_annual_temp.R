# code to visualize annual trend in air temperature at Roanoke weather station
# https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series/USW00013741/tavg/12/12/1948-2025?filter=true&filterType=loess 

noaa_roa_temp <- read_csv("analysis/data/ROA_annual_temp.csv",skip=3) |>
  mutate(Date = as.numeric(substr(Date,1,4))) |>
  rename(airtemp = Value)
  
ggplot(noaa_roa_temp, aes(x = Date, y = airtemp)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x="", y = "Mean Air Temperature (°C)") +
  theme_minimal()
ggsave("figures/summer_airtemp.jpg", width=8, height=6)

model <- lm(airtemp ~ Date, data = noaa_roa_temp)
summary(model)
