#Model to relate bvr inflow to noaa air temp

# Load in libraries
pacman::p_load(tidyverse,ggplot2,zoo,modelr,coda,daymetr,tidybayes,nimble)

#funciton to calculate the std error
stderr <- function(x) {
  sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))
}

#load in the "bvr inflow" temp or calculated bvr inflow from fcr measurements
bvr_inflow_calcs <- read.csv("./inputs/BVR_inflow_2015_2021_allfractions_2poolsDOC_withch4_metInflow.csv")

#only include data from 2020-09-25 to 2021-11-07
bvr_inflow_calcs <- bvr_inflow_calcs[as.Date(bvr_inflow_calcs$time) >= "2020-09-25" &
                                       as.Date(bvr_inflow_calcs$time) <= "2021-11-07", ]

#read in noaa
noaa_nc <- ncdf4::nc_open("./Raw_Data/observed-met-noaa_bvre.nc")
noaa_nc_time <- ncdf4::ncvar_get(noaa_nc, "time")
origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_nc, "time")$units, 13, 28)
origin <- lubridate::ymd_hm(origin)
noaa_nc_time <- origin + lubridate::hours(noaa_nc_time)
AirTemp <- ncdf4::ncvar_get(noaa_nc, "air_temperature")

met <- tibble::tibble(time = noaa_nc_time,
                      AirTemp = AirTemp)

#convert to daily met
met <- met %>% dplyr::mutate(mdate = lubridate::as_date(time)) %>%
  dplyr::group_by(mdate) %>%
  dplyr::summarize(AirTemp = mean(AirTemp)) %>%
  dplyr::mutate(AirTemp = AirTemp - 273.15) #convert from K to C

#visualize noaa air temp and bvr water temp
ggplot(met,aes(mdate, AirTemp)) + geom_point()
ggplot(bvr_inflow_calcs,aes(as.POSIXct(time), TEMP)) + geom_point()

#-----------------------------------------------------------------------------#
#starting model setup
airtemp <- met$AirTemp
watertemp <- bvr_inflow_calcs$TEMP

AR <- nimbleCode({
  #### Priors
  x[1] ~ dnorm(x_ic, sd = sd_ic)
  sd_add ~ dunif(0, 100)
  beta_0 ~ dnorm(0.1,sd=0.1) 
  beta_1 ~ dnorm(1,sd=0.1) 
  beta_2 ~ dnorm(0.05, sd=0.01)
  beta_x ~ dnorm(1,sd=0.1)
  
  #### Process Model
  for(t in 2:n){
    pred[t] <-  beta_0 + airtemp[t-1] *beta_1 +  airtemp[t]* beta_2 + beta_x  
    x[t] ~ dnorm(pred[t], sd = sd_add)
  }
  #### Data Model
  for(t in 1:n){
    y[t] ~ dnorm(x[t], sd = sd_obs)
  }
})

constants <- list(n = length(airtemp),
                  x_ic = met$AirTemp[1],
                  sd_ic = stderr(met$AirTemp),
                  sd_obs = stderr(bvr_inflow_calcs$TEMP),
                  airtemp = airtemp)

data <- list(y = watertemp)
nchain = 3
inits <- list()
for(i in 1:nchain){
 # y.samp = sample(watertemp, length(watertemp), replace = TRUE)
  inits[[i]] <- list(sd_add = 10, #sd(diff(y.samp)) 
                     x = watertemp,
                     beta_0 = rnorm(1,0.2,0.1),
                     beta_1 = rnorm(1,1.1,0.1),
                     beta_2 = rnorm(1,0.04,0.1),
                     beta_x = rnorm(1,0.9,0.1) 
  )}

nimble_out <- nimbleMCMC(code = AR,
                         data = data,
                         inits = inits,
                         constants = constants,
                         monitors = c("sd_add",
                                      "beta_0",
                                      "beta_1",
                                      "beta_2",
                                      "beta_x",
                                      "x",
                                      "y"),
                         niter = 12000,
                         nchains = 3,
                         samplesAsCodaMCMC = TRUE)

#plot(nimble_out)
plot(nimble_out[, c("sd_add")])
gelman.diag(nimble_out[, c("sd_add")])  ## determine convergence

## burn-in
burnin <- 1000                               
nimble_burn <- window(nimble_out, start = burnin)
plot(nimble_burn[, c("sd_add")])
plot(nimble_burn[, c("beta_0")])
plot(nimble_burn[, c("beta_1")])
plot(nimble_burn[, c("beta_2")])
plot(nimble_burn[, c("beta_x")])
effectiveSize(nimble_burn[, c("sd_add")])
gelman.diag(nimble_burn[, c("sd_add")])  ## determine convergence

chain_ar <- nimble_burn %>%
  spread_draws(y[day],x[day],sd_add) %>%
  mutate(y = y,
         x = x)
chain_ar %>%
  summarize(sd_add = mean(sd_add))

#plot modeled water temp
watertemp_pred <- chain_ar %>% group_by(day) %>% 
  summarise(mean = mean(x),
            upper = quantile(x, 0.975),
            lower = quantile(x, 0.025),.groups = "drop") %>% 
  mutate(date = bvr_inflow_calcs$time) 

ggplot(data= watertemp_pred, aes(x = as.Date(date), y = mean)) +
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = "lightblue", fill = "lightblue") +
  geom_point(data = bvr_inflow_calcs, aes(x = as.Date(time), y = TEMP), shape=21, color="red",size=1) + 
  scale_shape_identity() + labs(x = "Date", y = "Water Temp (C)", title = "Nimble AR") 

#Now pull the mean for each of the parameters
out <- nimble_burn %>%
  tidybayes::spread_draws(beta_0, beta_1, beta_2, beta_x) 

params <- data.frame(beta_0 = mean(out$beta_0), beta_1 = mean(out$beta_1),
                    beta_2 = mean(out$beta_2), beta_x = mean(out$beta_x))


#testing the model
wtemp <- data.frame(matrix(NA, nrow=409))
wtemp[1,] <- airtemp[1]

for(i in 2:length(met$mdate)){
  wtemp[i,2] <- 0.07702 + airtemp[i-1] * 0.80405 + airtemp[i]*0.05872 + 0.97773
}

wtemp$day <- 1:409


plot(watertemp_pred$mean~watertemp_pred$day,pch=21, col="blue")
points(wtemp$matrix.NA..nrow...409.~wtemp$day,pch=21,col="red")
points(met$AirTemp~c(1:409),col="black")
points(wtemp$V2~wtemp$day,pch=21,col="purple")

#maybe try again with something simpler/more of a gradual change than air temp???
