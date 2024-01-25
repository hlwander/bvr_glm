# script to interpolate bvr edi data for 0.3m intervals
# created 25 Jan 2024

#load packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, zoo)

#read in edi bathy data
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

bathy_raw <-read.csv(infile1) |> 
  filter(Reservoir == "BVR") |>
  select(Depth_m, SA_m2) |> 
  complete(Depth_m = seq(0, 13.5, 0.3)) |> 
  mutate(SA_m2 = ifelse(Depth_m == 13.5, 0, SA_m2)) |> 
  #set SA at 13.5m to be 0 m2 to match assumption that 13.5 is max depth in glm
  arrange(Depth_m) |> 
  mutate(SA_m2 = na.approx(SA_m2)) |> 
  filter(Depth_m <= 13.5 & Depth_m %in% c(seq(0,13.5,0.3)))

#c(rev(bathy_raw$SA_m2))

bathy_bvr_edi <-read.csv(infile1) |> 
  filter(Reservoir == "BVR") 

plot(bathy_raw$Depth_m~bathy_raw$SA_m2, type="o", pch=16, #nml
     ylim = rev(range(bathy_raw$Depth_m)))
lines(bathy_bvr_edi$Depth_m~bathy_bvr_edi$SA_m2, type="o",col="red", pch=16, #edi
      ylim = rev(range(bathy_bvr_edi$Depth_m)))

#test code (paste H and A from nml into data.frame() function)
#bathy_bvr_nml <- data.frame(   H = c(576, 576.3, 576.6, 576.9, 577.2, 577.5, 577.8, 578.1, 578.4, 578.7, 579, 579.3, 579.6, 579.9, 580.2, 580.5, 580.8, 581.1, 581.4, 581.7, 582, 582.3, 582.6, 582.9, 583.2, 583.5, 583.8, 584.1, 584.4, 584.7, 585, 585.3, 585.6, 585.9, 586.2, 586.5, 586.8, 587.1, 587.4, 587.7, 588, 588.3, 588.6, 588.9, 589.2, 589.5),
#                               A = c(0.00, 1817.00, 5546.00, 7458.00, 9370.00, 11282.00, 14778.75, 18275.50, 21772.25,
#                                     32206.00, 39143.00, 46080.00, 57932.50, 62848.00, 67763.50, 72679.00, 79472.00,
#                                     86265.00, 93058.00, 106164.00, 112477.00, 118790.00, 132083.75, 139064.50, 
#                                     146045.25, 153026.00, 164037.00, 175048.00, 186059.00, 207740.00, 218410.00, 
#                                     229080.00, 249453.50, 259157.00, 268860.50, 278564.00, 288742.25, 298920.50, 
#                                     309098.75, 326724.50, 334172.00, 341619.50, 360292.75, 371518.50, 382744.25,
#                                     393970.00))
#
#
#bathy_bvr_nml <- bathy_bvr_nml |> mutate(m = 576 - H) |> 
#                                  mutate(depth = 13.5 + m)
#
#plot(bathy_bvr_nml$depth~bathy_bvr_nml$A, type="o", pch=16, #nml
#     ylim = rev(range(bathy_bvr_nml$depth)))
#lines(bathy_bvr_edi$Depth_m~bathy_bvr_edi$SA_m2, type="o",col="red", pch=16, #edi
#      ylim = rev(range(bathy_bvr_edi$Depth_m)))



