#============================================================
# clear memory
#============================================================
rm(list = ls())

#============================================================
# set working directory
#============================================================
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)

#============================================================
# load libraries
#============================================================
library(rgeos)
library(maptools)
library(colorRamps)
library(rworldmap)
library(raster)
library(plyr)
library(dismo)
library(ROCR)
library(animation)
library(rworldmap)
library(zoo)

#============================================================
# source
#============================================================
source("R/DDU.R")
#source("R/sum_of_DDUs.R")
source("R/subs.R")
source("R/timeline.R")
source("R/matrix_to_raster.R")

#============================================================
# read data
#============================================================
for(i in 1950:2015){
  
  # read temperature data
  temp_data_raster <- brick(paste("data/mean_temperature_europe_", 
                          i, ".grd", sep = ""))
  
  aim <- temp_data_raster[[1]]

  # transform raster to matrix
  temp_data_matrix <- raster::getValues(temp_data_raster)
  
  rm(temp_data_raster)
  
  # identifier suitable and unsutiable transmission days
  SUBS <- unlist(lapply(i:i, function(x) sub(x)))
  
  # timevector
  tseq <- timeline(i, i)
  
  # calculate daily DDUs
  daily_DDUs <- apply(temp_data_matrix, 1, function(x) DDU(x))

  # transform matrix to raster
  yearly_sum_of_daily_DDUs_r <- matrix_to_raster(daily_DDUs[2, ], aim)
  day_of_potential_transmission_r <- matrix_to_raster(daily_DDUs[1, ], aim)

  # save raster I
  writeRaster(yearly_sum_of_daily_DDUs_r, 
              paste("output/sum_of_daily_DDUs_", i,".grd",sep=""), overwrite=T)
  
  # save raster II
  writeRaster(day_of_potential_transmission_r,
              paste("output/day_of_potential_transmission_", i,".grd",sep=""), overwrite=T)
  
  print(i)
}