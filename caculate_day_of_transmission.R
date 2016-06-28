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
source("R/subs.R")
source("R/timeline.R")
source("R/matrix_to_raster.R")

#============================================================
# read data
#============================================================
for(i in 1950:2015){
  
  i <- 1950
  
  # read temperature data
  temp_data_raster <- stack(paste("data/mean_temperature_europe_", 
                          i, ".grd", sep = ""))

  # transform raster to matrix
  temp_data_matrix <- getValues(temp_data_raster)
  
  # identifier suitable and unsutiable transmission days
  SUBS <- unlist(lapply(i:i, function(x) sub(x)))
  
  # timevector
  tseq <- timeline(i, i)
  
  # calculate DDUs
  res <- apply(temp_data_matrix, 1, function(x) DDU(x, tseq, SUBS))
  
  # transform matrix to raster
  raster <- matrix_to_raster(res, temp_data_raster[[1]])
  
  # save raster
  writeRaster(raster, paste("output/day_of_transmission_", i,".grd",sep=""),overwrite=T)
}