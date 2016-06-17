#f============================================================
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

newmap <- getMap(resolution = "low")


# read rasters
estimate_1a <- stack("output/1950_1955.grd")
estimate_1 <- stack("output/1956_1965.grd")
estimate_2 <- stack("output/1966_1975.grd")
estimate_3 <- stack("output/1976_1985.grd")
estimate_4 <- stack("output/1986_1995.grd")
estimate_5 <- stack("output/1996_2005.grd")
estimate_6 <- stack("output/2006_2015.grd")

estimate_full <- stack(estimate_1a[[3:6]],
                       estimate_1,
                       estimate_2,
                       estimate_3,
                       estimate_4,
                       estimate_5,
                       estimate_6)

############################################################
# model Ukraine
############################################################
# crop dataset to ukraine
#state.map <- readShapeSpatial("C:/Users/RenkeLuehken/Google Drive/projects/reserach_projects/general_data/country_shapes/ukraine/UKR_adm1.shp")
state.map <- readShapeSpatial("D:/NeuAll/projects/reserach_projects/general_data/country_shapes/ukraine/UKR_adm1.shp")

temp_ukraine_full <- crop(estimate_full, state.map)
temp_ukraine <- subset(temp_ukraine_full, (42-4):57)

#temp_data_matrix <- getValues(temp_ukraine)

# crop the raster with the different sites
FinalPopulation2<-lapply(1:27, function(x) 
  crop(temp_ukraine, state.map[c(x),]))

# get the values of each site
FinalPopulation4<-lapply(FinalPopulation2,
                         function(x) getValues(x))

# give it the names of the site
FinalPopulation5<-lapply(1:27,function(x) data.frame(FinalPopulation4[[x]],Name=state.map$NAME_1[x]))

# merge it all to one
FinalPopulation6 <- do.call(rbind,FinalPopulation5)

years <- seq((1997-4), 2012, 1)
# give it correct names
dimnames(FinalPopulation6)[[2]]<-c(as.character(years),"name")

days<-as.numeric()
for(i in 1:20){
  days<-c(days,FinalPopulation6[,i])
}

year<-as.character()
for(i in 1:20){
  year<-c(year,rep(colnames(FinalPopulation6)[i],nrow(FinalPopulation6)))
}

Sample.ID<-rep(FinalPopulation6$name,20)

FULL<-data.frame(Sample.ID = Sample.ID, year = year, days = days)
FULL
#FULL2<-FULL
#FULL2$minDays<-ifelse(FULL$days>0,1,0)
#percentageAllowSubmission<-ddply(FULL2,.(Sample.ID,year),summarize,perc=sum(minDays,na.rm=T)*100/length(minDays))

FULLsub <- FULL
FULLmean<-ddply(FULLsub,.(Sample.ID,year),summarize,mean=mean(days,na.rm=T))

# diro cases
d1 = read.table("data/Ukraine_cases.csv", sep=",",header=T)
d2 <- cbind(rep(0, 27),rep(0, 27),rep(0, 27),rep(0, 27), d1[,2:17])

cases<-as.numeric()
for(i in 1:27){
  cases<-c(cases,d2[i,c(1:20)])
}


dats <- data.frame(FULLmean,cases=as.numeric(cases))

#omit nas
dats2<-na.omit(dats)

# change to presens-absence data
dats2$PA<-ifelse(dats2$cases>0,1,0)
values <- ifelse((dats2$cases - 2) > 0, (dats2$cases - 2), 0) 

dats_UKR <- dats2
library(zoo)
dats_UKR2 <- data.frame(dats_UKR, 
                        roll_m = c(0,0,0,0,rollmean(dats_UKR$mean, 5)))

dats_UKR3 <- subset(dats_UKR2, as.numeric(as.vector(year)) >= 1997)

#============================================================
# Belarus
#============================================================
# crop dataset to ukraine
state.map <- readShapeSpatial("C:/Users/RenkeLuehken/Google Drive/projects/reserach_projects/general_data/country_shapes/belarus/BLR_adm1.shp")
state.map <- readShapeSpatial("D:/NeuAll/projects/reserach_projects/general_data/country_shapes/belarus/BLR_adm1.shp")

temp_belarus_full <- crop(estimate_full, state.map)
temp_belarus <- subset(temp_belarus_full, (42-4):60)

# crop the raster with the different sites
FinalPopulation2<-lapply(1:6,function(x) 
  crop(temp_belarus,state.map[c(x),]))

# get the values of each site
FinalPopulation4<-lapply(FinalPopulation2,
                         function(x) getValues(x))

# give it the names of the site
FinalPopulation5<-lapply(1:6,function(x) 
  data.frame(FinalPopulation4[[x]],Name=state.map$NAME_1[x]))

# merge it all to one
FinalPopulation6<-do.call(rbind,FinalPopulation5)

years <- seq((1997-4), 2015, 1)
# give it correct names
dimnames(FinalPopulation6)[[2]]<-c(as.character(years),"name")

days<-as.numeric()
for(i in 1:23){
  days<-c(days,FinalPopulation6[,i])
}

year<-as.character()
for(i in 1:23){
  year<-c(year,rep(colnames(FinalPopulation6)[i],nrow(FinalPopulation6)))
}

Sample.ID<-rep(FinalPopulation6$name,23)

FULL<-data.frame(Sample.ID,year,days)

#FULL2<-FULL
#FULL2$minDays<-ifelse(FULL$days>0,1,0)
#percentageAllowSubmission<-ddply(FULL2,.(Sample.ID,year),summarize,perc=sum(minDays,na.rm=T)*100/length(minDays))

FULLsub <- FULL

FULLmean<-ddply(FULLsub,.(Sample.ID,year),summarize,mean=mean(days,na.rm=T))
#mods = dlply(FULLmean, .(Sample.ID), lm, formula = mean~as.numeric(year))

# diro cases
d1 = read.table("data/belarus_cases.csv", sep=";",header=T)
d2 <- cbind(rep(0, 6),rep(0, 6),rep(0, 6),rep(0, 6), d1[,2:20])

cases<-as.numeric()
for(i in 1:6){
  cases<-c(cases,d2[i,c(1:23)])
}

dats<-data.frame(FULLmean,cases=as.numeric(cases))

#omit nas
dats2<-na.omit(dats)

# change to presens-absence data
dats2$PA<-ifelse(dats2$cases>0,1,0)

dats_BLR <- dats2

dats_BLR2 <- data.frame(dats_BLR, 
                        roll_m = c(0,0,0,0,rollmean(dats_BLR$mean, 5)))

dats_BLR3 <- subset(dats_BLR2, as.numeric(as.vector(year)) >= 1997)

# merge data
dats_full <- rbind(dats_UKR3, dats_BLR3)

#============================================================
# modelling
#============================================================

# number of bootstraps
n_boot <- 50

cor.test(dats_full$mean, dats_full$roll_m)

# subsetting poisitve and negative 
data_set_all_pos <- subset(dats_full, PA == 1)
data_set_all_neg <- subset(dats_full, PA == 0)

data_set_all_pos_sub <- lapply(1:n_boot, function(x) 
  sample(seq(1,nrow(data_set_all_pos)),
         sample(seq(round((nrow(data_set_all_pos)/factor)), nrow(data_set_all_pos)), 1)))

data_set_all_neg_sub <- lapply(1:n_boot, function(x) 
  sample(seq(1,nrow(data_set_all_neg)),
         sample(seq(round((nrow(data_set_all_neg)/factor)), nrow(data_set_all_neg)), 1)))

FULL <- lapply(1:n_boot, function(x) 
  rbind(data_set_all_pos[data_set_all_pos_sub[[x]],], 
        data_set_all_neg[data_set_all_neg_sub[[x]],]))

FULL <- lapply(1:n_boot, function(x) 
   dats_full[sample(nrow(dats_full), sample(nrow(dats_full), 1)),])

# 
#full_resampled <- lapply(FULL, 
#                         function(x) dats_full[c(x), ])

full_resampled <- FULL
# boosted regression trees
t2 <- lapply(1:n_boot, function(x) try(gbm.step(data =full_resampled[[x]] , gbm.x = c(6), 
                                                      gbm.y = 5, silent = F, tree.complexity = 1,
                                                learning.rate = 0.005, step.size = 10, n.folds=10)))

#
pred_full <- lapply(1:n_boot, function(x) as.vector(try(predict(t2[[x]],
                                                             full_resampled[[x]],
                                                             n.trees = t2[[x]]$gbm.call$best.trees,
                                                             type = "response"))))

# rownames((full_resampled[[1]]))

dfdf <- lapply(1:n_boot, function (x) cbind(full_resampled[[x]], pred_full[[x]]))

#dfdf <- (do.call(rbind, full_resampled))
#predictions_all <- data.frame(dfdf, 
#                         prediction = c(unlist(pred_full)))
predoo <- ddply(predictions_all, .(Sample.ID, year, mean, PA), summarize, 
                meamN_pred = mean(prediction))
newdata <- predoo[order(predoo$meamN_pred),]

# identify the threshold above which 90% of the samples are positive
newdata$cumsum <- cumsum(newdata[,4])/sum(newdata[,4])

library(ROCR)
pred <- prediction(predoo[,5], predoo[,4])
#perf <- performance(pred,"tpr","fpr")
perf <- max(performance(pred, "acc")@y.values[[1]])
perf <- performance(pred, "sens", "spec")
perf@alpha.values[[1]][which.max(perf@x.values[[1]]+perf@y.values[[1]])]
perf@x.values[[1]][which.max(perf@x.values[[1]]+perf@y.values[[1]])]
perf@y.values[[1]][which.max(perf@x.values[[1]]+perf@y.values[[1]])]
performance(pred, "auc")@y.values[[1]]
#0.5679675 [0.781893, 0.7822878], 90% =0.30235639


other_data <- data.frame(roll_m = seq(0, 200 , 0.01))

t3 <- lapply(1:n_boot, function(x) as.vector(try(predict(t2[[x]],
                                                         other_data,
                                                         n.trees = t2[[x]]$gbm.call$best.trees,
                                                         type = "response"))))

other_data_pred <- data.frame(do.call(cbind, t3))
nums_pred_full <- apply(other_data_pred, 2, as.numeric)
pred_pred_full <- rowMeans(nums_pred_full, na.rm = T)
plot(pred_pred_full, type = "l")
all_data <- data.frame(x = other_data$roll_m,
                       y = pred_pred_full)
plot(all_data$x, all_data$y, type = "l")
write.table(all_data, "data/table.txt", sep="\t")







#


t=[0:0.3:2*%pi]';
z=sin(t)*cos(t');
plot3d(t,t,z)
library(lattice)

SurfaceData <- data.frame(
  x=rep(seq(0,100,length.out=10),each=10,times=3),
  y=rep(rep(seq(0,100,length.out=10),times=10),times=3),
  z=c(rep(25,100),seq(30,70,length.out=100),seq(95,75,length.out=100)),
  type=factor(rep(c("A","B","C"),each=100)))
  
wireframe(z~x*y,data=SurfaceData)


############################################################
# validation
############################################################
folds <- 5

folds_pos <- lapply(1:n_boot, function(x) kfold(seq(1, nrow(data_set_all_pos)), k = folds))
folds_neg <- lapply(1:n_boot, function(x) kfold(seq(1, nrow(data_set_all_neg)), k = folds))

train_pos <- c(lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] == 1, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] == 2, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] == 3, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] == 4, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] == 5, ]))
train_neg <- c(lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] == 1, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] == 2, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] == 3, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] == 4, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] == 5, ]))
train_full <- lapply(1:(n_boot*folds), function(x) rbind(train_pos[[x]], train_neg[[x]]))

test_pos <- c(lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] != 1, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] != 2, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] != 3, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] != 4, ]),
               lapply(1:n_boot, function(x) data_set_all_pos[fold_pos[[x]] != 5, ]))
test_neg <- c(lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] != 1, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] != 2, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] != 3, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] != 4, ]),
               lapply(1:n_boot, function(x) data_set_all_neg[folds_neg[[x]] != 5, ]))
test_full <- lapply(1:(n_boot*folds), function(x) rbind(test_pos[[x]], test_neg[[x]]))

t2 <- lapply(train_full, function(x) try(gbm.step(data = x , gbm.x = 6, 
                                                  gbm.y = 5, silent = F)))

t3 <- lapply(1:(n_boot*folds), function(x) as.vector(try(predict(t2[[x]],
                                                                 test_full[[x]],
                                                                 n.trees = t2[[x]]$gbm.call$best.trees,
                                                                 type = "response"))))


pred_new <- lapply(1:(folds*n_boot), function(x) try(prediction(t3[[x]], test_full[[x]]$PA)))

pred_ff <- lapply(pred_new, function(x) try(performance(x, "auc")@y.values[[1]]))

# cacluate mean AUCs
xg <- split(unlist(pred_ff), unlist(lapply(1:n_boot, function(x) rep(x, folds))))
mean(unlist(lapply(xg, function(x) mean(as.numeric(x), na.rm = T))))
sd(unlist(lapply(xg, function(x) sd(as.numeric(x), na.rm = T))))


  ################################################################################
  # plotting
  ################################################################################
  
  ##########
  # crop the maps
  ##########
  
  # crop to Europe
  estimate_full_croped <- crop(estimate_full, c(-11, 41, 36, 60))
  
  # raster to matrix
  last2 <- getValues(estimate_full_croped)
  
  # calculate five year average
  last3 <- apply(last2, 1, function(x) rollapply(x, 5, mean, na.rm = TRUE))
  
  all_data <- read.table("data/table.txt", sep="\t")
  
  source("R/matrix_to_raster.R")
  last4 <- matrix_to_raster(last3, estimate_full_croped[[1]])
  
  
  rbPal4 <- colorRampPalette(c("skyblue", 'khaki1', "red"))
  
  all_data$Col <- rbPal4(10)[cut(all_data$y, 10)]
  plot(all_data$x, all_data$y, col = all_data$Col)
  abline(0.5679675, 0)
  abline(0.30235639, 0)
  
  ############################################################
  #
  ############################################################
  rbPal_diff1 <- colorRampPalette(c("blue",  'white'))
  rbPal_diff2 <- colorRampPalette(c('white', "red"))
  
  tst1 <- sum(last4[[1:10]]> 41.91, na.rm = T)
  tst2 <- sum(last4[[11:20]]> 41.91, na.rm = T)
  tst3 <- sum(last4[[21:30]]> 41.91, na.rm = T)
  tst4 <- sum(last4[[31:40]]> 41.91, na.rm = T)
  tst5 <- sum(last4[[41:50]]> 41.91, na.rm = T)
  tst6 <- sum(last4[[51:60]]> 41.91, na.rm = T)
  
  png(file = "figs/difference.png",width = 5, height=5.2, units = 'in', res = 500)
  par(mfrow=c(3,2), mai=c(0.3,0.3,0.3,0.3))
  plot(tst2-tst1, breaks = seq(-10, 10), col = c(rbPal_diff1(10),"white", rbPal_diff2(10)), legend = F, main = "1966-1975 vs 1956-1965")
  plot(newmap, add = T)
  plot(tst3-tst2, breaks = seq(-10, 10), col = c(rbPal_diff1(10),"white", rbPal_diff2(10)), legend = F, main = "1976-1985 vs 1966-1975")
  plot(newmap, add = T)
  plot(tst4-tst3,  breaks = seq(-10, 10), col = c(rbPal_diff1(10),"white", rbPal_diff2(10)), legend = F, main = "1986-1995 vs 1976-1985")
  plot(newmap, add = T)
  plot(tst5-tst4,breaks = seq(-10, 10), col = c(rbPal_diff1(10),"white", rbPal_diff2(10)), legend = F, main = "1996-2005 vs 1986-1995")
  plot(newmap, add = T)
  plot(tst6-tst5, breaks = seq(-10, 10), col = c(rbPal_diff1(10),"white", rbPal_diff2(10)), legend = F, main = "2006-2015 vs 1996-2005")
  plot(newmap, add = T)
  plot(tst6-tst1, breaks = seq(-10, 10), col = c(rbPal_diff1(10),"white", rbPal_diff2(10)), legend = F, main = "2006-2015 vs 1956-1965")
  plot(newmap, add = T)
  dev.off()

############################################################
rbPal_diff1 <- colorRampPalette(c("blue",  'white'))
rbPal_diff2 <- colorRampPalette(c('white', "orange" ,"red"))

plot(mean(last4[[51:60]], na.rm = T)-mean(last4[[1:10]], na.rm = T),
       breaks = seq(-39, 57),  col = c(rbPal_diff1(40),rbPal_diff2(58)[-1]))
plot(newmap, add = T)

plot(mean(last4[[51:60]], na.rm = T)-mean(last4[[41:50]], na.rm = T),
     breaks = seq(-27, 33),  col = c(rbPal_diff1(28),rbPal_diff2(35)[-1]))
plot(newmap, add = T)

png(file = "figs/risk_europe.png",width = 5, height=5.2, units = 'in', res = 500)
par(mfrow=c(3,2), mai=c(0.3,0.3,0.3,0.3))
plot(mean(last4[[1:10]], na.rm = T), breaks = all_data$x, col = all_data$Col, legend = F, main = "1956-1965")
plot(newmap, add = T)
plot(mean(last4[[11:20]], na.rm = T), breaks = all_data$x, col = all_data$Col, legend = F, main = "1966-1975")
plot(newmap, add = T)
plot(mean(last4[[21:30]], na.rm = T),  breaks = all_data$x, col = all_data$Col, legend = F, main = "1976-1985")
plot(newmap, add = T)
plot(mean(last4[[31:40]], na.rm = T),  breaks = all_data$x, col = all_data$Col, legend = F, main = "1986-1995")
plot(newmap, add = T)
plot(mean(last4[[41:50]], na.rm = T),  breaks = all_data$x, col = all_data$Col, legend = F, main = "1996-2005")
plot(newmap, add = T)
plot(mean(last4[[51:60]], na.rm = T),  breaks = all_data$x, col = all_data$Col, legend = F, main = "2006-2015")
plot(newmap, add = T)
dev.off()

# interval of gif
ATR<-rep(0.2,length(seq(1,60,1))) # 0.00000001 secs between each picture
ATR[length(ATR)]<-2 # last picture stays for 2 secs

saveGIF({
  for( ii in 1:60) {
    #par(mar = c(3, 3, 3,1), mfrow = c(1,1),cex=0.9) 
    plot(mean(last4[[ii]], na.rm = T),  
         breaks = all_data$x, col = all_data$Col, legend = F, main = seq(1956, 2015)[ii])
    plot(newmap, add = T)
  }},
  movie.name = "DDU.gif", interval = ATR, 
  ani.width = 600, 
  ani.height = 400
)

# new cases
png(file = "figs/new_cases.png",width = 13, height=4.8, units = 'in', res = 500)
par(mfrow=c(1,2), mai=c(0.5,0.5,0.5,0.5))
new_cases_crop <- crop(last4[[1:10]], c(5, 25, 46, 55))
plot(new_cases_crop[[1]], breaks = all_data$x, col = all_data$Col, legend = F, main = "1956-1965")
plot(newmap, add = T)
new_cases_crop <- crop(last4[[51:60]], c(5, 25, 46, 55))
plot(new_cases_crop[[1]], breaks = all_data$x, col = all_data$Col, legend = F, main = "2006-2015")
plot(newmap, add = T)
dev.off()
?im.convert

plot(mean(last4[[1:10]], na.rm = T), breaks = all_data$x, col = all_data$Col, legend = F)
plot(newmap, add = T)
plot(mean(last4[[51:60]], na.rm = T), breaks = all_data$x, col = all_data$Col, legend = F)
plot(newmap, add = T)



x <- seq(18,30)
y <- 0.0078*x-0.1117
plot(x, y)
0.0287*30

sel2 <- last4[[7:16]]
plot(mean(sel2,na.rm = T), breaks = seq(0,174.325, 174.325/30), col = c("gray",matlab.like2(30)))
newmap <- getMap(resolution = "low")
plot(newmap, add = T)

estimate_2_croped_2 <- crop(estimate_2_croped, c(-13.68265, 44.94073, 36, 60))
estimate_3_croped_2 <- crop(estimate_3_croped, c(-13.68265, 44.94073, 36, 60))
estimate_4_croped_2 <- crop(estimate_4_croped, c(-13.68265, 44.94073, 36, 60))
estimate_5_croped_2 <- crop(estimate_5_croped, c(-13.68265, 44.94073, 36, 60))
estimate_6_croped_2 <- crop(estimate_6_croped, c(-13.68265, 44.94073, 36, 60))



estimate_mean_1 <- mean(estimate_1_croped_2, na.rm = T)
estimate_mean_2 <- mean(estimate_2_croped_2, na.rm = T)
estimate_mean_3 <- mean(estimate_3_croped_2, na.rm = T)
estimate_mean_4 <- mean(estimate_4_croped_2, na.rm = T)
estimate_mean_5 <- mean(estimate_5_croped_2, na.rm = T)
estimate_mean_6 <- mean(estimate_6_croped_2, na.rm = T)

plot(estimate_5_croped_2)

quickfun <- function(y) median(y >= 65.2, na.rm = T)
quickfun <- function(y) mean(y - 65.2, na.rm = T)

per_val_1 <- calc(estimate_1_croped_2, quickfun)
per_val_2 <- calc(estimate_2_croped_2, quickfun)
per_val_3 <- calc(estimate_3_croped_2, quickfun)
per_val_4 <- calc(estimate_4_croped_2, quickfun)
per_val_5 <- calc(estimate_5_croped_2, quickfun)
per_val_6 <- calc(estimate_6_croped_2, quickfun)

plot(per_val_4, breaks = seq(-65.2, 106), col = matlab.like2(172))

dev.new()
plot(per_val_6==1)
seq(0,10)
matlab.like2(11)




# colurs
rbPal <- colorRampPalette(c('gray',"yellow", 'red'))



dat$Col <- c(rbPal(10)[as.numeric(cut(dat$y[dat$y<0.5066847],breaks = 10))],
             rep("red", length(dat$y[dat$y>0.5066847])))
dat2$Col <- c(rbPal(50)[as.numeric(cut(dat2$y[dat2$y<0.5066847],breaks = 50))],
             rep("red", length(dat2$y[dat2$y>0.5066847])))

dat2[709:800,]
dat2$Col <- c(rbPal(729),rep("red", nrow(dat2)-729))
#dat2$Col <- c(rbPal(nrow(dat2)))

dat$Col <- c(rbPal(10)[as.numeric(cut(dat$y[dat$y<0.5066847],breaks = 10))],
             rep("red", length(dat$y[dat$y>0.5066847])))

plot(rbPal(10)[as.numeric(cut(dat$y[dat$y<0.5066847],breaks = 10))])
plot(dat)



dat2$Col <- c(matlab.like2(100)[as.numeric(cut(dat2$y[dat2$y<0.57],breaks = 100))],
              rep("red", length(dat2$y[dat2$y>0.57])))

dat2$Col <- c(matlab.like2(nrow(dat2)))


dat2$Col <- c(rbPal(10)[as.numeric(cut(dat2$y[dat2$y<0.5066847],breaks = 10))],
             rep("red", length(dat2$y[dat2$y>0.5066847])))



dat2$Col <- c(rbPal1(100)[as.numeric(cut(dat2$y[dat2$y<0.5],breaks = 100))],
  rbPal2(100)[as.numeric(cut(dat2$y[dat2$y>0.5],breaks = 100))])

dat2$Col <- c(rbPal1(100)[as.numeric(cut(dat2$y[dat2$y<0.5],breaks = 100))],
              rbPal2(100)[as.numeric(cut(dat2$y[dat2$y>0.5],breaks = 100))])


rbPal1 <- colorRampPalette(c("gray", 'khaki1'))
rbPal2 <- colorRampPalette(c("khaki1", 'red'))
rbPal3 <- colorRampPalette(c('red'))

all_data[500:1500,]
all_data$Col <- c(rbPal1(375), rbPal2(375),
              rep("red", (nrow(all_data)-750)))

all_data2 <- all_data[1:1711,]
all_data2$Col <- c(rbPal1(30)[as.numeric(cut(all_data2$y[all_data2$y<0.59/2],
                                        breaks = 30))],
              rbPal2(30)[as.numeric(cut(all_data2$y[all_data2$y>0.59/2],
                                        breaks = 30))])


#dat2$Col <- rbPal(nrow(dat2))
plot(all_data2$x, all_data2$y, col = all_data2$Col)
dat2 <- all_data2
png(file = "figs/1_eins2.png",width = 5.55, height=5.2, units = 'in', res = 500)
par(mfrow=c(3,2), mai=c(0.3,0.3,0.3,0.3))
plot(estimate_mean_1, breaks = c(dat2$x), col = c(dat2$Col), legend = F, main = "1956-1965")
plot(newmap, add = T)
plot(estimate_mean_2, breaks = c(dat2$x), col = c(dat2$Col), legend = F, main = "1966-1975")
plot(newmap, add = T)
plot(estimate_mean_3, breaks = c(dat2$x), col = c(dat2$Col), legend = F, main = "1976-1985")
plot(newmap, add = T)
plot(estimate_mean_4, breaks = c(dat2$x), col = c(dat2$Col), legend = F, main = "1986-1995")
plot(newmap, add = T)
plot(estimate_mean_5, breaks = c(dat2$x), col = c(dat2$Col), legend = F, main = "1996-2005")
plot(newmap, add = T)
plot(estimate_mean_6, breaks = c(dat2$x), col = c(dat2$Col), legend = F, main = "2006-2015")
plot(newmap, add = T)
dev.off()

