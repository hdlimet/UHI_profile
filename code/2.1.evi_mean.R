###calculate the EVI mean in June, July, August for each year during 2003-2018 for each grid.

library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

if(!dir.exists(paste0("../output/1.1.evi_mean"))){dir.create(paste0("../output/1.1.evi_mean"))}

satellite="MYD"
cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="urban_rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

cl<-makeCluster(16, outfile=paste0('history_1.1.evi_mean'), type = "FORK")
registerDoParallel(cl)
for (city in 1:36) { 
  cat (city,index[city,2],"\n")
  if (!dir.exists(paste0("../output/1.1.evi_mean/",index[city,2]))) {dir.create(paste0("../output/1.1.evi_mean/",index[city,2]))} 
   cluster=cluster_u_r[cluster_u_r@data[,1]==index[city,7],]
   foreach (year=2003:2018) %dopar% {
      evi6=raster(paste0("../input/support/NDVI/tif/",year,"/",satellite,"/EVI_",year,"_6.tif"))/10000
      evi7=raster(paste0("../input/support/NDVI/tif/",year,"/",satellite,"/EVI_",year,"_7.tif"))/10000
      evi8=raster(paste0("../input/support/NDVI/tif/",year,"/",satellite,"/EVI_",year,"_8.tif"))/10000
      evi_u_r_6=mask(crop(evi6,cluster),cluster)
      evi_u_r_7=mask(crop(evi7,cluster),cluster)
      evi_u_r_8=mask(crop(evi8,cluster),cluster)
      evi=overlay(evi_u_r_6,evi_u_r_7,evi_u_r_8,fun=mean)
      writeRaster(evi,paste0("../output/1.1.evi_mean/",index[city,2],"/",satellite,"_evi_",year,".tif"),overwrite=TRUE)
   }
}
stopCluster(cl)