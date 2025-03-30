## calcualte water coverage for each pixel for each city based on MODIS land cover data in 2018

library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

if(!dir.exists(paste0("../output/4.water"))){dir.create(paste0("../output/4.water"))}

satellite="MYD"
cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="urban_rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
water=raster("../input/support/LULC/tif/water_modis_2018_agg.tif")

cl<-makeCluster(36, outfile=paste0('history_2.2.vcf_trend'), type = "FORK")
registerDoParallel(cl)
foreach (city=1:36) %dopar% {
   cat (city,index[city,2],"\n")	
   ref=raster(paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_rr.tif"))
   cluster=cluster_u_r[cluster_u_r@data[,1]==index[city,7],]
   water_sel=mask(crop(water,cluster),cluster)
   water_sel1=projectRaster(water_sel,ref,method="bilinear")
   writeRaster(water_sel1,paste0("../output/4.water/",index[city,2],".tif"),overwrite=TRUE)
}
stopCluster(cl)