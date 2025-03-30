###calculate the EVI mean in June, July, August for each year during 2003-2018 for each grid.

library("raster")                                                                
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

if(!dir.exists(paste0("../output/3.1.isa_mean"))){dir.create(paste0("../output/3.1.isa_mean"))}

satellite="MYD"
isa0=raster("../input/isa_year/raw_data/isa_cn.tif")
cluster_u_r=readOGR(dsn="../input/urban_rural_cluster_smooth_hole_captical",layer="urban_rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

cl<-makeCluster(16, outfile=paste0('history_3.1.isa_mean'), type = "FORK")
registerDoParallel(cl)
for (city in 1:36) { 
  cat (city,index[city,2],"\n")
  if (!dir.exists(paste0("../output/3.1.isa_mean/",index[city,2]))) {dir.create(paste0("../output/3.1.isa_mean/",index[city,2]))} 
  cluster0=cluster_u_r[cluster_u_r@data[,1]==index[city,7],]
  cluster=spTransform(cluster0,crs(isa0))
  ref=raster(paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_rr.tif"))
  isa=2019-mask(crop(isa0,cluster),cluster)
  foreach (year=2003:2018) %dopar% {
    isa1=isa
    isa1[which(isa1[]<=year)]=1
    isa1[which(isa1[]>year)]=0
    isa1_pro=projectRaster(isa1,ref,method="bilinear")
    writeRaster(isa1_pro,paste0("../output/3.1.isa_mean/",index[city,2],"/isa_",year,".tif"),overwrite=TRUE)
  }
}
stopCluster(cl)