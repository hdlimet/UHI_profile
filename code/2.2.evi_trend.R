###calculate the EVI trend during 2003-2018 for each grid.

library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

source ("0.1.function_trend.R")
if(!dir.exists(paste0("../output/1.2.evi_trend"))){dir.create(paste0("../output/1.2.evi_trend"))}

satellite="MYD"
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

cl<-makeCluster(36, outfile=paste0('history_1.2.evi_trend'), type = "FORK")
registerDoParallel(cl)
foreach (city=1:36) %dopar% {
  cat (city,index[city,2],"\n")
  sum0=foreach (year=c(2003:2018),.combine=rbind) %do% {
    evi=raster(paste0("../output/1.1.evi_mean/",index[city,2],"/",satellite,"_evi_",year,".tif"))
    evi[]
  }     
  regression=apply(sum0,2,function(x) regress_cal(x))   
  rr=evi
  pp=evi
  slope=evi
  rr[]=regression[1,]
  pp[]=regression[2,]
  slope[]=regression[3,]
  writeRaster(rr,paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_rr.tif"),overwrite=TRUE)
  writeRaster(pp,paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_pp.tif"),overwrite=TRUE)
  writeRaster(slope,paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_slope.tif"),overwrite=TRUE)
}
stopCluster(cl)