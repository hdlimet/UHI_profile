###calculate the ISA trend during 2003-2018 for each grid.

library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

source ("0.1.function_trend.R")
if(!dir.exists(paste0("../output/3.2.isa_trend"))){dir.create(paste0("../output/3.2.isa_trend"))}

index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

cl<-makeCluster(36, outfile=paste0('history_3.2.isa_trend'), type = "FORK")
registerDoParallel(cl)
foreach (city=1:36) %dopar% {
  cat (city,index[city,2],"\n")
  sum0=foreach (year=c(2003:2018),.combine=rbind) %do% {
    isa=raster(paste0("../output/3.1.isa_mean/",index[city,2],"/isa_",year,".tif"))
    isa[]
  }     
  regression=apply(sum0,2,function(x) regress_cal(x))   
  rr=isa
  pp=isa
  slope=isa
  rr[]=regression[1,]
  pp[]=regression[2,]
  slope[]=regression[3,]
  writeRaster(rr,paste0("../output/3.2.isa_trend/",index[city,2],"_rr.tif"),overwrite=TRUE)
  writeRaster(pp,paste0("../output/3.2.isa_trend/",index[city,2],"_pp.tif"),overwrite=TRUE)
  writeRaster(slope,paste0("../output/3.2.isa_trend/",index[city,2],"_slope.tif"),overwrite=TRUE)
}
stopCluster(cl)