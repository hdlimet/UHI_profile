library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

source ("0.1.function_trend.R")
if(!dir.exists(paste0("../output/6.5.lst_season_trend"))){dir.create(paste0("../output/6.5.lst_season_trend"))}

satellite="MYD"
index=read.csv("../../buffer/1.cca_cluster/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="nit"

cl<-makeCluster(36, outfile=paste0('history_6.5.lst_season_trend'), type = "FORK")
registerDoParallel(cl)
foreach (city=1:36) %dopar% {
  cat (city,index[city,2],"\n")
  sum0=foreach (year=c(2003:2018),.combine=rbind) %do% {
    lst=raster(paste0("../output/6.4.lst_season_mean/",index[city,2],"/",satellite,"_",time,"_",year,"_summer.tif"))
    lst[]
  }     
  regression=apply(sum0,2,function(x) regress_cal(x))   
  rr=lst
  pp=lst
  slope=lst
  rr[]=regression[1,]
  pp[]=regression[2,]
  slope[]=regression[3,]
  writeRaster(rr,paste0("../output/6.5.lst_season_trend/",satellite,"_",index[city,2],"_",time,"_summer_rr.tif"),overwrite=TRUE)
  writeRaster(pp,paste0("../output/6.5.lst_season_trend/",satellite,"_",index[city,2],"_",time,"_summer_pp.tif"),overwrite=TRUE)
  writeRaster(slope,paste0("../output/6.5.lst_season_trend/",satellite,"_",index[city,2],"_",time,"_summer_slope.tif"),overwrite=TRUE)
}
stopCluster(cl)