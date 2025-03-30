library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

if(!dir.exists(paste0("../output/6.4.lst_season_mean"))){dir.create(paste0("../output/6.4.lst_season_mean"))}

spring=60:151
summer=152:244
autumn=245:334
winter=c(1:59,335:365)
satellite="MYD"
index=read.csv("../../buffer/1.cca_cluster/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="nit"

cl<-makeCluster(16, outfile=paste0('history_6.4.lst_season_mean'), type = "FORK")
registerDoParallel(cl)
for (city in 1:36) { 
  cat (city,index[city,2],time,"\n")
  if (!dir.exists(paste0("../output/6.4.lst_season_mean/",index[city,2]))) {dir.create(paste0("../output/6.4.lst_season_mean/",index[city,2]))} 
  foreach (year=2003:2018) %dopar% {
    cm=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_cm.tif"))
    ca=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_ca.tif"))
    cp=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_cp.tif"))      
    lst0=lapply(summer,function (x) cm[]+ca[]*sin(2*pi*x/365-2*pi/365*cp[]+0.5*pi))
    lst=matrix(unlist(lst0), nrow = length(cm), byrow = F)
    lst_mean=cm
    lst_mean[]=apply(lst,1,mean)
    writeRaster(lst_mean,paste0("../output/6.4.lst_season_mean/",index[city,2],"/",satellite,"_",time,"_",year,"_summer.tif"),overwrite=TRUE)
  }
}
stopCluster(cl)