##estimate LST for every day based on the second fitted ATC model
library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

if(!dir.exists(paste0("../output/6.2.lst_atc"))){dir.create(paste0("../output/6.2.lst_atc"))}

satellite="MYD"
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="day"

cl<-makeCluster(36, outfile=paste0('history_6.2.lst_atc'), type = "FORK")
registerDoParallel(cl)
for (year in 2003:2018)  {
  cat (year, time, "\n") 
  foreach (city=1:36) %dopar% {
    if (!dir.exists(paste0("../output/6.2.lst_atc/",index[city,2]))) {dir.create(paste0("../output/6.2.lst_atc/",index[city,2]))}
    cm=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_cm.tif"))
    ca=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_ca.tif"))
    cp=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_cp.tif"))
    lst0=lapply(1:365,function (x) cm[]+ca[]*sin(2*pi*x/365-2*pi/365*cp[]+0.5*pi))
    lst=matrix(unlist(lst0), nrow = length(cm), byrow = F)
    write.csv(lst,file=paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv"),row.names = F,col.names = F)
  }
}
stopCluster(cl)