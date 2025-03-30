#do the second ATC modeling sing the data that exclude the outlier
##two-step ATC modeling to improve model accuracy and reduce the impacts of short-term distubance

library("raster")
library(minpack.lm)
library("sp")
library("rgdal")
library("rgeos")
library("doParallel")
library("foreach")
rm (list=ls())

source ("0.2.function_atc.R")
if(!dir.exists(paste0("../output/6.1.atc_fit"))){dir.create(paste0("../output/6.1.atc_fit"))}

satellite="MYD"
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="day"

cl<-makeCluster(16, outfile=paste0('history_6.1.atc_fit'), type = "FORK")
registerDoParallel(cl)
for (city in 1:36) {
  cat (time,index[city,2],"\n")
  if(!dir.exists(paste0("../output/6.1.atc_fit/",index[city,2]))){dir.create(paste0("../output/6.1.atc_fit/",index[city,2]))}
  ref=raster(paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_rr.tif"))
  foreach (year=2003:2018) %dopar% {
    if (year==2003) {
      lst=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year+1),".csv")))/50-273.15
    } else if (year==2018) {
      lst=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year-1),".csv")),
                read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))/50-273.15
    } else {
      lst=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year-1),".csv")),
                read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year+1),".csv")))/50-273.15
    }
    
    filter=read.csv(paste0("../output/5.4.filter_for_fit/",index[city,2],"/",satellite,"_",time,"_",year,".csv"))
    lst[,filter$filter==0]=NA
    
    atc_para=apply(lst,1,function(x) atc_fit(x))
    cm=ref
    ca=ref
    cp=ref
    cm[]=atc_para[1,]
    ca[]=atc_para[2,]
    cp[]=atc_para[3,]
    writeRaster(cm,paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_cm.tif"),overwrite=TRUE)
    writeRaster(ca,paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_ca.tif"),overwrite=TRUE)
    writeRaster(cp,paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",time,"_",year,"_cp.tif"),overwrite=TRUE)
  } 
}
stopCluster(cl)