library(reshape2)
library(smoothr)
library(foreach)
library("doParallel")
library(raster)
rm(list=ls()) 

satellite="MYD"
index=read.csv("../../buffer/1.cca_cluster/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="day"

for(city in 1:36) {
  cat(city,index[city,2],time,"\n")
  water=raster(paste0("../output/4.water/",index[city,2],".tif"))
  sel=which(water[]<0.01)
  sum_sum0=foreach (year=2003:2018,.combine=rbind) %do% {
    cat (city,year,time,"\n")
    if (year==2003) {
      lst_raw=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year+1),".csv")))/50-273.15
      lst_atc=cbind(read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))
    } else if (year==2018) {
      lst_raw=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year-1),".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))/50-273.15
      lst_atc=cbind(read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))
    } else {
      lst_raw=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year-1),".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year+1),".csv")))/50-273.15
      lst_atc=cbind(read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/6.2.lst_atc/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))
    }
    
    filter=read.csv(paste0("../output/5.4.filter_for_fit/",index[city,2],"/",satellite,"_",time,"_",year,".csv"))
    lst_raw[,filter$filter==0]=NA
    
    cbind(city,year,unlist(lst_raw[sel,]),unlist(lst_atc[sel,]))
  } 
  sum_sum1=as.data.frame(sum_sum0)
  names(sum_sum1)=c("city","year","raw","atc")
  write.csv(sum_sum1,paste0("../output/5.3.atc_evaluation/error_",index[city,2],"_",time,".csv"),row.names=FALSE)
}
stopCluster(cl)