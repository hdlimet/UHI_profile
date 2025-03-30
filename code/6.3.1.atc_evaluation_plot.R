library(reshape2)
library(smoothr)
library(foreach)
library("doParallel")
library(raster)
rm(list=ls()) 

satellite="MYD"
index=read.csv("../../buffer/1.cca_cluster/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="nit"

cl<-makeCluster(16, outfile=paste0('history_7.1.evaluation_plot'), type = "FORK")
registerDoParallel(cl)
for(city in 1:36) {
  cat(city,index[city,2],time,"\n")
  water=raster(paste0("../output/4.water/",index[city,2],".tif"))
  sel=which(water[]<0.01)
  sum0=foreach (year=2003:2018,.combine=rbind) %dopar% {
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
    
    lst_raw_0=apply(lst_raw[sel,],2,mean,na.rm=TRUE)
    lst_atc_0=apply(lst_atc[sel,],2,mean,na.rm=TRUE)
    
    if (year==2003 | year==2018) {
      rbind(cbind(city,year,1,rep((1:365+(year-2003)*365),2),lst_raw_0),
            cbind(city,year,2,rep((1:365+(year-2003)*365),2),lst_atc_0))
    } else {
      rbind(cbind(city,year,1,rep((1:365+(year-2003)*365),3),lst_raw_0),
            cbind(city,year,2,rep((1:365+(year-2003)*365),3),lst_atc_0))
    }
  } 
  sum1=as.data.frame(sum0)
  names(sum1)=c("city","year","type","doy","lst")
  write.csv(sum1,paste0("../output/5.3.atc_evaluation/plot_",index[city,2],"_",time,".csv"),row.names=FALSE)
}
stopCluster(cl)