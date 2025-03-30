##delete the outlier in the raw LST data for the second ATC modeling

library(reshape2)
library(smoothr)
library(foreach)
library(doParallel)
library(raster)
rm(list=ls())

if(!dir.exists(paste0("../output/5.4.filter_for_fit"))){dir.create(paste0("../output/5.4.filter_for_fit"))}

satellite="MYD"
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="day"

input=read.csv(paste0("../output/5.3.atc_evaluation/atc_evaluation_outlier_",time,".csv"))
quant=quantile(input$delta,na.rm=TRUE)
outlier=quant[4]+1.5*(quant[4]-quant[2])

cl<-makeCluster(16, outfile=paste0('history_5.4.filter_for_fit'), type = "FORK")
registerDoParallel(cl)
for (city in 1:36) {
  cat(city,index[city,2],time,"\n")
  if(!dir.exists(paste0("../output/5.4.filter_for_fit/",index[city,2]))){dir.create(paste0("../output/5.4.filter_for_fit/",index[city,2]))}
  water=raster(paste0("../output/4.water/",index[city,2],".tif"))
  sel=which(water[]<0.01)
  foreach (year=2003:2018) %dopar% {
    if (year==2003) {
      lst_raw=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year+1),".csv")))/50-273.15
      lst_atc=cbind(read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))
    } else if (year==2018) {
      lst_raw=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year-1),".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))/50-273.15
      lst_atc=cbind(read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))
    } else {
      lst_raw=cbind(read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year-1),".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",(year+1),".csv")))/50-273.15
      lst_atc=cbind(read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")),
                    read.csv(paste0("../output/5.2.lst_atc_nofilter/",index[city,2],"/",satellite,"_",time,"_",year,".csv")))
    }
    
    lst_raw_0=apply(lst_raw[sel,],2,mean,na.rm=TRUE)
    lst_atc_0=apply(lst_atc[sel,],2,mean,na.rm=TRUE)
    sel1=which(abs(lst_raw_0-lst_atc_0)<outlier)
    sum_sum=as.data.frame(cbind(city,colSums(!is.na(lst_raw))/max(colSums(!is.na(lst_raw))),abs(lst_raw_0-lst_atc_0),0))
    sum_sum[sel1,4]=1
    names(sum_sum)=c("city","ratio","delta","filter")
    write.csv(sum_sum,paste0("../output/5.4.filter_for_fit/",index[city,2],"/",satellite,"_",time,"_",year,".csv"),row.names=FALSE)
  }
}
stopCluster(cl)