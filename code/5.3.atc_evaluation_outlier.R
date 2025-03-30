##calcuate the error of LST based on the ATC model
##these errors will be used to determine the threshold for outlier

library(reshape2)
library(smoothr)
library(foreach)
library(doParallel)
library(raster)
rm(list=ls()) 

if(!dir.exists(paste0("../output/5.3.atc_evaluation"))){dir.create(paste0("../output/5.3.atc_evaluation"))}

satellite="MYD"
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="day"

cl<-makeCluster(16, outfile=paste0('history_5.3.atc_evaluation_outlier'), type = "FORK")
registerDoParallel(cl)

sum_sum0=foreach (city=1:36,.combine=rbind) %do% {
  cat (city,index[city,2],time,"\n")
  water=raster(paste0("../output/4.water/",index[city,2],".tif"))
  sel=which(water[]<0.01)
  sum0=foreach (year=2003:2018,.combine=rbind) %dopar% {
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
    cbind(city,colSums(!is.na(lst_raw))/max(colSums(!is.na(lst_raw))),abs(lst_raw_0-lst_atc_0))
  } 
  sum0
}
stopCluster(cl)
sum_sum1=as.data.frame(sum_sum0)
names(sum_sum1)=c("city","ratio","delta")
write.csv(sum_sum1,paste0("../output/5.3.atc_evaluation/atc_evaluation_outlier_",time,".csv"),row.names=FALSE)