##pre-processing data for atc fitting in the next step
##create a matrix of lst with values from day one to day 365
library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(doParallel)
library(foreach)
rm (list=ls())

if(!dir.exists(paste0("../output/5.0.lst_raw"))){dir.create(paste0("../output/5.0.lst_raw"))}

days=c(paste0("00",1:9),paste0("0",10:99),100:365)
satellite="MYD"
cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="urban_rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])
time="day"

cl<-makeCluster(16, outfile=paste0('history_5.0.lst_raw'), type = "FORK")
registerDoParallel(cl)
foreach (year=2003:2018) %dopar% {
  cat (year, "\n")
  rlist = list()
  doy=1
  lst0=raster(paste0("../input/LST_cn_daily/tif/",year,"/",satellite,"/lst_",time,"_",year,"_",days[doy],".tif"))
  for (city in 1:36) {
    if (!dir.exists(paste0("../output/5.0.lst_raw/",index[city,2]))) {dir.create(paste0("../output/5.0.lst_raw/",index[city,2]))}
    cluster=cluster_u_r[cluster_u_r@data[,1]==index[city,7],]
    lst=mask(crop(lst0,cluster),cluster)
    citymatrix.lib = matrix(NA,ncol=365,nrow=length(lst[]))
    citymatrix.lib[,doy] =  lst[]
    rlist[[city]] = citymatrix.lib
  }
  cat("doy 1 done!\n")
 
  for (doy in 2:365)  {
    lst0=raster(paste0("../input/LST_cn_daily/tif/",year,"/",satellite,"/lst_",time,"_",year,"_",days[doy],".tif"))
    cat (year, doy, "\n") 
    for (city in 1:36) {
      cluster=cluster_u_r[cluster_u_r@data[,1]==index[city,7],]
      lst=mask(crop(lst0,cluster),cluster)
      rlist[[city]][,doy] = lst[]
    } #end city
  } #end doy
  
  #save
  for (city in 1:36){
    write.csv(rlist[[city]],file=paste0("../output/5.0.lst_raw/",index[city,2],"/",satellite,"_",time,"_",year,".csv"),row.names = F,col.names = F)
  }
}
stopCluster(cl)
