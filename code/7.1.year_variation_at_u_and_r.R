##calculte the mean EVI, ISA, estimated LST, cm, ca, cp for each year in four urban categories and rural 

library("raster")
library("sp")
library("rgdal")
library("rgeos")
library(reshape2)
library(ggplot2)
library(foreach)
library("doParallel")
rm(list=ls()) 

if(!dir.exists(paste0("../output/7.1.year_variation"))){dir.create(paste0("../output/7.1.year_variation"))}

satellite="MYD"
times=c("day")
cluster_u0=cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="urban_cluster_captical")
cluster_r0=cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

cl<-makeCluster(16, outfile=paste0('history_7.1.year_variation'), type = "FORK")
registerDoParallel(cl)
sum_sum0=foreach (city=1:36,.combine=rbind) %do% {
  cat (city,index[city,2],"\n")
  cluster_r=cluster_r0[cluster_r0@data[,1]==index[city,7],]
  cluster_u=cluster_u0[cluster_r0@data[,1]==index[city,7],]
  
  divide=read.csv(paste0("../output/7.0.city_division/",satellite,"_",index[city,2],".csv"))
  names(divide)= c("city","u1","u2","ur1","ur2","r","u3","u4","u5","u6")
  u1_sel=divide$u1[!is.na(divide$u1)]
  u2_sel=divide$u2[!is.na(divide$u2)]
  ur1_sel=divide$ur1[!is.na(divide$ur1)]
  ur2_sel=divide$ur2[!is.na(divide$ur2)]
  r_sel=divide$r[!is.na(divide$r)]
  
  sum=foreach (year=2003:2018,.combine=rbind) %dopar% {
    isa=raster(paste0("../output/3.1.isa_mean/",index[city,2],"/isa_",year,".tif"))
    evi=raster(paste0("../output/1.1.evi_mean/",index[city,2],"/",satellite,"_evi_",year,".tif"))
     
    isa_u=mask(crop(isa,cluster_u),cluster_u)
    isa_r=mask(crop(isa,cluster_r),cluster_r)
    evi_u=mask(crop(evi,cluster_u),cluster_u)
    evi_r=mask(crop(evi,cluster_r),cluster_r)
    
    isa_u1m=mean(isa_u[u1_sel],na.rm=TRUE)
    isa_u2m=mean(isa_u[u2_sel],na.rm=TRUE)
    isa_ur1m=mean(isa_u[ur1_sel],na.rm=TRUE)
    isa_ur2m=mean(isa_u[ur2_sel],na.rm=TRUE)
    isa_rm=mean(isa_r[r_sel],na.rm=TRUE)
    
    evi_u1m=mean(evi_u[u1_sel],na.rm=TRUE)
    evi_u2m=mean(evi_u[u2_sel],na.rm=TRUE)
    evi_ur1m=mean(evi_u[ur1_sel],na.rm=TRUE)
    evi_ur2m=mean(evi_u[ur2_sel],na.rm=TRUE)
    evi_rm=mean(evi_r[r_sel],na.rm=TRUE)
    
    sum1=foreach (time=1,.combine=cbind) %do% {
      cat (city,year,times[time],"\n")
      lst=raster(paste0("../output/6.4.lst_season_mean/",index[city,2],"/",satellite,"_",times[time],"_",year,"_summer.tif"))
      cm=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",times[time],"_",year,"_cm.tif"))
      ca=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",times[time],"_",year,"_ca.tif"))
      cp=raster(paste0("../output/6.1.atc_fit/",index[city,2],"/",satellite,"_",times[time],"_",year,"_cp.tif"))
      
      lst_u=mask(crop(lst,cluster_u),cluster_u)
      lst_r=mask(crop(lst,cluster_r),cluster_r)
      cm_u=mask(crop(cm,cluster_u),cluster_u)
      cm_r=mask(crop(cm,cluster_r),cluster_r)
      ca_u=mask(crop(ca,cluster_u),cluster_u)
      ca_r=mask(crop(ca,cluster_r),cluster_r)
      cp_u=mask(crop(cp,cluster_u),cluster_u)
      cp_r=mask(crop(cp,cluster_r),cluster_r)
      
      lst_u1m=mean(lst_u[u1_sel],na.rm=TRUE)
      lst_u2m=mean(lst_u[u2_sel],na.rm=TRUE)
      lst_ur1m=mean(lst_u[ur1_sel],na.rm=TRUE)
      lst_ur2m=mean(lst_u[ur2_sel],na.rm=TRUE)
      lst_rm=mean(lst_r[r_sel],na.rm=TRUE)
      
      cm_u1m=mean(cm_u[u1_sel],na.rm=TRUE)
      cm_u2m=mean(cm_u[u2_sel],na.rm=TRUE)
      cm_ur1m=mean(cm_u[ur1_sel],na.rm=TRUE)
      cm_ur2m=mean(cm_u[ur2_sel],na.rm=TRUE)
      cm_rm=mean(cm_r[r_sel],na.rm=TRUE)
      
      ca_u1m=mean(ca_u[u1_sel],na.rm=TRUE)
      ca_u2m=mean(ca_u[u2_sel],na.rm=TRUE)
      ca_ur1m=mean(ca_u[ur1_sel],na.rm=TRUE)
      ca_ur2m=mean(ca_u[ur2_sel],na.rm=TRUE)
      ca_rm=mean(ca_r[r_sel],na.rm=TRUE)
      
      cp_u1m=mean(cp_u[u1_sel],na.rm=TRUE)
      cp_u2m=mean(cp_u[u2_sel],na.rm=TRUE)
      cp_ur1m=mean(cp_u[ur1_sel],na.rm=TRUE)
      cp_ur2m=mean(cp_u[ur2_sel],na.rm=TRUE)
      cp_rm=mean(cp_r[r_sel],na.rm=TRUE)
    
      c(lst_u1m,lst_u2m,lst_ur1m,lst_ur2m,lst_rm,
        cm_u1m,cm_u2m,cm_ur1m,cm_ur2m,cm_rm,
        ca_u1m,ca_u2m,ca_ur1m,ca_ur2m,ca_rm,
        cp_u1m,cp_u2m,cp_ur1m,cp_ur2m,cp_rm)
    }
    c(city,index[city,2],year,
      isa_u1m,isa_u2m,isa_ur1m,isa_ur2m,isa_rm,
      evi_u1m,evi_u2m,evi_ur1m,evi_ur2m,evi_rm,
      sum1)
  }
  sum
}
stopCluster(cl)
cat("calculation done","\n")
sum_sum_day=data.frame(sum_sum0[,1:33])
names(sum_sum_day)=c("ioa","city","year",
                 "isa_u1","isa_u2","isa_ur1","isa_ur2","isa_r",
                 "evi_u1","evi_u2","evi_ur1","evi_ur2","evi_r",
                 "lst_u1","lst_u2","lst_ur1","lst_ur2","lst_r",
                 "cm_u1","cm_u2","cm_ur1","cm_ur2","cm_r",
                 "ca_u1","ca_u2","ca_ur1","ca_ur2","ca_r",
                 "cp_u1","cp_u2","cp_ur1","cp_ur2","cp_r")
write.csv(sum_sum_day,paste0("../output/7.1.year_variation/",satellite,"_temporal_variation_day.csv"),row.names=FALSE)