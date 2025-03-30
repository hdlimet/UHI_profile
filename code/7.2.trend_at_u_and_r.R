##calculte the trend of EVI, ISA, and estimated LST in four urban categories and rural 

library(raster)
library("sp")
library("rgdal")
library("rgeos")
library(reshape2)
library(ggplot2)
library(smoothr)
library(foreach)
rm(list=ls()) 

satellite="MYD"
times=c("day")
cluster_u0=cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="urban_cluster_captical")
cluster_r0=cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

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
  
  isa_trend=raster(paste0("../output/3.2.isa_trend/",index[city,2],"_slope.tif"))
  isa_trend_u=mask(crop(isa_trend,cluster_u),cluster_u)
  isa_trend_r=mask(crop(isa_trend,cluster_r),cluster_r)
  isa_trend_u1m=mean(isa_trend_u[u1_sel],na.rm=TRUE)
  isa_trend_u2m=mean(isa_trend_u[u2_sel],na.rm=TRUE)
  isa_trend_ur1m=mean(isa_trend_u[ur1_sel],na.rm=TRUE)
  isa_trend_ur2m=mean(isa_trend_u[ur2_sel],na.rm=TRUE)
  isa_trend_rm=mean(isa_trend_r[r_sel],na.rm=TRUE)
  
  evi_trend=raster(paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_slope.tif"))
  evi_trend_u=mask(crop(evi_trend,cluster_u),cluster_u)
  evi_trend_r=mask(crop(evi_trend,cluster_r),cluster_r)
  evi_trend_u1m=mean(evi_trend_u[u1_sel],na.rm=TRUE)
  evi_trend_u2m=mean(evi_trend_u[u2_sel],na.rm=TRUE)
  evi_trend_ur1m=mean(evi_trend_u[ur1_sel],na.rm=TRUE)
  evi_trend_ur2m=mean(evi_trend_u[ur2_sel],na.rm=TRUE)
  evi_trend_rm=mean(evi_trend_r[r_sel],na.rm=TRUE)
  
  sum1=foreach (time=1:2,.combine=cbind) %do% {
    cat (city,times[time],"\n")
    lst_trend=raster(paste0("../output/6.5.lst_season_trend/",satellite,"_",index[city,2],"_",times[time],"_summer_slope.tif"))
    lst_trend_u=mask(crop(lst_trend,cluster_u),cluster_u)
    lst_trend_r=mask(crop(lst_trend,cluster_r),cluster_r)
    lst_trend_u1m=mean(lst_trend_u[u1_sel],na.rm=TRUE)
    lst_trend_u2m=mean(lst_trend_u[u2_sel],na.rm=TRUE)
    lst_trend_ur1m=mean(lst_trend_u[ur1_sel],na.rm=TRUE)
    lst_trend_ur2m=mean(lst_trend_u[ur2_sel],na.rm=TRUE)
    lst_trend_rm=mean(lst_trend_r[r_sel],na.rm=TRUE)
    c(lst_trend_u1m,lst_trend_u2m,lst_trend_ur1m,lst_trend_ur2m,lst_trend_rm)
  }
  c(city,index[city,2],
    isa_trend_u1m,isa_trend_u2m,isa_trend_ur1m,isa_trend_ur2m,isa_trend_rm,
    evi_trend_u1m,evi_trend_u2m,evi_trend_ur1m,evi_trend_ur2m,evi_trend_rm,
    sum1)
}
cat("calculation done","\n")
sum_sum_day=data.frame(sum_sum0[,1:17])
names(sum_sum_day)=c("ioa", "city",
                 "isa_u1","isa_u2","isa_ur1","isa_ur2","isa_r",
                 "evi_u1","evi_u2","evi_ur1","evi_ur2","evi_r",
                 "lst_u1","lst_u2","lst_ur1","lst_ur2","lst_r")
write.csv(sum_sum_day,paste0("../output/7.1.year_variation/slope_trend_day.csv"),row.names=FALSE)