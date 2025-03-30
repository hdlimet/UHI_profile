##divide four urban categories and rural category based on EVI and ISA and their trends

library(reshape2)
library(smoothr)
library(foreach)
library("doParallel")
library(dplyr)
library(raster)
library("rgdal")
library("rgeos")
rm(list=ls()) 

if(!dir.exists(paste0("../output/7.0.city_division"))){dir.create(paste0("../output/7.0.city_division"))}

satellite="MYD"
cluster_u0=cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="urban_cluster_captical")
cluster_r0=cluster_u_r=readOGR(dsn="../output/urban_rural_cluster_smooth_hole_captical",layer="rural_cluster_captical")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

cl<-makeCluster(36, outfile=paste0('history_7.0.city_division'), type = "FORK")
registerDoParallel(cl)
foreach (city=1:36) %dopar% {
  cat (city,"\n")
  cluster_r=cluster_r0[cluster_r0@data[,1]==index[city,7],]
  cluster_u=cluster_u0[cluster_r0@data[,1]==index[city,7],]
  
  isa0=raster(paste0("../output/3.1.isa_mean/",index[city,2],"/isa_2003.tif"))
  isa1=raster(paste0("../output/3.1.isa_mean/",index[city,2],"/isa_2018.tif"))
  isa0_u=mask(crop(isa0,cluster_u),cluster_u)
  isa0_r=mask(crop(isa0,cluster_r),cluster_r)
  isa1_r=mask(crop(isa1,cluster_r),cluster_r)
  
  isa_trend=raster(paste0("../output/3.2.isa_trend/",index[city,2],"_slope.tif"))
  isa_trend_u=mask(crop(isa_trend,cluster_u),cluster_u)

  evi_trend=raster(paste0("../output/1.2.evi_trend/",satellite,"_",index[city,2],"_slope.tif"))
  evi_trend_u=mask(crop(evi_trend,cluster_u),cluster_u)

  water=raster(paste0("../output/4.water/",index[city,2],".tif"))
  water_u=mask(crop(water,cluster_u),cluster_u)
  water_r=mask(crop(water,cluster_r),cluster_r)
  
  isa_trend_u[which(water_u[]>0.01)]=NA
  evi_trend_u[which(water_u[]>0.01)]=NA
  isa0_u[which(water_u[]>0.01)]=NA
  isa0_r[which(water_r[]>0.01)]=NA
  isa1_r[which(water_r[]>0.01)]=NA
  
  u1_sel=which(isa_trend_u[]<=0  & evi_trend_u[]>0 & isa0_u[]>=0.5) ##urban greening
  u2_sel=which(isa_trend_u[]<=0 & evi_trend_u[]<0 & isa0_u[]>=0.5)  # baring
  ur1_sel=which(isa_trend_u[]>0  & evi_trend_u[]>0 & isa0_u[]<0.5) #urbanization & greening
  ur2_sel=which(isa_trend_u[]>0 & evi_trend_u[]<0 & isa0_u[]<0.5) ##urbanization
  r_sel=which(isa0_r[]<0.01 & isa1_r[]<0.01)
  u3_sel=which(isa_trend_u[]==0 & isa0_u[]==0) #non-change-urban
  u4_sel=which(isa_trend_u[]<=0) ##de-urbanization
  u5_sel=which(isa_trend_u[]>0) ##urbanization
  u6_sel=which(!is.na(isa_trend_u[])) 
  
  max_length <- max(length(u1_sel),length(u2_sel),length(ur1_sel),length(ur2_sel),length(r_sel),length(u3_sel),length(u4_sel),length(u5_sel),length(u6_sel))
  length(u1_sel) <- max_length                      
  length(u2_sel) <- max_length
  length(ur1_sel) <- max_length 
  length(ur2_sel) <- max_length 
  length(r_sel) <- max_length 
  length(u3_sel) <- max_length 
  length(u4_sel) <- max_length
  length(u5_sel) <- max_length 
  length(u6_sel) <- max_length 
  sum0=cbind(city,u1_sel,u2_sel,ur1_sel,ur2_sel,r_sel,u3_sel,u4_sel,u5_sel,u6_sel)
  write.csv(sum0,file=paste0("../output/7.0.city_division/",satellite,"_",index[city,2],".csv"),row.names = F,col.names = F)
}
stopCluster(cl)