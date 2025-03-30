##extract the 36 studied cities and create the rural buffer for each of them

library(rgdal)
library(rgeos)
library(raster)
library(sf)
library(smoothr)
rm (list = ls())

radius=2
cluster0=readOGR(dsn="../output/urban_rural_cluster_smooth_hole",layer = "urban_cluster_smooth_hole_2018")
index=read.csv("../input/index_cluster_captical.csv")
index[,2]=tolower(index[,2])

urban=list()
rural=list()
urban_rural=list()
for (city in c(1:36)) {
  cat (index[city,2],"\n")
  cluster_g=cluster0[cluster0@data[,1]==index[city,7],]
  shp.disa = disaggregate(cluster_g)
  if (index[city,2] %in% c("hangzhou","wuhan","nanjing")) {
    shp.final = shp.disa[rev(order(area(shp.disa)))[1:2],]
  } else {
    shp.final = shp.disa[which.max(area(shp.disa)),]
  }

  flag = area(shp.final) #this is polygon area for the whole raster
  width.value = sqrt(radius*flag/pi)-sqrt(flag/pi)
  r3<- gBuffer(shp.final,byid=T, width = width.value, quadsegs = 10)
  r4<- erase(r3,shp.final)  #slow

  urban[[city]]=shp.final
  rural[[city]]=r4
  urban_rural[[city]]=r3
}
urban_sum=bind(urban)
rural_sum=bind(rural)
urban_rural_sum=bind(urban_rural)
writeOGR(urban_sum,dsn="../output/urban_rural_cluster_smooth_hole_captical",layer = paste0("urban_cluster_captical"),driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(rural_sum,dsn="../output/urban_rural_cluster_smooth_hole_captical",layer = paste0("rural_cluster_captical"),driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(urban_rural_sum,dsn="../output/urban_rural_cluster_smooth_hole_captical",layer = paste0("urban_rural_cluster_captical"),driver="ESRI Shapefile", overwrite_layer=TRUE)
