##create a shp file of urban cluster based on the CCA results
##here only do that for hte city >100km2

library("sp")
library("rgdal")
library("raster")
library("rgeos")
library(smoothr)

radius=2
for (year in 2018) {
  cat ("start",year,"\n")
  r = raster(paste0("../output/CCA_modis/cluster_cca_",year,".tif"))
  ##remove small cluster
  cat ("remove small cluster","\n")
  rr=table(r[])
  rr1 <- as.data.frame(rr)
  sel=which(rr1[,2]<100)                                                   
  remove=rr1[sel,1]
  remove.ing = as.integer(as.character(remove))
  r[r %in% remove.ing]=NA

  cat ("rastertopolygons","\n")
  r1=rasterToPolygons(r,n=16,na.rm=T,digits=1,dissolve=T)
  r2=r1[area(r1)>100000,]  #100km2
  cluster_1=smooth(r2,method="ksmooth",smoothness=30)#slow!
  cluster_g = fill_holes(cluster_1,threshold=1000000000)
  
  cat ("writeOGR","\n")
  writeOGR(cluster_g,dsn="../output/urban_rural_cluster_smooth_hole",layer=paste0("urban_cluster_smooth_hole_",year), driver="ESRI Shapefile", overwrite_layer=TRUE)
}
