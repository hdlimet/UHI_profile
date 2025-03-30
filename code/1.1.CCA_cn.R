# city cluster based on the MODIS 2018 land cover datasat using CCA library in R

library("sp")
library("rgdal")
library("raster")
library('osc')
rm (list=ls())

cn_shp=readOGR(dsn="../input/shapefile/CHN_adm",layer="CHN_adm0") 
proj_modis="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
cn_shp_pro=spTransform(cn_shp,proj_modis)
cn_shp_city=readOGR(dsn="../input/shapefile/CHN_adm",layer="CHN_adm2") 
cn_shp_city_pro=spTransform(cn_shp_city,proj_modis)
aa=data.frame(cn_shp_city_pro)
for (year in 2018) {
  cat ("start",year,"\n")
  lulc0 <- raster(paste0("../input/support/lulc/tif_file/lulc_modis_",year,".tif"))
  lulc_cn=mask(crop(lulc0,cn_shp_pro),cn_shp_pro)
  writeRaster(lulc_cn,paste0("../output/CCA_modis/lulc_cn_",year,".tif"),overwrite=TRUE) 
  for (nn in 1:nrow(aa)) {
   cat (nn,"/",nrow(aa),"\n")
   city_sel=cn_shp_city_pro[aa[,6]==nn,]
   lulc=mask(crop(lulc0,city_sel),city_sel)
   lulc[which(lulc[]!=13)]=0
   lulc[which(lulc[]==13)]=1 
   if (length(which(lulc[]==1))==0) {
   result=lulc*NA
   } else {
   if (aa[nn,7]%in%c("Wuhan","Hangzhou","Xiangtan","Ezhou","Nanjing","Wuzhou","Yangzhou")) {
     urban <- cca(lulc, cell.class=1, s=4*xres(lulc))  ## as these cities have big rivers that separate urban areas, so use larger values for the clustering
   } else {
     urban <- cca(lulc, cell.class=1, s=1.2*xres(lulc))
   }
   result <- lulc*NA
   result[cellFromXY(result,urban$cluster[,c("long","lat")])]<-urban$cluster[,"cluster_id"]
   }
   if (nn==1) {
    city=result
   } else {
    result1=result+max(city[],na.rm=TRUE)
    city=mosaic(city,result1,fun=mean)
   }
  }
  writeRaster(city,paste0("../output/CCA_modis/cluster_cca_",year,".tif"),overwrite=TRUE)
}