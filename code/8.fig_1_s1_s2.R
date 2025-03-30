###plot fig. 1, S1, S2
##notes: UedG="u1",UedB="u2",UingG="ur1",UingB="ur2",Rural="r"
library(reshape2)
library(foreach)
library(ggplot2)
library(smoothr)
library(dplyr)
library(sf)
library(rgdal)
library(rgeos)
library(raster)
rm(list=ls()) 

##general variables
{
rf=c("orangered4","orangered","royalblue4","royalblue","forestgreen")
time='day'
satellite="MYD"
index=read.csv("../input/index_cluster_captical.csv")
ref="+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
shp_cn0=st_transform(st_read(dsn="../input/china-shapefiles-master",layer="simplied_china_country"),ref)
shp_cn=st_transform(st_read(dsn="../input/china-shapefiles-master",layer="china"),ref)
lonlat<-st_transform(st_as_sf(index, coords = c("lonlat.lon","lonlat.lat"),crs = 4326),crs=ref)
plot_map = ggplot()+
  geom_sf(data=shp_cn,color = "grey59", fill = NA,size=0.2)+
  geom_sf(data=shp_cn0,color = "black", fill = "wheat1",size=0.2, alpha=0.1)+
  coord_sf(crs = st_crs(shp_cn0),xlim=c((st_bbox(shp_cn0)$xmin-200000),(st_bbox(shp_cn0)$xmax+200000)),
                              ylim=c((st_bbox(shp_cn0)$ymin-100000),(st_bbox(shp_cn0)$ymax+100000)),expand=FALSE)+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.title=element_blank(),
                   axis.text=element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = "none",
                   plot.margin = unit(c(0,0,0,0), "cm")) 

input_temporal=read.csv(paste0("../output/7.1.year_variation/",satellite,"_temporal_variation_",time,".csv"))
input_temporal_diff=data.frame(input_temporal[,c(1,3)],
                      (input_temporal$evi_u1-input_temporal$evi_r),(input_temporal$evi_u2-input_temporal$evi_r),
                      (input_temporal$evi_ur1-input_temporal$evi_r),(input_temporal$evi_ur2-input_temporal$evi_r),
                      (input_temporal$isa_u1-input_temporal$isa_r),(input_temporal$isa_u2-input_temporal$isa_r),
                      (input_temporal$isa_ur1-input_temporal$isa_r),(input_temporal$isa_ur2-input_temporal$isa_r))
names(input_temporal_diff)=c("ioa","year",
                "evi_u1_r","evi_u2_r",
                "evi_ur1_r","evi_ur2_r",
                "isa_u1_r","isa_u2_r",
                "isa_ur1_r","isa_ur2_r")
input_slope=read.csv(paste0("../output/7.1.year_variation/slope_trend_",time,".csv"))

mean_bin=apply(input_temporal[input_temporal$year %in% 2003:2005,-c(1:3)],2, mean,na.rm=TRUE)
mean_end=apply(input_temporal[input_temporal$year %in% 2016:2018,-c(1:3)],2, mean,na.rm=TRUE)
sd_bin=apply(input_temporal[input_temporal$year %in% 2003:2005,-c(1:3)],2, sd,na.rm=TRUE)
sd_end=apply(input_temporal[input_temporal$year %in% 2016:2018,-c(1:3)],2, sd,na.rm=TRUE)
}

#######distance plot##############
{
cluster_u0=readOGR(dsn="../input/urban_rural_cluster_smooth_hole_captical",layer="urban_cluster_captical")
sum0=foreach (city=1:36,.combine=rbind) %do% {
  cat (city,"\n")
  cluster_u=cluster_u0[cluster_u0@data[,1]==index[city,7],]
  isa_trend=raster(paste0("../output/3.2.isa_trend/",index[city,2],"_slope.tif"))
  isa_trend_u=mask(crop(isa_trend,cluster_u),cluster_u)
  
  nearestD0 =distance(isa_trend_u)
  nearestD0[nearestD0==0]=NA
  nearestD=distance(nearestD0)
  nearestD[nearestD==0]=NA
  
  divide=read.csv(paste0("../output/7.0.city_division/",satellite,"_",index[city,2],".csv"))
  u1_sel=divide$u1[!is.na(divide$u1)]
  u2_sel=divide$u2[!is.na(divide$u2)]
  ur1_sel=divide$ur1[!is.na(divide$ur1)]
  ur2_sel=divide$ur2[!is.na(divide$ur2)]
  r_sel=divide$r[!is.na(divide$r)]
  cbind(city,
        rbind(c(1,mean(nearestD[u1_sel])),
              c(2,mean(nearestD[u2_sel])),
              c(3,mean(nearestD[ur1_sel])),
              c(4,mean(nearestD[ur2_sel]))))
}
sum1=as.data.frame(sum0)
names(sum1)=c("ioa","position","value")
sum1$value=sum1$value/1000
sum_sum_mean=aggregate(sum1,by=list(sum1$position),FUN=mean,na.rm=TRUE)

plot_dis= ggplot(sum1,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1,alpha=0.75)+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB","Rural"))+
  scale_y_continuous(limits=c(1,6),breaks =seq(0,6,by=2))+
  ylab("Distance to urban boundary (km)")+ labs(fill="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   panel.border=element_blank(),
                   legend.position = "none",
                   axis.line=element_line(color="black",size=0.1),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(colour="black", size=7, face="plain"),
                   axis.text=element_text(colour="black", size=7, face="plain"),
                   plot.title = element_blank(),
                   axis.ticks = element_line(color = "black",size=0.2),    
                   axis.ticks.length = unit(1.5, "pt"),
                   panel.background = element_rect(fill = "transparent", colour = NA),  
                   plot.background = element_rect(fill = "transparent", colour = NA))
}

#####Area ratio######
{
sum_area=foreach (city=1:36,.combine=rbind) %do% {
  cat (city,"\n")
  divide=read.csv(paste0("../output/7.0.city_division/",satellite,"_",index[city,2],".csv"))[,1:5]
  names(divide)= c("city","u1","u2","ur1","ur2")
  u1_sel=divide$u1[!is.na(divide$u1)] #u1_sel=which(isa_trend_u[]<=0  & evi_trend_u[]>0 & isa0_u[]>0) ##urban greening
  u2_sel=divide$u2[!is.na(divide$u2)] #u2_sel=which(isa_trend_u[]<=0 & evi_trend_u[]<0 & isa0_u[]>0)  # baring
  ur1_sel=divide$ur1[!is.na(divide$ur1)]# ur1_sel=which(isa_trend_u[]>0  & evi_trend_u[]>0) #urbanization & greening
  ur2_sel=divide$ur2[!is.na(divide$ur2)]#ur2_sel=which(isa_trend_u[]>0 & evi_trend_u[]<0) ##urbanization
  c(city,length(u1_sel),length(u2_sel),length(ur1_sel),length(ur2_sel))
}

###I#########
input0=as.data.frame(sum_area)
input0[37,]=apply(input0,2,sum)
input0=cbind(input0[,1],c(index[,2],"All"),input0[,2:5])
input0$size=apply(input0[,3:6],1,sum)
input=input0 %>% arrange(desc(size))
level0=seq(1,4,by=1)
sum_area_ratio=foreach (city=1:37,.combine=rbind) %do% {
  level1=input[city,3]/sum(input[city,3:6])*100
  level2=input[city,4]/sum(input[city,3:6])*100
  level3=input[city,5]/sum(input[city,3:6])*100
  level4=input[city,6]/sum(input[city,3:6])*100
  cbind(city,level0,rbind(level1,level2,level3,level4))
}
sum_area_ratio1=as.data.frame(sum_area_ratio)
names(sum_area_ratio1)=c("city","level","percent")
plot_sum_percent=ggplot(sum_area_ratio1,aes(x=as.factor(city),y=percent,fill=as.factor(level))) + 
  geom_bar(stat="summary",width = 0.8)+
  scale_fill_manual(values=rf[1:4],label=c("UedG","UedD","UingG","UingD"))+
  guides(fill=guide_legend(nrow = 1, ncol = 4,override.aes = list(size=2)))+
  scale_x_discrete(labels = input[,2])+
  scale_y_continuous(limits = c(0,106))+
  ylab("Percent (%)")+xlab("")+
  labs(fill="")+theme_bw() +
  theme(text=element_text(family="serif"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour="black", size=8, face="plain"),
        axis.text.x =element_text(colour="black", size=8, face="plain",angle = 50,hjust = 1.2,vjust=1.1),
        axis.text.y =element_text(colour="black", size=8, face="plain"),
        panel.grid = element_blank(),
        legend.position = c(0.5,1.01),
        legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
        legend.text = element_text(colour="black", size=8, face="plain"),
        legend.title= element_text(colour="black", size=8, face="plain"),
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(0.25, "cm"),
        plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"),
        plot.title = element_blank())

sum_area_ratio=foreach (city=1:37,.combine=rbind) %do% {
  level1=input0[city,3]/sum(input0[city,3:6])*100
  level2=input0[city,4]/sum(input0[city,3:6])*100
  level3=input0[city,5]/sum(input0[city,3:6])*100
  level4=input0[city,6]/sum(input0[city,3:6])*100
  cbind(city,level1,level2,level3,level4,level5=0.00001)
}
sum_area_ratio2=data.frame(sum_area_ratio,lon=c(st_coordinates(lonlat)[,1],quantile(st_coordinates(lonlat)[,1],0.1)),
                           lat=c(st_coordinates(lonlat)[,2],max(st_coordinates(lonlat)[,2])))
sum_area_ratio2[sum_area_ratio2$city==1,]$lat=sum_area_ratio2[sum_area_ratio2$city==1,]$lat+60000
sum_area_ratio2[sum_area_ratio2$city==36,]$lat=sum_area_ratio2[sum_area_ratio2$city==36,]$lat-90000
library(scatterpie)
plot_sum_percent_map = ggplot()+
  geom_sf(data=shp_cn,color = "grey59", fill = NA,size=0.2)+
  geom_sf(data=shp_cn0,color = "black", fill = "wheat1",size=0.2, alpha=0.1)+
  geom_scatterpie(aes(x=lon, y=lat, group=city,r=80000),data=sum_area_ratio2[1:36,],
                  cols=c("level1","level2","level3","level4"),color=NA,alpha=0.65)+
  scale_fill_manual(values=rf,label=c("UedG","UedB","UingG","UingB"))+
  labs(fill="") + ylab("") + xlab("")+
  coord_sf(crs = st_crs(shp_cn0),xlim=c((st_bbox(shp_cn0)$xmin-200000),(st_bbox(shp_cn0)$xmax+200000)),
           ylim=c((st_bbox(shp_cn0)$ymin-100000),(st_bbox(shp_cn0)$ymax+100000)),expand=FALSE)+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.title=element_blank(),
                   axis.text=element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   panel.border = element_blank(),
                   legend.position = c(0.45,0.93),
                   legend.direction = "horizontal",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.3, "cm"),
                   legend.key.width = unit(0.3, "cm"),
                   plot.margin = unit(c(0,0,0,0), "cm"))
subplot = ggplot()+
    geom_scatterpie(aes(x=lon, y=lat, group=city,r=800000),data=sum_area_ratio2[36:37,],
                    cols=c("level1","level2","level3","level4"),color=NA,alpha=0.65)+
    scale_fill_manual(values=rf[1:4],label=c("UedG","UedD","UingG","UingD"))+
    coord_sf(crs = st_crs(shp_cn0),xlim=c((sum_area_ratio2[37,]$lon-1000000),(sum_area_ratio2[37,]$lon+1000000)),
                   ylim=c((sum_area_ratio2[37,]$lat-1000000),(sum_area_ratio2[37,]$lat+1000000)), expand=FALSE)+
    theme_bw()+theme(text=element_text(family="serif"),
                     legend.position ="none",
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid = element_blank(),
                     panel.border  = element_blank(),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     panel.background = element_rect(fill = "transparent", colour = NA), 
                     plot.background = element_rect(fill = "transparent", colour = NA))
plot_sum_percent_map = plot_sum_percent_map + 
  annotation_custom(grob = ggplotGrob(subplot), 
                    xmin=(sum_area_ratio2[37,]$lon-400000), xmax=(sum_area_ratio2[37,]$lon+400000), 
                    ymin=(sum_area_ratio2[37,]$lat-600000), ymax=(sum_area_ratio2[37,]$lat+200000))+
  annotate("text", x =sum_area_ratio2[37,]$lon, y = (sum_area_ratio2[37,]$lat+260000),
           label = "Average",size=3,family="serif")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))
}

#####plot_column_begin-end#####
{
input_bin_end_isa=data.frame(rbind(cbind(1,1,mean_bin[1],sd_bin[1]),
                                  cbind(2,1,mean_bin[2],sd_bin[2]),
                                  cbind(3,1,mean_bin[3],sd_bin[3]),
                                  cbind(4,1,mean_bin[4],sd_bin[4]),
                                  cbind(5,1,mean_bin[5],sd_bin[5]),
                                  cbind(1,2,mean_end[1],sd_end[1]),
                                  cbind(2,2,mean_end[2],sd_end[2]),
                                  cbind(3,2,mean_end[3],sd_end[3]),
                                  cbind(4,2,mean_end[4],sd_end[4]),
                                  cbind(5,2,mean_end[5],sd_end[5])))
names(input_bin_end_isa)=c("position","year","mean","sd")
plot_bar_isa = ggplot(data=input_bin_end_isa, aes(x=as.factor(position), y=mean,fill=as.factor(year))) +
  geom_bar(stat="identity",position=position_dodge(),alpha=0.65)+
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25,size=0.3,position=position_dodge(.9))+
  ylab("ISA")+ xlab("")+ labs(fill="")+
  scale_fill_manual(values=c("orangered4","royalblue4"),
                    label=c("Initial","Final"))+
  scale_x_discrete(labels = c("UedG","UedB","UingG","UingB","Rural"))+
  scale_y_continuous(breaks =seq(0,1,by=0.2))+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.title.y=element_text(colour="black", size=9, face="plain"),
                   axis.title.x=element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   panel.grid = element_blank(),
                   legend.position = c(0.9,0.93),
                   legend.direction =  "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=7, face="plain"),
                   legend.title= element_text(colour="black", size=7, face="plain",margin = margin(b = -0.1),hjust = 0.3),
                   legend.key.size = unit(0.4, "cm"),
                   legend.key.width = unit(0.4, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))

input_bin_end_evi=data.frame(rbind(   cbind(1,1,mean_bin[6],sd_bin[6]),
                                      cbind(2,1,mean_bin[7],sd_bin[7]),
                                      cbind(3,1,mean_bin[8],sd_bin[8]),
                                      cbind(4,1,mean_bin[9],sd_bin[9]),
                                      cbind(5,1,mean_bin[10],sd_bin[10]),
                                      cbind(1,2,mean_end[6],sd_end[6]),
                                      cbind(2,2,mean_end[7],sd_end[7]),
                                      cbind(3,2,mean_end[8],sd_end[8]),
                                      cbind(4,2,mean_end[9],sd_end[9]),
                                      cbind(5,2,mean_end[10],sd_end[10])))
names(input_bin_end_evi)=c("position","year","mean","sd")
plot_bar_evi = ggplot(data=input_bin_end_evi, aes(x=as.factor(position), y=mean,fill=as.factor(year))) +
  geom_bar(stat="identity",position=position_dodge(),alpha=0.65)+
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25,size=0.3,position=position_dodge(.9))+
  ylab("EVI")+ xlab("")+ labs(fill="")+
  scale_fill_manual(values=c("orangered4","royalblue4"),
                    label=c("Initial","Final"))+
  scale_x_discrete(labels = c("UedG","UedB","UingG","UingB","Rural"))+
  scale_y_continuous(breaks =seq(0,0.5,by=0.1))+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.title.y=element_text(colour="black", size=9, face="plain"),
                   axis.title.x=element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   panel.grid = element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
}
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 200), ylim = c(0, 100), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_sum_percent_map), xmin = 0, xmax = 120, ymin = 0, ymax = 100) +
  annotation_custom(ggplotGrob(plot_dis), xmin = 5, xmax = 50, ymin = 3, ymax = 40) +
  annotation_custom(ggplotGrob(plot_bar_isa), xmin = 120, xmax = 200, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_bar_evi), xmin = 120, xmax = 200, ymin = 0, ymax = 50) +
  annotate("text", x =8, y =96, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =8, y =43, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =165, y =96, label = "(c)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =165, y =46, label = "(d)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_1.tif"),width=20,height=10, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()
################################################
#####temporal variation of ISA, EVI, LST########
{
input1=input_temporal[,-2]
input1_mean=aggregate(input1,by=list(input1$year),FUN=mean,na.rm=TRUE)
input1_sd=aggregate(input1,by=list(input1$year),FUN=sd,na.rm=TRUE)
colnames(input1_mean) = paste0("mean_",colnames(input1_mean))
colnames(input1_sd) = paste0("sd_",colnames(input1_sd))
names(input1_mean)[1]="year"
names(input1_sd)[1]="year"

sum_merge_isa= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_isa_u1,input1_sd$sd_isa_u1),
                                cbind(12,input1_mean$year,input1_mean$mean_isa_u2,input1_sd$sd_isa_u2),
                                cbind(21,input1_mean$year,input1_mean$mean_isa_ur1,input1_sd$sd_isa_ur1),
                                cbind(22,input1_mean$year,input1_mean$mean_isa_ur2,input1_sd$sd_isa_ur2),
                                cbind(33,input1_mean$year,input1_mean$mean_isa_r,input1_sd$sd_isa_r)))
names(sum_merge_isa)=c("position","year","mean","sd")
cor.test(subset(sum_merge_isa,position==11)$mean,subset(sum_merge_isa,position==11)$year)$p.value
cor.test(subset(sum_merge_isa,position==12)$mean,subset(sum_merge_isa,position==12)$year)$p.value
cor.test(subset(sum_merge_isa,position==21)$mean,subset(sum_merge_isa,position==21)$year)$p.value
cor.test(subset(sum_merge_isa,position==22)$mean,subset(sum_merge_isa,position==22)$year)$p.value
cor.test(subset(sum_merge_isa,position==33)$mean,subset(sum_merge_isa,position==33)$year)$p.value
plot_temporal_isa=ggplot()+
  geom_point(data= sum_merge_isa,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_isa,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_isa,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                        color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab("ISA")+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y = element_text(colour="black", size=8, face="plain"),
                   axis.text = element_text(colour="black", size=8, face="plain"),
                   legend.position = c(0.2,0.8),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.3, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.title = element_blank(),
                   plot.margin = margin(0, 0, 0, 0, "cm"))

sum_merge_evi= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_evi_u1,input1_sd$sd_evi_u1),
                                cbind(12,input1_mean$year,input1_mean$mean_evi_u2,input1_sd$sd_evi_u2),
                                cbind(21,input1_mean$year,input1_mean$mean_evi_ur1,input1_sd$sd_evi_ur1),
                                cbind(22,input1_mean$year,input1_mean$mean_evi_ur2,input1_sd$sd_evi_ur2),
                                cbind(33,input1_mean$year,input1_mean$mean_evi_r,input1_sd$sd_evi_r)))
names(sum_merge_evi)=c("position","year","mean","sd")
cor.test(subset(sum_merge_evi,position==11)$mean,subset(sum_merge_evi,position==11)$year)$p.value
cor.test(subset(sum_merge_evi,position==12)$mean,subset(sum_merge_evi,position==12)$year)$p.value
cor.test(subset(sum_merge_evi,position==21)$mean,subset(sum_merge_evi,position==21)$year)$p.value
cor.test(subset(sum_merge_evi,position==22)$mean,subset(sum_merge_evi,position==22)$year)$p.value
cor.test(subset(sum_merge_evi,position==33)$mean,subset(sum_merge_evi,position==33)$year)$p.value
plot_temporal_evi=ggplot()+
  geom_point(data= sum_merge_evi,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_evi,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_evi,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                        color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab("EVI")+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y = element_text(colour="black", size=8, face="plain"),
                   axis.text = element_text(colour="black", size=8, face="plain"),
                   legend.position = "none",
                   plot.title = element_blank(),
                   plot.margin = margin(0, 0, 0, 0, "cm"))

#####difference between urban and rural#####
input1=input_temporal_diff
input1_mean=aggregate(input1,by=list(input1$year),FUN=mean,na.rm=TRUE)
input1_sd=aggregate(input1,by=list(input1$year),FUN=sd,na.rm=TRUE)
colnames(input1_mean) = paste0("mean_",colnames(input1_mean))
colnames(input1_sd) = paste0("sd_",colnames(input1_sd))
names(input1_mean)[1]="year"
names(input1_sd)[1]="year"

sum_merge_evi= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_evi_u1_r,input1_sd$sd_evi_u1_r),
                                cbind(12,input1_mean$year,input1_mean$mean_evi_u2_r,input1_sd$sd_evi_u2_r),
                                cbind(21,input1_mean$year,input1_mean$mean_evi_ur1_r,input1_sd$sd_evi_ur1_r),
                                cbind(22,input1_mean$year,input1_mean$mean_evi_ur2_r,input1_sd$sd_evi_ur2_r)))
names(sum_merge_evi)=c("position","year","mean","sd")
cor.test(subset(sum_merge_evi,position==11)$mean,subset(sum_merge_evi,position==11)$year)$p.value
cor.test(subset(sum_merge_evi,position==12)$mean,subset(sum_merge_evi,position==12)$year)$p.value
cor.test(subset(sum_merge_evi,position==21)$mean,subset(sum_merge_evi,position==21)$year)$p.value
cor.test(subset(sum_merge_evi,position==22)$mean,subset(sum_merge_evi,position==22)$year)$p.value
plot_temporal_evi_diff=ggplot()+
  geom_point(data= sum_merge_evi,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_evi,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_evi,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                        color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[1:4],label=c("U1","U2","UR1","UR2"))+  
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(paste(Delta,"EVI")))+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y = element_text(colour="black", size=8, face="plain"),
                   axis.text = element_text(colour="black", size=8, face="plain"),
                   legend.position = "none",
                   plot.title = element_blank(),
                   plot.margin = margin(0, 0, 0, 0, "cm"))

sum_merge_isa= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_isa_u1_r,input1_sd$sd_isa_u1_r),
                                        cbind(12,input1_mean$year,input1_mean$mean_isa_u2_r,input1_sd$sd_isa_u2_r),
                                        cbind(21,input1_mean$year,input1_mean$mean_isa_ur1_r,input1_sd$sd_isa_ur1_r),
                                        cbind(22,input1_mean$year,input1_mean$mean_isa_ur2_r,input1_sd$sd_isa_ur2_r)))
names(sum_merge_isa)=c("position","year","mean","sd")
plot_temporal_isa_diff=ggplot()+
  geom_point(data= sum_merge_isa,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_isa,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_isa,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                                color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[1:4],label=c("U1","U2","UR1","UR2"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(paste(Delta,"ISA")))+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_text(colour="black", size=8, face="plain"),
                   axis.text=element_text(colour="black", size=8, face="plain"),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
}

plot_sum=ggplot() +
  coord_equal(xlim = c(0, 155), ylim = c(0, 72), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_temporal_isa), xmin = 0, xmax = 50, ymin = 0, ymax = 70) +
  annotation_custom(ggplotGrob(plot_temporal_evi), xmin = 52, xmax = 102,  ymin = 0, ymax = 70) +
  annotation_custom(ggplotGrob(plot_temporal_evi_diff), xmin = 104, xmax = 154, ymin = 0, ymax = 70) +
  annotate("text", x =2, y =71, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =54, y =71, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =106, y =71, label = "(c)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s1.tif"),width=15.5,height=7.2, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()



################################################
###########map##################################
##input####################
{
input_sum=data.frame(input_temporal)
input_mean=aggregate(input_sum,by=list(input_sum$ioa),FUN=mean,na.rm=TRUE)
colnames(input_mean) = paste0("mean_",colnames(input_mean))
colnames(input_slope) = paste0("slope_",colnames(input_slope))
sum_sum=data.frame(input_mean[,-3],input_slope[,-2],lon=st_coordinates(lonlat)[,1],lat=st_coordinates(lonlat)[,2])
sum_sum[37,]=apply(sum_sum,2,FUN = mean)
sum_sum[37,]$lon=quantile(st_coordinates(lonlat)[,1],0.1)
sum_sum[37,]$lat=max(st_coordinates(lonlat)[,2])
}
####plot bar map###########
{
sum_sum[sum_sum$mean_ioa ==1,]$lat=sum_sum[sum_sum$mean_ioa ==1,]$lat+60000#beijing
sum_sum[sum_sum$mean_ioa ==36,]$lat=sum_sum[sum_sum$mean_ioa ==36,]$lat-90000#shenzhen
sum_sum[sum_sum$mean_ioa ==10,]$lat=sum_sum[sum_sum$mean_ioa ==10,]$lat+100000 #nanjing
sum_sum[sum_sum$mean_ioa ==34,]$lat=sum_sum[sum_sum$mean_ioa ==34,]$lat-90000 #ningbo
sum_sum[sum_sum$mean_ioa ==4,]$lon=sum_sum[sum_sum$mean_ioa ==4,]$lon-90000 #taiyuan

summary(sum_sum$mean_evi_u1)
summary(sum_sum$mean_evi_u2)
summary(sum_sum$mean_evi_ur1)
summary(sum_sum$mean_evi_ur2)
summary(sum_sum$mean_evi_r)
plot_map_bar_evi=plot_map
for (city in 1:37) {
  sel=sum_sum[city,]
  bar_data=data.frame(position=1:5,
                      value=rbind(sel$mean_evi_u1,sel$mean_evi_u2,
                                  sel$mean_evi_ur1,sel$mean_evi_ur2,
                                  sel$mean_evi_r))
  plot_bar <- ggplot(data=bar_data, aes(x=position,y=value,fill=as.factor(position))) +
    geom_bar(stat="identity",position = position_dodge(0.1),width = 0.9)+
    scale_fill_manual(values=rf,label=c("UedG","UedD","UingG","UingD","Rural"))+
    ylab("")+ xlab("")+
    coord_cartesian(ylim = c(0.13,0.47))+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.background = element_rect(fill='transparent'),
                     plot.background = element_rect(fill='transparent', color=NA),
                     panel.grid = element_blank(),
                     panel.border=element_blank(),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = "none",
                     plot.margin = unit(c(0,0,0,0), "cm"))
  plot_map_bar_evi = plot_map_bar_evi + 
    annotation_custom(grob = ggplotGrob(plot_bar), 
                      xmin=(sel$lon-160000), xmax=(sel$lon+160000), 
                      ymin=(sel$lat-130000), ymax=(sel$lat+130000))+
    {if (city==37) annotate("text", x =sum_sum[37,]$lon, y = (sum_sum[37,]$lat+260000),
                            label = "Average",size=3,family="serif")} 
}
legend_evi=data.frame(range=c(0,0.47))
plot_map_bar_evi_legend=ggplot(data=legend_evi, aes(x=1,y=range)) +
  geom_bar(stat="identity",width = 0.2,fill="black")+
  ylab("")+ xlab("")+
  coord_cartesian(ylim = c(0,0.47))+
  scale_y_continuous(breaks=c(0,0.2,0.4))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.background = element_rect(fill='transparent'),
                   plot.background = element_rect(fill='transparent', color=NA),
                   panel.grid = element_blank(),
                   #panel.border=element_blank(),
                   axis.title = element_blank(),
                   axis.text.y = element_text(colour="black", size=6, face="plain"),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(linewidth = 0.5),
                   legend.position = "none",
                   plot.margin = unit(c(0,0,0,0), "cm"))
plot_map_bar_evi = plot_map_bar_evi + 
  annotation_custom(grob = ggplotGrob(plot_map_bar_evi_legend), 
                    xmin=(min(sum_sum$lon)-1000000-160000), xmax=(min(sum_sum$lon)-1000000+160000), 
                    ymin=(min(sum_sum$lat)+1500-130000), ymax=(min(sum_sum$lat)+1500+130000)) 

summary(sum_sum$slope_evi_u1)
summary(sum_sum$slope_evi_u2)
summary(sum_sum$slope_evi_ur1)
summary(sum_sum$slope_evi_ur2)
summary(sum_sum$slope_evi_r)
plot_map_bar_evi_slope=plot_map
for (city in 1:37) {
  sel=sum_sum[city,]
  bar_data=data.frame(position=1:5,
                      value=rbind(sel$slope_evi_u1,sel$slope_evi_u2,
                                  sel$slope_evi_ur1,sel$slope_evi_ur2,
                                  sel$slope_evi_r))
  plot_bar <- ggplot(data=bar_data, aes(x=position,y=value,fill=as.factor(position))) +
    geom_bar(stat="identity",position = position_dodge(0.1),width = 0.9)+
    scale_fill_manual(values=rf,label=c("UedG","UedD","UingG","UingD","Rural"))+
    ylab("")+ xlab("")+
    coord_cartesian(ylim = c(-0.005,0.006))+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.background = element_rect(fill='transparent'),
                     plot.background = element_rect(fill='transparent', color=NA),
                     panel.grid = element_blank(),
                     panel.border=element_blank(),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = "none",
                     plot.margin = unit(c(0,0,0,0), "cm"))
  plot_map_bar_evi_slope = plot_map_bar_evi_slope + 
    annotation_custom(grob = ggplotGrob(plot_bar), 
                      xmin=(sel$lon-160000), xmax=(sel$lon+160000), 
                      ymin=(sel$lat-130000), ymax=(sel$lat+130000))+
    {if (city==37) annotate("text", x =sum_sum[37,]$lon, y = (sum_sum[37,]$lat+260000),
                            label = "Average",size=3,family="serif")} 
}
legend_evi_slope=data.frame(range=c(-0.005,0.006))
plot_map_bar_evi_slope_legend=ggplot(data=legend_evi_slope, aes(x=1,y=range)) +
  geom_bar(stat="identity",width = 0.2,fill="black")+
  ylab("")+ xlab("")+
  coord_cartesian(ylim = c(-0.005,0.006))+
  scale_y_continuous(breaks=c(-0.005,0,0.006))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.background = element_rect(fill='transparent'),
                   plot.background = element_rect(fill='transparent', color=NA),
                   panel.grid = element_blank(),
                   #panel.border=element_blank(),
                   axis.title = element_blank(),
                   axis.text.y = element_text(colour="black", size=6, face="plain"),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(linewidth = 0.5),
                   legend.position = "none",
                   plot.margin = unit(c(0,0,0,0), "cm"))
plot_map_bar_evi_slope = plot_map_bar_evi_slope + 
  annotation_custom(grob = ggplotGrob(plot_map_bar_evi_slope_legend), 
                    xmin=(min(sum_sum$lon)-1000000-250000), xmax=(min(sum_sum$lon)-1000000+250000), 
                    ymin=(min(sum_sum$lat)+1500-130000), ymax=(min(sum_sum$lat)+1500+130000)) 

summary(sum_sum$mean_isa_u1)
summary(sum_sum$mean_isa_u2)
summary(sum_sum$mean_isa_ur1)
summary(sum_sum$mean_isa_ur2)
summary(sum_sum$mean_isa_r)
plot_map_bar_isa=plot_map
for (city in 1:37) {
    sel=sum_sum[city,]
    bar_data=data.frame(position=1:5,
                        value=rbind(sel$mean_isa_u1,sel$mean_isa_u2,
                                    sel$mean_isa_ur1,sel$mean_isa_ur2,
                                    sel$mean_isa_r))
    plot_bar <- ggplot(data=bar_data, aes(x=position,y=value,fill=as.factor(position))) +
      geom_bar(stat="identity",position = position_dodge(0.1),width = 0.9)+
      scale_fill_manual(values=rf,label=c("UedG","UedD","UingG","UingD","Rural"))+
      ylab("")+ xlab("")+
      coord_cartesian(ylim = c(0,1))+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.background = element_rect(fill='transparent'),
                       plot.background = element_rect(fill='transparent', color=NA),
                       panel.grid = element_blank(),
                       panel.border=element_blank(),
                       axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = "none",
                       plot.margin = unit(c(0,0,0,0), "cm"))
    plot_map_bar_isa = plot_map_bar_isa + 
    {if (city<37) annotation_custom(grob = ggplotGrob(plot_bar), 
                        xmin=(sel$lon-160000), xmax=(sel$lon+160000), 
                        ymin=(sel$lat-130000), ymax=(sel$lat+130000))}+
    {if (city==37) annotation_custom(grob = ggplotGrob(plot_bar), 
                        xmin=(sel$lon-300000), xmax=(sel$lon+300000), 
                        ymin=(sel$lat-400000), ymax=(sel$lat+200000))}+
    {if (city==37) annotate("text", x =sum_sum[37,]$lon, y = (sum_sum[37,]$lat+260000),
                            label = "Average",size=3,family="serif")} 
}
legend_isa=data.frame(range=c(0,1))
plot_map_bar_isa_legend=ggplot(data=legend_isa, aes(x=1,y=range)) +
  geom_bar(stat="identity",width = 0.2,fill="black")+
  ylab("")+ xlab("")+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(breaks=c(0,0.5,1))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.background = element_rect(fill='transparent'),
                   plot.background = element_rect(fill='transparent', color=NA),
                   panel.grid = element_blank(),
                   #panel.border=element_blank(),
                   axis.title = element_blank(),
                   axis.text.y = element_text(colour="black", size=6, face="plain"),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(linewidth = 0.5),
                   legend.position = "none",
                   plot.margin = unit(c(0,0,0,0), "cm"))
plot_map_bar_isa = plot_map_bar_isa + 
  annotation_custom(grob = ggplotGrob(plot_map_bar_isa_legend), 
                    xmin=(min(sum_sum$lon)-1000000-150000), xmax=(min(sum_sum$lon)-1000000+150000), 
                    ymin=(min(sum_sum$lat)+1500-130000), ymax=(min(sum_sum$lat)+1500+130000)) 

summary(sum_sum$slope_isa_u1)
summary(sum_sum$slope_isa_u2)
summary(sum_sum$slope_isa_ur1)
summary(sum_sum$slope_isa_ur2)
summary(sum_sum$slope_isa_r)
plot_map_bar_isa_slope=plot_map
for (city in 1:37) {
  sel=sum_sum[city,]
  bar_data=data.frame(position=1:5,
                      value=rbind(sel$slope_isa_u1,sel$slope_isa_u2,
                                  sel$slope_isa_ur1,sel$slope_isa_ur2,
                                  sel$slope_isa_r))
  plot_bar <- ggplot(data=bar_data, aes(x=position,y=value,fill=as.factor(position))) +
    geom_bar(stat="identity",position = position_dodge(0.1),width = 0.9)+
    scale_fill_manual(values=rf,label=c("UedG","UedD","UingG","UingD","Rural"))+
    ylab("")+ xlab("")+
    coord_cartesian(ylim = c(0,0.071))+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.background = element_rect(fill='transparent'),
                     plot.background = element_rect(fill='transparent', color=NA),
                     panel.grid = element_blank(),
                     panel.border=element_blank(),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = "none",
                     plot.margin = unit(c(0,0,0,0), "cm"))
  plot_map_bar_isa_slope = plot_map_bar_isa_slope + 
    annotation_custom(grob = ggplotGrob(plot_bar), 
                      xmin=(sel$lon-160000), xmax=(sel$lon+160000), 
                      ymin=(sel$lat-130000), ymax=(sel$lat+130000))+
    {if (city==37) annotate("text", x =sum_sum[37,]$lon, y = (sum_sum[37,]$lat+260000),
                            label = "Average",size=3,family="serif")} 
}
legend_isa_slope=data.frame(range=c(0,0.071))
plot_map_bar_isa_slope_legend=ggplot(data=legend_isa_slope, aes(x=1,y=range)) +
  geom_bar(stat="identity",width = 0.2,fill="black")+
  ylab("")+ xlab("")+
  coord_cartesian(ylim = c(0,0.071))+
  scale_y_continuous(breaks=c(0,0.07))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.background = element_rect(fill='transparent'),
                   plot.background = element_rect(fill='transparent', color=NA),
                   panel.grid = element_blank(),
                   #panel.border=element_blank(),
                   axis.title = element_blank(),
                   axis.text.y = element_text(colour="black", size=6, face="plain"),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(linewidth = 0.5),
                   legend.position = "none",
                   plot.margin = unit(c(0,0,0,0), "cm"))
plot_map_bar_isa_slope = plot_map_bar_isa_slope + 
  annotation_custom(grob = ggplotGrob(plot_map_bar_isa_slope_legend), 
                    xmin=(min(sum_sum$lon)-1000000-180000), xmax=(min(sum_sum$lon)-1000000+180000), 
                    ymin=(min(sum_sum$lat)+1500-130000), ymax=(min(sum_sum$lat)+1500+130000)) 
}

aaa=data.frame(rbind(c(1,1),c(2,2),c(3,3),c(4,4),c(5,5)))
plot_legend=ggplot(data=aaa,aes(x=X1, y=X2, fill = as.factor(X2)))+ geom_col() + 
  scale_fill_manual(values=rf,
                    label=c("UedG","UedB","UingG","UingB","Rural"))+ labs(fill="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   legend.position = c(0.3,3.36),
                   legend.direction = "horizontal",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"))
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 200), ylim = c(0, 160), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_map_bar_isa), xmin = 0, xmax = 100, ymin = 80, ymax = 160) +
  annotation_custom(ggplotGrob(plot_map_bar_isa_slope), xmin = 100, xmax = 200,  ymin = 80, ymax = 160) +
  annotation_custom(ggplotGrob(plot_map_bar_evi), xmin = 0, xmax = 100,  ymin = 0, ymax = 80) +  
  annotation_custom(ggplotGrob(plot_map_bar_evi_slope), xmin = 100, xmax = 200, ymin = 0, ymax = 80) + 
  annotation_custom(ggplotGrob(plot_legend), xmin = 0, xmax = 100, ymin = -80, ymax = 0) + 
  annotate("text", x =10, y =92, label = "ISA",size=3,family="serif")+
  annotate("text", x =115, y =92, label = expression(paste(Trend[ISA]~'(/decade)')),size=3,family="serif")+
  annotate("text", x =10, y =12, label = "EVI",size=3,family="serif")+
  annotate("text", x =115, y =12, label = expression(paste(Trend[EVI]~'(/decade)')),size=3,family="serif")+
  annotate("text", x =96, y =158, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =196, y =158, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =96, y =78, label = "(c)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =196, y =78, label = "(d)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s2.tif"),width=20,height=16, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()