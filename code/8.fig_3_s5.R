###plot fig. 3, S5
##notes: UedG="u1",UedB="u2",UingG="ur1",UingB="ur2",Rural="r"

library(reshape2)
library(ggplot2)
library(ggrepel)
library(foreach)
library(smoothr)
library(sf)
rm(list=ls()) 


##general variables
{
rf=c("orangered4","orangered","royalblue4","royalblue","forestgreen")
time='day'
satellite="MYD"
start=1:3
end=14:16
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
                               (input_temporal$lst_u1-input_temporal$lst_r),(input_temporal$lst_u2-input_temporal$lst_r),
                               (input_temporal$lst_ur1-input_temporal$lst_r),(input_temporal$lst_ur2-input_temporal$lst_r),
                               (input_temporal$lst_u1-input_temporal$lst_u2),(input_temporal$lst_ur1-input_temporal$lst_ur2),
                               ((input_temporal$lst_u1+input_temporal$lst_u2)/2-(input_temporal$lst_ur1+input_temporal$lst_ur2)/2))
names(input_temporal_diff)=c("ioa","year",
                             "uhi_u1","uhi_u2",
                             "uhi_ur1","uhi_ur2",
                             "uhi_u1_u2","uhi_ur1_ur2",
                             "uhi_u_ur")
input_slope=read.csv(paste0("../output/7.1.year_variation/slope_trend_",time,".csv"))
}

######spatial
{
sum_lst0=foreach (city=1:36,.combine=rbind) %do% {
  input1=input_temporal[input_temporal$ioa==city,]
  rbind(cbind(city,2003,1,mean(input1$lst_u1[start]),mean(input1$isa_u1[start])),
             cbind(city,2003,2,mean(input1$lst_u2[start]),mean(input1$isa_u2[start])),
             cbind(city,2003,3,mean(input1$lst_ur1[start]),mean(input1$isa_ur1[start])),
             cbind(city,2003,4,mean(input1$lst_ur2[start]),mean(input1$isa_ur2[start])),
             cbind(city,2003,5,mean(input1$lst_r[start]),mean(input1$isa_r[start])),
             cbind(city,2018,1,mean(input1$lst_u1[end]),mean(input1$isa_u1[end])),
             cbind(city,2018,2,mean(input1$lst_u2[end]),mean(input1$isa_u2[end])),
             cbind(city,2018,3,mean(input1$lst_ur1[end]),mean(input1$isa_ur1[end])),
             cbind(city,2018,4,mean(input1$lst_ur2[end]),mean(input1$isa_ur2[end])),
             cbind(city,2018,5,mean(input1$lst_r[end]),mean(input1$isa_r[end])))
}
sum_lst=as.data.frame(sum_lst0)
names(sum_lst)=c("city","year","position","lst","isa")
sum_lst_mean=aggregate(sum_lst,by=list(sum_lst$position,sum_lst$year),FUN=mean,na.rm=TRUE)[,4:7]
sum_lst_sd=aggregate(sum_lst,by=list(sum_lst$position,sum_lst$year),FUN=sd,na.rm=TRUE)[,6:7]
sum_lst_atc=cbind(sum_lst_mean,sum_lst_sd)
names(sum_lst_atc)=c("year","position","mean_lst","mean_isa","sd_lst","sd_isa")

plot_spatial_lst=ggplot()+
  geom_point(data= sum_lst_atc,aes(x=position,y=mean_lst,color=as.factor(year)))+
  geom_line(data= sum_lst_atc,aes(x=position,y=mean_lst,color=as.factor(year)))+
  geom_errorbar(data= sum_lst_atc,aes(x=position, ymin= (mean_lst - sd_lst/10),ymax=(mean_lst + sd_lst/10),
                                      color=as.factor(year)), width=0.2,size=0.3)+
  xlim(c("UedG","UedB","UingG","UingB","Rural"))+
  scale_color_manual(values=rf[c(3,1)],label=c("Initial","Final"))+
  labs(color="")+
  ylab(expression(LST~'('*degree*C*')'))+xlab("")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = c(0.86,0.96),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

sum_uhi0=foreach (city=1:36,.combine=rbind) %do% {
  input1=input_temporal[input_temporal$ioa==city,]
  rbind(cbind(city,2003,1,(mean(input1$lst_u1[start])-mean(input1$lst_r[start])),(mean(input1$isa_u1[start])-mean(input1$isa_r[start]))),
             cbind(city,2003,2,(mean(input1$lst_u2[start])-mean(input1$lst_r[start])),(mean(input1$isa_u2[start])-mean(input1$isa_r[start]))),
             cbind(city,2003,3,(mean(input1$lst_ur1[start])-mean(input1$lst_r[start])),(mean(input1$isa_ur1[start])-mean(input1$isa_r[start]))),
             cbind(city,2003,4,(mean(input1$lst_ur2[start])-mean(input1$lst_r[start])),(mean(input1$isa_ur2[start])-mean(input1$isa_r[start]))),
             cbind(city,2018,1,(mean(input1$lst_u1[end])-mean(input1$lst_r[end])),(mean(input1$isa_u1[end])-mean(input1$isa_r[end]))),  
             cbind(city,2018,2,(mean(input1$lst_u2[end])-mean(input1$lst_r[end])),(mean(input1$isa_u2[end])-mean(input1$isa_r[end]))),
             cbind(city,2018,3,(mean(input1$lst_ur1[end])-mean(input1$lst_r[end])),(mean(input1$isa_ur1[end])-mean(input1$isa_r[end]))),
             cbind(city,2018,4,(mean(input1$lst_ur2[end])-mean(input1$lst_r[end])),(mean(input1$isa_ur2[end])-mean(input1$isa_r[end]))))
}

sum_uhi0=foreach (city=1:36,.combine=rbind) %do% {
  input1=input_temporal[input_temporal$ioa==city,]
  rbind(cbind(city,2003,1,(mean(input1$lst_u1[start])-mean(input1$lst_r[start])),(mean(input1$isa_u1[start])-mean(input1$isa_r[start]))),
        cbind(city,2003,2,(mean(input1$lst_u2[start])-mean(input1$lst_r[start])),(mean(input1$isa_u2[start])-mean(input1$isa_r[start]))),
        cbind(city,2003,3,(mean(input1$lst_ur1[start])-mean(input1$lst_r[start])),(mean(input1$isa_ur1[start])-mean(input1$isa_r[start]))),
        cbind(city,2003,4,(mean(input1$lst_ur2[start])-mean(input1$lst_r[start])),(mean(input1$isa_ur2[start])-mean(input1$isa_r[start]))),
        cbind(city,2003,5,0,0),
        cbind(city,2018,1,(mean(input1$lst_u1[end])-mean(input1$lst_r[end])),(mean(input1$isa_u1[end])-mean(input1$isa_r[end]))),  
        cbind(city,2018,2,(mean(input1$lst_u2[end])-mean(input1$lst_r[end])),(mean(input1$isa_u2[end])-mean(input1$isa_r[end]))),
        cbind(city,2018,3,(mean(input1$lst_ur1[end])-mean(input1$lst_r[end])),(mean(input1$isa_ur1[end])-mean(input1$isa_r[end]))),
        cbind(city,2018,4,(mean(input1$lst_ur2[end])-mean(input1$lst_r[end])),(mean(input1$isa_ur2[end])-mean(input1$isa_r[end]))),
        cbind(city,2018,5,0,0))
}
sum_uhi=as.data.frame(sum_uhi0)
names(sum_uhi)=c("city","year","position","lst","isa")
sum_uhi_mean=aggregate(sum_uhi,by=list(sum_uhi$position,sum_uhi$year),FUN=mean,na.rm=TRUE)[,4:7]
sum_uhi_sd=aggregate(sum_uhi,by=list(sum_uhi$position,sum_uhi$year),FUN=sd,na.rm=TRUE)[,6:7]
sum_uhi_atc=cbind(sum_uhi_mean,sum_uhi_sd)
names(sum_uhi_atc)=c("year","position","mean_lst","mean_isa","sd_lst","sd_isa")

plot_spatial_uhi=ggplot()+
  geom_point(data= sum_uhi_atc,aes(x=position,y=mean_lst,color=as.factor(year)))+
  geom_line(data= sum_uhi_atc,aes(x=position,y=mean_lst,color=as.factor(year)))+
  geom_errorbar(data= sum_uhi_atc,aes(x=position, ymin= (mean_lst - sd_lst/10),ymax=(mean_lst + sd_lst/10),
                                      color=as.factor(year)), width=0.15,size=0.3)+
  scale_color_manual(values=rf[c(3,1)],label=c("Initial","Final"))+
  scale_x_continuous(limits = c(1,5.3))+
  xlim(c("UedG","UedB","UingG","UingB","Rural"))+
  labs(color="")+
  ylab(expression(UHI~'('*degree*C*')'))+xlab("")+
  scale_fill_manual(values=rf[c(3,1)],label=c("Initial","Final"))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = c(0.86,0.98),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_blank())
}

#####plot temporal lst
{
input1=input_temporal[,-2]
input1_mean=aggregate(input1,by=list(input1$year),FUN=mean,na.rm=TRUE)
input1_sd=aggregate(input1,by=list(input1$year),FUN=sd,na.rm=TRUE)
colnames(input1_mean) = paste0("mean_",colnames(input1_mean))
colnames(input1_sd) = paste0("sd_",colnames(input1_sd))
names(input1_mean)[1]="year"
names(input1_sd)[1]="year"

sum_merge_lst= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_lst_u1,input1_sd$sd_lst_u1),
                                cbind(12,input1_mean$year,input1_mean$mean_lst_u2,input1_sd$sd_lst_u2),
                                cbind(21,input1_mean$year,input1_mean$mean_lst_ur1,input1_sd$sd_lst_ur1),
                                cbind(22,input1_mean$year,input1_mean$mean_lst_ur2,input1_sd$sd_lst_ur2),
                                cbind(33,input1_mean$year,input1_mean$mean_lst_r,input1_sd$sd_lst_r)))
names(sum_merge_lst)=c("position","year","mean","sd")
plot_temporal_lst=ggplot()+
  geom_point(data= sum_merge_lst,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_lst,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_lst,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                        color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf,label=c("UedG","UedD","UingG","UingD","Rural"))+
  scale_y_continuous(limits = c(33.8,40.5),breaks = seq(34,40,by=2))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+ guides(color=guide_legend(nrow=2))+
  ylab(expression(LST~'('*degree*C*')'))+xlab("")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = c(0.4,0.98),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0, "cm"),
                   legend.key.width = unit(0.3, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"))

input1=input_temporal_diff
input1_mean=aggregate(input1,by=list(input1$year),FUN=mean,na.rm=TRUE)
input1_sd=aggregate(input1,by=list(input1$year),FUN=sd,na.rm=TRUE)
colnames(input1_mean) = paste0("mean_",colnames(input1_mean))
colnames(input1_sd) = paste0("sd_",colnames(input1_sd))
names(input1_mean)[1]="year"
names(input1_sd)[1]="year"
sum_merge_uhi= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_uhi_u1,input1_sd$sd_uhi_u1),
                                cbind(12,input1_mean$year,input1_mean$mean_uhi_u2,input1_sd$sd_uhi_u2),
                                cbind(21,input1_mean$year,input1_mean$mean_uhi_ur1,input1_sd$sd_uhi_ur1),
                                cbind(22,input1_mean$year,input1_mean$mean_uhi_ur2,input1_sd$sd_uhi_ur2)))
names(sum_merge_uhi)=c("position","year","mean","sd")
cor.test(subset(sum_merge_uhi,position==11)$mean,subset(sum_merge_uhi,position==11)$year)$p.value
cor.test(subset(sum_merge_uhi,position==12)$mean,subset(sum_merge_uhi,position==12)$year)$p.value
cor.test(subset(sum_merge_uhi,position==21)$mean,subset(sum_merge_uhi,position==21)$year)$p.value
cor.test(subset(sum_merge_uhi,position==22)$mean,subset(sum_merge_uhi,position==22)$year)$p.value
plot_temporal_uhi=ggplot()+
  geom_point(data= sum_merge_uhi,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_uhi,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_uhi,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                        color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[1:4],label=c("UedG","UedB","UingG","UingB"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(UHI~'('*degree*C*')'))+xlab("")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = c(0.86,0.18),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

sum_merge_delta_uhi= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_uhi_u_ur,input1_sd$sd_uhi_u_ur),
                                      cbind(22,input1_mean$year,input1_mean$mean_uhi_u1_u2,input1_sd$sd_uhi_u1_u2),
                                      cbind(33,input1_mean$year,input1_mean$mean_uhi_ur1_ur2,input1_sd$sd_uhi_ur1_ur2)))
names(sum_merge_delta_uhi)=c("position","year","mean","sd")

plot_temporal_uhi_delta=ggplot()+
  geom_point(data= sum_merge_delta_uhi,aes(x=year,y=mean,color=as.factor(position)),size=0.3)+
  geom_errorbar(data= sum_merge_delta_uhi,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                              color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[c(1,3,5)],label=c(paste("Ued-Uing"),paste("Ued:G-B"),paste("Uing:G-B")))+
  scale_x_continuous(breaks =c(2003,2011,2018))+
  labs(color="")+
  ylab(expression(Delta~UHI~'('*degree*C*')'))+xlab("")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.2)+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.line=element_line(color="black",size=0.1),
                   panel.background=element_blank(),
                   panel.border=element_blank(),
                   axis.text = element_text(colour="black", size=8, face="plain"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_text(colour="black", size=8, face="plain",hjust=0.5,vjust=-1),
                   legend.position = c(0.8,0.91),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=7, face="plain"),
                   legend.title= element_text(colour="black", size=7, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
plot_temporal_uhi_delta=ggplot()+
  geom_point(data= sum_merge_delta_uhi,aes(x=year,y=mean,color=as.factor(position)))+
  geom_errorbar(data= sum_merge_delta_uhi,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                              color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[c(1,3,5)],label=c(paste("Ued-Uing"),paste("Ued:G-B"),paste("Uing:G-B")))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(Delta~UHI~'('*degree*C*')'))+xlab("")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.2)+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = c(0.82,0.94),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
}

####################################################
#######sum and export###############################
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 183), ylim = c(0, 60), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_spatial_uhi), xmin = 0, xmax = 60, ymin = 0, ymax = 60) +
  annotation_custom(ggplotGrob(plot_temporal_uhi), xmin = 61, xmax = 121, ymin = 0, ymax = 60) +
  annotation_custom(ggplotGrob(plot_temporal_uhi_delta), xmin = 122, xmax = 182, ymin = 0, ymax = 60) +
  annotate("text", x =35, y =58, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =96, y =58, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =160, y =58, label = "(c)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"),
  plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_3.tif"),width=18.3,height=6, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()

plot_sum=ggplot() +
  coord_equal(xlim = c(0, 124), ylim = c(0, 60), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_temporal_lst), xmin = 0, xmax = 60, ymin = 0, ymax = 60) +
  annotation_custom(ggplotGrob(plot_spatial_lst), xmin = 64, xmax = 124, ymin = 0, ymax = 60) +
  annotate("text", x =57, y =58, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =76.5, y =58, label = "(b)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"),
  plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s5.tif"),width=12.4,height=6, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()