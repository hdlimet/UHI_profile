###plot fig. 1, S1, S2
##notes: UedG="u1",UedB="u2",UingG="ur1",UingB="ur2",Rural="r"

library(reshape2)
library(ggplot2)
library(foreach)
library(smoothr)
rm(list=ls()) 

##general variables
{
rf=c("orangered4","orangered","royalblue4","royalblue","forestgreen")
time='day'
satellite="MYD"
input_temporal=read.csv(paste0("../output/7.1.year_variation/",satellite,"_temporal_variation_",time,".csv"))
input_temporal_diff=data.frame(input_temporal[,c(1,3)],
                               (input_temporal$cm_u1-input_temporal$cm_r),(input_temporal$cm_u2-input_temporal$cm_r),
                               (input_temporal$cm_ur1-input_temporal$cm_r),(input_temporal$cm_ur2-input_temporal$cm_r),
                               (input_temporal$ca_u1-input_temporal$ca_r),(input_temporal$ca_u2-input_temporal$ca_r),
                               (input_temporal$ca_ur1-input_temporal$ca_r),(input_temporal$ca_ur2-input_temporal$ca_r),
                               (input_temporal$cp_u1-input_temporal$cp_r),(input_temporal$cp_u2-input_temporal$cp_r),
                               (input_temporal$cp_ur1-input_temporal$cp_r),(input_temporal$cp_ur2-input_temporal$cp_r))
names(input_temporal_diff)=c("city","year",
                             "cm_u1_r","cm_u2_r",
                             "cm_ur1_r","cm_ur2_r",
                             "ca_u1_r","ca_u2_r",
                             "ca_ur1_r","ca_ur2_r",
                             "cp_u1_r","cp_u2_r",
                             "cp_ur1_r","cp_ur2_r")
}

###calculate ATC annual cycle
{
start=1:3  #staring year 
end=14:16  #ending year
input=input_temporal
atc0=foreach (city=1:36,.combine=rbind) %do% {
  input1=input[input$ioa==city,]
  lst_u1=matrix(nrow=365,ncol=16)
  lst_u2=matrix(nrow=365,ncol=16)
  lst_ur1=matrix(nrow=365,ncol=16)
  lst_ur2=matrix(nrow=365,ncol=16)
  lst_r=matrix(nrow=365,ncol=16)
  for (year in 1:16) {
    lst_u1[,year]=input1$cm_u1[year]+input1$ca_u1[year]*sin(2*pi*c(1:365)/365-2*pi/365*input1$cp_u1[year]+0.5*pi)
    lst_u2[,year]=input1$cm_u2[year]+input1$ca_u2[year]*sin(2*pi*c(1:365)/365-2*pi/365*input1$cp_u2[year]+0.5*pi)
    lst_ur1[,year]=input1$cm_ur1[year]+input1$ca_ur1[year]*sin(2*pi*c(1:365)/365-2*pi/365*input1$cp_ur1[year]+0.5*pi)
    lst_ur2[,year]=input1$cm_ur2[year]+input1$ca_ur2[year]*sin(2*pi*c(1:365)/365-2*pi/365*input1$cp_ur2[year]+0.5*pi)
    lst_r[,year]=input1$cm_r[year]+input1$ca_r[year]*sin(2*pi*c(1:365)/365-2*pi/365*input1$cp_r[year]+0.5*pi)
  }
  lst_2003_u1=apply(lst_u1[,start],1,mean)
  lst_2003_u2=apply(lst_u2[,start],1,mean)
  lst_2003_ur1=apply(lst_ur1[,start],1,mean)
  lst_2003_ur2=apply(lst_ur2[,start],1,mean)
  lst_2003_r=apply(lst_r[,start],1,mean)
  lst_2018_u1=apply(lst_u1[,end],1,mean)
  lst_2018_u2=apply(lst_u2[,end],1,mean)
  lst_2018_ur1=apply(lst_ur1[,end],1,mean)
  lst_2018_ur2=apply(lst_ur2[,end],1,mean)
  lst_2018_r=apply(lst_r[,end],1,mean)
  
  lst_all_u1=apply(lst_u1,1,mean)
  lst_all_u2=apply(lst_u2,1,mean)
  lst_all_ur1=apply(lst_ur1,1,mean)
  lst_all_ur2=apply(lst_ur2,1,mean)
  lst_all_r=apply(lst_r,1,mean)
  
  rbind(cbind(city,2003,1,1:365,lst_2003_u1),
       cbind(city,2003,2,1:365,lst_2003_u2),
       cbind(city,2003,3,1:365,lst_2003_ur1),
       cbind(city,2003,4,1:365,lst_2003_ur2),
       cbind(city,2003,5,1:365,lst_2003_r),
       cbind(city,2018,1,1:365,lst_2018_u1),
       cbind(city,2018,2,1:365,lst_2018_u2),
       cbind(city,2018,3,1:365,lst_2018_ur1),
       cbind(city,2018,4,1:365,lst_2018_ur2),
       cbind(city,2018,5,1:365,lst_2018_r),
       cbind(city,8888,1,1:365,(lst_2018_u1-lst_2003_u1)),
       cbind(city,8888,2,1:365,(lst_2018_u2-lst_2003_u2)),
       cbind(city,8888,3,1:365,(lst_2018_ur1-lst_2003_ur1)),
       cbind(city,8888,4,1:365,(lst_2018_ur2-lst_2003_ur2)),
       cbind(city,8888,5,1:365,(lst_2018_r-lst_2003_r)),
       cbind(city,9999,1,1:365,lst_all_u1),
       cbind(city,9999,2,1:365,lst_all_u2),
       cbind(city,9999,3,1:365,lst_all_ur1),
       cbind(city,9999,4,1:365,lst_all_ur2),
       cbind(city,9999,5,1:365,lst_all_r))
}
atc=as.data.frame(atc0)
names(atc)=c("city","year","position","doy","lst")
atc_agg=aggregate(atc,by=list(atc$doy,atc$position,atc$year),FUN=mean,na.rm=TRUE)[,5:8]
names(atc_agg)=c("year","position","doy","lst")
}

#######plot######################
####2003 to 2018#################
{
atc_agg_years=atc_agg[atc_agg$year<8888,]
plot_sum_abs_years=ggplot()+
  geom_line(data= atc_agg_years,aes(x=doy,y=lst,color=as.factor(position),linetype=as.factor(year)),size=0.5)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+
  scale_linetype_manual(values=c("twodash","solid"),label=c("2003","2018"))+
  labs(color="",linetype="")+
  ylab(expression(mean~LST~'('*degree*C*')'))+xlab("DOY")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   legend.position = c(0.9,0.9),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
}
###year average####
{atc_agg_all=atc_agg[atc_agg$year==9999,]
plot_sum_abs_all=ggplot()+
  geom_line(data= atc_agg_all,aes(x=doy,y=lst,color=as.factor(position)),size=0.8)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+
  labs(color="",linetype="")+
  ylab(expression(LST~'('*degree*C*')'))+xlab("DOY")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title = element_text(colour="black", size=10, face="plain"),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   legend.position = c(0.85,0.93),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"))

atc_agg_all_uhi=atc_agg_all[1:1460,]
atc_agg_all_uhi$lst=atc_agg_all_uhi$lst-rep(atc_agg_all[1461:1825,]$lst,4)
plot_sum_abs_all_uhi=ggplot()+
  geom_line(data= atc_agg_all_uhi,aes(x=doy,y=lst,color=as.factor(position)),size=0.8)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB"))+
  labs(color="",linetype="")+
  ylab(expression(LST~'('*degree*C*')'))+xlab("DOY")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   legend.position = c(0.85,0.9),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
}
####2003-2018 diff_lst###########
{atc_agg_diff=atc_agg[atc_agg$year==8888,]
aggregate(atc_agg_diff,by=list(atc_agg_diff$position),FUN="max")

#u1 and u2
atc_agg_diff_fill1 = subset(atc_agg_diff,position %in% c(1,2))
atc_agg_diff_fill1_use = reshape(atc_agg_diff_fill1, idvar = "doy", timevar = "position", direction = "wide")
sum_u12=sum(atc_agg_diff[atc_agg_diff$position==2,4]-atc_agg_diff[atc_agg_diff$position==1,4])
#ur1 and ur2
atc_agg_diff_fill2 = subset (atc_agg_diff,position %in% c(3,4))
atc_agg_diff_fill2_use = reshape(atc_agg_diff_fill2, idvar = "doy", timevar = "position", direction = "wide")
sum_ur12=sum(atc_agg_diff[atc_agg_diff$position==4,4]-atc_agg_diff[atc_agg_diff$position==3,4])
plot_sum_diff_lst = ggplot()+
  geom_line(data= atc_agg_diff,aes(x=doy,y=lst,color=as.factor(position)),size=0.8)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+
  labs(color="")+
  ylab(expression(Delta~LST[Final-Initial]~'('*degree*C*')'))+xlab("DOY")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.2)+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   legend.position = c(0.9,0.9),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"))
}
####2003-2018 uhi###########
{
  atc_agg_uhi=as.data.frame(rbind(cbind(1,2003,1:365,(atc_agg[(atc_agg$position==1 & atc_agg$year==2003),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2003),4])),
                                   cbind(2,2003,1:365,(atc_agg[(atc_agg$position==2 & atc_agg$year==2003),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2003),4])),
                                   cbind(3,2003,1:365,(atc_agg[(atc_agg$position==3 & atc_agg$year==2003),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2003),4])),
                                   cbind(4,2003,1:365,(atc_agg[(atc_agg$position==4 & atc_agg$year==2003),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2003),4])),
                                   cbind(1,2018,1:365,(atc_agg[(atc_agg$position==1 & atc_agg$year==2018),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2018),4])),
                                   cbind(2,2018,1:365,(atc_agg[(atc_agg$position==2 & atc_agg$year==2018),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2018),4])),
                                   cbind(3,2018,1:365,(atc_agg[(atc_agg$position==3 & atc_agg$year==2018),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2018),4])),
                                   cbind(4,2018,1:365,(atc_agg[(atc_agg$position==4 & atc_agg$year==2018),4]-
                                                       atc_agg[(atc_agg$position==5 & atc_agg$year==2018),4]))))
  names(atc_agg_uhi)=c("position","year","doy","uhi")
  plot_sum_abs_uhi_years=ggplot()+
    geom_line(data= atc_agg_uhi,aes(x=doy,y=uhi,color=as.factor(position),linetype=as.factor(year)),size=0.5)+
    scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB"))+
    scale_linetype_manual(values=c("solid","twodash"),label=c("2003","2018"))+
    labs(color="",linetype="") + guides(linetype = FALSE)+
    ylab(expression(UHI~'('*degree*C*')'))+xlab("DOY")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title = element_text(colour="black", size=10, face="plain"),
                     axis.text=element_text(colour="black", size=9, face="plain"),
                     legend.position = c(0.88,0.85),
                     legend.direction = "vertical",
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.25, "cm"),
                     plot.margin = margin(0, 0, 0, 0, "cm"))
  
  atc_agg_uhi_diff=data.frame(rbind(cbind(1,2003,1:365,(atc_agg[(atc_agg$position==1 & atc_agg$year==2003),4]-
                                                        atc_agg[(atc_agg$position==2 & atc_agg$year==2003),4])),
                                    cbind(2,2003,1:365,(atc_agg[(atc_agg$position==3 & atc_agg$year==2003),4]-
                                                        atc_agg[(atc_agg$position==4 & atc_agg$year==2003),4])),
                                    cbind(1,2018,1:365,(atc_agg[(atc_agg$position==1 & atc_agg$year==2018),4]-
                                                        atc_agg[(atc_agg$position==2 & atc_agg$year==2018),4])),
                                    cbind(2,2018,1:365,(atc_agg[(atc_agg$position==3 & atc_agg$year==2018),4]-
                                                        atc_agg[(atc_agg$position==4 & atc_agg$year==2018),4]))))
  names(atc_agg_uhi_diff)=c("position","year","doy","uhi")
  plot_sum_abs_uhi_diff_years=ggplot()+
    geom_line(data= atc_agg_uhi_diff,aes(x=doy,y=uhi,color=as.factor(position),linetype=as.factor(year)),size=0.5)+
    scale_color_manual(values=rf[c(1,3)],label=c("Ued:G-B","Uing:G-B"))+
    scale_linetype_manual(values=c("solid","twodash"),label=c("2003","2018"))+
    labs(color="",linetype="") + guides(linetype = FALSE)+
    ylab(expression(Delta~UHI~'('*degree*C*')'))+xlab("DOY")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title = element_text(colour="black", size=10, face="plain"),
                     axis.text=element_text(colour="black", size=9, face="plain"),
                     legend.position = c(0.84,0.9),
                     legend.direction = "vertical",
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.25, "cm"),
                     plot.margin = margin(0, 0, 0, 0, "cm"))
}

####2003-2018 diff_uhi###########
{
atc_agg_diff1=as.data.frame(rbind(cbind(1,1:365,(atc_agg_diff[atc_agg_diff$position==1,4]-atc_agg_diff[atc_agg_diff$position==5,4])),
                                  cbind(2,1:365,(atc_agg_diff[atc_agg_diff$position==2,4]-atc_agg_diff[atc_agg_diff$position==5,4])),
                                  cbind(3,1:365,(atc_agg_diff[atc_agg_diff$position==3,4]-atc_agg_diff[atc_agg_diff$position==5,4])),
                                  cbind(4,1:365,(atc_agg_diff[atc_agg_diff$position==4,4]-atc_agg_diff[atc_agg_diff$position==5,4]))))
names(atc_agg_diff1)=c("position","doy","lst")
aggregate(atc_agg_diff1,by=list(atc_agg_diff1$position),FUN="max")
aggregate(atc_agg_diff1,by=list(atc_agg_diff1$position),FUN="min")
aggregate(atc_agg_diff1,by=list(atc_agg_diff1$position),
          FUN=function(x) {mean(x[c(1:59,335:365)])})
plot_sum_diff_uhi = ggplot()+
  geom_line(data= atc_agg_diff1,aes(x=doy,y=lst,color=as.factor(position)),size=0.8)+
  scale_color_manual(values=rf[1:4],label=c("UedG","UedB","UingG","UingB"))+
  labs(color="")+
  ylab(expression(Delta~UHI[Final-Initial]~'('*degree*C*')'))+xlab("DOY")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.2)+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title = element_text(colour="black", size=10, face="plain"),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   legend.position = c(0.85,0.94),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"))

atc_agg_diff2=as.data.frame(rbind(cbind(1,1:365,((atc_agg_diff[atc_agg_diff$position==3,4]+atc_agg_diff[atc_agg_diff$position==4,4])/2-
                                                 (atc_agg_diff[atc_agg_diff$position==1,4]+atc_agg_diff[atc_agg_diff$position==2,4])/2)),
                                  cbind(2,1:365,(atc_agg_diff[atc_agg_diff$position==2,4]-atc_agg_diff[atc_agg_diff$position==1,4])),
                                  cbind(3,1:365,(atc_agg_diff[atc_agg_diff$position==4,4]-atc_agg_diff[atc_agg_diff$position==3,4]))))
names(atc_agg_diff2)=c("position","doy","lst")
aggregate(atc_agg_diff2,by=list(atc_agg_diff2$position),FUN="max")
plot_sum_diff_uhi_internal = ggplot()+
  geom_line(data= atc_agg_diff2,aes(x=doy,y=lst,color=as.factor(position)),size=0.8)+
  scale_color_manual(values=rf[c(1,3,5)],label=c(paste("Uing-Ued"),paste("Ued:B-G"),paste("Uing:B-G")))+
  labs(color="")+
  ylab(expression(Delta~UHI[Final-Initial]~'('*degree*C*')'))+xlab("DOY")+
  geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.2)+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title = element_text(colour="black", size=10, face="plain"),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   legend.position = c(0.82,0.96),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"),
                   plot.margin = margin(0, 0, 0, 0, "cm"))
}

########plot Cm ca cp##########
{
input1=input_temporal[,-2]
input1_mean=aggregate(input1,by=list(input1$year),FUN=mean,na.rm=TRUE)
input1_sd=aggregate(input1,by=list(input1$year),FUN=sd,na.rm=TRUE)
colnames(input1_mean) = paste0("mean_",colnames(input1_mean))
colnames(input1_sd) = paste0("sd_",colnames(input1_sd))
names(input1_mean)[1]="year"
names(input1_sd)[1]="year"
sum_merge_cm= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_cm_u1,input1_sd$sd_cm_u1),
                               cbind(12,input1_mean$year,input1_mean$mean_cm_u2,input1_sd$sd_cm_u2),
                               cbind(21,input1_mean$year,input1_mean$mean_cm_ur1,input1_sd$sd_cm_ur1),
                               cbind(22,input1_mean$year,input1_mean$mean_cm_ur2,input1_sd$sd_cm_ur2),
                               cbind(33,input1_mean$year,input1_mean$mean_cm_r,input1_sd$sd_cm_r)))
names(sum_merge_cm)=c("position","year","mean","sd")
cor.test(subset(sum_merge_cm,position==11)$mean,subset(sum_merge_cm,position==11)$year)$p.value
cor.test(subset(sum_merge_cm,position==12)$mean,subset(sum_merge_cm,position==12)$year)$p.value
cor.test(subset(sum_merge_cm,position==21)$mean,subset(sum_merge_cm,position==21)$year)$p.value
cor.test(subset(sum_merge_cm,position==22)$mean,subset(sum_merge_cm,position==22)$year)$p.value
cor.test(subset(sum_merge_cm,position==33)$mean,subset(sum_merge_cm,position==33)$year)$p.value
plot_temporal_cm=ggplot()+
  geom_point(data= sum_merge_cm,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_cm,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_cm,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                       color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+
  labs(color="")+
  ylab(expression(Cm~'('*degree*C*')'))+xlab("year")+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  scale_y_continuous(limits=c(22.5,27.5),breaks =seq(23,27,by=1))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

sum_merge_ca= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_ca_u1,input1_sd$sd_ca_u1),
                               cbind(12,input1_mean$year,input1_mean$mean_ca_u2,input1_sd$sd_ca_u2),
                               cbind(21,input1_mean$year,input1_mean$mean_ca_ur1,input1_sd$sd_ca_ur1),
                               cbind(22,input1_mean$year,input1_mean$mean_ca_ur2,input1_sd$sd_ca_ur2),
                               cbind(33,input1_mean$year,input1_mean$mean_ca_r,input1_sd$sd_ca_r)))
names(sum_merge_ca)=c("position","year","mean","sd")
cor.test(subset(sum_merge_ca,position==11)$mean,subset(sum_merge_ca,position==11)$year)$p.value
cor.test(subset(sum_merge_ca,position==12)$mean,subset(sum_merge_ca,position==12)$year)$p.value
cor.test(subset(sum_merge_ca,position==21)$mean,subset(sum_merge_ca,position==21)$year)$p.value
cor.test(subset(sum_merge_ca,position==22)$mean,subset(sum_merge_ca,position==22)$year)$p.value
cor.test(subset(sum_merge_ca,position==33)$mean,subset(sum_merge_ca,position==33)$year)$p.value
plot_temporal_ca=ggplot()+
  geom_point(data= sum_merge_ca,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_ca,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_ca,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                       color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf,label=c("U1","U2","UR1","UR2","R"))+
  labs(color="")+
  ylab(expression(Ca~'('*degree*C*')'))+xlab("year")+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

sum_merge_cp= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_cp_u1,input1_sd$sd_cp_u1),
                               cbind(12,input1_mean$year,input1_mean$mean_cp_u2,input1_sd$sd_cp_u2),
                               cbind(21,input1_mean$year,input1_mean$mean_cp_ur1,input1_sd$sd_cp_ur1),
                               cbind(22,input1_mean$year,input1_mean$mean_cp_ur2,input1_sd$sd_cp_ur2),
                               cbind(33,input1_mean$year,input1_mean$mean_cp_r,input1_sd$sd_cp_r)))
names(sum_merge_cp)=c("position","year","mean","sd")
cor.test(subset(sum_merge_cp,position==11)$mean,subset(sum_merge_cp,position==11)$year)$p.value
cor.test(subset(sum_merge_cp,position==12)$mean,subset(sum_merge_cp,position==12)$year)$p.value
cor.test(subset(sum_merge_cp,position==21)$mean,subset(sum_merge_cp,position==21)$year)$p.value
cor.test(subset(sum_merge_cp,position==22)$mean,subset(sum_merge_cp,position==22)$year)$p.value
cor.test(subset(sum_merge_cp,position==33)$mean,subset(sum_merge_cp,position==33)$year)$p.value
plot_temporal_cp=ggplot()+
  geom_point(data= sum_merge_cp,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_cp,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_cp,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                       color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf,label=c("U1","U2","UR1","UR2","R"))+
  labs(color="")+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  ylab(expression(Cp~'(DOY)'))+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
}
########plot Cm ca cp diff###############
{
input1=input_temporal_diff
input1_mean=aggregate(input1,by=list(input1$year),FUN=mean,na.rm=TRUE)
input1_sd=aggregate(input1,by=list(input1$year),FUN=sd,na.rm=TRUE)
colnames(input1_mean) = paste0("mean_",colnames(input1_mean))
colnames(input1_sd) = paste0("sd_",colnames(input1_sd))
names(input1_mean)[1]="year"
names(input1_sd)[1]="year"
sum_merge_cm= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_cm_u1_r,input1_sd$sd_cm_u1_r),
                               cbind(12,input1_mean$year,input1_mean$mean_cm_u2_r,input1_sd$sd_cm_u2_r),
                               cbind(21,input1_mean$year,input1_mean$mean_cm_ur1_r,input1_sd$sd_cm_ur1_r),
                               cbind(22,input1_mean$year,input1_mean$mean_cm_ur2_r,input1_sd$sd_cm_ur2_r)))
names(sum_merge_cm)=c("position","year","mean","sd")
cor.test(subset(sum_merge_cm,position==11)$mean,subset(sum_merge_cm,position==11)$year)$p.value
cor.test(subset(sum_merge_cm,position==12)$mean,subset(sum_merge_cm,position==12)$year)$p.value
cor.test(subset(sum_merge_cm,position==21)$mean,subset(sum_merge_cm,position==21)$year)$p.value
cor.test(subset(sum_merge_cm,position==22)$mean,subset(sum_merge_cm,position==22)$year)$p.value
plot_temporal_cm_diff=ggplot()+
  geom_point(data= sum_merge_cm,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_cm,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_cm,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                       color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[1:4],label=c("U1","U2","UR1","UR2"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(paste(Delta,Cm~'('*degree*C*')')))+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))


sum_merge_ca= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_ca_u1_r,input1_sd$sd_ca_u1_r),
                               cbind(12,input1_mean$year,input1_mean$mean_ca_u2_r,input1_sd$sd_ca_u2_r),
                               cbind(21,input1_mean$year,input1_mean$mean_ca_ur1_r,input1_sd$sd_ca_ur1_r),
                               cbind(22,input1_mean$year,input1_mean$mean_ca_ur2_r,input1_sd$sd_ca_ur2_r)))
names(sum_merge_ca)=c("position","year","mean","sd")
cor.test(subset(sum_merge_ca,position==11)$mean,subset(sum_merge_ca,position==11)$year)$p.value
cor.test(subset(sum_merge_ca,position==12)$mean,subset(sum_merge_ca,position==12)$year)$p.value
cor.test(subset(sum_merge_ca,position==21)$mean,subset(sum_merge_ca,position==21)$year)$p.value
cor.test(subset(sum_merge_ca,position==22)$mean,subset(sum_merge_ca,position==22)$year)$p.value
plot_temporal_ca_diff=ggplot()+
  geom_point(data= sum_merge_ca,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_ca,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_ca,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                       color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[1:4],label=c("U1","U2","UR1","UR2"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(paste(Delta,Ca~'('*degree*C*')')))+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

sum_merge_cp= data.frame(rbind(cbind(11,input1_mean$year,input1_mean$mean_cp_u1_r,input1_sd$sd_cp_u1_r),
                               cbind(12,input1_mean$year,input1_mean$mean_cp_u2_r,input1_sd$sd_cp_u2_r),
                               cbind(21,input1_mean$year,input1_mean$mean_cp_ur1_r,input1_sd$sd_cp_ur1_r),
                               cbind(22,input1_mean$year,input1_mean$mean_cp_ur2_r,input1_sd$sd_cp_ur2_r)))
names(sum_merge_cp)=c("position","year","mean","sd")
cor.test(subset(sum_merge_cp,position==11)$mean,subset(sum_merge_cp,position==11)$year)$p.value
cor.test(subset(sum_merge_cp,position==12)$mean,subset(sum_merge_cp,position==12)$year)$p.value
cor.test(subset(sum_merge_cp,position==21)$mean,subset(sum_merge_cp,position==21)$year)$p.value
cor.test(subset(sum_merge_cp,position==22)$mean,subset(sum_merge_cp,position==22)$year)$p.value
plot_temporal_cp_diff=ggplot()+
  geom_point(data= sum_merge_cp,aes(x=year,y=mean,color=as.factor(position)))+
  geom_smooth(data= sum_merge_cp,aes(x=year,y=mean,color=as.factor(position)),method="lm",size=0.5,fill=NA)+
  geom_errorbar(data= sum_merge_cp,aes(x=year, ymin= (mean - sd/10),ymax=(mean + sd/10),
                                       color=as.factor(position)), width=0.2,size=0.3)+
  scale_color_manual(values=rf[1:4],label=c("U1","U2","UR1","UR2"))+
  scale_x_continuous(breaks =seq(2003,2018,by=3))+
  labs(color="")+
  ylab(expression(paste(Delta,Cp~"(Day)")))+xlab("year")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   legend.position = "none",
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

}

########plotbox Cm ca cp diff###############
{
input_cm=as.data.frame(rbind(cbind(1,input$ioa,input$year,input$cm_u1),
                             cbind(2,input$ioa,input$year,input$cm_u2),
                             cbind(3,input$ioa,input$year,input$cm_ur1),
                             cbind(4,input$ioa,input$year,input$cm_ur2),
                             cbind(5,input$ioa,input$year,input$cm_r)))
names(input_cm)=c("position","ioa","year","value")
plot_box_cm= ggplot(input_cm,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1)+
  stat_summary(fun="mean", geom="line", size=50, color="red")+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB","Rural"))+
  ylab(expression(Cm~'('*degree*C*')'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.title = element_blank())
input_cm_diff=as.data.frame(rbind(cbind(1,input$ioa,input$year,input$cm_u1-input$cm_r),
                                  cbind(2,input$ioa,input$year,input$cm_u2-input$cm_r),
                                  cbind(3,input$ioa,input$year,input$cm_ur1-input$cm_r),
                                  cbind(4,input$ioa,input$year,input$cm_ur2-input$cm_r)))
names(input_cm_diff)=c("position","ioa","year","value")
plot_box_cm_diff= ggplot(input_cm_diff,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1)+
  stat_summary(fun="mean", geom="line", size=50, color="red")+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB"))+
  ylab(expression(paste(Delta,Cm~'('*degree*C*')')))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.title = element_blank())

input_ca=as.data.frame(rbind(cbind(1,input$ioa,input$year,input$ca_u1),
                             cbind(2,input$ioa,input$year,input$ca_u2),
                             cbind(3,input$ioa,input$year,input$ca_ur1),
                             cbind(4,input$ioa,input$year,input$ca_ur2),
                             cbind(5,input$ioa,input$year,input$ca_r)))
names(input_ca)=c("position","ioa","year","value")
plot_box_ca= ggplot(input_ca,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1)+
  stat_summary(fun="mean", geom="line", size=50, color="red")+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB","Rural"))+
  ylab(expression(Ca~'('*degree*C*')'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.title = element_blank())
input_ca_diff=as.data.frame(rbind(cbind(1,input$ioa,input$year,input$ca_u1-input$ca_r),
                                  cbind(2,input$ioa,input$year,input$ca_u2-input$ca_r),
                                  cbind(3,input$ioa,input$year,input$ca_ur1-input$ca_r),
                                  cbind(4,input$ioa,input$year,input$ca_ur2-input$ca_r)))
names(input_ca_diff)=c("position","ioa","year","value")
plot_box_ca_diff= ggplot(input_ca_diff,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1)+
  stat_summary(fun="mean", geom="line", size=50, color="red")+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB"))+
  scale_y_continuous(limits = c(-0.5,4))+
  ylab(expression(paste(Delta,Ca~'('*degree*C*')')))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.title = element_blank())

input_cp=as.data.frame(rbind(cbind(1,input$ioa,input$year,input$cp_u1),
                             cbind(2,input$ioa,input$year,input$cp_u2),
                             cbind(3,input$ioa,input$year,input$cp_ur1),
                             cbind(4,input$ioa,input$year,input$cp_ur2),
                             cbind(5,input$ioa,input$year,input$cp_r)))
names(input_cp)=c("position","ioa","year","value")
plot_box_cp= ggplot(input_cp,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1)+
  stat_summary(fun="mean", geom="line", size=50, color="red")+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB","Rural"))+
  scale_y_continuous(limits = c(165,210))+
  ylab(expression(Cp~'(DOY)'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.title = element_blank())
input_cp_diff=as.data.frame(rbind(cbind(1,input$ioa,input$year,input$cp_u1-input$cp_r),
                                  cbind(2,input$ioa,input$year,input$cp_u2-input$cp_r),
                                  cbind(3,input$ioa,input$year,input$cp_ur1-input$cp_r),
                                  cbind(4,input$ioa,input$year,input$cp_ur2-input$cp_r)))
names(input_cp_diff)=c("position","ioa","year","value")
plot_box_cp_diff= ggplot(input_cp_diff,aes(x=as.factor(position),y=value,fill=as.factor(position))) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1)+
  stat_summary(fun="mean", geom="line", size=50, color="red")+
  scale_fill_manual(values=rf)+
  scale_x_discrete(label=c("UedG","UedB","UingG","UingB"))+
  scale_y_continuous(limits = c(-10,10))+
  ylab(expression(paste(Delta,Cp~"(Day)")))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text.y=element_text(colour="black", size=10, face="plain"),
                   axis.text.x=element_text(colour="black", size=8, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.title = element_blank())
}

########export LST UHI############################
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 180), ylim = c(0, 100), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_sum_abs_all), xmin = 0, xmax = 60, ymin = 0, ymax = 100) +  
  annotation_custom(ggplotGrob(plot_sum_diff_uhi), xmin = 60, xmax = 120, ymin = 0, ymax = 100) +
  annotation_custom(ggplotGrob(plot_sum_diff_uhi_internal), xmin = 120, xmax = 180, ymin = 0, ymax = 100) +
  annotate("text", x =13, y =98, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =75, y =98, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =134, y =98, label = "(c)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_2.tif"),width=18,height=10, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()

aaa=data.frame(rbind(c(1,1),c(2,2),c(3,3),c(4,4),c(5,5)))
plot_legend_line=ggplot(data=aaa,aes(x=X1, y=X2, color = as.factor(X2)))+ geom_line() + geom_point() +
  scale_color_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+ labs(color="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   legend.position = c(0.5,5.15),
                   legend.direction = "horizontal",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=9, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"))
plot_legend_area=ggplot(data=aaa,aes(x=X1, y=X2, fill = as.factor(X2)))+ geom_col() + 
  scale_fill_manual(values=rf,label=c("UedG","UedB","UingG","UingB","Rural"))+ labs(fill="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   legend.position = c(0.5,5.15),
                   legend.direction = "horizontal",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=9, face="plain"),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"))
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 215), ylim = c(0, 154), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_box_cm), xmin = 0, xmax = 55, ymin = 100, ymax = 150) +  
  annotation_custom(ggplotGrob(plot_box_ca), xmin = 0, xmax = 55, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_box_cp), xmin = -1.5, xmax = 55, ymin = 0, ymax = 50) + 
  annotation_custom(ggplotGrob(plot_box_cm_diff), xmin = 55, xmax = 105, ymin = 100, ymax = 150) +  
  annotation_custom(ggplotGrob(plot_box_ca_diff), xmin = 55, xmax = 105, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_box_cp_diff), xmin = 53, xmax = 105, ymin = 0, ymax = 50) +  
  annotation_custom(ggplotGrob(plot_temporal_cm), xmin = 105, xmax = 160, ymin = 100, ymax = 150) +  
  annotation_custom(ggplotGrob(plot_temporal_ca), xmin = 105, xmax = 160, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_temporal_cp), xmin = 103, xmax = 160, ymin = 0, ymax = 50) + 
  annotation_custom(ggplotGrob(plot_temporal_cm_diff), xmin = 160, xmax = 215, ymin = 100, ymax = 150) +  
  annotation_custom(ggplotGrob(plot_temporal_ca_diff), xmin = 160, xmax = 215, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_temporal_cp_diff), xmin = 163, xmax = 215, ymin = 0, ymax = 50) +  
  annotation_custom(ggplotGrob(plot_legend_area), xmin = 0, xmax = 105, ymin = -50, ymax = 0) + 
  annotation_custom(ggplotGrob(plot_legend_line), xmin = 105, xmax = 210, ymin = -50, ymax = 0) + 
  annotate("text", x =32, y =147, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =85, y =147, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =137, y =147, label = "(c)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =193, y =147, label = "(d)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s3.tif"),width=21.5,height=15.4, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()

plot_legend=ggplot()+
  geom_line(data= atc_agg_uhi_diff,aes(x=doy,y=uhi,linetype=as.factor(year)),size=0.5)+
  scale_linetype_manual(values=c("solid","twodash"),label=c("Initial","Final"))+
  labs(linetype="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   legend.position = c(0.88,2.17),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_blank(),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.25, "cm"))
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 140), ylim = c(0, 80), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_sum_abs_uhi_years), xmin = 0, xmax = 69, ymin = 0, ymax = 80) +  
  annotation_custom(ggplotGrob(plot_sum_abs_uhi_diff_years), xmin = 71, xmax = 140, ymin = 0, ymax = 80) +
  annotation_custom(ggplotGrob(plot_legend), xmin = 0, xmax = 70, ymin = -80, ymax = 0) + 
  annotation_custom(ggplotGrob(plot_legend), xmin = 70, xmax = 140, ymin = -80, ymax = 0) + 
  annotate("text", x =12, y =78, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =86, y =78, label = "(b)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s4.tif"),width=14,height=8, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()