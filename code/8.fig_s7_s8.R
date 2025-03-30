###plot fig. s7,s
##notes: UedG="u1",UedB="u2",UingG="ur1",UingB="ur2",Rural="r"

library(reshape2)
library(ggplot2)
library(smoothr)
library(foreach)
library(raster)
rm(list=ls()) 
rf=c("orangered4","orangered","royalblue4","royalblue","forestgreen")

##interannual variation of LST,ISA,EVI in urban and rural#######
satellite="MYD"
times=c("day")
index=read.csv("../input/index_cluster_captical_size.csv")
time=1
########################################################################################################################################
plot_sum=list()
sum_sum=foreach (city=1:36.,.combine=rbind) %do% {
  cat(city,"\n")
  sum_sum1=read.csv(paste0("../output/5.3.atc_evaluation/plot_",tolower(index[city,2]),"_",times[time],".csv"))
  lst_raw_1=sum_sum1[sum_sum1$type==1,5]
  lst_atc_1=sum_sum1[sum_sum1$type==2,5]
  
  lst_raw_2=lst_raw_1[!is.na(lst_raw_1)&!is.na(lst_atc_1)]
  lst_atc_2=lst_atc_1[!is.na(lst_raw_1)&!is.na(lst_atc_1)]
  
  rmse=round(sqrt(mean((lst_raw_2-lst_atc_2)^2)),2)
  me=round(mean(lst_raw_2-lst_atc_2),2)
  
  rr=round(cor(lst_atc_2,lst_raw_2),2)
  pvalue=round(cor.test(lst_atc_2,lst_raw_2)$p.value,2)
  slope=round(coef(lm(lst_atc_2~lst_raw_2+0)),2)
  
  plot_sum[[city]]=ggplot()+
    geom_point(data= sum_sum1,aes(x=doy,y=lst,color=as.factor(type)),size=0.2,shape=16)+
    scale_color_manual(values=rf[c(4,2)],label=c("OBS","ATC"))+
    scale_x_continuous(limits = c(1,(365*16)), breaks = c(182+365*(0:15)),labels = c(2003:2018))+
    labs(color="")+
    ylab(expression(LST~'('*degree*C*')'))+xlab("")+
    annotate("text", x =2900, y =50, 
             label = as.expression(bquote("Regression slope="~.(slope)~"  R="~.(rr)~"  P-value="~.(pvalue)~"  RMSE="~.(rmse)*degree*C*"  Mean error="~.(me)*degree*C)),
             size=3,family="serif")+
    annotate("text", x =50, y =50, label = index[city,2], size=3,family="serif")+
    guides(colour = guide_legend(override.aes = list(size=1)))+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     legend.position = c(0.96,0.97),
                     legend.direction = "vertical",
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain"),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.25, "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
    c(city,rmse,me,rr,pvalue,slope)
}
sum_sum1=as.data.frame(cbind(index[,2],sum_sum))
names(sum_sum1)=c("city","id","rmse","merror","rr","p_value","slope")

plot_sum1=ggplot() +
  coord_equal(xlim = c(0, 180), ylim = c(0, 170), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_sum[[1]]), xmin = 0, xmax = 180, ymin = 110, ymax = 170) +  
  annotation_custom(ggplotGrob(plot_sum[[9]]), xmin = 0, xmax = 180, ymin = 55, ymax = 115) +
  annotation_custom(ggplotGrob(plot_sum[[11]]), xmin = 0, xmax = 180, ymin = 0, ymax = 60) +
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s7.tif"),width=18,height=17, units = "cm", res = 300, compression = "lzw")
plot(plot_sum1)
dev.off()

sum_sum20=data.frame(sum_sum,index[,c(2,8)])
names(sum_sum20)=c("id","rmse","merror","rr","p_value","slope","city","size")
library(dplyr)
sum_sum2=sum_sum20 %>% arrange(desc(size))
plot_sum_rmse=ggplot(sum_sum2,aes(x=as.factor(id),y=rmse)) + 
  geom_bar(stat="identity",fill=rf[3],alpha=0.8)+
  scale_x_discrete(labels = sum_sum2$city)+
  scale_y_continuous(limits = c(0,5.8),breaks =seq(0,5.8,by=1.5))+
  ylab(expression(RMSE~'('*degree*C*')'))+xlab("")+
  labs(fill="")+theme_bw() +
  theme(text=element_text(family="serif"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour="black", size=10, face="plain",vjust = 2.5),
        axis.text.x =element_text(colour="black", size=10, face="plain",angle = 60,hjust = 1.2,vjust=1.1),
        axis.text.y =element_text(colour="black", size=10, face="plain"),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"),
        plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
plot_sum_rr=ggplot(sum_sum2,aes(x=as.factor(id),y=rr)) + 
  geom_bar(stat="identity",fill=rf[3],alpha=0.8)+
  scale_x_discrete(labels = index[,2])+
  ylab("Correlation Coefficient")+xlab("")+
  labs(fill="")+theme_bw() +
  theme(text=element_text(family="serif"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour="black", size=10, face="plain"),
        axis.text.x =element_blank(),
        axis.text.y =element_text(colour="black", size=10, face="plain"),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"),
        plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
plot_sum_me=ggplot(sum_sum2,aes(x=as.factor(id),y=merror)) + 
  geom_bar(stat="identity",fill=rf[3],alpha=0.8)+
  scale_x_discrete(labels = index[,2])+
  ylab(expression(Mean~error~'('*degree*C*')'))+xlab("")+
  labs(fill="")+theme_bw() +
  theme(text=element_text(family="serif"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour="black", size=10, face="plain"),
        axis.text.x =element_blank(),
        axis.text.y =element_text(colour="black", size=10, face="plain"),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"),
        plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
plot_sum_slope=ggplot(sum_sum2,aes(x=as.factor(id),y=slope)) + 
  geom_bar(stat="identity",fill=rf[3],alpha=0.8)+
  scale_x_discrete(labels = index[,2])+
  ylab("Regression slope")+xlab("")+
  labs(fill="")+theme_bw() +
  theme(text=element_text(family="serif"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour="black", size=10, face="plain"),
        axis.text.x =element_blank(),
        axis.text.y =element_text(colour="black", size=10, face="plain"),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0, 0.1, "cm"),
        plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

data_rmse=data.frame(position=1,value=sum_sum[,2])
plot_box_rmse=ggplot(data_rmse,aes(x=position,y=value)) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(fatten = NULL,outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1, fill=rf[3],alpha=0.8)+
  stat_summary(fun.min = mean, fun = mean, fun.max = mean, geom="errorbar", width=0.5,size=0.5, linetype="dashed",position=position_dodge(width=.75))+
  ylab(expression(RMSE~'('*degree*C*')'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   legend.position = "none",
                   axis.title = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   plot.title = element_blank())
data_rr=data.frame(position=1,value=sum_sum[,4])
plot_box_rr=ggplot(data_rr,aes(x=position,y=value)) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(fatten = NULL,outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1, fill=rf[3],alpha=0.8)+
  stat_summary(fun.min = mean, fun = mean, fun.max = mean, geom="errorbar", width=0.5,size=0.5, linetype="dashed",position=position_dodge(width=.75))+
  ylab("Correlation coefficient")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   legend.position = "none",
                   axis.title = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   plot.title = element_blank())
data_slope=data.frame(position=1,value=sum_sum[,6])
plot_box_slope=ggplot(data_slope,aes(x=position,y=value)) + 
  stat_boxplot(geom = "errorbar", width = 0.15,size=0.1) +  
  geom_boxplot(fatten = NULL,outlier.size = 0,outlier.shape = NA,width=0.5, size=0.1, fill=rf[3],alpha=0.8)+
  stat_summary(fun.min = mean, fun = mean, fun.max = mean, geom="errorbar", width=0.5,size=0.5, linetype="dashed",position=position_dodge(width=.75))+
  ylab("Regression slope")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   legend.position = "none",
                   axis.title = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   plot.title = element_blank())

plot_sum=ggplot() +
  coord_equal(xlim = c(0, 170), ylim = c(0, 140), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_sum_slope), xmin = 0, xmax = 140, ymin = 100, ymax = 140) +  
  annotation_custom(ggplotGrob(plot_sum_rr), xmin = 0, xmax = 140, ymin = 60, ymax = 100) +
  annotation_custom(ggplotGrob(plot_sum_rmse), xmin = 1, xmax = 140, ymin = 0, ymax = 60) + 
  annotation_custom(ggplotGrob(plot_box_slope), xmin = 140, xmax = 170, ymin = 100, ymax = 140) +  
  annotation_custom(ggplotGrob(plot_box_rr), xmin = 141, xmax = 170, ymin = 60, ymax = 100) +
  annotation_custom(ggplotGrob(plot_box_rmse), xmin = 141, xmax = 170, ymin = 20, ymax = 60) +
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_s8.tif"),width=17,height=14, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()