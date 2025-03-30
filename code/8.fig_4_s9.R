###plot fig. 4, S9
##notes: UedG="u1",UedB="u2",UingG="ur1",UingB="ur2",Rural="r"

library(reshape2)
library(ggplot2)
require(broom)
library(sf)
library(foreach)

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
  input_temporal_diff=cbind(input_temporal[,1:3],
                            (input_temporal[,4:7]-input_temporal[,8]),
                            (input_temporal[,9:12]-input_temporal[,13]),
                            (input_temporal[,24:27]-input_temporal[,28]))
}

###regression fitting
{
  attribute=matrix(nrow=37, ncol=20)
  sum0=foreach (city=1:37,.combine = rbind) %do% {
    cat (city,"\n")
    if (city<37) {
      input_sel=input_temporal_diff[input_temporal_diff$ioa==city,]
    } else {
      input_sel=input_temporal_diff
    }
    
    standard_isa = input_sel[names(input_sel) %in% c("isa_ur1","isa_ur2")]
    standard_evi = input_sel[names(input_sel) %in% c("evi_u1","evi_u2","evi_ur1","evi_ur2")]
    
    input_sel[names(input_sel) %in% c("isa_ur1","isa_ur2")] = (input_sel[names(input_sel) %in% c("isa_ur1","isa_ur2")]-mean(as.matrix(standard_isa)))/sd(as.matrix(standard_isa))
    input_sel[names(input_sel) %in% c("evi_u1","evi_u2","evi_ur1","evi_ur2")] = (input_sel[names(input_sel) %in% c("evi_u1","evi_u2","evi_ur1","evi_ur2")]-mean(as.matrix(standard_evi)))/sd(as.matrix(standard_evi))
    
    sum00=foreach (position=1:4,.combine = rbind) %do% {
      input_sel1=input_sel[,c(c(3,7,11)+position)]
      names(input_sel1)=c("isa","evi","lst")
      if (position %in% 1:2) {
        lmModel=lm(lst~evi,data=input_sel1)
        para=summary(lmModel)$coefficients[2,c(1,4)]
        c(city,position,0,para[1],0,para[2],glance(lmModel)$p.value)
      } else {
        lmModel=lm(lst~isa+evi,data=input_sel1)
        para=summary(lmModel)$coefficients[c(2,3),c(1,4)]
        c(city,position,para[1,1],para[2,1],para[1,2],para[2,2],glance(lmModel)$p.value)
      }
    }
    sel_bin=apply(input_sel[input_sel$year %in% 2003:2005,-c(1:3)],2, mean,na.rm=TRUE)
    sel_end=apply(input_sel[input_sel$year %in% 2016:2018,-c(1:3)],2, mean,na.rm=TRUE)
    attribute[city,]=c((sel_end[1]-sel_bin[1])*sum00[1,3],
                       (sel_end[2]-sel_bin[2])*sum00[2,3],
                       (sel_end[3]-sel_bin[3])*sum00[3,3],
                       (sel_end[4]-sel_bin[4])*sum00[4,3],
                       (sel_end[5]-sel_bin[5])*sum00[1,4],
                       (sel_end[6]-sel_bin[6])*sum00[2,4],
                       (sel_end[7]-sel_bin[7])*sum00[3,4],
                       (sel_end[8]-sel_bin[8])*sum00[4,4],
                       (sel_end[1:12]-sel_bin[1:12]))
    
    sum00=foreach (position=1:4,.combine = rbind) %do% {
      input_sel1=input_sel[,c(c(3,7,11)+position)]
      names(input_sel1)=c("isa","evi","lst")
      if (position %in% 1:2) {
        lmModel=lm(lst~evi,data=input_sel1)
        para=summary(lmModel)$coefficients[2,c(1,4)]
        c(city,position,0,para[1]/sd(as.matrix(standard_evi)),0,para[2]/sd(as.matrix(standard_evi)),glance(lmModel)$p.value)
      } else {
        lmModel=lm(lst~isa+evi,data=input_sel1)
        para=summary(lmModel)$coefficients[c(2,3),c(1,4)]
        c(city,position,para[1,1]/sd(as.matrix(standard_isa)),para[2,1]/sd(as.matrix(standard_evi)),
          para[1,2]/sd(as.matrix(standard_isa)),para[2,2]/sd(as.matrix(standard_evi)),
          glance(lmModel)$p.value)      }
    }
    if (max(sum00[,7]>0.01)) {
      cbind(sum00,0)
    } else { 
      cbind(sum00,1)
    }
  }
  lst_reg=data.frame(sum0)
  names(lst_reg)=c("city","position","slope_isa","slope_evi","p_isa","p_evi","p_overall","pass")
  lst_reg_city=lst_reg[lst_reg$city<37,]
  lst_reg_all=lst_reg[lst_reg$city==37,]
}
###attribution######
{
  attribute_sum=as.data.frame(cbind(1:37,attribute))
  
  names(attribute_sum)=c("city",paste0("att_",names(input_temporal_diff)[4:11]),names(input_temporal_diff)[4:15])
 
  attribute_mean=rbind(t(apply(attribute_sum[1:36,],2,mean)),attribute_sum[37,])[1,]
  attribute_mean$att_isa_ur2/(attribute_mean$att_evi_ur2+attribute_mean$att_isa_ur2)
  attribute_mean$att_evi_ur2/(attribute_mean$att_evi_ur2+attribute_mean$att_isa_ur2)
  attribute_mean$att_isa_ur1/(attribute_mean$att_evi_ur1+attribute_mean$att_isa_ur1)
  attribute_mean$att_evi_ur1/(attribute_mean$att_evi_ur1+attribute_mean$att_isa_ur1)
  attribute_colum = data.frame(
    Group = rep(c("U1", "U2", "UR1","UR2"), each = 3),
    Component = rep(c("aa", "cc", "bb"), times = 4),
    Contribute =c(attribute_mean$lst_u1,
                  attribute_mean$att_isa_u1,
                  attribute_mean$att_evi_u1,
                  attribute_mean$lst_u2,
                  attribute_mean$att_isa_u2,
                  attribute_mean$att_evi_u2,
                  attribute_mean$lst_ur1,
                  attribute_mean$att_isa_ur1,
                  attribute_mean$att_evi_ur1,
                  attribute_mean$lst_ur2,
                  attribute_mean$att_isa_ur2,
                  attribute_mean$att_evi_ur2))
  
  attribute_colum = data.frame(
    Group = rep(c("aa", "bb","cc","dd"), each = 2),
    Component = rep(c("aa", "bb"), times = 4),
    Contribute =rbind(
      c(attribute_mean$lst_u1,0),
      c(attribute_mean$att_isa_u1, attribute_mean$att_evi_u1),
      c(attribute_mean$lst_u2,0),
      c(attribute_mean$att_isa_u2,attribute_mean$att_evi_u2),
      c(attribute_mean$lst_ur1,0),
      c(attribute_mean$att_isa_ur1,attribute_mean$att_evi_ur1),
      c(attribute_mean$lst_ur2,0),
      c(attribute_mean$att_isa_ur2,attribute_mean$att_evi_ur2)))
  names(attribute_colum)= c("Type","Geno", "ISA", "EVI")
  
  library(tidyr)
  library(dplyr)
  attribute_colum = attribute_colum %>% 
    pivot_longer(c(EVI, ISA)) %>% # Reshape your dataframe to get one column of factor with levels "Count", "subcount"
    group_by(Type, Geno) %>%             # Compute the sum of 'Count' and 'subcount' value
    mutate(cum_tot = cumsum(value)) 
  attribute_colum[which(attribute_colum$value<0),]$cum_tot=attribute_colum[which(attribute_colum$value<0),]$value
  plot_att0 = ggplot(data=attribute_colum,aes(x=Type, y=cum_tot, fill = interaction(name,Geno)) )+ 
    geom_col(data = . %>% filter(name=="ISA"), position = position_dodge(width = 0.9)) +
    geom_col(data = . %>% filter(name=="EVI"), position = position_dodge(width = 0.9)) +
    scale_fill_manual(values=c('gray','forestgreen','gray','darkred'),
                      labels = c("LST", "EVI", "Total", "ISA"))+
    scale_x_discrete(labels = c("UedG","UedB","UingG","UingB"))+
    geom_hline(yintercept = 0, linetype = "dashed",alpha=0.6)+
    labs(fill="",x="",y=expression(Delta~UHI~'('*degree*C*')'))+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.text=element_text(colour="black", size=9, face="plain"),
                     axis.title.y=element_text(colour="black", size=10, face="plain"),
                     axis.title.x = element_blank(),
                     legend.position = "none",
                     plot.margin = margin(0, 0, 0, 0, "cm"),
                     plot.title = element_blank())
  aaa=data.frame(rbind(c(1,1),c(2,2),c(3,3)))
  plot_legend=ggplot(data=aaa,aes(x=X1, y=X2, fill = as.factor(X2)))+ geom_col() + 
    scale_fill_manual(values=c('gray','forestgreen','darkred'),
                      labels = c(expression(Delta*"UHI"[obs]), 
                                 expression(Delta*"UHI"[EVI]),
                                 expression(Delta*"UHI"[ISA])))+ labs(fill="")+
    theme_bw()+theme(legend.position = c(0.15,3),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"))
  plot_att=ggplot() +
    coord_equal(xlim = c(0, 130), ylim = c(0, 100), expand = FALSE) +
    annotation_custom(ggplotGrob(plot_att0), xmin = 1, xmax = 130, ymin = 0, ymax = 100) +
    annotation_custom(ggplotGrob(plot_legend), xmin = 1, xmax = 130, ymin = -100, ymax = -25) +
    theme(text=element_text(family="serif"),plot.margin = unit(c(0,0,0,0), "cm"))+theme_void()
  tiff(paste0("../figure/Fig_s9.tif"),width=10,height=8, units = "cm", res = 300, compression = "lzw")
  plot(plot_att)
  dev.off()
  ###only divide
  attribute_colum = data.frame(
    Group = rep(c("aa", "bb","cc","dd"), each = 1),
    Component = rep(c("aa"), times = 4),
    Contribute =rbind(
      c(attribute_mean$att_isa_u1, attribute_mean$att_evi_u1),
      c(attribute_mean$att_isa_u2,attribute_mean$att_evi_u2),
      c(attribute_mean$att_isa_ur1,attribute_mean$att_evi_ur1),
      c(attribute_mean$att_isa_ur2,attribute_mean$att_evi_ur2)))
  names(attribute_colum)= c("Type","Geno", "ISA", "EVI")

  attribute_colum = attribute_colum %>% 
    pivot_longer(c(EVI, ISA)) %>% # Reshape your dataframe to get one column of factor with levels "Count", "subcount"
    group_by(Type, Geno) %>%             # Compute the sum of 'Count' and 'subcount' value
    mutate(cum_tot = cumsum(value)) 
  plot_att = ggplot(data=attribute_colum,aes(x=Type, y=cum_tot, fill = interaction(name,Geno)) )+ 
    geom_col(data = . %>% filter(name=="ISA"), position = position_dodge(width = 0.9)) +
    geom_col(data = . %>% filter(name=="EVI"), position = position_dodge(width = 0.9)) +
    scale_fill_manual(values=c('darkred','forestgreen'),
                      labels = c(expression(Delta*"LST"[ISA]),
                                 expression(Delta*"LST"[EVI])))+ 
    scale_x_discrete(labels = c("UedG","UedB","UingG","UingB"))+
    geom_hline(yintercept = 0, linetype = "dashed",alpha=0.6)+
    labs(fill="",x="",y=expression(Delta~UHI~'('*degree*C*')'))+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.text=element_text(colour="black", size=9, face="plain"),
                     axis.title.y=element_text(colour="black", size=10, face="plain"),
                     axis.title.x = element_blank(),
                     legend.position = c(0.2,0.95),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain"),
                     legend.key.size = unit(0.3, "cm"),
                     legend.key.width = unit(0.5, "cm"),
                     plot.margin = margin(0, 0, 0, 0, "cm"),
                     plot.title = element_blank())
}

# Reshape the data
attribute_sumlong_data <- reshape(attribute_sum,
                                  varying = list(
                                    att_isa = grep("^att_isa_", names(attribute_sum), value = TRUE), # Columns with "isa_"
                                    att_evi = grep("^att_evi_", names(attribute_sum), value = TRUE),
                                    isa = grep("^isa_", names(attribute_sum), value = TRUE),
                                    evi = grep("^evi_", names(attribute_sum), value = TRUE),
                                    lst = grep("^lst_", names(attribute_sum), value = TRUE)# Columns with "evi_"
                                  ),
                                  v.names = c("att_isa", "att_evi","isa","evi","lst"),    # New column names for values
                                  timevar = "position",            # Column name for the time identifiers
                                  times = c("1", "2", "3", "4"), # Time labels
                                  direction = "long")          # Convert to long format
#reorder
attribute_sumlong_data <- attribute_sumlong_data[order(attribute_sumlong_data$city), ] 
attribute_sumlong_m = merge(attribute_sumlong_data,lst_reg_city,by=c("city","position"))
attribute_sumlong_m$percent_isa=attribute_sumlong_m$att_isa/(attribute_sumlong_m$att_isa+attribute_sumlong_m$att_evi)
attribute_sumlong_m$percent_evi=attribute_sumlong_m$att_evi/(attribute_sumlong_m$att_isa+attribute_sumlong_m$att_evi)

UHIcontribution= aggregate(attribute_sumlong_m,by=list(attribute_sumlong_m$position),mean,na.rm=T)
UHIcontribution_sd= aggregate(attribute_sumlong_m,by=list(attribute_sumlong_m$position),sd,na.rm=T)
UHIcontribution$position=1:4
UHIcontribution$percent_isa=UHIcontribution$att_isa/(UHIcontribution$att_isa+UHIcontribution$att_evi)
UHIcontribution$percent_evi=UHIcontribution$att_evi/(UHIcontribution$att_isa+UHIcontribution$att_evi)
UHIcontribution_slope_isa=cbind(UHIcontribution[c("position","slope_isa")],UHIcontribution_sd$slope_isa)
names(UHIcontribution_slope_isa)=c("position","mean","sd")
plot_slope_isa=ggplot(UHIcontribution_slope_isa,aes(as.factor(position),mean,fill=as.factor(position)))+
  geom_col(position = position_dodge(width = 0.9))+
  geom_errorbar(data= UHIcontribution_slope_isa,aes(x=position, ymin= (mean - sd),ymax=(mean + sd),
                                        linecolor="black"), width=0.2,size=0.3)+
  scale_fill_manual(values=rf[1:4])+
  scale_x_discrete(labels = c("UedG","UedB","UingG","UingB"))+
  labs(fill="",x="")+
  ylab(expression(Delta*'LST/'*Delta*ISA~'('*degree*C*'/%'*')'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_blank())
UHIcontribution_slope_evi=cbind(UHIcontribution[c("position","slope_evi")],UHIcontribution_sd$slope_evi)
names(UHIcontribution_slope_evi)=c("position","mean","sd")
plot_slope_evi=ggplot(UHIcontribution_slope_evi,aes(as.factor(position),mean,fill=as.factor(position)))+
  geom_col(position = position_dodge(width = 0.9))+
  geom_errorbar(data= UHIcontribution_slope_evi,aes(x=position, ymin= (mean - sd),ymax=(mean + sd),
                                                    linecolor="black"), width=0.2,size=0.3)+
  scale_fill_manual(values=rf[1:4])+
  scale_x_discrete(labels = c("UedG","UedB","UingG","UingB"))+
  labs(fill="",x="")+
  ylab(expression(Delta*'LST/'*Delta*EVI~'('*degree*C*'/1'*')'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_blank())
UHIcontribution_isa=cbind(UHIcontribution[c("position","isa")],UHIcontribution_sd$isa)
names(UHIcontribution_isa)=c("position","mean","sd")
plot_isa=ggplot(UHIcontribution_isa,aes(as.factor(position),mean,fill=as.factor(position)))+
  geom_col(position = position_dodge(width = 0.9))+
  geom_errorbar(data= UHIcontribution_isa,aes(x=position, ymin= (mean - sd),ymax=(mean + sd),
                                              linecolor="black"),  width=0.2,size=0.3)+
  scale_fill_manual(values=rf[1:4])+
  scale_x_discrete(labels = c("UedG","UedB","UingG","UingB"))+
  labs(fill="",x="")+
  ylab(expression(Delta*'ISA'~'('*'%'*')'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_blank())
UHIcontribution_evi=cbind(UHIcontribution[c("position","evi")],UHIcontribution_sd$evi)
names(UHIcontribution_evi)=c("position","mean","sd")
plot_evi=ggplot(UHIcontribution_evi,aes(as.factor(position),mean,fill=as.factor(position)))+
  geom_col(position = position_dodge(width = 0.9))+
  geom_errorbar(data= UHIcontribution_evi,aes(x=position, ymin= (mean - sd),ymax=(mean + sd),
                                              linecolor="black"), width=0.2,size=0.3)+
  scale_fill_manual(values=rf[1:4])+
  scale_x_discrete(labels = c("UedG","UedB","UingG","UingB"))+
  labs(fill="",x="")+
  ylab(expression(Delta*'EVI'))+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text=element_text(colour="black", size=9, face="plain"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x = element_blank(),
                   legend.position = "none",
                   plot.margin = margin(0, 0, 0, 0, "cm"),
                   plot.title = element_blank())

plot_sum=ggplot() +
  coord_equal(xlim = c(0, 210), ylim = c(0, 121), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_att), xmin = 1, xmax = 70, ymin = 0, ymax = 120) +
  annotation_custom(ggplotGrob(plot_isa), xmin = 73, xmax = 140, ymin = 61, ymax = 120) +
  annotation_custom(ggplotGrob(plot_evi), xmin = 73, xmax = 140, ymin = 0, ymax = 59) +
  annotation_custom(ggplotGrob(plot_slope_isa), xmin = 143, xmax = 210, ymin = 61, ymax = 120) +
  annotation_custom(ggplotGrob(plot_slope_evi), xmin = 141, xmax = 210, ymin = 0, ymax = 59) +
  annotate("text", x =40, y =118, label = "(a)",size=3.5,family="serif",fontface = "bold")+
  annotate("text", x =110, y =118, label = "(b)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =178, y =118, label = "(c)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =110, y =57, label = "(d)",size=3.5,family="serif",fontface="bold")+
  annotate("text", x =178, y =57, label = "(e)",size=3.5,family="serif",fontface="bold")+
  theme(text=element_text(family="serif"),plot.margin = unit(c(0,0,0,0), "cm"))+theme_void()
tiff(paste0("../figure/Fig_4.tif"),width=18,height=10.3, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()