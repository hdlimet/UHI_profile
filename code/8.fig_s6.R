###plot fig. s6
##notes: UedG="u1",UedB="u2",UingG="ur1",UingB="ur2",Rural="r"

library(raster)
library("sp")
library("rgdal")
library("rgeos")
library(reshape2)
library(ggplot2)
library(foreach)
rf=c("orangered4","orangered","royalblue4","royalblue","forestgreen")

time='day'
satellite="MYD"
input0=read.csv(paste0("../output/7.1.year_variation/",satellite,"_temporal_variation_",time,".csv"))
input=cbind(input0[,c(1:3)],(input0[,4:7]-input0[,8]),(input0[,9:12]-input0[,13]),(input0[,24:27]-input0[,28]))

category=c("(a) UedG","(b) UedB","(c) UingG","(d) UingB")
plot_evi=list()
plot_isa=list()
for (position in 1:4) {
  input_sel1=as.data.frame(input[,c(1,c(3,7,11)+position)])
  names(input_sel1)=c("ioa","isa","evi","lst")
  rr_isa=round(cor(input_sel1[,2],input_sel1[,4]),2)
  rr_evi=round(cor(input_sel1[,3],input_sel1[,4]),2)
  pvalue_isa= round(cor.test(input_sel1[,2],input_sel1[,4])$p.value,2)*100
  pvalue_evi= round(cor.test(input_sel1[,3],input_sel1[,4])$p.value,2)*100
  slope_isa=round(coef(lm(input_sel1[,4]~input_sel1[,2])),2)[2]
  slope_evi=round(coef(lm(input_sel1[,4]~input_sel1[,3])),2)[2]
  if (pvalue_isa<0.01) {pvalue_isa="p<0.01"} else if (pvalue_isa>0.01 & pvalue_isa<0.05) {pvalue_isa="p<0.05"} else {pvalue_isa=paste0("p=",sprintf("%.1f",pvalue_isa))}
  if (pvalue_evi<0.01) {pvalue_evi="p<0.01"} else if (pvalue_evi>0.01 & pvalue_evi<0.05) {pvalue_evi="p<0.05"} else {pvalue_evi=paste0("p=",sprintf("%.1f",pvalue_evi))}
  plot_evi[[position]]=ggplot()+
    geom_point(data= input_sel1,aes(x=evi,y=lst,
                                    color=as.factor(ioa)),size=0.4,shape = 16)+
    geom_smooth(data= input_sel1,aes(x=evi,y=lst),method="lm",size=0.5,fill=NA,color="black")+
    scale_y_continuous(limits = c(-2,8),breaks = seq(0,8,by=2))+
    scale_x_continuous(limits = c(-0.35,0.1),breaks = seq(-0.35,0.1,by=0.1))+
    labs(color="")+ylab("")+xlab("")+
    geom_text(label=paste0("r=",rr_evi,"  ",pvalue_evi),
              aes(x= Inf,y = Inf), hjust = 1.3,vjust =1.2,
              colour = "black",fontface = "plain",size=3.5,family="serif") +
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title = element_blank(),
                     legend.position = "none",
                     axis.text = element_text(face="plain",size=10,vjust=0,color = "black"))
  
  plot_isa[[position]]=ggplot()+
    geom_point(data= input_sel1,aes(x=isa,y=lst,
                                    color=as.factor(ioa)),size=0.4,shape = 16)+
    geom_smooth(data= input_sel1,aes(x=isa,y=lst),method="lm",size=0.5,fill=NA,color="black")+
    scale_y_continuous(limits = c(-2,8),breaks = seq(0,8,by=2))+
    scale_x_continuous(limits = c(0,1),breaks = seq(0,1,by=0.2))+
    labs(color="")+ylab("")+xlab("")+
    geom_text(label=paste0("r=",rr_isa,"  ",pvalue_isa),
              aes(x= Inf,y = Inf), hjust = 1.2,vjust =1.2,
              colour = "black",fontface = "plain",size=3.5,family="serif") +
    geom_text(label=category[position],
              aes(x= -Inf,y = Inf), hjust = -0.1,vjust =1.2,
              colour = "black",fontface = "bold",size=3.5,family="serif") +
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title = element_blank(),
                     legend.position = "none",
                     axis.text = element_text(face="plain",size=10,vjust=0,color = "black"))
} 
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 125), ylim = c(0, 179), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_isa[[1]]), xmin = 5, xmax = 65, ymin = 134, ymax = 179) +
  annotation_custom(ggplotGrob(plot_isa[[2]]), xmin = 5, xmax = 65, ymin = 90, ymax =135) +
  annotation_custom(ggplotGrob(plot_isa[[3]]), xmin = 5, xmax = 65, ymin = 46, ymax = 91) +
  annotation_custom(ggplotGrob(plot_isa[[4]]), xmin = 5, xmax = 65, ymin = 2, ymax = 47) +
  annotation_custom(ggplotGrob(plot_evi[[1]]), xmin = 65, xmax = 125, ymin = 134, ymax = 179) +
  annotation_custom(ggplotGrob(plot_evi[[2]]), xmin = 65, xmax = 125, ymin = 90, ymax = 135) +
  annotation_custom(ggplotGrob(plot_evi[[3]]), xmin = 65, xmax = 125, ymin = 46, ymax = 91) +
  annotation_custom(ggplotGrob(plot_evi[[4]]), xmin = 65, xmax = 125, ymin = 2, ymax = 47) +
  annotate("text", x =3, y =160, label = expression(UHI~'('*degree*C*')'),size=3,family="serif",angle = 90)+
  annotate("text", x =3, y =115, label = expression(UHI~'('*degree*C*')'),size=3,family="serif",angle = 90)+
  annotate("text", x =3, y =70, label = expression(UHI~'('*degree*C*')'),size=3,family="serif",angle = 90)+
  annotate("text", x =3, y =25, label = expression(UHI~'('*degree*C*')'),size=3,family="serif",angle = 90)+
  annotate("text", x =35, y =2, label = expression(paste(Delta,"ISA")),size=3,family="serif",angle = 0)+
  annotate("text", x =95, y =2, label = expression(paste(Delta,"EVI")),size=3,family="serif",angle = 0)+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()

tiff(paste0("../figure/Fig_s6.tif"),width=12.5,height=17.9, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()