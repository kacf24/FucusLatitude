library(readr)
library(gridExtra)

library(extrafont)
font_import()
loadfonts(device = "win")
loadfonts(device = "postscript")
loadfonts()

all_H <- data.frame(read_csv("C:/Users/Laminaria/Desktop/ASV Correlations/FinalCorrelations/SIMPER/all_H.csv"))
all_R <- data.frame(read_csv("C:/Users/Laminaria/Desktop/ASV Correlations/FinalCorrelations/SIMPER/all_R.csv"))
all_V <- data.frame(read_csv("C:/Users/Laminaria/Desktop/ASV Correlations/FinalCorrelations/SIMPER/all_V.csv"))




H<-subset(df,Tissue=="H"&Site!="Green")
H<-H[H$ASVid %in% all_H$ASVid,]

R<-subset(df,Tissue=="R"&Site!="Green")
R<-R[R$ASVid %in% all_R$ASVid,]

V<-subset(df,Tissue=="V"&Site!="Green")
V<-V[V$ASVid %in% all_V$ASVid,]






for(q in 1:length(unique(all_H$ASVid))){
  
  subH <- subset(H,H$ASVid == unique(all_H$ASVid)[q])
  subHW <- subset(subH,subH$lon < -30)
  subHE <- subset(subH,subH$lon > -30)
  g1<-ggplot()+geom_point(size=0.5,data=subHW,aes(x=subHW[,all_H[q,4]],y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="black",data=subHW,aes(x=subHW[,all_H[q,4]],y=measurement),se=FALSE)  + ylab("Relative Abundance")
  g1<-g1 + geom_point(size=0.5,data=subHE,aes(x=subHE[,all_H[q,4]],y=measurement), color = "red")+geom_smooth(method="gam",formula=y~s(x,k=5),color="red",data=subHE,aes(x=subHE[,all_H[q,4]],y=measurement),se=FALSE)+ylim(c(0,max(subH$measurement)))+xlim(c(min(subH[,all_H[q,4]]),max(subH[,all_H[q,4]])))
  g2<-ggplot()+geom_point(size=0.5,data=subHW,aes(x=subHW[,all_H[q,6]],y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="black",data=subHW,aes(x=subHW[,all_H[q,6]],y=measurement),se=FALSE)  + ylab("Relative Abundance")
  g2<-g2 + geom_point(size=0.5,data=subHE,aes(x=subHE[,all_H[q,6]],y=measurement), color = "red")+geom_smooth(method="gam",formula=y~s(x,k=5),color="red",data=subHE,aes(x=subHE[,all_H[q,6]],y=measurement),se=FALSE)+ylim(c(0,max(subH$measurement)))+xlim(c(min(subH[,all_H[q,6]]),max(subH[,all_H[q,6]])))
  g3<-ggplot()+geom_point(size=0.5,data=subHW,aes(x=lat,y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="blue",data=subH,aes(x=lat,y=measurement),se=FALSE) + xlab("Latitude (°)") + ylab("Relative Abundance")
  g3<-g3 + geom_point(size=0.5,data=subHE,aes(x=lat,y=measurement), color = "red")+ylim(c(0,max(subH$measurement)))+xlim(c(min(subH$lat),max(subH$lat)))
  g3 <- g3 +theme(text=element_text(size=6, family="Arial"))
  qq<-unlist(strsplit(as.character(all_H[q,4]),"_"))
  qqqq<-unlist(strsplit(as.character(all_H[q,6]),"_"))

if(qq[2] != "PAR"){    
if(qq[1] == "min"){qq[1] <- "Minimum"}
if(qq[1] == "max"){qq[1] <- "Maximum"}
if(qq[1] == "median"){qq[1] <- "Median"}
if(qq[2] == "windBearing"){qq[2] <- "Wind Bearing (°)"}
if(qq[2] == "windSpeed"){qq[2] <- "Wind Speed (m/s)"}
if(qq[2] == "dewPoint"){qq[2] <- "Dew Point (°C)"}
if(qq[2] == "Photoperiod"){qq[2] <- "Photoperiod (h)"}
if(qq[2] == "TidalHeight"){qq[2] <- "Relative Tidal Height"}

if(qq[2] == "temperature"){qq[2] <- "Air Temperature (°C)"}
if(qq[2] == "SST"){qq[2] <- "SST (°C)"}
if(qq[2] == "humidity"){qq[2] <- "Relative Humidity"}
qq <- paste(qq[1],qq[2])
}else{
if(qq[2] == "PAR" & qq[1] == "min"){qq <- bquote('Minimum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qq[2] == "PAR" & qq[1] == "max"){qq <- bquote('Maximum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qq[2] == "PAR" & qq[1] == "median"){qq <- bquote('Median PAR (Einsteins' ~ m^-2~day^-1*')')}
}
  
if (qqqq[2] != "PAR"){
if(qqqq[1] == "min"){qqqq[1] <- "Minimum"}
if(qqqq[1] == "max"){qqqq[1] <- "Maximum"}
if(qqqq[1] == "median"){qqqq[1] <- "Median"}
if(qqqq[2] == "windBearing"){qqqq[2] <- "Wind Bearing (°)"}
if(qqqq[2] == "windSpeed"){qqqq[2] <- "Wind Speed (m/s)"}
if(qqqq[2] == "dewPoint"){qqqq[2] <- "Dew Point (°C)"}
if(qqqq[2] == "Photoperiod"){qqqq[2] <- "Photoperiod (h)"}
if(qqqq[2] == "TidalHeight"){qqqq[2] <- "Relative Tidal Height"}
if(qqqq[2] == "windSpeed"){qqqq[2] <- "Wind Speed"}
if(qqqq[2] == "temperature"){qqqq[2] <- "Air Temperature (°C)"}
if(qqqq[2] == "SST"){qqqq[2] <- "SST (°C)"}
if(qqqq[2] == "humidity"){qqqq[2] <- "Relative Humidity"}
qqqq <- paste(qqqq[1],qqqq[2])
}else{
if(qqqq[2] == "PAR" & qqqq[1] == "min"){qqqq <- bquote('Minimum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qqqq[2] == "PAR" & qqqq[1] == "max"){qqqq <- bquote('Maximum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qqqq[2] == "PAR" & qqqq[1] == "median"){qqqq <- bquote('Median PAR (Einsteins' ~ m^-2~day^-1*')')}
}

g1<-g1+theme(text=element_text(size=6, family="Arial")) + xlab(qq)+ggtitle("Holdfast")+theme(plot.title = element_text(hjust = 0.5))
g2<-g2+theme(text=element_text(size=6, family="Arial")) + xlab(qqqq)
  
  
  
  subR <- subset(R,R$ASVid == unique(all_R$ASVid)[q])
  subRW <- subset(subR,subR$lon < -30)
  subRE <- subset(subR,subR$lon > -30)
  g4<-ggplot()+geom_point(size=0.5,data=subRW,aes(x=subRW[,all_R[q,4]],y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="black",data=subRW,aes(x=subRW[,all_R[q,4]],y=measurement),se=FALSE)  + ylab("")
  g4<-g4 + geom_point(size=0.5,data=subRE,aes(x=subRE[,all_R[q,4]],y=measurement), color = "red")+geom_smooth(method="gam",formula=y~s(x,k=5),color="red",data=subRE,aes(x=subRE[,all_R[q,4]],y=measurement),se=FALSE)+ylim(c(0,max(subR$measurement)))+xlim(c(min(subR[,all_R[q,4]]),max(subR[,all_R[q,4]])))
  g5<-ggplot()+geom_point(size=0.5,data=subRW,aes(x=subRW[,all_R[q,6]],y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="black",data=subRW,aes(x=subRW[,all_R[q,6]],y=measurement),se=FALSE)  + ylab("")
  g5<-g5 + geom_point(size=0.5,data=subRE,aes(x=subRE[,all_R[q,6]],y=measurement), color = "red")+geom_smooth(method="gam",formula=y~s(x,k=5),color="red",data=subRE,aes(x=subRE[,all_R[q,6]],y=measurement),se=FALSE)+ylim(c(0,max(subR$measurement)))+xlim(c(min(subR[,all_R[q,6]]),max(subR[,all_R[q,6]])))
  g6<-ggplot()+geom_point(size=0.5,data=subRW,aes(x=lat,y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="blue",data=subR,aes(x=lat,y=measurement),se=FALSE) + xlab("Latitude (°)") + ylab("")
  g6<-g6 + geom_point(size=0.5,data=subRE,aes(x=lat,y=measurement), color = "red")+ylim(c(0,max(subR$measurement)))+xlim(c(min(subR$lat),max(subR$lat)))
  g6 <- g6 +theme(text=element_text(size=6, family="Arial"))
  qq<-unlist(strsplit(as.character(all_R[q,4]),"_"))
  qqqq<-unlist(strsplit(as.character(all_R[q,6]),"_"))
  
if(qq[2] != "PAR"){  
if(qq[1] == "min"){qq[1] <- "Minimum"}
if(qq[1] == "max"){qq[1] <- "Maximum"}
if(qq[1] == "median"){qq[1] <- "Median"}
if(qq[2] == "windBearing"){qq[2] <- "Wind Bearing (°)"}
if(qq[2] == "windSpeed"){qq[2] <- "Wind Speed (m/s)"}
if(qq[2] == "dewPoint"){qq[2] <- "Dew Point (°C)"}
if(qq[2] == "Photoperiod"){qq[2] <- "Photoperiod (h)"}
if(qq[2] == "TidalHeight"){qq[2] <- "Relative Tidal Height"}

if(qq[2] == "temperature"){qq[2] <- "Air Temperature (°C)"}
if(qq[2] == "SST"){qq[2] <- "SST (°C)"}
if(qq[2] == "humidity"){qq[2] <- "Relative Humidity"}
qq <- paste(qq[1],qq[2])
}else{
if(qq[2] == "PAR" & qq[1] == "min"){qq <- bquote('Minimum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qq[2] == "PAR" & qq[1] == "max"){qq <- bquote('Maximum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qq[2] == "PAR" & qq[1] == "median"){qq <- bquote('Median PAR (Einsteins' ~ m^-2~day^-1*')')}
}
  
if(qqqq[2] != "PAR"){
if(qqqq[1] == "min"){qqqq[1] <- "Minimum"}
if(qqqq[1] == "max"){qqqq[1] <- "Maximum"}
if(qqqq[1] == "median"){qqqq[1] <- "Median"}
if(qqqq[2] == "windBearing"){qqqq[2] <- "Wind Bearing (°)"}
if(qqqq[2] == "windSpeed"){qqqq[2] <- "Wind Speed (m/s)"}
if(qqqq[2] == "dewPoint"){qqqq[2] <- "Dew Point (°C)"}
if(qqqq[2] == "Photoperiod"){qqqq[2] <- "Photoperiod (h)"}
if(qqqq[2] == "TidalHeight"){qqqq[2] <- "Relative Tidal Height"}
if(qqqq[2] == "windSpeed"){qqqq[2] <- "Wind Speed"}
if(qqqq[2] == "temperature"){qqqq[2] <- "Air Temperature (°C)"}
if(qqqq[2] == "SST"){qqqq[2] <- "SST (°C)"}
if(qqqq[2] == "humidity"){qqqq[2] <- "Relative Humidity"}
qqqq <- paste(qqqq[1],qqqq[2])
}else{
if(qqqq[2] == "PAR" & qqqq[1] == "min"){qqqq <- bquote('Minimum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qqqq[2] == "PAR" & qqqq[1] == "max"){qqqq <- bquote('Maximum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qqqq[2] == "PAR" & qqqq[1] == "median"){qqqq <- bquote('Median PAR (Einsteins' ~ m^-2~day^-1*')')}
}


g4<-g4+theme(text=element_text(size=6, family="Arial")) + xlab(qq)+labs(title="Receptacle")+theme(plot.title = element_text(hjust = 0.5))
g5<-g5+theme(text=element_text(size=6, family="Arial")) + xlab(qqqq)
    
  subV <- subset(V,V$ASVid == unique(all_V$ASVid)[q])
  subVW <- subset(subV,subV$lon < -30)
  subVE <- subset(subV,subV$lon > -30)
  g7<-ggplot()+geom_point(size=0.5,data=subVW,aes(x=subVW[,all_V[q,4]],y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="black",data=subVW,aes(x=subVW[,all_V[q,4]],y=measurement),se=FALSE)  + ylab("")
  g7<-g7 + geom_point(size=0.5,data=subVE,aes(x=subVE[,all_V[q,4]],y=measurement), color = "red")+geom_smooth(method="gam",formula=y~s(x,k=5),color="red",data=subVE,aes(x=subVE[,all_V[q,4]],y=measurement),se=FALSE)+ylim(c(0,max(subV$measurement)))+xlim(c(min(subV[,all_V[q,4]]),max(subV[,all_V[q,4]])))
  g8<-ggplot()+geom_point(size=0.5,data=subVW,aes(x=subVW[,all_V[q,6]],y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="black",data=subVW,aes(x=subVW[,all_V[q,6]],y=measurement),se=FALSE)  + ylab("")
  g8<-g8 + geom_point(size=0.5,data=subVE,aes(x=subVE[,all_V[q,6]],y=measurement), color = "red")+geom_smooth(method="gam",formula=y~s(x,k=5),color="red",data=subVE,aes(x=subVE[,all_V[q,6]],y=measurement),se=FALSE)+ylim(c(0,max(subV$measurement)))+xlim(c(min(subV[,all_V[q,6]]),max(subV[,all_V[q,6]])))
  g9<-ggplot()+geom_point(size=0.5,data=subVW,aes(x=lat,y=measurement))+geom_smooth(method="gam",formula=y~s(x,k=5),color="blue",data=subV,aes(x=lat,y=measurement),se=FALSE) + xlab("Latitude (°)") + ylab("Relative Abundance")
  g9<-g9 + geom_point(size=0.5,data=subVE,aes(x=lat,y=measurement), color = "red")+ylim(c(0,max(subV$measurement)))+xlim(c(min(subV$lat),max(subV$lat)))
  g9 <- g9 + theme(text=element_text(size=6, family="Arial"))
  qq<-unlist(strsplit(as.character(all_V[q,4]),"_"))
  qqqq<-unlist(strsplit(as.character(all_V[q,6]),"_"))
  
if(qq[2]!="PAR"){  
if(qq[1] == "min"){qq[1] <- "Minimum"}
if(qq[1] == "max"){qq[1] <- "Maximum"}
if(qq[1] == "median"){qq[1] <- "Median"}
if(qq[2] == "windBearing"){qq[2] <- "Wind Bearing (°)"}
if(qq[2] == "windSpeed"){qq[2] <- "Wind Speed (m/s)"}
if(qq[2] == "dewPoint"){qq[2] <- "Dew Point (°C)"}
if(qq[2] == "Photoperiod"){qq[2] <- "Photoperiod (h)"}
if(qq[2] == "TidalHeight"){qq[2] <- "Relative Tidal Height"}

if(qq[2] == "temperature"){qq[2] <- "Air Temperature (°C)"}
  if(qq[2] == "SST"){qq[2] <- "SST (°C)"}
if(qq[2] == "humidity"){qq[2] <- "Relative Humidity"}
qq <- paste(qq[1],qq[2])
} else{
if(qq[2] == "PAR" & qq[1] == "min"){qq <- bquote('Minimum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qq[2] == "PAR" & qq[1] == "max"){qq <- bquote('Maximum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qq[2] == "PAR" & qq[1] == "median"){qq <- bquote('Median PAR (Einsteins' ~ m^-2~day^-1*')')}
}
  
if(qqqq[2]!="PAR"){  
if(qqqq[1] == "min"){qqqq[1] <- "Minimum"}
if(qqqq[1] == "max"){qqqq[1] <- "Maximum"}
if(qqqq[1] == "median"){qqqq[1] <- "Median"}
if(qqqq[2] == "windBearing"){qqqq[2] <- "Wind Bearing (°)"}
if(qqqq[2] == "windSpeed"){qqqq[2] <- "Wind Speed (m/s)"}
if(qqqq[2] == "dewPoint"){qqqq[2] <- "Dew Point (°C)"}
if(qqqq[2] == "Photoperiod"){qqqq[2] <- "Photoperiod (h)"}
if(qqqq[2] == "TidalHeight"){qqqq[2] <- "Relative Tidal Height"}
if(qqqq[2] == "windSpeed"){qqqq[2] <- "Wind Speed"}
if(qqqq[2] == "temperature"){qqqq[2] <- "Air Temperature (°C)"}
if(qqqq[2] == "SST"){qqqq[2] <- "SST (°C)"}
if(qqqq[2] == "humidity"){qqqq[2] <- "Relative Humidity"}
qqqq <- paste(qqqq[1],qqqq[2])
}else{
if(qqqq[2] == "PAR" & qqqq[1] == "min"){qqqq <- bquote('Minimum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qqqq[2] == "PAR" & qqqq[1] == "max"){qqqq <- bquote('Maximum PAR (Einsteins' ~ m^-2~day^-1*')')}
if(qqqq[2] == "PAR" & qqqq[1] == "median"){qqqq <- bquote('Median PAR (Einsteins' ~ m^-2~day^-1*')')}
  }
g7<-g7+theme(text=element_text(size=6, family="Arial")) + xlab(qq)+ggtitle("Vegetative")+theme(plot.title = element_text(hjust = 0.5))
g8<-g8+theme(text=element_text(size=6, family="Arial")) + xlab(qqqq)  
  
  
  #print(g1)

  x<-arrangeGrob(g1,g4,g7,g2,g5,g8,g3,g6,g9, ncol=3)
  #print(x)
  ggsave(paste0(unique(all_H$ASVid)[q],".png"),x,width=130,height=130,units="mm")
  
}

