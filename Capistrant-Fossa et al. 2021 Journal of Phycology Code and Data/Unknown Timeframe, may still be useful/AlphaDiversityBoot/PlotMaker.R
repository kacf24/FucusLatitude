library("rgeos")
library("ggplot2")
library("sf")  
library("rnaturalearth")
library("rnaturalearthdata")
library("ggrepel")
library("ggspatial")



x<-data.frame(matrix(ncol = 6,nrow=1000*1311))
y<-data.frame(unlist(simResults))
y<-data.frame(simResults[[1]])

library(dplyr)

for(q in 1:length(simResults)){
  simResults[[q]][,"SampleName"]<-rownames(simResults[[q]])
  
}

y<-bind_rows(simResults, .id = "column_label")[,-c(1)]

for(z in 1:10){
y$SampleName<-stringr::str_replace(y$SampleName,"\\.","_")
}




Meta <- read_csv("~/Desktop/TransatlanticData/Files/FucusPumEnvironmental/FucusPumEnvironmental4918Metadata.csv")
Lats <- read_csv("~/Desktop/TransatlanticData/AlphaDiversityBoot/Lats.csv")

for(z in 1:10){
Meta$SampleName<-stringr::str_replace(Meta$SampleName,"\\.","_")
}




data<-merge(y,Meta,by="SampleName")
data <- merge(data,Lats,by.x="Site",by.y="Sites")

Fv<-subset(data,Species=="Fv")
Fv$Side<-ifelse(Fv$lon > -30,"E","W")

aggdataHE <-aggregate(FvHE[,which(colnames(FvHE)==mets[j])], by=list(FvHE$lat,FvHE$lon), FUN=mean, na.rm=TRUE)
aggdataHW <-aggregate(FvHW[,which(colnames(FvHW)==mets[j])], by=list(FvHW$lat,FvHE$lon), FUN=mean, na.rm=TRUE)


library(ggplot2)
library(gridExtra)

mets <- c("colSums.ASVs3.","shan","simp","invsimp","pie")
tissue <- c("H","R","V")
xlabnames <- c("Number of ASVs", "Shannon", "Simpson", "InvSimpson", "Pielou's")
for(q in 1:length(tissue)){
  
FvHE<-subset(Fv,Tissue==tissue[q]&Side=="E")
FvHW<-subset(Fv,Tissue==tissue[q]&Side=="W")

for(j in 1:length(mets)){
p <- ggplot(FvHE, aes(reorder(Site,lat), FvHE[,which(colnames(FvHE)==mets[j])]))
p<-p + coord_flip()+ geom_boxplot()+ylab(c(xlabnames[j]))+xlab("Sites")
p



p2 <- ggplot(FvHW, aes(reorder(Site,lat), FvHW[,which(colnames(FvHW)==mets[j])]))
p2<-p2 + coord_flip()+geom_boxplot()+ylab(c(xlabnames[j]))+xlab("Sites")

g <- grid.arrange(p2, p,ncol=2)

ggsave(file=paste0("Pu",tissue[q],"_",mets[j],".pdf"),g, width = 11, height = 8.5, units = "in")


}
}

Fv<-subset(data,Species=="Fv")

FvH<-subset(Fv,Tissue==tissue[q])
aggdataHmean <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=mean, na.rm=TRUE)
aggdataHmedian <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=median, na.rm=TRUE)
aggdataHmean$x<-as.numeric(aggdataHmean$x)
aggdataHmedian$x<-as.numeric(aggdataHmedian$x)





library(ggplot2)
library(gridExtra)

mets <- c("colSums.ASVs3.","shan","simp","invsimp","pie")
tissue <- c("H","R","V")
xlabnames <- c("Number of ASVs", "Shannon", "Simpson", "InvSimpson", "Pielou's")
world <- ne_countries(scale = "medium", returnclass = "sf")

for(q in 1:length(tissue)){
  
FvH<-subset(Fv,Tissue==tissue[q])



for(j in 1:length(mets)){
aggdataHmean <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=mean, na.rm=TRUE)
aggdataHmedian <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=median, na.rm=TRUE)
aggdataHmean$x<-as.numeric(aggdataHmean$x)
aggdataHmedian$x<-as.numeric(aggdataHmedian$x)
  


p1<-ggplot(data = world) +
    geom_sf() +
    scale_x_continuous(name = "Longitude") +
    scale_y_continuous(name = "Latitude") +
    coord_sf(xlim = c(-77, 17), ylim=c(34,70))+geom_point(data=aggdataHmean,x=aggdataHmean$Group.2,y=aggdataHmean$Group.1,size=4,aes(color = x))+ scale_color_distiller(palette = "Spectral")+labs(color = paste("Mean",xlabnames[j]))
  
p2<-ggplot(data = world) +
    geom_sf() +
    scale_x_continuous(name = "Longitude") +
    scale_y_continuous(name = "Latitude") +
    coord_sf(xlim = c(-77, 17), ylim=c(34,70))+geom_point(data=aggdataHmedian,x=aggdataHmedian$Group.2,y=aggdataHmedian$Group.1,size=4,aes(color = x))+ scale_color_distiller(palette = "Spectral") +labs(color = paste("Median",xlabnames[j]))
  


g <- grid.arrange(p1, p2,ncol=1)

ggsave(file=paste0("Fv",tissue[q],"_",mets[j],".pdf"),g, height = 11, width = 8.5, units = "in")


}
}


