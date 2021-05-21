library(readr)
library(ggplot2)
PCO <- read_csv("C:/Users/Rocky/Desktop/Capistrant-Fossa et al. 2021 Journal of Phycology Code and Data/Principal Coordinate Analysis (PCO)/PCO.csv")
library(extrafont)
#font_import()
loadfonts(device = "win")
loadfonts(device = "postscript")
loadfonts()
library(plotly)

PCO<-PCO[-c(which(PCO$Site=="Tasi")),]
PCO<-PCO[-c(which(PCO$Species=="Fd")),]
PCO<-PCO[-c(which(PCO$Species=="Fs")),]

PCO[c(which(PCO$Site=="Bodo")),"Site"] <- "Bodø"
PCO[c(which(PCO$long=="e")),"long"] <- "E"
PCO[c(which(PCO$long=="w")),"long"] <- "W"

d <- ggplot(PCO, aes(x=PCO1, y=PCO2, shape = Tissue, color = Tissue)) + geom_point() + theme_classic() + scale_color_manual(values=c("gray","gray0","gray50"))+scale_shape_manual(values=c(16,15,17)) +  
  theme(legend.position = c(1, 0),
  legend.justification = c("right", "bottom"),
  legend.box.just = "right",
  legend.margin = margin(1, 1, 1, 1)) + theme(text=element_text(size=10, family="Arial")) + labs(tag = "d") + xlab("PCO1 [17%]") + ylab("PCO2 [8%]")
d
a <- ggplot(PCO, aes(x=PCO1, y=PCO2, shape = lat,color=lat)) + geom_point() + theme_classic() + scale_color_manual(values=c("gray0","gray","gray50"))+scale_shape_manual(values=c(16,15,17)) +  
  theme(legend.position = c(1, 0),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1)) + theme(text=element_text(size=10, family="Arial")) + labs(color = "Region",shape="Region") + labs(tag = "a") + xlab("PCO1 [17%]") + ylab("PCO2 [8%]")
a
b <- ggplot(PCO, aes(x=PCO1, y=PCO2, shape = long,color=long)) + geom_point() + theme_classic() + scale_color_manual(values=c("gray0","gray","gray87"))+scale_shape_manual(values=c(16,15,17)) +  
  theme(legend.position = c(1, 0),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right", 
        legend.margin = margin(1, 1, 1, 1)) + theme(text=element_text(size=10, family="Arial")) + labs(color = "Side",shape="Side") + labs(tag = "b") + xlab("PCO1 [17%]") + ylab("PCO2 [8%]")
b







#######################Make Panel D
dismatrix_all <- data.frame(read_csv("C:/Users/Rocky/Desktop/dismatrix_all.csv"))
Meta <- read_csv("C:/Users/Rocky/Desktop/Capistrant-Fossa et al. 2021 Journal of Phycology Code and Data/Rarified Data Sets/FucusPumEnvironmental4918Metadata.csv")
Lats <- read_csv("C:/Users/Rocky/Desktop/Capistrant-Fossa et al. 2021 Journal of Phycology Code and Data/Supplemental Data/Lats.csv")

Meta<-merge(Meta,Lats,by.x="Site",by.y="Sites")


row.names(dismatrix_all) <- dismatrix_all$X1
dismatrix_all<-dismatrix_all[,-c(1)]
dismatrix_all[upper.tri(dismatrix_all)] <- NA
library(tidyr)
dismatrix_all$Site1<-rownames(dismatrix_all) 
data_long <- gather(dismatrix_all, Site2, Similarity,colnames(dismatrix_all)[1:865], factor_key=TRUE)


longNoSame<-subset(data_long,data_long$Site1!=data_long$Site2)

longNoSame<-subset(longNoSame,is.na(longNoSame$Similarity)==FALSE)

Site1Meta<-merge(longNoSame,Meta[,c(1,2,13,14)],by.x="Site1",by.y="SampleName")
colnames(Site1Meta)[5:6] <- c("Lat1","Lon1")
Site2Meta<-merge(Site1Meta,Meta[,c(1,2,13,14)],by.x="Site2",by.y="SampleName")
colnames(Site2Meta)[8:9] <- c("Lat2","Lon2")

library(geosphere)

Site2Meta$geoDistKM <- NA
for(q in 1:nrow(Site2Meta)){
  Site2Meta$geoDistKM[q]<-distHaversine(p1=c(Site2Meta$Lon1[q],Site2Meta$Lat1[q]),p2=c(Site2Meta$Lon2[q],Site2Meta$Lat2[q]))/1000
}

Site2Meta$Side1<-ifelse(Site2Meta$Lon1 < -30,"West","East")
Site2Meta$Side2<-ifelse(Site2Meta$Lon2 < -30,"West","East")
Site2Meta<-subset(Site2Meta,Site2Meta$Site.x != "Green")
Site2Meta<-subset(Site2Meta,Site2Meta$Site.y != "Green")

Site2Meta$SameSide <- Site2Meta$Side1 == Site2Meta$Side2

library(ggplot2)

Site2Meta[c(which(Site2Meta$SameSide=="TRUE")),"SameSide"] <- "Same"
Site2Meta[c(which(Site2Meta$SameSide=="FALSE")),"SameSide"] <- "Opposite"


c <- ggplot(Site2Meta, aes(x=geoDistKM, y=Similarity, color=SameSide)) + geom_point() + theme_classic() + scale_color_manual(values = c("gray0","gray")) + xlab("Geographic Distance (km)") +
theme(legend.position = c(1, 1),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(1, 1, 1, 1)) + theme(text=element_text(size=10, family="Arial")) + labs(tag = "c") + labs(color = "Atlantic Side") 






library(gridExtra)

x<-grid.arrange(a, b , c, d, ncol=2)

ggsave("testsave.png",x,width=8,height=6.5,units="in")







