install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library(rgeos)
library("ggplot2")
theme_set(theme_bw())
library("sf")  

library("rnaturalearth")
library("rnaturalearthdata")
library("readr")

SiteInfoLats <- read_csv("C:/Users/Laminaria/Desktop/LatsFull.csv")
SiteInfoLats<-SiteInfoLats[-c(1),]
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

SiteInfoLats<-SiteInfoLats[-c(11),]
SiteInfoLats[2,1] <- "Bodø"
SiteInfoLats[3,1] <- "Cádiz"

ggplot(data = world) + geom_sf()

library(ggrepel)
library("ggspatial")
ggplot(data = world) +
    geom_hline(yintercept=55, linetype="dashed")+
    geom_hline(yintercept=40, linetype="dashed")+
    geom_sf() +
    scale_x_continuous(name = "Longitude") +
    scale_y_continuous(name = "Latitude") +
    coord_sf(xlim = c(-77, 17), ylim=c(32,70))+ theme(legend.title = element_blank(),legend.position = "none")+geom_label_repel(force=6,size=2.11,data=SiteInfoLats,aes(x=SiteInfoLats$lon,y=SiteInfoLats$lat,label=SiteInfoLats$Sites),color="darkblue",fontface="bold",segment.size = 0.2)+geom_point(color="darkblue",data=SiteInfoLats,aes(x=SiteInfoLats$lon,y=SiteInfoLats$lat))+theme(plot.title=element_text(size=6,face="bold", family="Arial"),axis.text=element_text(size=6, family="Arial"),axis.title=element_text(size=6, family="Arial"))+
    geom_point(x=-53.5,y=70.5, aes(fill="orange"),pch=25,size=2)+
    geom_point(x=20,y=70.5, aes(fill="orange"),pch=25,size=2)+
    geom_point(x=-76,y=34.7, aes(fill="orange"),pch=24,size=2)+
    geom_point(x=-9.5,y=32, aes(fill="orange"),pch=24,size=2)

ggsave("Map.pdf",width=88,height=88,scale=2,dpi=300,units="mm", device = cairo_pdf)
