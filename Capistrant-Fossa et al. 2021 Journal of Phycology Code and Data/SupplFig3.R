library(readxl)
library(readr)
library(ggplot2)
library(extrafont)
loadfonts(device = "win")
loadfonts(device = "postscript")
loadfonts()
PCO <- read_excel("C:/Users/Rocky/Desktop/PCOnewSuppl.xlsx")
Meta <- read_csv("C:/Users/Rocky/Desktop/Capistrant-Fossa et al. 2021 Journal of Phycology Code and Data/Rarified Data Sets/FucusPumEnvironmental4918Metadata.csv")
PCOMeta<-merge(PCO,Meta,by="SampleName")
PCOMeta<-PCOMeta[-c(which(PCOMeta$Site=="Green")),]
PCOMeta<-PCOMeta[-c(which(PCOMeta$Site=="Avei")),]

PCOMeta$TissueSite<-paste0(PCOMeta$Tissue,PCOMeta$Site)

PCOMeta[PCOMeta$Tissue=="filter","Tissue"] <- "WC"

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
a<-ggplot(PCOMeta, aes(x=`Axis 1`, y=`Axis 2`, col = Tissue)) + geom_point() +
  xlab("PCO 1") + ylab("PCO 2")+ theme_classic()+scale_color_manual(values=cbPalette) +  theme(text=element_text(size=10, family="Arial")) + labs(tag = "A") +
  theme(legend.position = c(1, 0),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(1, 1, 1, 1)) + ylab("PCO2 [17%]") + xlab("PCO1 [9%]")
a

PCOMeta[c(which(PCOMeta$Site=="Bodo")),"Site"] <- "Bodø"
shapePalette <-c(16,16,16,16,16,16,16,1,1,1,1,1,1,1,1,1)
cbPalette <- c("#0072B2","#009E73","#000000","#CC79A7","#F0E442","#E69F00","#D55E00","#0072B2","#56B4E9","#009E73","#000000","#999999","#CC79A7","#F0E442","#E69F00","#D55E00")
rightLabs<- c("Uumm","Hali","Scho","Wood","Newp","Lewe","Beau","Bodø","Oban","Mine","Sidm","Vian","Lima","Torr","Tagu","Cadi")
b<-ggplot(PCOMeta, aes(x=`Axis 1`, y=`Axis 2`, col = Site, shape = Site)) + geom_point(lwd=2, stroke=2) +  theme(text=element_text(size=10, family="Arial")) + labs(tag = "B") +
  xlab("PCO1 [9%]") + ylab("PCO2 [17%]")+ theme_classic() + guides(col=guide_legend(ncol=2)) + scale_color_manual(values=cbPalette,breaks=c("Uum","Hali","SCH","WH","Newp","Lew","Beau","Bodø","Oban","Mine","Sid","Viana","Lima","Torr","Tagus","Cad"), labels=rightLabs) +scale_shape_manual(values=shapePalette, breaks=c("Uum","Hali","SCH","WH","Newp","Lew","Beau","Bodø","Oban","Mine","Sid","Viana","Lima","Torr","Tagus","Cad"),labels = rightLabs) + labs(color = "Site",shape="Site")
  
b
library(gridExtra)
x<-grid.arrange(a, b , ncol=1)
ggsave("testsave2.png",x,width=7.5,height=8,units="in")
