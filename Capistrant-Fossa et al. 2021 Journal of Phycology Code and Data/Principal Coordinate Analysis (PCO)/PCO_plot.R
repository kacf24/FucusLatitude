library(readr)
library(ggplot2)
PCO <- read_csv("C:/Users/Laminaria/Desktop/PCO.csv")
library(extrafont)
loadfonts(device = "win")
loadfonts(device = "postscript")
loadfonts()
library(plotly)

PCO<-PCO[-c(which(PCO$Site=="Tasi")),]
PCO<-PCO[-c(which(PCO$Species=="Fd")),]
PCO<-PCO[-c(which(PCO$Species=="Fs")),]
shapePalette <-c(16,16,16,16,16,16,16,1,1,1,1,1,1,1,1,1)
cbPalette <- c("#0072B2","#009E73","#000000","#CC79A7","#F0E442","#E69F00","#D55E00","#0072B2","#56B4E9","#009E73","#000000","#999999","#CC79A7","#F0E442","#E69F00","#D55E00")
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette<-colorRampPalette(cbbPalette)(19)
#cbPalette<-c("#5FB0CC","#38ABC1","#2BAA70","#77C059","#000000","#583D00","#B27A00","#C29D2D","#95AA80","#17A393","#4F978C","#1E749B","#BBD24F","#ACC360","#5E6862","#AA6227","#D26324","#CE6D65","#CC79A7")
PCO$TissueSite <- paste0(PCO$Site,PCO$Tissue)

PCO$Shape <- 1
PCO$Order <- 2

#+scale_color_manual(values=cbPalette,breaks=c("Uumm","Hali","Scho","Scho_Fd","Scho_Fs","Wood","Newp","Lewe","Beau","Bodo","Oban","Mine","Sidm","Vian","Lima","Torr","Tagu","Cadi"))+scale_shape_manual(values = PCO$Shape)
for(q in 1:nrow(PCO)){
  if(PCO$long[q] == "e" & PCO$Tissue[q] == "H"){PCO$Shape[q] <- 1}
  if(PCO$long[q] == "w" & PCO$Tissue[q] == "H"){PCO$Shape[q] <- 16}
  if(PCO$long[q] == "e" & PCO$Tissue[q] == "R"){PCO$Shape[q] <- 0}
  if(PCO$long[q] == "w" & PCO$Tissue[q] == "R"){PCO$Shape[q] <- 15}
  if(PCO$long[q] == "e" & PCO$Tissue[q] == "V"){PCO$Shape[q] <- 2}
  if(PCO$long[q] == "w" & PCO$Tissue[q] == "V"){PCO$Shape[q] <- 17}
#  if(PCO$Species[q] == "Fd"){PCO$Shape[q] <- 3}
#  if(PCO$Species[q] == "Fs"){PCO$Shape[q] <- 4}
  
}



PCO[c(which(PCO$Site=="Bodo")),"Site"] <- "Bodø"

a <- ggplot(PCO, aes(x=PCO1, y=PCO2, col = Site, shape =TissueSite)) + geom_point() +
  xlab("PCO 1") + ylab("PCO 2")+
  theme(text=element_text(size=6, family="Arial")) + guides(col=guide_legend(ncol=2)) +
  theme(legend.position="right")+scale_color_manual(values=cbPalette,breaks=c("Uumm","Hali","Scho","Wood","Newp","Lewe","Beau","Bodø","Oban","Mine","Sidm","Vian","Lima","Torr","Tagu","Cadi"))+scale_shape_manual(values = PCO$Shape,breaks=PCO$TissueSite,guide="none")

a <- a + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.background=element_blank())
a

ggplotly(a)
ggsave("PCOTissueSquare2RAW.pdf",width=119*1.5,height=119,dpi=300,units="mm", device = cairo_pdf)
