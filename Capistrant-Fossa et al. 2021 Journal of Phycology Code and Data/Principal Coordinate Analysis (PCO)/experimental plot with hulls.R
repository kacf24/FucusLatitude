PCO <- read_csv("C:/Users/Laminaria/Desktop/PCO.csv")
library(extrafont)
font_import()
loadfonts(device = "win")
loadfonts(device = "postscript")
loadfonts()

library(ggplot2)

#Beau,Bodo,Cadi,Hali,Lewe, Lima, Mine,Newp, Oban, Scho, Sidm, Tagu, Tasi, Torr, Uumm, Vian, Wood
shapePalette <-c(16,16,16,16,15,17,16,16,16,16,1,1,1,1,1,1,1,1,1)
cbPalette <- c("#0072B2","#56B4E9","#009E73","#000000","#000000","#000000","#CC79A7","#F0E442","#E69F00","#D55E00","#0072B2","#56B4E9","#009E73","#000000","#999999","#CC79A7","#F0E442","#E69F00","#D55E00")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette<-colorRampPalette(cbbPalette)(19)
cbPalette<-c("#5FB0CC","#38ABC1","#2BAA70","#77C059","#000000","#583D00","#B27A00","#C29D2D","#95AA80","#17A393","#4F978C","#1E749B","#BBD24F","#ACC360","#5E6862","#AA6227","#D26324","#CE6D65","#CC79A7")


a <- ggplot(PCO, aes(x=PCO1, y=PCO2, col = Site, shape = Tissue)) + geom_point() +
  xlab("PCO 1") + ylab("PCO 2")+
  theme(text=element_text(size=6, family="Arial")) + guides(col=guide_legend(ncol=2),shape=guide_legend(ncol=1)) +
  theme(legend.position="right")+scale_color_manual(values=cbPalette,breaks=c("Uumm","Tasi","Hali","Scho","Scho_Fd","Scho_Fs","Wood","Newp","Lewe","Beau","Bodo","Oban","Mine","Sidm","Vian","Lima","Torr","Tagu","Cadi"))
a <- a + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(legend.background=element_blank())

a
library(plyr)
chulls <- ddply(PCO, .(Tissue), function(df) df[chull(df$PCO1, df$PCO2), ])

b <- a + geom_polygon(data=chulls, aes(x=PCO1, y=PCO2, group=Tissue,fill=Tissue),color="black",alpha=0.075)+scale_fill_manual(values=c("black","blue","orange"))
b

ggsave("PCOTissueSquare.pdf",width=119,height=119,dpi=300,units="mm", device = cairo_pdf)
