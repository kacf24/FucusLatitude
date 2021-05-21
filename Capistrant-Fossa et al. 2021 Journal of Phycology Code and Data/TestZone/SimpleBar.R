#Load in required packages
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

`%notin%` <- Negate(`%in%`)

setwd("C:/Users/Rocky/Desktop/Capistrant-Fossa et al. 2021 Journal of Phycology Code and Data/Rarified Data Sets")
Meta <- read_csv("FucusPumEnvironmental4918Metadata.csv")
ASVs <- read_csv("tFucusPumEnvironmental4918.csv")
Tax <- read_csv("FucusPumEnvironmental4918Taxonomy.csv")

data_long <- gather(ASVs, ASVid, measurement, colnames(ASVs[,2:1781]), factor_key=TRUE)
data_long_met<-merge(data_long,Meta,by="SampleName")
data_long_met_tax<-merge(data_long_met,Tax,by="ASVid")
Fv<-subset(data_long_met_tax,data_long_met_tax$Species=="Fv")

FvAvg<-aggregate(x = Fv$measurement, by = list(Fv$genus), FUN = "sum")
FvAvg$Avg <- FvAvg$x/sum(FvAvg$x)


FvAvg <- FvAvg[-c(which(FvAvg$Avg < 0.02)),]
Fv[(which(Fv$genus %notin% FvAvg$Group.1 == TRUE)),"genus"] <- "Other"
Fv[(which(Fv$genus  == "Bacteria_ph_cl_or_fa_ge")),"genus"] <- "Unknown"

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73","gray87" ,"#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")


Fv[c(which(Fv$genus=="Alphaproteobacteria_or_fa_ge")),"genus"] <- "unk_Alpha"
Fv[c(which(Fv$genus=="BurkholderiaCaballeroniaParaburkholderia")),"genus"] <- "BCP"


ggplot(Fv,aes(fill=genus,y=measurement,x=Tissue)) + geom_bar(position="fill",stat="identity")+scale_fill_manual(values=cbbPalette, name = "Genus")+theme_classic()+ylab("Proportion") + theme(text=element_text(size=10, family="Arial")) + theme(legend.text = element_text(face = "italic"))

ggsave("Figure4.png",width = 3.5,units = "in")
