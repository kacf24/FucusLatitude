library("rgeos")
library("ggplot2")
library("sf")  
library("rnaturalearth")
library("rnaturalearthdata")
library("ggrepel")
library("ggspatial")
library(readr)

stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                     geom = "boxplot", position = "dodge",
                     ...,
                     qs = c(.05, .25, 0.5, 0.75, 0.95),
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
      data = data,
      mapping = mapping,
      stat = StatBoxplotCustom,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
      na.rm = na.rm,
      qs = qs,
      ...
      )
  )
}

StatBoxplotCustom <- ggproto("StatBoxplotCustom", Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    params$width <- ggplot2:::"%||%"(
      params$width, (resolution(data$x) * 0.75)
    )

    if (is.double(data$x) && !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
      warning(
        "Continuous x aesthetic -- did you forget aes(group=...)?",
        call. = FALSE
      )
    }

    params
  },

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, qs = c(.05, .25, 0.5, 0.75, 0.95)) {

    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
    stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])

    outliers <- (data$y < stats[1]) | (data$y > stats[5])

    if (length(unique(data$x)) > 1)
    width <- diff(range(data$x)) * 0.9

    df <- as.data.frame(as.list(stats))
    df$outliers <- list(data$y[outliers])

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
    df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df
  }
)





`%notin%` <- Negate(`%in%`)


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




Meta <- read_csv("C:/Users/Laminaria/Desktop/FucusPumEnvironmental/FucusPumEnvironmental4918Metadata.csv")
Lats <- read_csv("C:/Users/Laminaria/Desktop/Lats.csv")

for(z in 1:10){
Meta$SampleName<-stringr::str_replace(Meta$SampleName,"\\.","_")
}

y[which(y$SampleName  %notin% Meta$SampleName),]


data<-merge(y,Meta,by="SampleName")
data <- merge(data,Lats,by.x="Site",by.y="Sites")




Fv<-subset(data,Species=="Fv" & Site != "Green")
Fv$Side<-ifelse(Fv$lon > -30,"E","W")

#aggdataHE <-aggregate(FvHE[,which(colnames(FvHE)==mets[j])], by=list(FvHE$lat,FvHE$lon), FUN=mean, na.rm=TRUE)
#aggdataHW <-aggregate(FvHW[,which(colnames(FvHW)==mets[j])], by=list(FvHW$lat,FvHE$lon), FUN=mean, na.rm=TRUE)


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

#ggsave(file=paste0("Pu",tissue[q],"_",mets[j],".pdf"),g, width = 11, height = 8.5, units = "in")


}
}

Fv<-subset(data,Species=="Fv" & Site!="Green")

FvH<-subset(Fv,Tissue==tissue[q])
aggdataHmean <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=mean, na.rm=TRUE)
aggdataHmedian <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=median, na.rm=TRUE)
aggdataHmean$x<-as.numeric(aggdataHmean$x)
aggdataHmedian$x<-as.numeric(aggdataHmedian$x)





library(ggplot2)
library(gridExtra)
library(RColorBrewer)

mets <- c("colSums.ASVs3.","shan","simp","invsimp","pie")
tissue <- c("H","R","V")
xlabnames <- c("Number of ASVs", "Shannon", "Simpson", "InvSimpson", "Pielou's")
world <- ne_countries(scale = "medium", returnclass = "sf")

for(q in 1:length(tissue)){
  
FvH<-subset(Fv,Tissue==tissue[q]&Site!="Green")



for(j in 1:length(mets)){
aggdataHmean <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=mean, na.rm=TRUE)
aggdataHmedian <-aggregate(FvH[,which(colnames(FvH)==mets[j])], by=list(FvH$lat,FvH$lon), FUN=median, na.rm=TRUE)
aggdataHmean$x<-as.numeric(aggdataHmean$x)
aggdataHmedian$x<-as.numeric(aggdataHmedian$x)
  

moo <- merge(aggdataHmedian,Lats,by.x="Group.1",by.y="lat")
moo2<-merge(moo,FvH,by.x="Sites",by.y="Site")

moo2[which(moo2$Sites == "Bodo"),1] <- "Bodø"
moo2[which(moo2$Sites == "Uum"),1] <- "Uumm"
moo2[which(moo2$Sites == "Sid"),1] <- "Sidm"
moo2[which(moo2$Sites == "SCH"),1] <- "Scho"
moo2[which(moo2$Sites == "Viana"),1] <- "Vian"
moo2[which(moo2$Sites == "WH"),1] <- "Wood"
moo2[which(moo2$Sites == "Tagus"),1] <- "Tagu"
moo2[which(moo2$Sites == "Cad"),1] <- "Cadi"



p1<-ggplot(data = world) +
    geom_sf() +
    scale_x_continuous(name = "Longitude") +
    scale_y_continuous(name = "Latitude") +
    ggtitle(paste0("Fv",tissue[q]," ",xlabnames[j]))+
    theme(plot.title = element_text(hjust = 0.5))+
    coord_sf(xlim = c(-77, 17), ylim=c(34,70))+geom_point(data=aggdataHmedian,x=aggdataHmean$Group.2,y=aggdataHmean$Group.1,size=4,aes(color = x))+ scale_color_distiller(palette = "YlOrRd",trans="reverse")+labs(color = paste(xlabnames[j]))
  
p2 <- ggplot(moo2, aes(fill=x,reorder(Sites,lat), moo2[,which(colnames(moo2)==mets[j])])) + scale_fill_distiller(palette = "YlOrRd",trans="reverse")+labs(fill = paste(xlabnames[j]))
p2<-p2 + coord_flip()+ stat_boxplot_custom(qs = c(0, 0.025, 0.5, 0.975, 1))+ylab(c(xlabnames[j]))+xlab("Sites")
  


g <- grid.arrange(p1, p2,ncol=1)

ggsave(file=paste0(xlabnames[j],"_Fv_",tissue[q],".png"),g, height = 11, width = 8.5, units = "in")


}
}


