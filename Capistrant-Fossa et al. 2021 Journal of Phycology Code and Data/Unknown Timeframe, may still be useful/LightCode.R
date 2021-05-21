library(reshape2)
library(data.table)
library(tidyverse)

#MyPathCode
os<-.Platform$OS.type
path<-ifelse(os == "unix","~/Desktop/LightData/","C:\\Users\\Bismuth\\Desktop\\GitHub\\")
#paste0(path,"RestOfPath"
.repath <- function(fullpath) {
  h <- gsub("/","\\",fullpath, fixed = TRUE)
}

fullpath <-paste0(path,"2016/")
if(os != "unix"){fullpath <- .repath(fullpath)}


files <- list.files(fullpath)

for(i in 1:length(files)){
  filename=files[i]
  data=read.csv(paste0(fullpath,filename))
  data<-melt(setDT(data), measure.vars = 2:8641, variable.name = "Lon")
  filename<-str_remove(filename, ".csv")
  data<-data[-c(which(data$value=="NaN")),]
  data$Lon<-str_remove(data$Lon, "X.")
  data$Lon<-as.numeric(data$Lon)
  assign(x = filename,value = data)
  rm(data)
}


rm(filename)
rm(files)
rm(fullpath)
rm(i)
rm(os)
rm(path)

Sites<-c("Amorosa","Beaufort","Bodo","Cadiz","Halifax","Lewes","Minehead","Oban","RhodeIsland","Schoodic","Sidmouth","Tas","Uum","Viana","Woods")
lat<-c(41.6429,34.70229,67.289591,36.46783,44.491883,38.790496,51.18323,56.29607,50.675183,41.450748,44.333747,65.613076,70.673201,41.6958,41.515104)
lon<-c(-8.824233,-76.65178,14.397472,-6.25158,-63.912172,-75.163743,-3.38488,-5.6541,-3.2468,-71.356392,-68.058195,-37.630477,-52.133505,-8.85105,-70.655403)

SiteInfo<-data.frame(Sites,lat,lon)
rm(lat)
rm(lon)
rm(Sites)

SiteInfo$June <- NA
SiteInfo$July <- NA                                              
SiteInfo$August <- NA                                              
SiteInfo$September <- NA

for(i in 1:nrow(SiteInfo)){
  June[,paste0(SiteInfo$Sites[i],"Dist")]<-sqrt((June$Lat-SiteInfo$lat[i])^2+(June$Lon-SiteInfo$lon[i])^2)
  July[,paste0(SiteInfo$Sites[i],"Dist")]<-sqrt((July$Lat-SiteInfo$lat[i])^2+(July$Lon-SiteInfo$lon[i])^2)
  August[,paste0(SiteInfo$Sites[i],"Dist")]<-sqrt((August$Lat-SiteInfo$lat[i])^2+(August$Lon-SiteInfo$lon[i])^2)  
  September[,paste0(SiteInfo$Sites[i],"Dist")]<-sqrt((September$Lat-SiteInfo$lat[i])^2+(September$Lon-SiteInfo$lon[i])^2)
}

June <- data.frame(June)
July <- data.frame(July)
August <- data.frame(August)
September <- data.frame(September)

for(i in 4:ncol(August)){
z <- i-3
SiteInfo[z,4]<-June[which.min(June[,i]),3]  
SiteInfo[z,5]<-July[which.min(July[,i]),3]  
SiteInfo[z,6]<-August[which.min(August[,i]),3]  
SiteInfo[z,7]<-September[which.min(September[,i]),3]  
}

write.csv(SiteInfo,"~/Desktop/LightData/2016/SiteInfo2016.csv")
save.image("~/Desktop/LightData/2016/LightData2016.RData")

