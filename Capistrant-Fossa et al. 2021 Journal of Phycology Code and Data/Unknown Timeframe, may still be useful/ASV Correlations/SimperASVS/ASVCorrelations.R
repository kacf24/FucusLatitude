library(readr)
library(lubridate)
library(ggvegan)
library(dplyr)
library(StreamMetabolism)
library(MASS)
library(leaps)
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}


setwd("~/Desktop/TrueDarkSkyData/Hourly")

SummerFucusCollectionDays <- data.frame(read_csv("~/Desktop/SummerFucusCollectionDays.csv"))
SummerFucusCollectionDays$Date<-mdy(SummerFucusCollectionDays$Date)

PAR <- data.frame(read_csv("~/Desktop/SateliteData/AllPAR.csv",col_types = cols(Date = col_date(format = "%m/%d/%y"))))
SST <- data.frame(read_csv("~/Desktop/SateliteData/AllSST.csv",col_types = cols(Date = col_date(format = "%m/%d/%y"))))
Tides <- data.frame(read_csv("~/Desktop/SateliteData/Tides.csv", col_types = cols(DateTime = col_datetime(format = "%m/%d/%y %H:%M"))))
Lats <- read_csv("~/Desktop/TransatlanticData/Files/Lats.csv")

Tides$Date<-date(Tides$DateTime)
TidesProp <- Tides

for(q in 2:19){
  max <- max(TidesProp[,q])
  min <- min(TidesProp[,q])
  TidesProp[,q] <- (TidesProp[,q] - min)/(max+abs(min))
}

sunRise <- data.frame(matrix(nrow=1096,ncol=19))
sunSet <- data.frame(matrix(nrow=1096,ncol=19))
names(sunRise) <- names(PAR)
sunRise$Date <- PAR$Date
names(sunSet) <- names(PAR)
sunSet$Date <- PAR$Date
for(q in 1:18){
lat <- Lats$lat[q]
long <- Lats$lon[q]  
sun<-sunrise.set(lat=lat,long = long, date=sunRise$Date[1]+1, timezone=Lats$Tz[q],num.days = 1096)  
sunRise[,q+1] <- sun$sunrise
sunSet[,q+1] <- sun$sunset
}


detach(package:StreamMetabolism, unload = TRUE)
detach(package:chron, unload = TRUE)


hourlynames<-list.files()
for(i in 1:length(hourlynames)){
tempHolder <- data.frame(read_csv(hourlynames[i], col_types = cols(pressure = col_double(),time = col_datetime(format = "%Y-%m-%d %H:%M:%S"))))
tempHolder <- tempHolder[,-c(5)]
tempHolder$Date <- date(tempHolder$time)
newName <- as.character(strsplit(hourlynames[i],".csv")[1])
assign(newName,tempHolder)
}





computeStats<-function(site_hourly=NA,site=NA,sampleDays=NA,PAR=PAR,SST=SST,Tides = Tides,sunRise,sunSet){
site_coll<-subset(sampleDays,Location==site)
nDays<-nrow(site_coll)
selectorSST<-which(colnames(SST)==site)
subSST <- SST[,c(1,selectorSST)]
selectorPAR<-which(colnames(PAR)==site)
subPAR <- PAR[,c(1,selectorPAR)]
selectorTides<-which(colnames(Tides)==site)
subTides<-Tides[,c(20,selectorTides)]
selectorSunSet<-which(colnames(sunSet)==site)
subSet<-sunSet[,c(1,selectorSunSet)]
selectorSunRise<-which(colnames(sunRise)==site)
subRise<-sunRise[,c(1,selectorSunRise)]


TidesProp <- Tides
for(q in 2:19){
  max <- max(TidesProp[,q])
  min <- min(TidesProp[,q])
  TidesProp[,q] <- (TidesProp[,q] - min)/(max+abs(min))
}
subTidesProp<-TidesProp[,c(20,selectorTides)]


singleDay_stats <- data.frame(matrix(ncol = 0, nrow = (nDays)))
weekly_stats <- data.frame(matrix(ncol = 0, nrow = (nDays)))
for (sampleDayCounter in 1:nDays) {
  weekCounter <- 0
  for (z in 0:14) {
    tempDay <- site_hourly[which(site_hourly$Date == site_coll$Date[sampleDayCounter] - days(z)),]
    tempPAR <- subPAR[which(subPAR$Date == site_coll$Date[sampleDayCounter] - days(z)),]
    tempSST <- subSST[which(subSST$Date == site_coll$Date[sampleDayCounter] - days(z)),]
    tempTides <- subTides[which(subTides$Date == site_coll$Date[sampleDayCounter] - days(z)),]
    tempTidesProp <- subTidesProp[which(subTidesProp$Date == site_coll$Date[sampleDayCounter] - days(z)),]
    tempSunRise <- subRise[which(subRise$Date == site_coll$Date[sampleDayCounter] - z),]
    tempSunSet <- subSet[which(subSet$Date == site_coll$Date[sampleDayCounter] - z),]
    
    for (varCounter in 2:(ncol(tempDay) - 1)) {
      var <- names(tempDay)[varCounter]
      singleDay_stats[sampleDayCounter, paste0("median_", var, "_", z, "DaysBefore")] <- median(tempDay[, varCounter], na.rm = TRUE)
      singleDay_stats[sampleDayCounter, paste0("max_", var, "_", z, "DaysBefore")] <- max(tempDay[, varCounter], na.rm = TRUE)
      singleDay_stats[sampleDayCounter, paste0("min_", var, "_", z, "DaysBefore")] <- min(tempDay[, varCounter], na.rm = TRUE)
      singleDay_stats[sampleDayCounter, paste0("PAR_", z, "DaysBefore")] <- tempPAR[1, 2]
      singleDay_stats[sampleDayCounter, paste0("SST_", z, "DaysBefore")] <- tempSST[1, 2]
      singleDay_stats[sampleDayCounter, paste0("max_TidalHeight_", z, "DaysBefore")] <- max(tempTides[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("min_TidalHeight_", z, "DaysBefore")] <- min(tempTides[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("range_TidalHeight_", z, "DaysBefore")] <- max(tempTides[, 2],na.rm=TRUE)-min(tempTides[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("median_TidalHeight_", z, "DaysBefore")] <- median(tempTides[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("max_TidalProp_", z, "DaysBefore")] <- max(tempTidesProp[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("min_TidalProp_", z, "DaysBefore")] <- min(tempTidesProp[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("range_TidalProp_", z, "DaysBefore")] <- max(tempTidesProp[, 2],na.rm=TRUE)-min(tempTidesProp[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("median_TidalProp_", z, "DaysBefore")] <- median(tempTidesProp[, 2],na.rm=TRUE)
      singleDay_stats[sampleDayCounter, paste0("Photoperiod_", z, "DaysBefore")] <- ifelse(is.na(difftime(tempSunSet[, 2],tempSunRise[, 2]))==TRUE,24,difftime(tempSunSet[, 2],tempSunRise[, 2]))
     
      
    }
  }
  for (nWeekPrior in 1:8) {

    tempWeek <- site_hourly[which(site_hourly$Date <= site_coll$Date[sampleDayCounter] - days(1 + (7 * weekCounter)) & site_hourly$Date >= site_coll$Date[sampleDayCounter] - days(7 + (7 * weekCounter))),]
    tempPAR <- subPAR[which(subPAR$Date <= site_coll$Date[sampleDayCounter] - days(1 + (7 * weekCounter)) & subPAR$Date >= site_coll$Date[sampleDayCounter] - days(7 + (7 * weekCounter))),]
    tempSST <- subSST[which(subSST$Date <= site_coll$Date[sampleDayCounter] - days(1 + (7 * weekCounter)) & subSST$Date >= site_coll$Date[sampleDayCounter] - days(7 + (7 * weekCounter))),]
    tempTides <- subTides[which(subTides$Date <= site_coll$Date[sampleDayCounter] - days(1 + (7 * weekCounter)) & subTides$Date >= site_coll$Date[sampleDayCounter] - days(7 + (7 * weekCounter))),]
    tempTidesProp <- subTidesProp[which(subTidesProp$Date <= site_coll$Date[sampleDayCounter] - days(1 + (7 * weekCounter)) & subTidesProp$Date >= site_coll$Date[sampleDayCounter] - days(7 + (7 * weekCounter))),]
    
    for (varCounter in 2:(ncol(tempWeek) - 1)) {
      var <- names(tempWeek)[varCounter]
      weekly_stats[sampleDayCounter, paste0("median_", var, "_", nWeekPrior, "WeeksBefore")] <- median(tempWeek[, varCounter], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("max_", var, "_", nWeekPrior, "WeeksBefore")] <- max(tempWeek[, varCounter], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("min_", var, "_", nWeekPrior, "WeeksBefore")] <- min(tempWeek[, varCounter], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("median_PAR_", nWeekPrior, "WeeksBefore")] <- median(tempPAR[, 2], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("max_PAR_", nWeekPrior, "WeeksBefore")] <- max(tempPAR[, 2], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("min_PAR_", nWeekPrior, "WeeksBefore")] <- min(tempPAR[, 2], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("median_SST_", nWeekPrior, "WeeksBefore")] <- median(tempSST[, 2], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("max_SST_", nWeekPrior, "WeeksBefore")] <- max(tempSST[, 2], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("min_SST_", nWeekPrior, "WeeksBefore")] <- min(tempSST[, 2], na.rm = TRUE)
      weekly_stats[sampleDayCounter, paste0("median_TidalHeight_", nWeekPrior, "WeeksBefore")] <- median(tempTides[, 2],na.rm=TRUE)
      weekly_stats[sampleDayCounter, paste0("median_TidalProp_", nWeekPrior, "WeeksBefore")] <- median(tempTidesProp[, 2],na.rm=TRUE)

    }

    weekCounter <- weekCounter + 1
  }
}














returndf<-cbind(site_coll$Location,site_coll$Year,site_coll$Day,site_coll$Date,singleDay_stats)
return(returndf)

}


BEAU<-computeStats(site_hourly=Beaufort_hourly,site="Beau",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
BODO<-computeStats(site_hourly=Bodo_hourly,site="Bodo",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
CAD<-computeStats(site_hourly=Cadiz_hourly,site="Cad",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
TAS<-computeStats(site_hourly=Tas_hourly,site="Green",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
HALI<-computeStats(site_hourly=Halifax_hourly,site="Hali",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
LEW<-computeStats(site_hourly=Lewes_hourly,site="Lew",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
LIMA<-computeStats(site_hourly=Lima_hourly,site="Lima",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
Mine<-computeStats(site_hourly=Minehead_hourly,site="Mine",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
Newp<-computeStats(site_hourly=Newport_hourly,site="Newp",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
Oban<-computeStats(site_hourly=Oban_hourly,site="Oban",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
SCH<-computeStats(site_hourly=Schoodic_hourly,site="SCH",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
SID<-computeStats(site_hourly=Sidmouth_hourly,site="Sid",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
Tagus<-computeStats(site_hourly=Tagus_hourly,site="Tagus",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
Torr<-computeStats(site_hourly=Torriera_hourly,site="Torr",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
UUM<-computeStats(site_hourly=Uum_hourly,site="Uum",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
VIANA<-computeStats(site_hourly=Viana_hourly,site="Viana",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)
WH<-computeStats(site_hourly=WoodsHole_hourly,site="WH",sampleDays =SummerFucusCollectionDays,PAR,SST,Tides,sunRise,sunSet)





Env<-rbind(BEAU,BODO,CAD,TAS,HALI,LEW,LIMA,Mine,Newp,Oban,SCH,SID,Tagus,Torr,UUM,VIANA,WH)





#Load in required packages
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


Meta<- read_csv("~/Desktop/TransatlanticData/Files/FucusPumEnvironmental/FucusPumEnvironmental4918Metadata.csv")
ASVs <- read_csv("~/Desktop/TransatlanticData/Files/FucusPumEnvironmental/FucusPumEnvironmental4918.csv")


ASVs <- subset(ASVs,ASVid=="t10392"|ASVid=="t3260"|ASVid=="t3536"|ASVid=="t8371"|ASVid=="t10317"|ASVid=="t12214"|ASVid=="t13307"|ASVid=="t14585"|ASVid=="t5033"|ASVid=="t8053"|ASVid=="t8372")
#ASVs<-ASVs[which(rowSums(ASVs[,2:1306]) > 1000),]


setwd("~/Desktop/TransatlanticData/ASV Correlations/")
#Coverts data into a long format for easy subsetting
data_long <- gather(ASVs, SampleName, measurement, colnames(ASVs[, 2:1306]), factor_key = FALSE)

data_long_met <- merge(data_long, Meta, by = "SampleName")

#####
###North
#####


Env_anal_all<-merge(data_long_met,Env,by.y = c("site_coll$Location","site_coll$Year","site_coll$Day"),by.x=c("Site","Year","Day"))
Env_anal_all <- subset(Env_anal_all,Species=="Fv")
Env_anal_all <- subset(Env_anal_all,Site=="Uum"|Site=="Green"|Site=="Bodo"|Site=="Oban")



Env_anal <- Env_anal_all
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared

}
y <- y[order(y$ASVid),]
write.csv(y,"all_N.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="V")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_N_V.csv",row.names = FALSE)


Env_anal <- subset(Env_anal_all,Tissue=="H")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_N_H.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="R")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_N_R.csv",row.names = FALSE)

#####
###SOUTH
#####

Env_anal_all<-merge(data_long_met,Env,by.y = c("site_coll$Location","site_coll$Year","site_coll$Day"),by.x=c("Site","Year","Day"))
Env_anal_all <- subset(Env_anal_all,Species=="Fv")
Env_anal_all <- subset(Env_anal_all,Site=="Cad"|Site=="Tagus"|Site=="Lew"|Site=="Beau")



Env_anal <- Env_anal_all
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_S.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="V")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_S_V.csv",row.names = FALSE)


Env_anal <- subset(Env_anal_all,Tissue=="H")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_S_H.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="R")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_S_R.csv",row.names = FALSE)


#####
###MID
#####


Env_anal_all<-merge(data_long_met,Env,by.y = c("site_coll$Location","site_coll$Year","site_coll$Day"),by.x=c("Site","Year","Day"))
Env_anal_all <- subset(Env_anal_all,Species=="Fv")
Env_anal_all <- subset(Env_anal_all,Site=="Hali"|Site=="SCH"|Site=="WH"|Site=="Newp"|Site=="Mine"|Site=="Sid"|Site=="Amo"|Site=="Viana"|Site=="Lima"|Site=="Torr")



Env_anal <- Env_anal_all
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_M.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="V")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_M_V.csv",row.names = FALSE)


Env_anal <- subset(Env_anal_all,Tissue=="H")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_M_H.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="R")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_M_R.csv",row.names = FALSE)

#####
###All
#####


Env_anal_all<-merge(data_long_met,Env,by.y = c("site_coll$Location","site_coll$Year","site_coll$Day"),by.x=c("Site","Year","Day"))
Env_anal_all <- subset(Env_anal_all,Species=="Fv")




Env_anal <- Env_anal_all
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="V")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_V.csv",row.names = FALSE)


Env_anal <- subset(Env_anal_all,Tissue=="H")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_H.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="R")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_R.csv",row.names = FALSE)

#####
###East
#####


Env_anal_all<-merge(data_long_met,Env,by.y = c("site_coll$Location","site_coll$Year","site_coll$Day"),by.x=c("Site","Year","Day"))
Env_anal_all <- subset(Env_anal_all,Species=="Fv")
Env_anal_all <- subset(Env_anal_all,Site=="Bodo"|Site=="Oban"|Site=="Mine"|Site=="Sid"|Site=="Amo"|Site=="Viana"|Site=="Lima"|Site=="Torr"|Site=="Tagus"|Site=="Cad")



Env_anal <- Env_anal_all
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared

}
y <- y[order(y$ASVid),]
write.csv(y,"all_E.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="V")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_E_V.csv",row.names = FALSE)


Env_anal <- subset(Env_anal_all,Tissue=="H")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_E_H.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="R")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_E_R.csv",row.names = FALSE)

#####
###West
#####


Env_anal_all<-merge(data_long_met,Env,by.y = c("site_coll$Location","site_coll$Year","site_coll$Day"),by.x=c("Site","Year","Day"))
Env_anal_all <- subset(Env_anal_all,Species=="Fv")
Env_anal_all <- subset(Env_anal_all,Site=="Green"|Site=="Uum"|Site=="Hali"|Site=="SCH"|Site=="WH"|Site=="Newp"|Site=="Lew"|Site=="Beau")



Env_anal <- Env_anal_all
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared

}
y <- y[order(y$ASVid),]
write.csv(y,"all_W.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="V")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_W_V.csv",row.names = FALSE)


Env_anal <- subset(Env_anal_all,Tissue=="H")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_W_H.csv",row.names = FALSE)




Env_anal <- subset(Env_anal_all,Tissue=="R")
y<-data.frame(matrix(ncol=17,nrow=length(unique(Env_anal$ASVid))))
names(y) <- c("ASVid","overall_Formula","overall_R2","T1","T1_R2","T2","T2_R2","T3","T3_R2","T4","T4_R2","T5","T5_R2","T1:T2_R2","T1:T3_R2","T1:T4_R2","T1:T5_R2")

for(z in 1:length(unique(Env_anal$ASVid))){
modsub<-subset(Env_anal,ASVid==unique(Env_anal$ASVid)[z])
modsub <- modsub[,c(6,16:405)]

models <- regsubsets(measurement ~., data = modsub, nvmax = 5,method = "seqrep",really.big = TRUE,nbest=1)
x<-get_model_formula(5,models,"measurement")
y[z,1] <-unique(Env_anal$ASVid)[z]
y[z,3] <-summary(lm(x,data=modsub))$r.squared
y[z,2] <- paste0(x[2],x[1],x[3])
y[z,c(4,6,8,10,12)]<-unlist(strsplit(y[z,2],"+",fixed=TRUE))
y[z,4]<-unlist(strsplit(as.character(y[z,4]),"~",fixed=TRUE))[2]
y[z,5] <-summary(lm(paste0("measurement~",y[z,4]),data=modsub))$r.squared
y[z,7] <-summary(lm(paste0("measurement~",y[z,6]),data=modsub))$r.squared
y[z,9] <-summary(lm(paste0("measurement~",y[z,8]),data=modsub))$r.squared
y[z,11] <-summary(lm(paste0("measurement~",y[z,10]),data=modsub))$r.squared
y[z,13] <-summary(lm(paste0("measurement~",y[z,12]),data=modsub))$r.squared
terms <- c(4,6,8,10,12)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
vals <- c(5,7,9,11,13)[order(c(y[z,5],y[z,7],y[z,9],y[z,11],y[z,13]),decreasing = TRUE)]
y[z,c(4,6,8,10,12)]<-y[z,terms]
y[z,c(5,7,9,11,13)]<-y[z,vals]
y[z,2] <- paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12])
y[z,14] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6]),data=modsub))$r.squared
y[z,15] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8]),data=modsub))$r.squared
y[z,16] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10]),data=modsub))$r.squared
y[z,17] <-summary(lm(paste0("measurement~",y[z,4],"+",y[z,6],"+",y[z,8],"+",y[z,10],"+",y[z,12]),data=modsub))$r.squared
}
y <- y[order(y$ASVid),]
write.csv(y,"all_W_R.csv",row.names = FALSE)

