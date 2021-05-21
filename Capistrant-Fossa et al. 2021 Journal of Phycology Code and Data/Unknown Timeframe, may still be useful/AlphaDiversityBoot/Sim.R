library(readr)


#Load the used libraries
library(vegan)
library(svglite)
library(readr)
library(ggplot2)
library(foreach)
library(doMC)

#Set the number of cores you'd like to use for parallel computing
registerDoMC(2)



ASVs <- data.frame(read_csv("~/Desktop/Transatlantic Files/Raw_No_Mito/ASVsNoMitoNoFall.csv"))


simResults <- foreach(i = 1:1000) %dopar% {

ASVs2 <- ASVs

csum<-colSums(ASVs2[,2:ncol(ASVs2)])
min(csum)
ASVs2 <- ASVs2[,-(which(csum<4918)+1)]
csum<-colSums(ASVs2[,2:ncol(ASVs2)])
min(csum)


new_seed<-as.numeric(sample(1:999999,1))
set.seed(new_seed)
for(ii in 2:ncol(ASVs2)){
  workingThingy<-data.frame(ASVs2[c(1,ii)])
  workingThingy<-rep(workingThingy[,1],workingThingy[,2])
  x<-data.frame(table(sample(workingThingy,4918)))
  ASVs2[,ii] <- 0
for(mm in 1:nrow(x)){
  ASVs2[which(ASVs2[,1]==x$Var1[mm]),ii] <- x$Freq[mm]
}
}

ASVs2 <- ASVs2[,-c(1)]

shan<-diversity(t(ASVs2),"shannon")
simp<-diversity(t(ASVs2),"simpson")
invsimp<-diversity(t(ASVs2),"invsimpson")
pie <- shan/log(specnumber(t(ASVs2)))



ASVs3<-ifelse(ASVs2 >= 1, 1, 0)



res<-data.frame(colSums(ASVs3))
res$seed <- new_seed
res$shan <- shan
res$simp <- simp
res$invsimp <- invsimp
res$pie <- pie

print(res)
}



x<-data.frame(matrix(nrow=1000,ncol=1318))
for(i in 1:1000){
  x[i,]<-simResults[[i]]
}

for(q in 1:1000){
  
  
  
}





names(x)<-names(simResults[[1]])

x_t<-data.frame(t(x))
x<-cbind(row.names(x_t),x_t)
Lats<-read.csv("Lats.csv",stringsAsFactors = FALSE)
Meta<-read.csv("Metadata.csv",stringsAsFactors = FALSE)

metalat<-merge(Meta,Lats,by.x="Site",by.y="Sites")

x$`row.names(x_t)` <- as.character(x$`row.names(x_t)`)




metaCounts<-merge(metalat,x,by.x="SampleName",by.y="row.names(x_t)")
metaCounts2<-subset(metaCounts, Species=="Fv")

#metaCounts2<-metaCounts2[,13:1014]

library(tidyr)
library(ggplot2)
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <- gather(metaCounts2, run, ASVs, X1:X1000, factor_key=TRUE)

str(data_long)

data_long$Side<-ifelse(data_long$lon < -30,"East","West" )
ggplot(data_long, aes(x=lat, y=ASVs)) +
    geom_point()

