hourlynames<-list.files()


base<-read.csv(hourlynames[1],stringsAsFactors = FALSE)
base$file <-unlist(strsplit(hourlynames[1],".csv"))
for(i in 2:length(hourlynames)){
file<-read.csv(hourlynames[i],stringsAsFactors = FALSE)
file$file <-unlist(strsplit(hourlynames[i],".csv"))
base<-rbind(base,file)
}


base<-subset(base,overall_R2 >= 0.5)
for(c in c(4,6,8,10,12)){
for(q in 1:nrow(base)){
list<-unlist(strsplit(base[q,c],"_"))
base[q,c]<-ifelse(length(list)==3,list[2],ifelse(length(list)==2,list[1],list[1]))
}
for(q in 1:nrow(base)){
list<-unlist(strsplit(base[q,c],"Median"))
base[q,c]<-ifelse(length(list)==2,list[2],list[1])
}
for(q in 1:nrow(base)){
list<-unlist(strsplit(base[q,c],"Min"))
base[q,c]<-ifelse(length(list)==2,list[2],list[1])
}
for(q in 1:nrow(base)){
list<-unlist(strsplit(base[q,c],"Max"))
base[q,c]<-ifelse(length(list)==2,list[2],list[1])
}
for(q in 1:nrow(base)){
list<-unlist(strsplit(base[q,c],"Range"))
base[q,c]<-ifelse(length(list)==2,list[2],list[1])
}
for(q in 1:nrow(base)){
list<-unlist(strsplit(base[q,c]," "))
base[q,c]<-ifelse(length(list)==2,list[2],list[1])
}
}



g1<-graph(edges=c(rbind(base$ASVid,base[q,c])),directed = FALSE)



layout_in_circles <- function(g, group=1) {
    layout <- lapply(split(V(g), group), function(x) {
        layout_in_circle(induced_subgraph(g,x))
    })
    layout <- Map(`*`, layout, seq_along(layout))
    x <- matrix(0, nrow=vcount(g), ncol=2)
    split(x, group) <- layout
    x
}
l <- layout_with_kk(g1)
V(g1)
V(g1)$type <- c(0,1,0,0,1,0,1,1,0,0,0,1,1,1,1,1,0,0,1,0)
V(g1)$color <- c("pink","orange","pink","pink","orange","pink","orange","orange","pink","pink","pink","orange","orange","orange","orange","orange","pink","pink","orange","pink")
E(g1)$strength <- base$overall_R2
E(g1)$width <- E(g1)$strength*2
plot(g1,layout=l,edge.color="black",edge.width=as.integer(cut(abs(E(g1)$strength), breaks = 5)))
#layout=layout_in_circles(g1, group=V(g1)$type<1)
write.csv(base,"based.csv")
