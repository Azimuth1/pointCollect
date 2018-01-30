t<-0.2   #threshold for complete link clustering
fov<-t
id<-1:1050  #vertex IDs
X<-c(runif(1000,-74,-69),runif(50,-71.3,-71))
Y<-c(runif(1000,36,39),runif(50,37.7,38))
coords<-cbind(id,X,Y)
names(coords)<-c("ID","X","Y")
collections<-NULL


g<-graph.empty(n=0,directed=FALSE) + vertices(id)

for (i in id){
  for (j in id){
    if (((X[i]-X[j])^2+(Y[i]-Y[j])^2)<=t^2){
      g<-add.edges(g,c(id[i],id[j]))

    }
  }
}

cl<-maximal.cliques(g,min=2)  #find complete subgraphs
maxCliqueIDs<-cl[length(cl)][[1]]  #get the IDs of the largest complete subgraph
minX<-min(coords[maxCliqueIDs,2])  #define the collection bounds...
maxX<-max(coords[maxCliqueIDs,2])
minY<-min(coords[maxCliqueIDs,3])
maxY<-max(coords[maxCliqueIDs,3])
collections<-rbind(collections, c(minX,minY,maxX,maxY))

id<-subset(id, !(id %in% maxCliqueIDs))  #remove the IDs of the point targets just collected


plot(coords[,2:3],cex=2,pch=".",col="blue")
points(coords[maxCliqueIDs,2:3],col="green")
rect(minX, minY, maxX, maxY, border="red", )
