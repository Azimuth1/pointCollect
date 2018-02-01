pointCollect <- function(pointfile, fov, plotting){
  data <- read.csv(pointfile, header=T, row.names=NULL)

  #t<-0.2   #threshold for complete link clustering
  fov<-as.numeric(fov)

#  id<-1:150  #vertex IDs

#  X<-c(runif(100,-74,-69),runif(50,-71.3,-71))
#  Y<-c(runif(100,36,39),runif(50,37.7,38))
#  coords<-cbind(X,Y,id)
  names(data)<-c("ID","X","Y","T")
  collections<-NULL


#coords<-data
  g<-graph.empty(n=0,directed=FALSE) + vertices(data$ID)
  V(g)$X<-data$X
  V(g)$Y<-data$Y
  #V(g)$T<-data$T

  for (i in 1:gorder(g)){
    for (j in 1:gorder(g)){
      #if (((X[i]-X[j])^2+(Y[i]-Y[j])^2)<=t^2){
      if (i != j){
        if (max(abs(data$X[i]-data$X[j]),abs(data$Y[i]-data$Y[j]))<=fov){
          g<-g+edge(V(g)[i]$name,V(g)[j]$name)
        }
      }
    }
  }


  if(plotting==TRUE) {
  ###################
  # PLOT SET TO TRUE
  ###################


  tryCatch({
    plot(V(g)$X,V(g)$Y,cex=2,pch=".",col="blue")
    collections<-NULL
    #points(coords[maxCliqueIDs,2:3],col="green")
    sceneCount<-0
    while (gsize(g) >= 1){   #gsize gives number of edges
      cl<-largest_cliques(g)[[1]]  #find complete subgraphs
      minX<-min(cl$X)  #define the collection bounds...
      maxX<-max(cl$X)
      minY<-min(cl$Y)
      maxY<-max(cl$Y)
      collections<-rbind(collections, c(minX,minY,maxX,maxY))

      #id<-subset(id, !(id %in% cl))  #remove the IDs of the point targets just collected

      rect(minX, minY, maxX, maxY, border="red")
      g<-delete_vertices(g,cl)
      sceneCount<-sceneCount+1
    }, error = function(e){
        stop(e);
    })

  }else{
  ###################
  # PLOT SET TO FALSE
  ###################
    return(collections)
  }
}
