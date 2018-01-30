pointCollect <- function(pointfile, fov, plotting){
  data <- read.csv(pointfile, header=T, row.names=NULL)

  #t<-0.2   #threshold for complete link clustering
  fov<-as.numeric(fov)
  t<-fov
  id<-1:1050  #vertex IDs

  X<-c(runif(100,-74,-69),runif(50,-71.3,-71))
  Y<-c(runif(100,36,39),runif(50,37.7,38))
  coords<-cbind(X,Y,id)
  names(coords)<-c("X","Y","T")
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

  if(plotting==TRUE) {
  ###################
  # PLOT SET TO TRUE
  ###################

  tryCatch({
      plot(coords[,1:2],cex=2,pch=".",col="blue",asp=1)
      points(coords[maxCliqueIDs,2:3],col="green")
      rect(minX, minY, maxX, maxY, border="red", )
    }, error = function(e){
        stop(e);
    })

  }else{
  ###################
  # PLOT SET TO FALSE
  ###################
    return(coords)
  }
}
