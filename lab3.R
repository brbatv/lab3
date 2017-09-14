euclidean<-function(a,b)
{
  a0=a # first element of sequence
  a1=b # second element
  while (a1!=0) 
  {
    r=a0%%a1 # rest of euclidean division of a0 by a1
    a0=a1 
    a1=r
  }
  return(a0)
}


dijkstra <- function(graph, init_node){
  if (is.data.frame(graph)==TRUE && is.numeric(init_node)==TRUE && is.vector(graph[[1]])==TRUE && is.vector(graph[[2]])==TRUE && is.vector(graph[[3]])==TRUE){
    v1<-graph[[1]]
    v2<-graph[[2]]
    w<-graph[[3]]
    n<- #number of nodes
    dist<-length(n) #distances
    elem<-length(n) #element in the graph
    elem[1]<-init_node
    for(j in 1:n){
      a<-Inf
      for(i in 1:length(v1)){
        if(v1[i]==init_node && w[i]<a){
          a<-w[i] #save the weight
          b<-i #save the position of the next node
        }
      }
      elem[j]<-b
      dist[j]<-a
    }
  }else{
    stop()
  }
}

dijkstra <- function(graph, init_node){
  if (is.data.frame(graph)==TRUE && is.numeric(init_node)==TRUE && is.vector(graph[[1]])==TRUE && is.vector(graph[[2]])==TRUE && is.vector(graph[[3]])==TRUE){
    v1<-graph[[1]]
    v2<-graph[[2]]
    w<-graph[[3]]
    n<- #number of nodes
    dist<-length(n) #distances

    for(i in 1:n){
      if(init_node==i){
        dist[i]=0
      }else{
        a<-Inf
        b<-0
        node<-init_node
        while(v2[b]!=i){
        for(j in 1:length(v1)){
          if(v1[j]==node && w[j]<a){
            a<-w[j] #save the weight
            b<-j #save the position of the next node
            node<-v2[j]
              }
          }
        }
        dist[i]<-a
        }
      }
    }
    
  }else{
    stop()
  }
}