dijkstra <-
function(graph, init_node){
  
   if (is.data.frame(graph)==TRUE && is.numeric(init_node)==TRUE && is.vector(graph[[1]])==TRUE && is.vector(graph[[2]])==TRUE && is.vector(graph[[3]])==TRUE){
    
      v1<-graph[[1]] #vector of nodes "from"
      v2<-graph[[2]] #vector of nodes "to"
      w<-graph[[3]] #vector of weights
      n<-length(unique(v1)) #number of nodes
      stopifnot(init_node<=n)
      dist<-length(n) #vector of output
      Q<-c(1:n) #subset of nodes of our graph
     
      for(i in 1:n){ 
        dist[i]<-Inf
      }
      
      dist[init_node]<-0 
     
      while(length(Q)!=0){
        
        u<-which(dist==dist[Q[which.min(dist[Q])]])
        Q<-Q[-which(Q==u)]
        
        for(i in 1:length(v1)){
          if(v1[i]==u && (dist[u]+w[i]<dist[v2[i]]))
            dist[v2[i]]<-dist[u]+w[i]
        }
      }
      
    return(dist)
  }else{
    stop()
  }
}
