#'
#' Find shortest distance in a graph
#' 
#' 
#' @param graph The Graph. Should be data.frame with 3 numerical columns. The first column contains the starting vertices of each edges, when the second one contains the ending vertices of these edges. The third columns contains the weights of each edge. 
#' @param init_node The vertix fro?m which the distances are calculated
#' @return A vector with the distance between \code{init_node} all the other vertices. 
#' @examples
#' dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5), w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), init_node=1)
#' @references \url{ https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm }
#' @export dijkstra


dijkstra <-
function(graph, init_node){
    stopifnot(dim(graph)[2]==3 && all(names(graph)==c("v1","v2","w")))
  
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
