dijkstra <- function(graph, init_node){
  a<-length(graph[1])
  b<-length(graph[2])
  c<-length(graph[3])
  if (is.data.frame(graph)==TRUE && is.numeric(init_node)==TRUE && is.vector(graph[1])==TRUE && is.vector(graph[2])==TRUE && is.vector(graph[3])==TRUE && a==b && b==c){
    
    
  }else{
    stop()
  }
}