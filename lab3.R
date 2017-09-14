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
  a<-length(graph[1])
  b<-length(graph[2])
  c<-length(graph[3])
  if (is.data.frame(graph)==TRUE && is.numeric(init_node)==TRUE && is.vector(graph[1])==TRUE && is.vector(graph[2])==TRUE && is.vector(graph[3])==TRUE && a==b && b==c){
    
    
  }else{
    stop()
  }
}

