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
