findipl <-
function(XX,FF,j)
{
  n=dim(XX)[1]
  x1=XX[1]
  y1=FF[1]
  x2=XX[j]
  y2=FF[j]
  flin1=function(x)
  {
    lin2(x1,y1,x2,y2,x)
  }
  sl=0
  for (i in 1:(j-1))
  {
    sl=sl+eixf(XX,FF,flin1,i)
  }
  x1=XX[j]
  y1=FF[j]
  x2=XX[n]
  y2=FF[n]
  flin2=function(x)
  {
    lin2(x1,y1,x2,y2,x)
  }
  sr=0
  for (k in j:(n-1))
  {
    sr=sr+eixf(XX,FF,flin2,k)
  }
  c(j,XX[j],sl,sr)
  }
