findiplist <-
function(x,y,index)
{
  #Output for ESE, EDE methods as defined theoretically in:
  #Demetris T. Christopoulos(2012).'Developing methods for identifying the
  #inflection point of a convex/ concave curve'. arXiv:1206.5478v2 [math.NA]
  #Contact Emails: dchristop@econ.uoa.gr or dem.christop@gmail.com
  n=length(x)
  #For convex/concave data (upward sigmoid) give index=0
  #For concave/convex data (downward sigmoid) give index=1
  if(index==1){y=-y}
  #
  ifelse(n>=4,{A=ese(x,y,index);B=ede(x,y,index)},
         {A=matrix(NA,ncol=3,nrow=1);rownames(A)="ESE";colnames(A)=c("j1","j2","chi");B=A;rownames(B)="EDE";colnames(B)=c("j1","j2","chi");
         warning('Insufficient number of points, please provide at least 4 points!')});
  #
  out=rbind(A,B)
  #
  return(out)
}
