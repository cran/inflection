ese <-
function(x,y,index)
  {
    #Output for ESE method as defined theoretically in:
    #Demetris T. Christopoulos(2012).'Developing methods for identifying the
    #inflection point of a convex/ concave curve'. arXiv:1206.5478v2 [math.NA]
    #Contact Emails: dchristop@econ.uoa.gr or dem.christop@gmail.com
    n=length(x);
    #For convex/concave data (upward sigmoid) give index=0
    #For concave/convex data (downward sigmoid) give index=1
    if(index==1){y=-y}
    #
    ifelse(n>=4,
           {
           slsr=matrix(sapply(2:(n-1),function(i,x,y){c(findipl(x,y,i)[3:4])},x,y),ncol=2,byrow = T);
           jl=which.min(slsr[,1])+1;jr=which.max(slsr[,2])+1;xl=x[jl];xr=x[jr];
           ifelse((jl-jr>=2)==TRUE,{xs<-.5*(xl+xr)},{xs<-NaN})
           }
           ,
           {jl<-NaN;jr<-NaN;xs<-NaN} 
    )
    #
    out=matrix(c(jr,jl,xs),nrow=1,ncol=3,byrow=TRUE);rownames(out)="ESE";colnames(out)=c("j1","j2","chi");
    return(out)
  }
