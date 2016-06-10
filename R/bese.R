bese <-
function(x,y,index)
  {
    ESE<-c();BESE<-c();a<-c(x[1]);b<-c(x[length(x)]);nped<-c(length(x));x2<-x;y2<-y;
    A=ese(x,y,index);ESE<-c(ESE,A[1,3]);BESE<-c(BESE,A[1,3]);iplast=A[1,3];
    #
    #ESE iterations:
    #
    j<-0
    while (!(is.nan(A[1,3])))
    {
      ifelse (A[1,2]>=A[1,1]+3,
              {
                j<-j+1;
                x2<-x2[A[1,1]:A[1,2]];y2<-y2[A[1,1]:A[1,2]];A<-ese(x2,y2,index);#print(A)
                ifelse(!(is.nan(A[1,3])),
                       {a=c(a,x2[A[1,1]]);b=c(b,x2[A[1,2]]);nped<-c(nped,length(x2));ESE<-c(ESE,A[1,3]);BESE<-c(BESE,A[1,3]);iplast=A[1,3];}
                       ,
                       {break})
              }
              ,
              {break}
      )
    }
    #
    #Set output...
    #
    iters=as.data.frame(cbind(nped,a,b,BESE));colnames(iters)=c("n","a","b","ESE");rownames(iters)=1:length(nped);
    out=list();out[["iplast"]]=iplast;out[["iters"]]=iters;
    return(out)
  }
