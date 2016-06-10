findipiterplot <-
function(x,y,index,plots=TRUE,crossrun=FALSE,ci=FALSE){
BESE<-c();CRESE<-c();
BEDE<-c();CREDE<-c();
xpes<-c();ypes<-c();npes<-c();
xped<-c();yped<-c();nped<-c();
A<-findiplist(x,y,index);
B<-A;
x1<-x;y1<-y;
x2<-x;y2<-y;
BESE<-c(BESE,A[1,3])
BEDE<-c(BEDE,A[2,3])
#First run:
out1<-matrix(c(A[1,1],A[1,2],A[1,3],A[2,1],A[2,2],A[2,3]),nrow=2,ncol=3,byrow=TRUE,dimnames=list(c("ESE","EDE"),c("i1","i2","chi_S,D")));
#ESE iterations:
#
i<-0
while (!(is.nan(A[1,3])))  
{
  if (A[1,2]>A[1,1]+3)
  {
i<-i+1
xpes<-c(xpes,x1);ypes<-c(ypes,y1);npes<-c(npes,length(x1));
x1<-x1[A[1,1]:A[1,2]]
y1<-y1[A[1,1]:A[1,2]]
A<-findiplist(x1,y1,index)
if (A[1,2]>A[1,1]+3)
{
  # ESE<-c(ESE,A[1,3]);
  BESE<-c(BESE,A[1,3]);
  # EDE<-c(EDE,A[2,3]);
  CREDE<-c(CREDE,A[2,3]); 
}  
  }
  else
break
}
#
#EDE iterations:
#
j<-0
while (!(is.nan(B[2,3])))
{
  if (B[2,2]>B[2,1]+3)
  {
j<-j+1
xped<-c(xped,x2);yped<-c(yped,y2);nped<-c(nped,length(x2));
x2<-x2[B[2,1]:B[2,2]]
y2<-y2[B[2,1]:B[2,2]]
B<-findiplist(x2,y2,index)
if (B[2,2]>B[2,1]+3)
{
  if(!(is.nan(B[2,3])))
  {
# ESE<-c(ESE,B[1,3]);
CRESE<-c(CRESE,B[1,3]);
# EDE<-c(EDE,B[2,3]);
BEDE<-c(BEDE,B[2,3]);
  }
  else
break
}  
  }
  else
break
}
#
#Create output environment...
#
out=new.env()
out$first=out1
out$BESE=rbind(BESE);rownames(out$BESE)=c("ESE iterations");colnames(out$BESE)=1:length(BESE);
out$BEDE=rbind(BEDE);rownames(out$BEDE)=c("EDE iterations");colnames(out$BEDE)=1:length(BEDE);
#
#Cross iterative runs
#
if(crossrun)
{
#
  ESE=c(BESE,CRESE);EDE=c(BEDE,CREDE)
  out$esm=rbind(ESE);rownames(out$esm)=c("ESE all iterations");
  out$edm=rbind(EDE);rownames(out$edm)=c("EDE all iterations");
#Cross iterative run of ESE...
#
if(!(is.null(CRESE)))
  {
   out$CRESE=rbind(CRESE);
   rownames(out$CRESE)=c("ESE results from EDE iters");
   #95% confidence interval:
   if(ci)
   {
   bmesm=mean(CRESE,na.m = TRUE);bsesm=sd(CRESE,na.rm = TRUE);
   beresm=qt(0.975,df=length(CRESE)-1)*bsesm/sqrt(length(CRESE));
   blciesm=bmesm-beresm;brciesm=bmesm+beresm;
   out$besmout=cbind(bmesm,bsesm,blciesm,brciesm);
   colnames(out$besmout)=c("mean","sdev","95%(l)","95%(r)");
   rownames(out$besmout)="ESE from EDE iters";
   } 
   }
#
#Cross iterative run of EDE...
#
if(!(is.null(CREDE)))
  {
   out$CREDE=rbind(CREDE);
   rownames(out$CREDE)=c("EDE results from ESE iters");
   #95% confidence interval
   if(ci)
   {
   bmedm=mean(CREDE,na.rm = TRUE);bsedm=sd(CREDE,na.rm = TRUE);
   beredm=qt(0.975,df=length(CREDE)-1)*bsedm/sqrt(length(CREDE));
   blciedm=bmedm-beredm;brciedm=bmedm+beredm;
   out$bedmout=cbind(bmedm,bsedm,blciedm,brciedm)
   colnames(out$bedmout)=c("mean","sdev","95%(l)","95%(r)")
   rownames(out$bedmout)=c("EDE from ESE iters")
   }
  }
#
}
#
#Iterative run of methods - 95% confidence intervals:
#
if(ci)
{
amesm=mean(BESE,na.rm = TRUE);amedm=mean(BEDE,na.rm = TRUE);
asesm=sd(BESE,na.rm = TRUE);asedm=sd(BEDE,na.rm = TRUE);
aeresm=qt(0.975,df=length(BESE)-1)*asesm/sqrt(length(BESE));
aeredm=qt(0.975,df=length(BEDE)-1)*asedm/sqrt(length(BEDE));
alciesm=amesm-aeresm;arciesm=amesm+aeresm;
alciedm=amedm-aeredm;arciedm=amedm+aeredm;
out$aesmout=cbind(amesm,asesm,alciesm,arciesm)
colnames(out$aesmout)=c("mean","sdev","95%(l)","95%(r)")
rownames(out$aesmout)=c("ESE method")
out$aedmout=cbind(amedm,asedm,alciedm,arciedm)
colnames(out$aedmout)=c("mean","sdev","95%(l)","95%(r)")
rownames(out$aedmout)=c("EDE method")
if(crossrun)
{
mesm=mean(ESE,na.rm = TRUE);medm=mean(EDE,na.rm = TRUE);
sesm=sd(ESE,na.rm = TRUE);sedm=sd(EDE,na.rm = TRUE);
eresm=qt(0.975,df=length(ESE)-1)*sesm/sqrt(length(ESE));
eredm=qt(0.975,df=length(EDE)-1)*sedm/sqrt(length(EDE));
lciesm=mesm-eresm;rciesm=mesm+eresm;
lciedm=medm-eredm;rciedm=medm+eredm;
out$esmout=cbind(mesm,sesm,lciesm,rciesm)
colnames(out$esmout)=c("mean","sdev","95%(l)","95%(r)")
rownames(out$esmout)=c("ESE method, all results")
out$edmout=cbind(medm,sedm,lciedm,rciedm)
colnames(out$edmout)=c("mean","sdev","95%(l)","95%(r)")
rownames(out$edmout)=c("EDE method, all results")
}
}
#
#Estimation of inflection point using results from both ESE and EDE methods:
IP=c(ESE,EDE);mip=mean(IP,na.rm = TRUE);sip=sd(IP,na.rm = TRUE);
#If ci=TRUE and crossrun=TRUE...
#
if(ci & crossrun)
{erip=qt(0.975,df=length(IP)-1)*sip/sqrt(length(IP));
lcip=mip-erip;rcip=mip+erip;
out$ipall=cbind(mip,sip,lcip,rcip)
colnames(out$ipall)=c("mean","stdev","95%(l)","95%(r)")
rownames(out$ipall)=c("All methods, all results")
print(matrix(c(mip,sip,lcip,rcip),nrow=1,ncol=4,byrow=TRUE,dimnames=list(c("ip all methods"),c("mean","sdev","95%(l)","95%(r)"))))
}
#
ns=cbind(npes);nd=cbind(nped);nps<-length(npes);npd<-length(nped);
xsm<-cbind(xpes);ysm<-cbind(ypes);xdm<-cbind(xped);ydm<-cbind(yped);
#Plots if plots=TRUE
#
ifelse(plots,
{
#
dev.new()
par(mfrow=c(2,ceiling(nps/2)))
n0<-0;
dfs<-c();
# cols=colorRampPalette(c("blue", "red"))(nps); 
cols=colorRampPalette(c("blue", "green"))(nps); 
for (i in 1:nps)
{
  x1<-xsm[(n0+1):(n0+ns[1])];y1<-ysm[(n0+1):(n0+ns[1])];
  x<-x1[!is.na(x1)];y<-y1[!is.na(y1)];dfs[[i]]=as.data.frame(cbind(x,y));
  plot(x,y,xlab=paste("x",i),ylab=paste("y",i),col=cols[i],pch=19)
  abline(v=BESE[i],lty=2,col='blue')
  title(paste("ESE iter",i),sub=paste("ip=",BESE[i]))
  n0<-n0+ns[i];
}
#
dev.new()
par(mfrow=c(2,ceiling(npd/2)))
n0<-0;
dfd<-c();
cols=colorRampPalette(c("blue", "red"))(npd); 
for (i in 1:npd)
{
  x1<-xdm[(n0+1):(n0+nd[1])];y1<-ydm[(n0+1):(n0+nd[1])];
  x<-x1[!is.na(x1)];y<-y1[!is.na(y1)];dfd[[i]]=as.data.frame(cbind(x,y));
  plot(x,y,xlab=paste("x",i),ylab=paste("y",i),col=cols[i],pch=19)
  abline(v=BEDE[i],lty=2,col='red')
  title(paste("EDE iter",i),sub=paste("ip=",BEDE[i]))
  n0<-n0+nd[i];
}
#
out$xysl=dfs;
out$xydl=dfd;
}
,
{
  n0<-0;
  dfs<-c();
  for (i in 1:nps)
  {
    x1<-xsm[(n0+1):(n0+ns[1])];y1<-ysm[(n0+1):(n0+ns[1])];
    x<-x1[!is.na(x1)];y<-y1[!is.na(y1)];dfs[[i]]=as.data.frame(cbind(x,y));
    plot(x,y,xlab=paste("x",i),ylab=paste("y",i))
    abline(v=BESE[i],lty=2,col="blue")
    title(paste("ESE iter",i),sub=paste("ip=",BESE[i]))
    n0<-n0+ns[i];
  }
  #
  n0<-0;
  dfd<-list();
  for (i in 1:npd)
  {
    x1<-xdm[(n0+1):(n0+nd[1])];y1<-ydm[(n0+1):(n0+nd[1])];
    x<-x1[!is.na(x1)];y<-y1[!is.na(y1)];dfd[[i]]=as.data.frame(cbind(x,y));
    n0<-n0+nd[i];
  }
  #
  out$xysl=dfs;
  out$xydl=dfd;
}
)
#
#Return out
return(out)
}
