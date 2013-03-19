findiplist <-
function(XX,FF,index)
{
  n=dim(XX)[1]
  #For convex/concave data (upward sigmoid) give index=0
  #For concave/convex data (downward sigmoid) give index=1
  if(index==1){FF=-FF}
  x1=XX[1]
  y1=FF[1]
  x2=XX[n]
  y2=FF[n]
  flinp=function(x)
  {
    lin2(x1,y1,x2,y2,x)
  }
  LF=FF-flinp(XX)
  jf1=which.min(LF)
  xf1=XX[jf1]
  jf2=which.max(LF)
  xf2=XX[jf2]
  if(xf2<xf1){xfx='NaN'} else {xfx=.5*(xf1+xf2)}
  vsl<-c()
  vsr<-c()
  for (i in(2:n)){
    A=findipl(XX,FF,i) 
    vsl[i-1]<-A[3]
    vsr[i-1]<-A[4]
  }
  jl=which.min(vsl)+1
  xl=XX[jl]
  jr=which.max(vsr)+1
  xr=XX[jr]
  xs=.5*(xl+xr)
  print("Output for ESE, EDE methods as defined theoretically in:")
  print("Demetris T. Christopoulos(2012).'Developing methods for identifying the")
  print("inflection point of a convex/ concave curve'. arXiv:1206.5478v1 [math.NA]")
  print("Contact Emails: dchristop@econ.uoa.gr or dem.christop@gmail.com")
  print("======================================================================")
  if(index==1)
  print("Data is first concave and then convex, like a downward sigmoidal curve")
  else
  print("Data is first convex and then concave, like an upward sigmoidal curve")  
  print("Extremum Surface Estimator - ESE method - inflection point = chi_S") 
  print("J_right: the index of x_right, J_left: the index of x_left ")
  print(matrix(c(jr,jl,xs),nrow=1,ncol=3,byrow=TRUE,dimnames = list(c("ESE_method"),c("J_r","J_l","chi_S"))))
  if(index==1)
  {
    print("Extremum Distance Estimator - EDE method")
    print("EDE method is not applicable because J_F1 > J_F2")
    print("J_F1: the index of x_Fl , J_F2: the index of x_F2")
    print(matrix(c(jf1,jf2,xfx),nrow=1,ncol=3,byrow=TRUE,dimnames = list(c("EDE_method"),c("J_F1","J_F2","chi_D"))))
    ans=matrix(c(jr,jl,xs),nrow=1,ncol=3,byrow=TRUE,dimnames=list(c("ESE"),c("i1","i2","chi_S,D")))
  }
  else
  {
    print("Extremum Distance Estimator - EDE method - inflection point = chi_D")
    print("J_F1: the index of x_Fl , J_F2: the index of x_F2")
    print(matrix(c(jf1,jf2,xfx),nrow=1,ncol=3,byrow=TRUE,dimnames = list(c("EDE_method"),c("J_F1","J_F2","chi_D"))))
    ans=matrix(c(jr,jl,xs,jf1,jf2,xfx),nrow=2,ncol=3,byrow=TRUE,dimnames=list(c("ESE","EDE"),c("i1","i2","chi_S,D")))
  }
  print("======================================================================")
  ans
  }
