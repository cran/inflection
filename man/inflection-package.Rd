\name{inflection-package}
\alias{inflection-package}
\alias{inflection}
\docType{package}
\title{
Finds the inflection point of a curve
}
\description{
Implementation of methods Extremum Surface Estimator and Extremum Distance Estimator for identifying the inflection point of a convex/cocave curve 
}
\details{
\tabular{ll}{
Package: \tab inflection\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-03-09\cr
License: \tab GPL 2\cr
}
The x,y data should be column vectors\cr
For a convex/concave curve:\cr
findiplist(x,y,0)\cr
For a concave/convex curve:\cr
findiplist(x,y,1)
}
\author{
Demetris T. Christopoulos \cr
Maintainer: Demetris T. Christopoulos <dchristop@econ.uoa.gr>
}
\references{
Demetris T. Christopoulos(2012). Developing methods for identifying the inflection point of a convex/ concave curve. arXiv:1206.5478v1 [math.NA]
}
\keyword{inflection}
\examples{
#Lets create some convex/concave data based on the Fisher-Pry model:
xx<-cbind(seq(0,10,by=0.1))
yy<-5*(1+tanh(xx-5))+2*0.1*(runif(dim(xx)[1])-0.5)
N<-dim(xx)[1]
#Because of the total symmetry we expect inflection point to be estimated near the middle of x-range:
A<-findiplist(xx,yy,0)
print(A)
#Our expectation came true
#Let's plot some interesting approximately results.
#We define the line chords:
js=round((A[1,1]+A[1,2])/2)
jd=round((A[2,1]+A[2,2])/2)
fl<-function(x){yy[1] + (yy[js] - yy[1]) * (x - xx[1]) / (xx[js] - xx[1])}
fr<-function(x){yy[js] + (yy[N] - yy[js]) * (x - xx[js]) / (xx[N] - xx[js])}
flin<-function(x){yy[1] + (yy[N] - yy[1]) * (x - xx[1]) / (xx[N] - xx[1])}
#Now we plot everything we have found:
xxl=xx[1:js,]
yyl=fl(xxl)
xxr=xx[js:N,]
yyr=fr(xxr)
plot(xx,yy,type="l",col="red")
lines(xxl,yyl,type="l",col="green")
lines(xxr,yyr,type="l",col="blue")
lines(xx,flin(xx),type="l",col="black")
points(xx[js],yy[js], type = "p",pch = 19,col="black")
points(xx[jd],yy[jd], type = "p",pch = 19,col="blue")
text(xx[js+1],0,expression(chi[S]))
text(xx[jd-1],0,expression(chi[D]))
}