\name{inflection-package}
\alias{inflection-package}
\alias{inflection}
\docType{package}
\title{
Finds the inflection point of a curve
}
\description{
Implementation of methods Extremum Surface Estimator and Extremum Distance Estimator for identifying the inflection point of a convex/concave curve 
}
\details{
\tabular{ll}{
Package: \tab inflection\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-04-04\cr
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
x<-cbind(seq(0,10,by=0.1))
y<-5*(1+tanh(x-5))+2*0.1*(runif(dim(x)[1])-0.5)
#The inflection point is ip=5.0
N<-dim(x)[1]
#Because of the total symmetry we expect inflection point to be estimated near the middle of x-range:
A<-findiplist(x,y,0)
print(A)
#Our expectation came true
#Let's plot some interesting approximately results.
#We define the line chords:
js=round((A[1,1]+A[1,2])/2)
jd=round((A[2,1]+A[2,2])/2)
fl<-function(t){y[1] + (y[js] - y[1]) * (t - x[1]) / (x[js] - x[1])}
fr<-function(t){y[js] + (y[N] - y[js]) * (t - x[js]) / (x[N] - x[js])}
flin<-function(t){y[1] + (y[N] - y[1]) * (t - x[1]) / (x[N] - x[1])}
#Now we plot everything we have found:
xl=x[1:js,]
yl=fl(xl)
xr=x[js:N,]
yr=fr(xr)
plot(x,y,type="l",col="red")
lines(xl,yl,type="l",col="green")
lines(xr,yr,type="l",col="blue")
lines(x,flin(x),type="l",col="black")
points(x[js],y[js], type = "p",pch = 19,col="black")
abline(v=x[js],lty=2,col="black")
points(x[jd],y[jd], type = "p",pch = 19,col="blue")
abline(v=x[jd],lty=2,col="blue")
text(x[js+10],0,expression(chi[S]))
text(x[jd-10],y[30],expression(chi[D]))
#Let's make some ESE and EDE iterations and plot them:
a<-findipiterplot(x,y,0);
#Show ESE iterations
t(a$aese);
#Show ESE iterations from EDE iterations
t(a$bese);
#Show all ESE iterations
t(a$esm);
#Show EDE iterations
t(a$aede);
#Show EDE iterations from ESE iterations
t(a$bede);
#Show all EDE iterations
t(a$edm);
#Show first solution
a$first;
#Statistics and 95 pc confidence interval for ESE
a$aesmout;
#Statistics and 95 pc confidence interval for ESE throw EDE 
a$besmout;
#Statistics and 95 pc confidence interval for EDE
a$aedmout;
#Statistics and 95 pc confidence interval for EDE throw ESE
a$bedmout;
#Statistics and 95 pc confidence interval for all ESE results
a$esmout;
#Statistics and 95 pc confidence interval for all EDE results
a$edmout;
#Statistics and 95 pc confidence interval for all results
a$ipall;
}