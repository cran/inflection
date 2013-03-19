\name{findiplist}
\alias{findiplist}
\title{
The Extremum Surface Estimator (ESE) and Extremum Distance Estimator (EDE) methods for finding the inflection point of a convex/concave curve.
}
\description{
Given the (xi,yi), i=1,...,N errored or not data we want to estimate the inflection point of the corresponding curve.
The curve can be first convex then concave or vice versa.
The ESE and EDE methods are applied and the results are returned as a matrix.
}
\usage{
findiplist(XX, FF, index)
}
\arguments{
  \item{XX}{
The column vector of x-abscissas
}
  \item{FF}{
The column vector of the errored or not y-ordinates
}
  \item{index}{
If data is convex/concave then index=0 \cr
If data is concave/convex then index=1
}
}
\details{
If data is from an unknown function and without error then we can find the inflection point in a way similar to that of bisection method' s way for a root.
If data is errored, then we have two consistent estimators of the trapezoidal estimated inflection point, i.e. we consistently estimate what we could find by computing the relevant areas with elementary trapezoids.
}
\value{
A matrix of size 2 x 3 is returned with elements:
  \item{A(1,1)=i1}{The index j-right for ESE method}
  \item{A(1,2)=i2}{The index j-left for ESE method}
  \item{A(1,3)=chi_S}{The Extremum Surface Estimator (ESE) for inflection point}
  \item{A(2,1)=i1}{The index j1 for EDE method}
  \item{A(2,2)=i2}{The index j2 for EDE method}
  \item{A(2,3)=chi_D}{The Extremum Distance Estimator (EDE) for inflection point, if this method is applicable}
}
\references{
Demetris T. Christopoulos(2012).Developing methods for identifying the inflection point of a convex/ concave curve. arXiv:1206.5478v1 [math.NA]
}
\author{
Demetris T. Christopoulos
}
\note{
When data is concave/convex we simply change sign to the FF values and treat it as the convex/concave case.
}
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
\keyword{inflection}
\keyword{ESE }
\keyword{EDE }