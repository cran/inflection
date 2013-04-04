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
findiplist(x, y, index)
}
\arguments{
  \item{x}{
The column vector of x-abscissas
}
  \item{y}{
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
When data is concave/convex we simply change sign to the y values and treat it as the convex/concave case.
}
\examples{
#Lets create some convex/concave data based on the Fisher-Pry model:
x<-cbind(seq(0,10,by=0.1))
y<-5*(1+tanh(x-5))+2*0.1*(runif(dim(x)[1])-0.5)
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
text(x[js+5],0,expression(chi[S]))
text(x[jd-5],y[30],expression(chi[D]))
}
\keyword{inflection}
\keyword{ESE }
\keyword{EDE }