\name{findipl}
\alias{findipl}
\title{
Finds the s-left and s-right for a given internal point x
}
\description{
From the definitions (4), (5) and (23) of reference it is necessary to find s_l and s_r in order to estimate the Extremum Surface Estimator (ESE) of the inflection point.
}
\usage{
findipl(XX, FF, j)
}
\arguments{
  \item{XX}{
The column vector of x-abscissas
}
  \item{FF}{
The column vector of the errored or not y-ordinates
}
  \item{j}{
The data index j such that x=XX[j]
}
}
\details{
This function is using function eixf() for computing the elemenatry areas by the trapezoidal rule.
}
\value{
A list is returned that contains
\item{j}{The data index j suvh that x=XX[j]}
\item{x=XX[j]}{The corresponding x-abscissa}
\item{sl}{The value of s-left}
\item{sr}{The value of s-right}
}
\references{
Demetris T. Christopoulos(2012).Developing methods for identifying the inflection point of a convex/ concave curve. arXiv:1206.5478v1 [math.NA]
}
\author{
Demetris T. Christopoulos
}
\note{
We have defined this small function to be used when we are scanning for the position of inflection point in ESE method.
}
\examples{
#Lets create some data based on the Fisher-Pry model:
xx<-cbind(seq(0,10,by=0.1))
yy<-5*(1+tanh(xx-5))+2*0.1*(runif(dim(xx)[1])-0.5)
N<-dim(xx)[1]
#We know that total symmetry exists, so for the middle point it is most probable to compute |sl|=|sr|
j=(N-1)/2+1
#Define the left and right chord:
fl<-function(x){yy[1] + (yy[j] - yy[1]) * (x - xx[1]) / (xx[j] - xx[1])}
fr<-function(x){yy[j] + (yy[N] - yy[j]) * (x - xx[j]) / (xx[N] - xx[j])}
#Find the s-left and s-right:
L<-findipl(xx,yy,j)
print(L)
#Show all results in a plot:
xxl=xx[1:j,]
yyl=fl(xxl)
xxr=xx[j:N,]
yyr=fr(xxr)
plot(xx,yy,type="l",col="red")
lines(xxl,yyl,type="l",col="green")
lines(xxr,yyr,type="l",col="blue")
points(xx[j],yy[j], type = "p",pch = 19,col="black")
}
\keyword{ ESE }