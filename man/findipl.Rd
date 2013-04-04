\name{findipl}
\alias{findipl}
\title{
Finds the s-left and s-right for a given internal point x
}
\description{
From the definitions (4), (5) and (23) of reference it is necessary to find s_l and s_r in order to estimate the Extremum Surface Estimator (ESE) of the inflection point.
}
\usage{
findipl(x, y, j)
}
\arguments{
  \item{x}{
The column vector of x-abscissas
}
  \item{y}{
The column vector of the errored or not errored y-ordinates
}
  \item{j}{
The data index j such that x=x[j]
}
}
\details{
This function is using function eixf() for computing the elemenatry areas by the trapezoidal rule.
}
\value{
A list is returned that contains
\item{j}{The data index j such that x=x[j]}
\item{x=x[j]}{The corresponding x-abscissa}
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
x<-cbind(seq(0,10,by=0.1))
y<-5*(1+tanh(x-5))+2*0.1*(runif(dim(x)[1])-0.5)
N<-dim(x)[1]
#We know that total symmetry exists, so for the middle point it is most probable to compute |sl|=|sr|
j=(N-1)/2+1
#Define the left and right chord:
fl<-function(t){y[1] + (y[j] - y[1]) * (t - x[1]) / (x[j] - x[1])}
fr<-function(t){y[j] + (y[N] - y[j]) * (t - x[j]) / (x[N] - x[j])}
#Find the s-left and s-right:
L<-findipl(x,y,j)
print(L)
#Show all results in a plot:
xl=x[1:j,]
yl=fl(xl)
xr=x[j:N,]
yr=fr(xr)
plot(x,y,type="l",col="red")
lines(xl,yl,type="l",col="green")
lines(xr,yr,type="l",col="blue")
points(x[j],y[j], type = "p",pch = 19,col="black")
}
\keyword{ ESE }