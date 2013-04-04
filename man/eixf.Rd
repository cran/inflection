\name{eixf}
\alias{eixf}
\title{
Computes elementary surfaces via trapezoidal rule
}
\description{
Computes the trapezoidal estimation of the surface between a line-chord and the data-curve for the interval [x[i],x[i+1]
}
\usage{
eixf(x, y, f, i)
}
\arguments{
  \item{x}{The column vector of x data}
  \item{y}{The column vector of y data that contains or not errors}
  \item{f}{The function that describes the proper line chord}
  \item{i}{The running index}
}
\details{
By adding the values of eixf(x, y, f, i) for a proper range of i we can find the signed value of the surface between our curve and the linear chord that is used from ESE method.
}
\value{
Returns the surface between the line f(x) and the part of the curve starting from [x[i],y[i]] and ending to [x[i+1,y[i+1]]] \cr
The value is positive if the curve is above (convex) the linear chord and negative (concave) vice versa.}
\references{Demetris T. Christopoulos(2012). Developing methods for identifying the inflection point of a convex/concavecurve. arXiv:1206.5478v1 [math.NA]
}
\author{Demetris T. Christopoulos}
\examples{
#(1)Find the distance for i=10, i+1=11 and for Fisher-Pry model:
#options(digits=16)
x<-cbind(seq(0,10,by=0.1))
y<-5*(1+tanh(x-5))+2*0.1*(runif(dim(x)[1])-0.5)
N<-dim(x)[1]
#Define the total chord:
f<-function(t){y[1] + (y[N] - y[1]) * (t - x[1]) / (x[N] - x[1])}
s<-eixf(x,y,f,10)
print(s)
#(2)Find the algebraic area between data and total chord:
stot<-0.0
s<-c()
for (i in 2:N-1)
{ 
s[i]<-eixf(x,y,f,i);
stot<-stot+eixf(x,y,f,i)
}
print(s)
print(stot)
#It is almost zero because of the total symmetry presented at this example
#Look at the relevant plots of data (red) and total chord (green):
plot(x,y,type="l",col="red")
lines(x,f(x),type="l",col="green")
}
\keyword{area}