\name{findiplist}
\alias{findiplist}
\title{
The Extremum Surface Estimator (ESE) and Extremum Distance Estimator (EDE) Methods for Finding the Inflection Point of a Convex/Concave Curve.
}
\description{
Given the (xi,yi), i=1,...,N noisy or not data we want to estimate the inflection point of the corresponding curve.
The curve can be convex before the inflection point and then concave or vice versa.
The ESE and EDE methods are applied and the results are returned as a matrix.
}
\usage{
findiplist(x, y, index)
}
\arguments{
  \item{x}{
The numeric vector of x-abscissas, must be of length at least 4.
}
  \item{y}{
The numeric vector of the noisy or not y-ordinates, must be of length at least 4.
}
  \item{index}{
If data is convex/concave then index=0 \cr
If data is concave/convex then index=1
}
}
\details{
If data is from an unknown function and without error then we can find the inflection point in a way similar to that of bisection method' s way for a root.
If data is noisy, then we have two consistent estimators of the inflection point.
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
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve, arXiv:1206.5478v2 [math.NA], 2012.\cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016.
}
\author{
Demetris T. Christopoulos
}
\note{
It is a simple implementation of both ESE and EDE methods to a data set of at least 4 xy-pairs.
}
\examples{
#Lets create some convex/concave data based on the Fisher-Pry model
#by using 1001 not equal spaced abscissas with data right asymmetry
N=1001;
#Case I: data without noise
set.seed(2016-06-10);x=sort(runif(N,0,15));y=5+5*tanh(x-5);
A=findiplist(x,y,0);A;
plot(x,y,type="l",xaxt="n",lwd=2);axis(1,at=seq(0,15));
abline(v=A[,3],col=c("blue","red"))
text(A[1,3]+0.5,0,expression(chi[S]),font=2);
text(A[2,3]-0.5,0,expression(chi[D]),font=2);
grid();
#
###Case II: noisy data
set.seed(2016-06-10);x=sort(runif(N,0,15));y=5+5*tanh(x-5)+rnorm(N,0,0.05);
A=findiplist(x,y,0);A;
plot(x,y,type="l",xaxt="n",lwd=2);axis(1,at=seq(0,15));
abline(v=A[,3],col=c("blue","red"))
text(A[1,3]+0.5,0,expression(chi[S]),font=2);
text(A[2,3]-0.5,0,expression(chi[D]),font=2);
grid();
#
}