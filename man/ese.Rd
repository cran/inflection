\name{ese}
\alias{ese}
\title{
The Extremum Surface Estimator (ESE) for Finding the Inflection Point of a Convex/Concave Curve
}
\description{
Implementation of ESE method as defined in [1] and [2] by giving a simple output of the method.
}
\usage{
ese(x, y, index)
}
%- maybe also 'usage' for other objects documented here.
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
If data is from an unknown function and without noise then we can find the inflection point by a way similar to bisection method way for root finding.
If data is noisy, then we have two consistent estimators of the trapezoidal estimated inflection point, i.e. we consistently estimate what we could find by computing the relevant areas with elementary trapezoids.
This method is not so fast as \code{\link{ede}} but it can be used for a fine-tuning of the result returned be EDE.
}
\value{
A matrix of size 1 x 3 is returned with elements:
  \item{A(1,1)=i1}{The index j-right for ESE method}
  \item{A(1,2)=i2}{The index j-left for ESE method}
  \item{A(1,3)=chi_S}{The Extremum Surface Estimator (ESE) for inflection point}
}
\references{
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve, arXiv:1206.5478v2 [math.NA], 2012.\cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016.
}
\author{
Demetris T. Christopoulos
}
\note{
For large data sets (one million rows) it is better to use first \code{\link{ede}} or \code{\link{bede}}
in order to locate a smaller neighbour of the inflection point. Then ese gives a better estimation, since it uses 
surfaces and not distances from total chord.\cr
New function in version 1.2
}
\seealso{
See also the iterative version \code{\link{bese}}.
}
\examples{
#Fisher-pry model with heavy noise and unequal spaces.
N=1001;
set.seed(2016-06-09);x=sort(runif(N,0,10));y=5+5*tanh(x-5)+runif(N,-1,1);
plot(x,y,type='l',ylab=expression(5+5*tanh(x-5)+epsilon[i]))
tese=ese(x,y,0);tese;
abline(v=tese[3],col='blue')
#
}
\keyword{ese}