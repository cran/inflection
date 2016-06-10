\name{bese}
\alias{bese}
\title{
Bisection Extremum Surface Estimator Method
}
\description{
It iterates in a way similar to the well known bisection method in root finding, with the only exception is that 
our \eqn{[a_{n},b_{n}]}{[a_n,b_n]} intervals contain the inflection point now and the rule for choosing them follows definitions
and Lemmas of [1], [2].
}
\usage{
bese(x, y, index)
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
This function is suitable for making a 'fine tuning' while searching for inflection point.
For very large data sets it is better using first EDE method, see \code{\link{ede}}.
Then we apply BESE at a smaller range.
}
\value{
It returns a list of two elements: \cr
\item{iplast}{the last estimation found}
\item{iters}{a matrix with 4 columns ("n", "a", "b", "ESE") that give the number of x-y pairs 
used at each iteration, the [a,b] range where we searched and the ESE estimated inflection point.}
}
\references{
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve, arXiv:1206.5478v2 [math.NA], 2012.\cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016.
}
\author{
Demetris T. Christopoulos
}
\note{
New function in version 1.2
}
\seealso{
See also the simple version \code{\link{ese}}.
}
\examples{
#Fisher-pry model with noise and 50k cases:
N=5*10^4+1;
set.seed(2016-06-10);x=seq(0,15,length.out = N);y=5+5*tanh(x-5)+runif(N,-0.25,0.25);
#We first run BEDE to find a smaller neighborhood for inflection point
iters=bede(x,y,0)$iters;
iters;
#Now we find last interval
ab=apply(iters[dim(iters)[1],c('a','b')],2,function(xx,x){which(x==xx)},x);ab;
#Apply BESE to that
eseit=bese(x[ab[1]:ab[2]],y[ab[1]:ab[2]],0)
eseit$iplast
eseit$iters
#
}
\keyword{bese}
