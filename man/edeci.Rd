\name{edeci}
\alias{edeci}
\title{
An Improved Version of EDE that Provides Us with a Chebyshev Confidence Interval for Inflection Point
}
\description{
It computes except from the common EDE output the Chebyshev confidence interval based on Chebyshev inequality.
}
\usage{
edeci(x, y, index, k = 5)
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
  \item{k}{
According to Chebyshev's inequality we find a relevant Chebyshev confidence interval of 
the form \eqn{[\mu-k\,\sigma,\mu+k\,\sigma]}{[mu-k*sigma,mu+k*sigma]}
}
}
\details{
We define as Chebyshev confidence interval the
\deqn{[\mu-k\,\sigma,\mu+k\,\sigma]}{[mu-k*sigma,mu+k*sigma]}
where usually k=5 because it corresponds to 96\%, while an estimator of \eqn{\sigma}{sigma} is given by \eqn{s_{D}}, see Eq. (29) of [2]:\cr
\deqn{s_{D}^2=\frac{1}{2}\,s^2=\frac{1}{2}\,\sum\limits_{i=1}^{n}{(\frac{y_{i}-y_{i-1}}{2})^2}}{1/n Sum(((y[i]-y[i-1])/2)^2,i=1..n)}
}
\value{
A one row matrix with elements the output of EDE, the given k and the Chebyshev c.i.
}
\references{
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve, arXiv:1206.5478v2 [math.NA], 2012.\cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016.
}
\author{
Demetris T. Christopoulos
}
\note{
This function works better if the noise is of a "zig-zag"" pattern, see [2].\cr
New function in version 1.2
}
\seealso{
See also the simple version \code{\link{ede}}.
}
\examples{
#Gompertz model with noise, unequal spaces
#and 1 million cases:
N=10^6+1;
set.seed(2016-06-10);x=sort(runif(N,0,10));y=10*exp(-exp(5)*exp(-x))+runif(N,-0.05,0.05);
#EDE one time only
ede(x,y,0)
#Not so close to the exact point
#Let's reduce the size using BEDE
iters=bede(x,y,0)$iters;iters;
#Now we choose pre-last interval, in order for EDE to be applicable
ab=apply(iters[dim(iters)[1]-1,c('a','b')],2,function(xx,x){which(x==xx)},x);ab;
#Apply edeci...
edeci(x[ab[1]:ab[2]],y[ab[1]:ab[2]],0)
#Very close to the true inflection point.
#
}
\keyword{edeci}