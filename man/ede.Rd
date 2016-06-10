\name{ede}
\alias{ede}
\title{
The Extremum Distance Estimator (EDE) for Finding the Inflection Point of a Convex/Concave Curve
}
\description{
Implementation of EDE method as defined in [1] and [2] by giving a simple output of the method.
}
\usage{
ede(x, y, index)
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
We also obtain the x_F1 and x_F2 points, see [1], [2].
}
\value{
A matrix of size 1 x 3 is returned with elements:
    \item{A(1,1)=i1}{The index jF1 for EDE method}
  \item{A(1,2)=i2}{The index jF2 for EDE method}
  \item{A(1,3)=chi_S}{The Extremum Distance Estimator (EDE) for inflection point}
}
\references{
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve. arXiv:1206.5478v2 [math.NA],2012.\cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point, International Journal of Mathematics and Scientific Computing, (ISSN: 2231-5330), vol. 6(1), 2016.
}
\author{
Demetris T. Christopoulos
}
\note{
This function is for real big data sets, more than one million rows. It is the fastest available method, see [2] for comparison to other methods.\cr
New function in version 1.2
}
\seealso{
See also the iterative version \code{\link{bede}}.
}
\examples{
#Fisher-pry model with heavy noise, unequal spaces
#and 1 million cases:
N=10^6+1;
set.seed(2016-06-09);x=sort(runif(N,0,10));y=5+5*tanh(x-5)+runif(N,-1,1);
ptm <- proc.time()
tede=ede(x,y,0);tede;
proc.time() - ptm
#
}
\keyword{ede}