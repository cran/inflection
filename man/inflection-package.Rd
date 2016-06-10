\name{inflection-package}
\alias{inflection-package}
\alias{inflection}
\docType{package}
\title{
Finds the Inflection Point of a Curve
}
\description{
Implementation of methods Extremum Surface Estimator and Extremum Distance Estimator for identifying the inflection point of a convex/concave curve.\cr
Methods used: ESE, BESE, EDE, BEDE, while "B" stands for prefix "Bisection", ie iterative method.\cr
}
\details{
\tabular{ll}{
Package: \tab inflection\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2016-06-10\cr
License: \tab GPL (>=2)\cr
}
The x,y data should be numeric vectors of length at least 4.\cr
Examples of usage for a convex/concave curve:\cr
findiplist(x,y,0)\cr
ede(x,y,0)\cr
bede(x,y,0)\cr
ese(x,y,0)\cr
bese(x,y,0)\cr
For a concave/convex curve:\cr
findiplist(x,y,1)\cr
and similar for other functions.
}
\author{
Demetris T. Christopoulos \cr
Maintainer: Demetris T. Christopoulos <dchristop@econ.uoa.gr>
}
\references{
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve. arXiv:1206.5478v1 [math.NA], 2012 \cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016
}
\keyword{inflection}
\examples{
#Lets create some convex/concave data based on the Fisher-Pry model
#by using 1001 not equal spaced abscissas with data right asymmetry
N=1001;
#Case I: not noisy data
#
set.seed(2016-06-09);x=sort(runif(N,0,15));y=5+5*tanh(x-5);
tese=ese(x,y,0);tese;#...simple run of ESE
tede=ede(x,y,0);tede;#...simple run of EDE
edeci(x,y,0);#...Run EDE and compute 95% Chebyshev c.i.
eseit=bese(x,y,0);#...Bisection ESE (BESE)
eseit$iplast#...last estimation for inflection point
eseit$iters#...all iterations done...
edeit=bede(x,y,0);#...Bisection EDE (BEDE)
edeit$iplast#...last estimation for inflection point
edeit$iters#...all iterations done
A=findiplist(x,y,0);A;#...Run both ESE & EDE at once...
#Let's plot some interesting approximately results.
plot(x,y,type="l",xaxt="n",lwd=2);axis(1,at=seq(0,x[N]));
lines(c(x[1],x[A[1,2]]),c(y[1],y[A[1,2]]),col="green",lty=2);
lines(c(x[N],x[A[1,1]]),c(y[N],y[A[1,1]]),col="blue",lty=2);
lines(c(x[1],x[N]),c(y[1],y[N]),col="black",lty=2);
abline(v=A[,3],col=c('blue','red'),lty=2);
points(x[A[1,1:2]],y[A[1,1:2]], type = "p",pch = 19,col="blue",font=2);
points(x[A[2,1:2]],y[A[2,1:2]], type = "p",pch = 19,col="red",font=2);
text(A[1,3]+0.5,0,expression(chi[S]),font=2);
text(A[2,3]-0.5,0,expression(chi[D]),font=2);
grid();
#
###Case II: noisy data
set.seed(161718);x=sort(runif(N,0,15));
r=0.1;y=5+5*tanh(x-5)+rnorm(N,0,0.05);
tese=ese(x,y,0);tese;#...simple run of ESE
tede=ede(x,y,0);tede;#...simple run of EDE
edeci(x,y,0);#...Run EDE and compute 95% Chebyshev c.i.
eseit=bese(x,y,0);#...Bisection ESE (BESE)
eseit$iplast#...last estimation for inflection point
eseit$iters#...all iterations done...
edeit=bede(x,y,0);#...Bisection EDE (BEDE)
edeit$iplast#...last estimation for inflection point
edeit$iters#...all iterations done
A=findiplist(x,y,0);A;#...Run both ESE & EDE at once...
#Let's plot some interesting approximately results.
plot(x,y,type="l",xaxt="n",lwd=2);axis(1,at=seq(0,x[N]));
lines(c(x[1],x[A[1,2]]),c(y[1],y[A[1,2]]),col="green",lty=2);
lines(c(x[N],x[A[1,1]]),c(y[N],y[A[1,1]]),col="blue",lty=2);
lines(c(x[1],x[N]),c(y[1],y[N]),col="black",lty=2);
abline(v=A[,3],col=c('blue','red'),lty=2);
points(x[A[1,1:2]],y[A[1,1:2]], type = "p",pch = 19,col="blue",font=2);
points(x[A[2,1:2]],y[A[2,1:2]], type = "p",pch = 19,col="red",font=2);
text(A[1,3]+0.5,0,expression(chi[S]),font=2);
text(A[2,3]-0.5,0,expression(chi[D]),font=2);
grid();
#
}
