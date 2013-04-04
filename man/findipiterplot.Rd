\name{findipiterplot}
\alias{findipiterplot}
\title{
An iterative application of direct and cross ESE and EDE methods with plots of direct results.
}
\description{
We apply the ESE and EDE methods to the initial data. If it is possible then we do ESE iterations and compute both ESE( direct) and EDE (cross) estimations. Again if it is possible we do EDE iterations and compute both EDE (direct) and ESE (cross) estimations. Finally we plot the convergence of the direct ESE and EDE iterations.
}
\usage{
findipiterplot(x, y, index)
}
\arguments{
  \item{x}{
The column vector of x-abscissas
}
  \item{y}{
The column vector of the errored or not errored y-ordinates
}
  \item{index}{
If data is convex/concave then index=0 \cr
If data is concave/convex then index=1
}
}
\details{
If data is from an unknown function and without error then we can find the inflection point by a way similar to bisection method way for root finding.
If data is errored, then we have two consistent estimators of the trapezoidal estimated inflection point, i.e. we consistently estimate what we could find by computing the relevant areas with elementary trapezoids.
}
\value{
It returns an ans environment value with 16 components:
  \item{ans$aese}{The vector of direct ESE iterations}
  \item{ans$bese}{The vector of cross ESE iterations, ie estimations with data from EDE iterations}
  \item{ans$esm}{The vector of all available ESE iterations}
  \item{ans$aede}{The vector of direct EDE iterations}
  \item{ans$bede}{The vector of cross EDE iterations, ie estimations with data from ESE iterations}
  \item{ans$edm}{The vector of all available EDE iterations}
  \item{ans$first}{The output of first run from findiplisit(x,y,index)}
  \item{ans$aesmout}{Mean, Std Deviation and 95 pc confidence interval for direct ESE iterations}
  \item{ans$besmout}{Mean, Std Deviation and 95 pc confidence interval for cross ESE iterations}
  \item{ans$esmout}{Mean, Std Deviation and 95 pc confidence interval for all ESE iterations}
  \item{ans$aedmout}{Mean, Std Deviation and 95 pc confidence interval for direct EDE iterations}
  \item{ans$bedmout}{Mean, Std Deviation and 95 pc confidence interval for cross EDE iterations}
  \item{ans$edmout}{Mean, Std Deviation and 95 pc confidence interval for all EDE iterations}
  \item{ans$ipall}{Mean, Std Deviation and 95 pc confidence interval for all available iterations}
  \item{ans$xysl}{A list of xy data frames containing the data of every direct EDE iteration}
  \item{ans$xydl}{A list of xy data frames containing the data of every direct ESE iteration}  
}
\references{
Demetris T. Christopoulos(2012).Developing methods for identifying the inflection point of a convex/ concave curve. arXiv:1206.5478v1 [math.NA]
}
\author{
Demetris T. Christopoulos
}
\note{
Useful for graphical investigation of the inflection point.
}
\seealso{
See Also as \code{\link{findiplist}}.
}
\examples{
#Lets create some convex/concave data based on a Gompertz sigmoid curve:
x<-cbind(seq(3.5,9.5,by=0.05))
y<-10*exp(-exp(5)*exp(-x))+2*0.03*(runif(dim(x)[1])-0.5)
#which has inflection point ip=5.0
#We shall find the inflection point with 4 iterative methods:
a<-findipiterplot(x,y,0);
#The vector of direct ESE iterations:
t(a$aese);
#The vector of cross ESE iterations, ie estimations with data from EDE iterations:
t(a$bese);
#The vector of all available ESE iterations:
t(a$esm);
#The vector of direct EDE iterations:
t(a$aede);
#The vector of cross EDE iterations, ie estimations with data from ESE iterations
t(a$bede);
#The vector of all available EDE iterations:
t(a$edm);
#The output of first run from findiplisit(x,y,index):
a$first;
#Mean, Std Deviation and 95 pc confidence interval for direct ESE iterations:
a$aesmout;
#Mean, Std Deviation and 95 pc confidence interval for cross ESE iterations
a$besmout;
#Mean, Std Deviation and 95 pc  confidence interval for all ESE iterations:
a$esmout;
#Mean, Std Deviation and 95 pc confidence interval for direct EDE iterations:
a$aedmout;
#Mean, Std Deviation and 95 pc confidence interval for cross EDE iterations:
a$bedmout;
#Mean, Std Deviation and 95 pc confidence interval for all EDE iterations:
a$edmout;
#Mean, Std Deviation and 95 pc confidence interval for all available iterations:
a$ipall;
#From the list of data frames you can also do plots, 1st data set:
plot(a$xysl[1:2],xlab=paste("x",1),ylab=paste("y",1))
abline(v=a$aese[1],lty=2,col="blue")
title(paste("ESE iter",1),sub=paste("ip=",a$aese[1]))
#2nd data set:
plot(a$xysl[3:4],xlab=paste("x",2),ylab=paste("y",2))
abline(v=a$aese[2],lty=2,col="blue")
title(paste("ESE iter",2),sub=paste("ip=",a$aese[2]))
}
\keyword{inflection}
\keyword{ESE }
\keyword{EDE }
\keyword{iterative}
