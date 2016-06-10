\name{findipiterplot}
\alias{findipiterplot}
\title{
An Iterative Application of Direct and Cross ESE and EDE Methods with Plots of Direct Results
}
\description{
We apply the ESE and EDE methods to the initial data. If it is possible then we do ESE iterations and compute both ESE( direct) and EDE (cross) estimations.
 Again if it is possible we do EDE iterations and compute both EDE (direct) and ESE (cross) estimations.
 Finally we plot the sequence of the direct ESE and EDE iterations and compute 95\% confidence intervals for all available methods.
}
\usage{
findipiterplot(x, y, index, plots = TRUE, crossrun = FALSE, ci = FALSE)
}
\arguments{
  \item{x}{
The numeric vector of x-abscissas, must be of length at least 4.
}
  \item{y}{
The numeric vector of the noisy or not y-ordinates
}
  \item{index}{
If data is convex/concave then index=0 \cr
If data is concave/convex then index=1
}
\item{plots}{
When plots==TRUE the plot commands are executed
}
\item{crossrun}{
When crossrun==TRUE the cross ESE and EDE are computed, if possible
}
\item{ci}{
When ci==TRUE the 95\% confidence intervals are computed, if possible
}
}
\details{
It applies when that is theoretically allowable the methods ESE, EDE, stores all useful result and according to 
the input computes 95\% confidences intervals and plots the two sequences (BESE, BEDE) of results. 
}
\value{
It returns an ans environment value with 16 components:
  \item{ans$BESE}{The vector of direct ESE iterations}
  \item{ans$CRESE}{The vector of cross ESE iterations, ie estimations with data from EDE iterations}
  \item{ans$esm}{The vector of all available ESE iterations}
  \item{ans$BEDE}{The vector of direct EDE iterations}
  \item{ans$CREDE}{The vector of cross EDE iterations, ie estimations with data from ESE iterations}
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
[1]Demetris T. Christopoulos, Developing methods for identifying the inflection point of a convex/ concave curve, arXiv:1206.5478v2 [math.NA], 2012.\cr
[2]Demetris T. Christopoulos, On the efficient identification of an inflection point,International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), vol. 6(1), 2016.
}
\author{
Demetris T. Christopoulos
}
\note{
Useful for graphical investigation of the inflection point.
}
\seealso{
See also \code{\link{bese}} and \code{\link{bede}}.
}
\examples{
#Lets create some convex/concave data based on the Fisher-Pry model
x<-seq(0,10,by=0.05);
set.seed(161718);y<-5*(1+tanh(x-5))+runif(length(x),-0.1,0.1);
#The exact inflection point is ip=5.0
#Because of the total symmetry we expect inflection point near the middle of x-range:
A<-findiplist(x,y,0);A;#Our expectation came true
#Let's make some ESE and EDE iterations and plot them:
a<-findipiterplot(x,y,0,TRUE,TRUE,TRUE);
a$first;#Show first solution
a$BESE;#Show ESE iterations
a$CRESE;#Show cross ESE iterations from EDE iterations
a$esm;#Show all ESE iterations
a$BEDE;#Show EDE iterations
a$CREDE;#Show cross EDE iterations from ESE iterations
a$edm;#Show all EDE iterations
a$aesmout;#Statistics and 95\%c c.i. for ESE
a$besmout;#Statistics and 95\%c c.i. for cross EsE
a$aedmout;#Statistics and 95\%c c.i. for EDE
a$bedmout;#Statistics and 95\%c c.i. for cross EDE
a$esmout;#Statistics and 95\%c c.i. for all ESE results
a$edmout;#Statistics and 95\%c c.i. for all EDE results
a$ipall;#Statistics and 95\%c c.i. for results
#Close the 2 previously opened devises
dev.off();dev.off();
#
}
\keyword{inflection}
\keyword{ese}
\keyword{ede}
\keyword{iterative}