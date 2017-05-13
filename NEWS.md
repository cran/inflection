## What's new in version 1.3

# Use of parallel computing under request (doparallel=TRUE) for the functions:

ese()

bese()

findiplist()

findipiterplot()

##Repair of function findipiterplot()
1. Fix bugs in calling the function
1. Remove indirect methods CRESE, CREDE due to their limited functionality
1. Plot in two separate pdfs 'ese_iterations.pdf' and 'ede_iterations.pdf'
1. Check for the existence of sufficient number of results before creating confidence intervals

###What's new in version 1.2
1. The function eixf(x, y, f, i) was removed as not essentially necessary
2. New functions with self declaring names were added:
*ese(x,y,index)
*bese(x,y,index)
*ede(x,y,index)
*edeci(x,y,index)
*bede(x,y,index)
All functions require length(x)>=4 in order to create numeric output
3. Thef unction findipiterplot(x,y,index) was improved and became 
 findipiterplot(x, y, index, plots = TRUE, crossrun = FALSE, ci = FALSE)
4. Changes in NAMESPACE, added folder inst/CITATION
5. Reference to new paper:


Demetris T. Christopoulos, [On the efficient identification of an inflection point] (https://www.researchgate.net/publication/304557351), International Journal of Mathematics and Scientific Computing, (ISSN: 2231-5330), vol. 6(1), 2016