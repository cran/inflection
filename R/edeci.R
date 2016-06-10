edeci <-
function(x,y,index,k=5)
  { #Value and Chebyshev confidence interval (mean-k*s,mean+k*s) for EDE as defined in papers:
    #
    #Demetris T. Christopoulos, 'On the efficient identification of an inflection point',
    #International Journal of Mathematics and Scientific Computing,(ISSN: 2231-5330), VOL. 6, NO. 1, 2016
    #
    #Demetris T. Christopoulos, 'Developing methods for identifying the inflection point
    #of a convex/ concave curve'. arXiv:1206.5478v2 [math.NA], 2012
    #Contact Emails: dchristop@econ.uoa.gr or dem.christop@gmail.com
    #
    tede=ede(x,y,index);m=tede[3];
    #
    ifelse(!(is.nan(m)),
           {
             dy=diff(y,1,1);ss=0.25*sum(dy^2);s=sqrt(2*ss/(length(y)-1));
             #Find CI...
             xleft=m-k*s;xright=m+k*s;
           }
           ,
           {xleft=NaN;xright=NaN;}
    )
    #
    out=matrix(c(tede,k,xleft,xright),nrow=1,ncol=6,byrow=TRUE);rownames(out)="EDE";
    colnames(out)=c('j1','j2','chi',"k",paste0('chi-',k,'*s'),paste0('chi+',k,'*s'));
    return(out)
  }
