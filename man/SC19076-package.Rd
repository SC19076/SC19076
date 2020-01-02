\name{SC19076-package}
\alias{SC19076-package}
\alias{SC19076}
\docType{package}
\title{Nonparametric kernel estimation
}
\description{
  SC19076 is a simple R package developed to Compare GM algorithm and NW algorithm of two algorithms in non-parametric kernel regression,
  comparing the estimation of the regression part in non-parametric kernel regression by these two algorithms, 
  and setting different bandwidth to study the performance of these two algorithms under different bandwidth.
}
\details{
  Before using these two functions, the parameters of the function should be set first. Here, the function has three parameters,
    namely x,y,h.x represents the randomly generated seq sequence,y represents the real distribution, and h represents the bandwidth.
  Two algorithms are used to compare their performance in the non-parametric kernel estimation, 
  and then different bandwidths are set to compare the performance of the two algorithms.
}
\author{
Mengyao Chen, chenmy19@mail.ustc.edu.cn.

Maintainer: Mengyao Chen <chenmy19@mail.ustc.edu.cn>
}
\references{
  This optional section can contain literature or other references for
  background information.
}
\keyword{ SC19076 }
\seealso{
  Optional links to other man pages
}
\examples{
  \dontrun{
     ## Optional simple examples of the most important functions 
     ## These can be in \dontrun{} and \donttest{} blocks.     
  }
}
