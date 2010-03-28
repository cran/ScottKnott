\name{summary}
\alias{summary.SK}
\alias{summary.SK.nest}

\title{
  Summary Method for SK and SK.nest Objects
}

\description{
  Returns (and prints) a summary list for \code{SK} and \code{SK.nest} objects.
}

\usage{
  \method{summary}{SK}(object, \dots)
  \method{summary}{SK.nest}(object, \dots)
}

\arguments{
  \item{object}{A given object of the class \code{SK} or \code{SK.nest}.}
  \item{\dots}{Potential further arguments (require by generic).}
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr
}

\references{
  Chambers, J. M. and Hastie, T. J. (1992) Statistical Models in S.
  Wadsworth & Brooks/Cole.
}

\seealso{\code{\link{ScottKnott}}}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: vectors, design matrix and the response variable,
  ## data.frame or aov
  data(CRD2)
  
  ## From: design matrix (dm) and response variable (y)
  sk1 <- with(CRD2, SK(x=dm, y=y, model='y ~ x', which='x', sig.level=0.005,
    id.trim=5))
  summary(sk1)

  ##
  ## Example: Randomized Complete Block Design (RCBD)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(RCBD)
  
  ## Design matrix (dm) and response variable (y)
  sk1 <- with(RCBD, SK(x=dm, y=y, model='y ~ blk + tra', which = 'tra'))
  summary(sk1)

  ##
  ## Example: Latin Squares Design (LSD)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(LSD)
  
  ## From: design matrix (dm) and response variable (y)
  sk1 <- with(LSD, SK(x=dm, y=y, model='y ~ rows + cols + tra', which='tra'))
  summary(sk1)

  ##
  ## Example: Factorial Experiment (FE)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(FE)
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: N
  sk1 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N * P * K', 
    which='N'))
  summary(sk1)

  ## Nested: N/P=1
  nsk1 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
    which='N:P', fl2=1))
  summary(nsk1)

  ## Nested: N/P=2/K=2
  nsk2 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
    which='N:P:K', fl2=2, fl3=2))
  summary(nsk2)

  ## Nested: P/N=1/K=1
  nsk3 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + P * N * K',
    which='P:N:K', fl2=1, fl3=1))
  summary(nsk3)

  ## Nested: K/N=1/P=1
  nsk4 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + K * N * P',
    which='K:N:P', fl2=1, fl3=1))
  summary(nsk4)

  ##
  ## Example: Split-plot Experiment (SPE)
  ## More details: demo(package='ScottKnott')
  ##
  
  data(SPE)

  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: p
  sk1 <- with(SPE, SK(x=dm, y=y, model='y ~ blk + sp*p + Error(blk/p)',
    which='p', error ='blk:p'))
  summary(sk1)

  ## Nested: sp/p=1
  skn1 <- with(SPE, SK.nest(x=dm, y=y, model='y ~ blk + sp*p + Error(blk/p)',
    which='sp:p', error ='Within', fl2=1 ))
  summary(skn1)

  data(SSPE)

  ## From: design matrix (dm) and response variable (y)
  ## Main factor: p
  sk1 <- with(SSPE, SK(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
    which='p', error='blk:p'))
  summary(sk1)
  
  # Main factor: sp
  sk2 <- with(SSPE, SK(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
    which='sp', error='blk:p:sp', sig.level=0.025))
  summary(sk2)
  
  # Main factor: ssp
  sk3 <- with(SSPE, SK(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
    which='ssp', error='Within', sig.level=0.1))
  summary(sk3)
  
  ## Nested: sp/p=1
  skn1 <- with(SSPE, SK.nest(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
    which='sp:p', error='blk:p:sp', fl2=1))
  summary(skn1)

  ## From: aovlist
  av <- with(SSPE, aov(y ~  blk + ssp*sp*p + Error(blk/p/sp), data=dfm))
  summary(av)   

  ## Nested: ssp/sp/p (at various levels of sp and p) 
  skn2 <- SK.nest(av, which='ssp:sp:p', error='Within', fl2=1, fl3=1)
  summary(skn2)

  skn3 <- SK.nest(av, which='ssp:sp:p', error='Within', fl2=2, fl3=1)
  summary(skn3)
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

