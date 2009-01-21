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
  ## Example: Split-plot Experiment (SPE)
  ##

  ## Experimental structure:
  ##                  +------------------------------------+
  ##                  |             Block                  |
  ## +---------+------+------------------------------------+
  ## | Variety | Man. |    I    II   III    IV     V    VI |
  ## +---------+------+------------------------------------+
  ## |   Ladak |   A  | 2.17  1.88  1.62  2.34  1.58  1.66 |
  ## |         |   B  | 1.58  1.26  1.22  1.59  1.25  0.94 |
  ## |         |   C  | 2.29  1.60  1.67  1.91  1.39  1.12 |
  ## |         |   D  | 2.23  2.01  1.82  2.10  1.66  1.10 |
  ## +---------+------+------------------------------------+
  ## | Cossack |   A  | 2.33  2.01  1.70  1.78  1.42  1.35 |
  ## |         |   B  | 1.38  1.30  1.85  1.09  1.13  1.06 |
  ## |         |   C  | 1.86  1.70  1.81  1.54  1.67  0.88 |
  ## |         |   D  | 2.27  1.81  2.01  1.40  1.31  1.06 |
  ## +---------+------+------------------------------------+
  ## |  Ranger |   A  | 1.75  1.95  2.13  1.78  1.31  1.30 |
  ## |         |   B  | 1.52  1.47  1.80  1.37  1.01  1.31 |
  ## |         |   C  | 1.55  1.61  1.82  1.56  1.23  1.13 |
  ## |         |   D  | 1.56  1.72  1.99  1.55  1.51  1.33 |
  ## +---------+-------------------------------------------+

  ## Generating data
  y <- c(2.17, 1.88, 1.62, 2.34, 1.58, 1.66,
         1.58, 1.26, 1.22, 1.59, 1.25, 0.94,
         2.29, 1.60, 1.67, 1.91, 1.39, 1.12,
         2.23, 2.01, 1.82, 2.10, 1.66, 1.10,
         2.33, 2.01, 1.70, 1.78, 1.42, 1.35,
         1.38, 1.30, 1.85, 1.09, 1.13, 1.06,
         1.86, 1.70, 1.81, 1.54, 1.67, 0.88,
         2.27, 1.81, 2.01, 1.40, 1.31, 1.06,
         1.75, 1.95, 2.13, 1.78, 1.31, 1.30,
         1.52, 1.47, 1.80, 1.37, 1.01, 1.31,
         1.55, 1.61, 1.82, 1.56, 1.23, 1.13,
         1.56, 1.72, 1.99, 1.55, 1.51, 1.33)
  var <- sort(gl(3, 24, lab=c('Ladak', 'Cossack', 'Ranger')))
  man <- rep(gl(4, 6, lab=LETTERS[1:4]), 3)
  sub <- rep(gl(4, 6), 3)
  blo <- factor(rep(1:6, 12))
  dm  <- data.frame(var, man, sub, blo) # Design matrix (a data.frame object)
  dfm <- data.frame(var, sub, man, blo, y)

  ## PARAMETERS ARE THE DESIGN MATRIX AND THE RESPONSE VARIABLE
  ## MAIN FACTOR ANALYSIS
  ## Main factor = var
  sk1 <- SK(x=dm, y=y, model='y ~ blo + man*var + Error(blo/var)', which='var',
            error ='blo:var')
  summary(sk1)
  plot(sk1)

  ## Main factor = man
  sk2 <- SK(x=dm, y=y, model='y ~ blo + man*var + Error(blo/var)', which='man',
            error ='Within', sig.level=0.025 )
  summary(sk2)
  plot(sk2, title='man', xlab='Groups', ylab='Group means')

  ## NESTED ANALYSIS
  ## Nested man/var=1 -> SK.nest
  sk3 <- SK.nest(x=dm, y=y, model='y ~ blo + man*var + Error(blo/var)',
                 which='man:var', error ='Within', fl2=1 )
  summary(sk3)
  plot(sk3, title='man/var=1')
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

