\name{plot.SK}
\alias{plot.SK}
\alias{plot.SK.nest}

\title{
  Plot SK and SK.nest Objects
}

\description{
  Those are S3 methods for objects of class \code{SK} and \code{SK.nest}.
}

\usage{
  \method{plot}{SK}(x, inches=TRUE, col=NULL, xlab=NULL, ylab=NULL,
    title="Groups of means divided by colors", \dots)
  \method{plot}{SK.nest}(x, inches=TRUE, col=NULL, xlab=NULL, ylab=NULL,
    title="Groups of means divided by colors", \dots)
}

\arguments{
  \item{x}{A \code{SK} or \code{SK.nest} object.}
  \item{inches}{'TRUE', 'FALSE' or a positive number.See symbols-package:graghics}
  \item{col}{A vector of colors for the thermometers representation. See symbols-package:graghics.}
  \item{xlab}{A label for the x axis.}
  \item{ylab}{A label for the y axis.}
  \item{title}{A title for the graphics.}
  \item{\dots}{Other graphical parameters may also be passed as arguments to
    this function.}
}

\details{
	The \code{plot.SK} and \code{plot.SK.nest} functions generates a series of
  thermometers where the means are marked by a line in the midle, and the
  groups are diferentiated by diferent colors.
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr
}

\references{
  Murrell, P. (2005) R Graphics. Chapman & Hall/CRC Press.
}

\seealso{\code{\link{plot}}}

\examples{
  ##
  ## Example: Factorial Experiment (FE)
  ##

  yeld <- c(53, 27, 40, 45, 43, 45, 32, 12,
            45, 57, 29, 69, 42, 32, 61, 54,
            95, 27, 24, 60, 60, 98, 11, 26)
  labs     <- factor(rep(1:2, 12))
  runs     <- factor(sort(rep(1:12, 2)))
  pressure <- gl(2, 12)
  catalyst <- factor(rep(sort(rep(1:2, 2)), 6))
  dm       <- data.frame(pressure, catalyst, labs) # Design matrix (a data.frame object)
  dfm      <- data.frame(labs, pressure, catalyst, yeld)

  ## PARAMETERS ARE THE DESIGN MATRIX AND THE RESPONSE VARIABLE
  ## MAIN FACTOR ANALYSIS
  ## Main factor = catalyst
  sk1 <- SK(x=dm, y=yeld, model='yeld ~ labs * catalyst * pressure',
            which='catalyst')
  summary(sk1)
  plot(sk1, title='catalyst')

  ## Main factor = pressure
  sk2 <- SK(x=dm, y=yeld, model='yeld ~ labs * catalyst * pressure',
            which='pressure')
  summary(sk2)
  plot(sk2, title='pressure')

  ## NESTED ANALYSIS
  ## Nested catalyst/pressure=1 -> SK.nest
  nsk1 <- SK.nest(x=dm, y=yeld, model='yeld ~ labs * catalyst * pressure',
                  which='catalyst:pressure', fl2=1)
  summary(nsk1)
  plot(nsk1, title='catalyst/pressure=1')

  ## Nested catalyst/pressure=2 -> SK.nest
  nsk2 <- SK.nest(x=dm, y=yeld, model='yeld ~ labs * catalyst * pressure',
                  which='catalyst:pressure', fl2=2)
  summary(nsk2)
  plot(nsk2, title = 'catalyst/pressure=2')

  ## Nested catalyst/pressure=2/labs=1 -> SK.nest
  nsk3 <- SK.nest(x=dm, y=yeld, model='yeld ~ labs * catalyst * pressure',
                  which='labs:catalyst:pressure', fl2=2, fl3=1)
  summary(nsk3)
  plot(nsk3, title='catalyst/pressure=2/labs=1')
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

