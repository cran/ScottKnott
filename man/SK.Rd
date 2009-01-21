\name{SK}
\alias{SK}
\alias{SK.default}
\alias{SK.aov}
\alias{SK.aovlist}

\title{
  The ScottKnott Clustering Algoritm for Single Experiments
}

\description{
  These are methods for objects of class \code{vector}, \code{matrix} or
  \code{data.frame} joined as default, \code{aov} and \code{aovlist} for
  single experiments.
}

\usage{
  \method{SK}{default}(x, y=NULL, model, which, error, sig.level=.05, \dots)
  \method{SK}{aov}(x, which=NULL, sig.level=.05, \dots)
  \method{SK}{aovlist}(x, which, error, sig.level=.05, \dots)  
}

\arguments{
  \item{x}{A design matrix, \code{data.frame} or an \code{aov} object.}
  \item{y}{A vector of response variable. It is necessary to inform this
    parameter only if \code{x} represent the design matrix}
  \item{which}{The name of the treatment to be used in the comparison.
    The name must be inside quoting marks.}
  \item{model}{If x is a \code{data.frame} object, the model to be used in the
    aov must be specified.}
  \item{error}{The error to be considered.}
  \item{sig.level}{Level of Significance used in the SK algorithm to create
    the groups of means. The default value is 0.05.}
  \item{\dots}{Potential further arguments (require by generic).}    
}

\details{
  The function \code{SK} returns an object of class \code{SK} 
  respectivally containing the groups of means plus other
  necessary variables for summary and plot.
  
  The generic functions \code{summary} and \code{plot} are used to obtain and
  print a summary and a plot of the results.
}

\value{
  The function \code{SK} returns a list of the class \code{SK} with the slots:
  \item{av}{A \code{list} storing the result of \code{aov}.}
  \item{groups}{A vector of length equal the number of treatments marking the groups generated.}
  \item{nms}{A vector of the labels of the treatments.}
  \item{ord}{A vector which keeps the position of the means of the treatments in decreasing order.}
  \item{means}{A vector which keeps the means of the treatments in decreasing order.}
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.}
  \item{mnumber}{A vector of length 1 giving the number of treatments.}
  \item{r}{A vector of length 1 giving the number of replicates.}
  \item{which}{The name of the factor whose levels were tested.}
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr
}

\references{
  Ramalho MAP, Ferreira DF, Oliveira AC 2000. \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Scott RJ, Knott M 1974. A cluster analysis method for grouping mans in the
  analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.
}

\examples{
  ##
  ## Example: Split-split-plot Experiment (SSPE)
  ##

  A   <- gl(3, 60, 180, 1:3)
  C   <- rep(gl(3, 20, 60, 1:3), 3)
  D   <- rep(gl(5, 4, 20, 1:5), 9)
  blo <- rep(gl(4, 1, 4, 1:4), 45)

  y <- c(
    3029, 3857, 2448, 2448, 2438, 3086, 3771, 4657, 3543, 4314, 3888, 3945,
    4318, 4514, 4497, 4215, 4610, 4590, 4499, 4545, 3448, 3600, 4267, 3895,
    3533, 5048, 3467, 4095, 3770, 3840, 3699, 3801, 3975, 3840, 3725, 3952,
    4010, 3880, 3785, 3903, 3706, 3815, 3795, 3900, 3870, 4003, 3987, 3938,
    4040, 3879, 3777, 4218, 3888, 3964, 3800, 3715, 3815, 3793, 3905, 3918,
    3074, 3852, 2483, 2453, 2553, 3121, 3786, 4672, 3558, 4429, 3913, 3860,
    4233, 4538, 4612, 4240, 4635, 4615, 4524, 4570, 3473, 3625, 4292, 3920,
    3558, 5193, 3492, 4210, 3780, 3850, 3709, 3723, 3895, 3940, 3735, 3962,
    4190, 3890, 3795, 3813, 3716, 3825, 3825, 3985, 3862, 3898, 3982, 3993,
    4105, 3874, 3772, 4213, 3896, 3982, 3808, 3833, 3893, 3871, 3923, 3936,
    3256, 3804, 2745, 2475, 2565, 3163, 3798, 4684, 3770, 4341, 3815, 3972,
    4355, 4541, 4411, 4229, 4624, 4504, 4713, 4509, 3462, 3614, 4281, 3909,
    3547, 4962, 3481, 4109, 3874, 3654, 3713, 3828, 4092, 3867, 3752, 3979,
    3837, 3707, 3872, 3730, 3833, 3942, 3722, 3912, 3882, 4015, 3989, 3890,
    4052, 3891, 3765, 4197, 3899, 3905, 3911, 3726, 3876, 3804, 3916, 3819)

  dm  <- data.frame(A, C, D, blo) # Design matrix (a data.frame object)
  dfm <- data.frame(A, C, D, blo, y)

  ## PARAMETERS ARE THE DESIGN MATRIX AND THE RESPONSE VARIABLE
  ## MAIN FACTOR ANALYSIS
  ## Main factor = A
  sk1 <- SK(dm, y, model='y ~ blo + D*C*A + Error(blo/A/C)', which='A',
            error='blo:A')
  summary(sk1)
  plot(sk1)

  # Main factor = C
  sk2 <- SK(dm, y, model='y ~ blo + D*C*A + Error(blo/A/C)', which='C',
            error='blo:A:C', sig.level=0.025)
  summary(sk2)
  plot(sk2, title='C', col=rainbow(3))

  # Main factor = D
  sk3 <- SK(dm, y, model='y ~ blo + D*C*A + Error(blo/A/C)', which='D',
            error='Within', sig.level=0.1)
  summary(sk3)
  plot(sk3, title='D', col=heat.colors(3))

  ## NESTED ANALYSIS
  ## Nested C/A=1 -> SK.nest
  sk4 <- SK.nest(dm, y, model='y ~ blo + D*C*A + Error(blo/A/C)', which='C:A',
                 error='blo:A:C', fl2=1)
  summary(sk4)
  plot(sk4, title='C/A=1', col='darkgray')
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

