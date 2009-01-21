\name{ScottKnott-package}
\alias{ScottKnott-package}
\alias{ScottKnott}
\docType{package}

\title{
  The ScottKnott Clustering Algoritm
}

\description{
  The Scott & Knott clustering algoritm is a very useful and widely used method
  of multiple comparizon in the Analysis of Variance context, as for example
  Gates and Bilbro (1978), Bony et al. (2001), Dilson et al. (2002) and
  Jyotsna et al. (2003).

  It was developed by Scott, A.J. and Knott, M (Scott and Knott, 1974).
  All methods used up to that date as, for example, the t-test, Tukey, Duncan,
  Newman-Keuls procedures, have overlapping problems. By overlapping we mean
  the possibility of one or more treatments to be classified in more than one
  group, in fact, as the number of treatments reach a number of twenty or more,
  the number of overlappings could increse as reaching 5 or greater what makes
  almost impossible to the experimenter to really distinguish the real groups
  to which the means should belong. The Scott & Knott method does not
  have this problem, what is often cited as a very good quality of this
  procedure.

  The Scott & Knott method make use of a clever algoritm of cluster analysis,
  where, starting from the the whole group of observed mean effects, it divides,
  and keep dividing the sub-groups in such a way that the intersection of any
  two groups formed in that manner is empty.

  Using their own words `we study the consequences of using a well-known method
  of cluster analysis to partition the sample treatment means in a balanced
  design and show how a corresponding likelihood ratio test gives a method of
  judging the significance of difference among groups abtained'.

  Many studies, using the method of Monte Carlo, suggest that the Scott Knott method
  performs very well compared to other methods due to fact that it has high
  power and type I error rate almost always in accordance with the nominal levels.
  So that, it should be a preferred method in experimental studies.

  The ScottKnott package performs this algoritm starting either from
  \code{vectors}, \code{matrices} or \code{data.frames} joined as
  \code{default}, a \code{aov} or \code{aovlist} resulting object of previous
  analysis of variance. The results are given in the usual way as well as in
  graphical way using thermometers with diferent group colors.

  In a few words, the test of Scott & Knott is a clustering algoritm used as an
  one of the alternatives where multiple comparizon procedures are applied with
  a very important and almost unique characteristic: it does not present
  overlapping in the results.
}

\details{
  \tabular{ll}{
  Package: \tab ScottKnott\cr
  Type: \tab Package\cr
  Version: \tab 1.0.0\cr
  Date: \tab 2008-08-19\cr
  License: \tab GPL (>= 2)\cr
  }
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr  
}

\references{
  Bony S, Pichon N, Ravel C, Durix A, Balfourier F, Guillaumin JJ 2001. The
  Relationship be-tween Mycotoxin Synthesis and Isolate Morphology in Fungal
  Endophytes of Lolium perenne. \emph{New Phytologist}, \bold{1521}, 125-137.

  Borges LC, FERREIRA DF 2003. Poder e taxas de erro tipo I dos testes
  Scott-Knott, Tukey e Student-Newman-Keuls sob distribuicoes normal e nao
  normais dos residuos. Power and type I errors rate of Scott-Knott, Tukey and
  Student-Newman-Keuls tests under normal and no-normal distributions of the
  residues. \emph{Rev. Mat. Estat.}, Sao Paulo, \bold{211}: 67-83.

  Calinski T, Corsten LCA 1985. Clustering Means in ANOVA by Simultaneous
  Testing. \emph{Bio-metrics}, \bold{411}, 39-48.

  Da Silva EC, Ferreira DF, Bearzoti E 1999. Evaluation of power and type I
  error rates of Scott-Knotts test by the method of Monte Carlo.
  \emph{Cienc. agrotec.}, Lavras, \bold{23}, 687-696.

  Dilson AB, David SD, Kazimierz J, William WK 2002. Half-sib progeny evaluation
  and selection of potatoes resistant to the US8 genotype of Phytophthora
  infestans from crosses between resistant and susceptible parents.
  \emph{Euphytica}, \bold{125}, 129-138.

  Gates CE, Bilbro JD 1978. Illustration of a Cluster Analysis Method for Mean
  Separation. \emph{Agron J}, \bold{70}, 462-465.

  Jyotsna S, Zettler LW, van Sambeek JW, Ellersieck MR, Starbuck CJ 2003.
  Symbiotic Seed Germination and Mycorrhizae of Federally Threatened Platanthera
  PraeclaraOrchidaceae.
  \emph{American Midland Naturalist}, \bold{1491}, 104-120.

  Ramalho MAP, Ferreira DF, Oliveira AC 2000. \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Scott RJ, Knott M 1974. A cluster analysis method for grouping mans in the
  analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ##
  ## Generating data
  x <- gl(4, 6, labels=LETTERS[1:4])
  r <- factor(rep(1:6, 4))
  y <- c(58, 49, 51, 56, 50, 48,
         60, 55, 66, 61, 54, 61,
         59, 47, 44, 49, 62, 60,
         45, 33, 34, 48, 42, 44)
  dm  <- data.frame(x, r) # Design matrix (a data.frame object)
  dfm <- data.frame(dm, y)

  ## PARAMETERS ARE ATOMIC VECTORS, DESIGN MATRIX AND THE RESPONSE VARIABLE
  ## From atomics 'x' and 'y'
  sk1 <- SK(x=x, y=y, model='y ~ x', which='x')
  summary(sk1)
  #plot(sk1, title='Treatments', col=rainbow(1)) # will not work
  plot(sk1, title='Treatments', col=rainbow(2))

  ## From design matrix (dm) and response variable (y)
  sk2 <- SK(x=dm, y=y, model='y ~ x', which='x')
  summary(sk2)
  plot(sk2, title='Treatments', xlab='Groups', ylab='Groups means')

  ## PARAMETER IS DATA.FRAME
  ## From data.frame (dfm)
  sk3 <- SK(x=dfm, model='y ~ x', which='x')
  summary(sk3)
  plot(sk3, title='Treatments', col=cm.colors(2))

  ## PARAMETER IS AOV
  sk4 <- SK(x=aov(y ~ x, data=dfm), which='x')
  summary(sk4)
  plot(sk4, title='Treatments')
  

  ##
  ## Example: Randomized Complete Block Design (RCBD)
  ##
  ## Generating data
  y <- c(142.36, 144.78, 145.19, 138.88,
         139.28, 137.77, 144.44, 130.61,
         140.73, 134.06, 136.07, 144.11,
         150.88, 135.83, 136.97, 136.36,
         153.49, 165.02, 151.75, 150.22)
  tra <- gl(5, 4, labels=LETTERS[1:5])
  blk <- factor(rep(1:4, 5))
  dm  <- data.frame(tra, blk) # Design matrix (a data.frame object)
  dfm <- data.frame(dm, y)

  ## PARAMETERS ARE DESIGN MATRIX AND THE RESPONSE VARIABLE
  ## From design matrix (dm) and response variable (y)
  sk1 <- SK(x=dm, y=y, model='y ~ blk + tra', which = 'tra')
  summary(sk1)
  plot(sk1)

  ## PARAMETER IS DATA.FRAME
  ## From data.frame (dfm)
  sk2 <- SK(x=dfm, model='y ~ blk + tra', which='tra')
  summary(sk2)
  plot(sk2, title='Treatments')

  ## which='blk'
  sk3 <- SK(x=dfm, model='y ~ blk + tra', which='blk')
  summary(sk3)
  plot(sk3, title='Blocks')

  ## PARAMETER IS AOV
  ## Doing a previous ANOVA
  av1 <- aov(y ~ blk + tra, data=dfm)
  summary(av1)

  ## From aov, which='blk' implicit
  sk4 <- SK(x=av1)
  summary(sk4)
  plot(sk4, title='Blocks')

  ## From aov, which='blk' explicit
  sk6 <- SK(x=av1, which='blk')
  summary(sk6)
  plot(sk6,title='Blocks')

  ## From aov, which='tra' explicit
  sk5 <- SK(x=av1, which='tra')
  summary(sk5)
  plot(sk5, title='Treatments')

  ##
  ## Example: Latin Squares Design (LSD)
  ##
  ## Generating data
  y <- c(518, 524, 420, 486, 515,
         458, 550, 384, 494, 318,
         583, 724, 556, 501, 660,
         432, 400, 297, 500, 438,
         331, 478, 489, 313, 394)
  tra  <- gl(5, 5, labels=LETTERS[1:5])
  rows <- factor(rep(1:5, 5))
  cols <- factor(c(2, 3, 5, 4, 1,
                   3, 4, 2, 1, 5,
                   4, 1, 3, 5, 2,
                   1, 5, 4, 2, 3,
                   5, 2, 1, 3, 4))
  dm  <- data.frame(tra, rows, cols) # Design matrix (a data.frame object)
  dfm <- data.frame(dm, y)

  ## PARAMETERS ARE DESIGN MATRIX AND THE RESPONSE VARIABLE
  ## From design matrix (dm) and response variable (y)
  sk1 <- SK(x=dm, y=y, model='y ~ rows + cols + tra', which='tra')
  summary(sk1)
  plot(sk1)

  ## PARAMETER IS DATA.FRAME
  ## From data.frame
  sk2 <- SK(x=dfm, model='y ~ rows + cols + tra', which='tra')
  summary(sk2)
  plot(sk2, title='Treatments')

  ## PARAMETER IS AOV
  ## Doing a previous ANOVA
  av1 <- aov(y ~ rows + cols + tra, data=dfm)
  summary(av1)

  ## From aov object, sig.level=5
  sk3 <- SK(av1, which='tra')
  summary(sk3)
  plot(sk5, title='Treatments, sig.level=.05')

  ## From aov object, sig.level=8
  sk4 <- SK(av1, which='tra', sig.level=.08)
  summary(sk4)
  plot(sk4, title='Treatments, sig.level=.08')
}
