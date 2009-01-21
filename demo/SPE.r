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

## PARAMETER IS DATA.FRAME
## MAIN FACTOR ANALYSIS
## Main factor = man
sk4 <- SK(dfm, model='y ~ blo + man*var + Error(blo/var)', which='man',
          error ='Within', sig.level=0.025 )
summary(sk4)
plot(sk4, title='man')
                         
## NESTED ANALYSIS
## Nested man/var=1 -> SK.nest
sk5 <- SK.nest(dfm, model='y ~ blo + man*var + Error(blo/var)', which='man:var',
               error ='Within', fl2=1 )
summary(sk5)
plot(sk5, title='var/man')

## OBJECT IS AOVLIST
## Doing a previous anova
av1 <- aov(y ~ blo + man*var + Error(blo/var), data=dfm)
summary(av1)

## MAIN FACTOR ANALYSIS
## Main factor = man
sk6 <- SK(av1, which='man', error ='Within', sig.level=0.1)
summary(sk6)
plot(sk6, title='man')

## Main factor = var
sk7 <- SK(av1, which='var', error='blo:var')
summary(sk7)
plot(sk7, title='var')

## NESTED ANALYSIS
## Nested man/var=1 -> SK.nest
sk8 <- SK.nest(av1, which='man:var', error='Within', fl2=1)
summary(sk8)
plot(sk8, title='man/var=Ladak')

## Nested man/var=2 -> SK.nest
sk9 <- SK.nest(av1, which='man:var', error='Within', fl2=2)
summary(sk9)
plot(sk9, title='man/var=Cossack')

## Nested man/var=3 -> SK.nest
sk10 <- SK.nest(av1, which='man:var', error='Within', fl2=3, sig.level=0.08)
summary(sk10)
plot(sk10, title='man/var=Ranger')