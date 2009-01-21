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
