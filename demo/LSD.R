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

## From aov object, sig.level=.05
sk3 <- SK(av1, which='tra')
summary(sk3)
plot(sk3, title='Treatments, sig.level=.05')

## From aov object, sig.level=8%
sk4 <- SK(av1, which='tra', sig.level=.08)
summary(sk4)
plot(sk4, title='Treatments, sig.level=.08')
