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

## PARAMETER IS DATA.FRAME
## Main factor = catalyst
sk3 <- SK(x=dfm, model='yeld ~ labs * catalyst * pressure', which='catalyst')
summary(sk3)
plot(sk3, title='catalyst')

## Main factor = pressure
sk4 <- SK(x=dfm, model='yeld ~ labs * catalyst * pressure', which='pressure',
          sig.level=0.025)
summary(sk4)
plot(sk4, title='pressure')

## NESTED ANALYSIS
## Nested catalyst/pressure=1 -> SK.nest
nsk5 <- SK.nest(x=dfm, model='yeld ~ labs * catalyst * pressure',
                which='catalyst:pressure', fl2=1)
summary(nsk5)
plot(nsk5, title='catalyst/pressure=1')

## OBJECT IS AOV
## Doing a previous ANOVA
nav1 <- aov(yeld ~ labs * catalyst * pressure , data=dfm)
summary(nav1)

## MAIN FACTOR ANALYSIS
## Main factor = pressure
nsk4 <- SK(nav1, which='pressure')
summary(nsk4)
plot(nsk4, title='pressure')

## NESTED ANALYSIS
## Nested catalyst/pressure=1
nsk5 <- SK.nest(nav1, which='catalyst:pressure', fl2=1)
summary(nsk5)
plot(nsk5, title='catalyst/pressure=1')

## Nested catalyst/pressure=2/labs=1 -> SK.nest
nsk6 <- SK.nest(nav1, which='labs:catalyst:pressure', fl2=2, fl3=1)
summary(nsk6)
plot(nsk6, title='catalyst/pressure=2/labs=1')