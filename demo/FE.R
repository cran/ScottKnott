##
## Example: Factorial Experiment (FE)
##

## The parameters can be: design matrix and the response variable,
## data.frame or aov

library(ScottKnott)
data(FE)

## From: design matrix (dm) and response variable (y)
## Main factor: N
sk1 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N * P * K', 
  which='N'))
summary(sk1)
plot(sk1, title='Main effect: N')

## Main factor: P
sk2 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N * P * K',
  which='P'))
summary(sk2)
plot(sk2, title='Main effect: P')

## Main factor: K
sk3 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N * P * K',
  which='K'))
summary(sk3)
plot(sk3, title='Main effect: K')
                         
## Nested: N/P=1
nsk1 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:P', fl2=1))
summary(nsk1)
plot(nsk1, title='Effect: N/P=1')

## Nested: N/P=2
nsk2 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:P', fl2=2))
summary(nsk2)
plot(nsk2, title = 'Effect: N/P=2')

## Nested: N/K=1
nsk3 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:K', fl2=1))
summary(nsk3)
plot(nsk3, title='Effect: N/K=1')

## Nested: N/K=2
nsk4 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:K', fl2=2))
summary(nsk4)
plot(nsk4, title = 'Effect: N/K=2')

## Nested: P/K=1
nsk5 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='P:K', fl2=1))
summary(nsk5)
plot(nsk5, title='Effect: P/K=1')

## Nested: P/K=2
nsk6 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='P:K', fl2=2))
summary(nsk6)
plot(nsk6, title = 'Effect: P/K=2')

## Nested: N/P=1/K=1
nsk7 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:P:K', fl2=1, fl3=1))
summary(nsk7)
plot(nsk7, title='Effect: N/P=1/K=1')

## Nested: N/P=2/K=2
nsk8 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:P:K', fl2=2, fl3=2))
summary(nsk8)
plot(nsk8, title='Effect: N/P=2/K=2')

## Nested: P/N=1/K=1
nsk9 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + P * N * K',
  which='P:N:K', fl2=1, fl3=1))
summary(nsk9)
plot(nsk9, title='Effect: P/N=1/K=1')

## Nested: P/N=2/K=2
nsk8 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + P * N * K',
  which='P:N:K', fl2=2, fl3=2))
summary(nsk8)
plot(nsk8, title='Effect: N/P=2/K=2')

## Nested: K/N=1/P=1
nsk10 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + K * N * P',
  which='K:N:P', fl2=1, fl3=1))
summary(nsk10)
plot(nsk10, title='Effect: K/N=1/P=1')

## Nested: K/N=2/K=2
nsk11 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + K * N * P',
  which='K:N:P', fl2=2, fl3=2))
summary(nsk11)
plot(nsk11, title='Effect: K/N=2/P=2')


## From: data.frame
## Nested: N/P=1/K=2
nsk12 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:P:K', fl2=1, fl3=2))
summary(nsk12)
plot(nsk12, title='Effect: N/P=1/K=2')

## Nested: K/P=2/K=1
nsk13 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N * P * K',
  which='N:P:K', fl2=2, fl3=1))
summary(nsk13)
plot(nsk13, title='Effect: N/P=2/K=1')


## From aov
nav1 <- with(FE, aov(y ~ blk + N * P * K , data=dfm))
summary(nav1)

## Main factor: N
nsk14 <- SK(nav1, which='N')
summary(nsk14)
plot(nsk14, title='Main effect: N')

## Nested: P/K=1
nsk15 <- SK.nest(nav1, which='P:K', fl2=1)
summary(nsk15)
plot(nsk15, title='Effect: P/K=1')

## Nested: N/P=1/K=2
nsk16 <- SK.nest(nav1, which='N:P:K', fl2=1, fl3=2)
summary(nsk16)
plot(nsk16, title='Effect: N/P=1/K=2')

# Changing the order of factors (test)
nav2 <- with(FE, aov(y ~ blk + K * N * P, data=dfm))
summary(nav2)

## Nested: K/N=1/P=1
nsk17 <- SK.nest(nav2, which='K:N:P', fl2=1, fl3=1)
summary(nsk17)
plot(nsk17, title='Effect: K/N=1/P=1')
