##
## Example: Split-plot Experiment (SPE)
##

## The parameters can be: design matrix and the response variable,
## data.frame or aov

library(ScottKnott)
data(SPE)

## From: design matrix (dm) and response variable (y)
## Main factor: p
sk1 <- with(SPE, SK(x=dm, y=y, model='y ~ blk + sp*p + Error(blk/p)',
  which='p', error ='blk:p'))
summary(sk1)
plot(sk1)

## Main factor: sp
sk2 <- with(SPE, SK(x=dm, y=y, model='y ~ blk + sp*p + Error(blk/p)',
  which='sp', error ='Within', sig.level=0.025))
summary(sk2)
plot(sk2, xlab='Groups', ylab='Main effect: sp',
  title='Main effect: sp, sig.level=0.025')

## Nested: sp/p=1
skn1 <- with(SPE, SK.nest(x=dm, y=y, model='y ~ blk + sp*p + Error(blk/p)',
  which='sp:p', error ='Within', fl2=1))
summary(skn1)
plot(skn1, title='Effect: sp/p=1')


## From: data.frame
## Main factor: sp
sk3 <- with(SPE, SK(dfm, model='y ~ blk + sp*p + Error(blk/p)',
  which='sp', error ='Within', sig.level=0.025))
summary(sk3)
plot(sk3, title='Main effect: sp, sig.level=0.025')

## Nested: sp/p=1
skn2 <- with(SPE, SK.nest(dfm, model='y ~ blk + sp*p + Error(blk/p)',
  which='sp:p', error ='Within', fl2=1 ))
summary(skn2)
plot(skn2, title='Effect: p/sp')


## From: aovlist
av1 <- with(SPE, aov(y ~ blk + sp*p + Error(blk/p), data=dfm))
summary(av1)

## Main factor: sp
sk4 <- SK(av1, which='sp', error ='Within', sig.level=0.1)
summary(sk4)
plot(sk4, title='Main effect: sp, sig.level=0.1')

## Main factor: p
sk5 <- SK(av1, which='p', error='blk:p')
summary(sk5)
plot(sk5, title='Main effect: p')

## Nested: sp/p=1
skn3 <- SK.nest(av1, which='sp:p', error='Within', fl2=1)
summary(skn3)
plot(skn3, title='Effect: sp/p=p1')

## Nested: sp/p=2
skn4 <- SK.nest(av1, which='sp:p', error='Within', fl2=2)
summary(skn4)
plot(skn4, title='Effect: sp/p=p2')

## Nested: sp/p=3
skn5 <- SK.nest(av1, which='sp:p', error='Within', fl2=3, sig.level=0.08)
summary(skn5)
plot(skn5, title='Effect: sp/p=p3, sig.level=0.08')
