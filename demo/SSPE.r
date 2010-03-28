##
## Example: Split-split-plot Experiment (SSPE)
##

library(ScottKnott)
data(SSPE)

## From: design matrix (dm) and response variable (y)
## Main factor: p
sk1 <- with(SSPE, SK(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='p', error='blk:p'))
summary(sk1)
plot(sk1)

# Main factor: sp
sk2 <- with(SSPE, SK(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='sp', error='blk:p:sp', sig.level=0.025))
summary(sk2)
plot(sk2, title='Main effect: sp, sig.level=0.025')

# Main factor: ssp
sk3 <- with(SSPE, SK(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='ssp', error='Within', sig.level=0.1, id.trim=4))
summary(sk3)
plot(sk3, col=heat.colors(max(sk3$groups)),
  title='Main effect: ssp, sig.level=0.1')

## Nested: sp/p=1
skn1 <- with(SSPE, SK.nest(dm, y, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='sp:p', error='blk:p:sp', fl2=1))
summary(skn1)
plot(skn1, col='darkgray', title='Effect: sp/p=1')


## From: data.frame
## Main factor: p
sk4 <- with(SSPE, SK(dfm, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='p', error='blk:p'))
summary(sk4)
plot(sk4, title='Main effect: p')

## Nested: sp/p=2
skn2 <- with(SSPE, SK.nest(dfm, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='sp:p', error='blk:p:sp', fl2=2, sig.level=0.01))
summary(skn2)
plot(skn2, title='Effect: sp/p=2, sig.level=0.01')

## Nested: ssp/p=2
skn3 <- with(SSPE, SK.nest(dfm, model='y ~ blk + ssp*sp*p + Error(blk/p/sp)',
  which='ssp:p', error='Within', fl2=2, sig.level=0.005))
summary(skn3)
plot(skn3, title='Effect: ssp/p=2, sig.level=0.005')


## From: aovlist
av <- with(SSPE, aov(y ~  blk + ssp*sp*p + Error(blk/p/sp), data=dfm))
summary(av)

## Main factor: p 
sk5 <- SK(av, which='p', error='blk:p')
summary(sk5)
plot(sk5, title='Main effect: p')

## Main factor: ssp
sk6 <- SK(av, which='ssp', error='Within', sig.level=0.025, id.trim=4)
summary(sk6)
plot(sk6, col=c('black', 'darkgray', 'gray'),
  title='Main effect = ssp, sig.level=0.025')

## Nested: sp/p=1
skn4 <- SK.nest(av, which='sp:p', error='blk:p:sp', fl2=1)
summary(skn4)
plot(skn4, title='Effect: sp/p=1')

## Nested: sp/p=2
skn5 <- SK.nest(av, which='sp:p', error='blk:p:sp', fl2=2)
summary(skn5)
plot(skn5, title='Effect: sp/p=2')

## Nested: ssp/sp/p (at various levels of sp and p)
skn6 <- SK.nest(av, which='ssp:sp:p', error='Within', fl2=1, fl3=1)
summary(skn6)
plot(skn6, col=rainbow(5), title='Effect: ssp/sp=1/p=1')

skn7 <- SK.nest(av, which='ssp:sp:p', error='Within', fl2=2, fl3=1)
summary(skn7)
plot(skn7, col='darkgreen', title='Effect: ssp/sp=2/p=1')

skn8 <- SK.nest(av, which='ssp:sp:p', error='Within', fl2=3, fl3=3)
summary(skn8)
plot(skn8, col='darkgreen', title='Effect: ssp/sp=3/p=3')

skn9 <- SK.nest(av, which='ssp:sp:p', error='Within', fl2=2, fl3=3)
summary(skn9)
plot(skn9, id.lab=paste('test', 1:length(skn9$groups) , sep='_'),
  title='Effect: ssp/sp=2/p=3')
