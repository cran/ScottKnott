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

## PARAMETER IS DATA.FRAME
## MAIN FACTOR ANALYSIS
## Main factor = A
sk5 <- SK(dfm, model='y ~ blo + D*C*A + Error(blo/A/C)', which='A',
          error='blo:A')
summary(sk5)
plot(sk5, title='A')

## NESTED ANALYSIS
## Nested C/A=2 -> SK.nest
sk6 <- SK.nest(dfm, model='y ~ blo + D*C*A + Error(blo/A/C)', which='C:A',
               error='blo:A:C', fl2=2, sig.level=0.01)
summary(sk6)
plot(sk6, title='C/A=2 sig.level=0.01')

## Nested D/A=2 -> SK.nest
sk7 <- SK.nest(dfm, model='y ~ blo + D*C*A + Error(blo/A/C)', which='D:A',
               error='Within', fl2=2, sig.level=0.005 )
summary(sk7)
plot(sk7, title='D/A sig.level=0.005')

## OBJECT IS AOVLIST
## Doing a previous anova

av <- aov(y ~  blo + D*C*A + Error(blo/A/C), data=dfm)
summary(av)

## MAIN FACTOR ANALYSIS
## Main factor = A 
sk8 <- SK(av, which='A', error='blo:A')
summary(sk8)
plot(sk8)

## Main factor = D
sk9 <- SK(av, which='D', error='Within', sig.level=0.025)
summary(sk9)
plot(sk9, col=c('black', 'darkgray', 'gray'))

## NESTED ANALYSIS
## Nested C/A=1 -> SK.nest
sk10 <- SK.nest(av, which='C:A', error='blo:A:C', fl2=1)
summary(sk10)
plot(sk10, title='C/A=1')

## Nested C/A=2 -> SK.nest 
sk11 <- SK.nest(av, which='C:A', error='blo:A:C', fl2=2)
summary(sk11)
plot(sk11, title='C/A=2')

## Nested D/C/A at various levels of C and A -> SK.nest 
sk12 <- SK.nest(av, which='D:C:A', error='Within', fl2=1, fl3=1)
summary(sk12)
plot(sk12, col=rainbow(5), title='D/C=1/A=1')

sk13 <- SK.nest(av, which='D:C:A', error='Within', fl2=2, fl3=1)
summary(sk13)
plot(sk13, col='darkgreen', title='D/C=2/A=1')

sk14 <- SK.nest(av, which='D:C:A', error='Within', fl2=3, fl3=3)
summary(sk14)
plot(sk14, col=cm.colors(10), title='D/C=3/A=3')

sk15 <- SK.nest(av, which='D:C:A', error='Within', fl2=2, fl3=3)
summary(sk15)
plot(sk15, title='D/C=2/A=3')
