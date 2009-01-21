##
## Examples: Completely Randomized Design (CRD)
##

## Example 1
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


## Example 2
## Generating data
pro <- c(
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
tra <- factor(c(
  111, 111, 111, 111, 112, 112, 112, 112, 113, 113, 113, 113, 114, 114,
  114, 114, 115, 115, 115, 115, 121, 121, 121, 121, 122, 122, 122, 122,
  123, 123, 123, 123, 124, 124, 124, 124, 125, 125, 125, 125, 131, 131,
  131, 131, 132, 132, 132, 132, 133, 133, 133, 133, 134, 134, 134, 134,
  135, 135, 135, 135, 211, 211, 211, 211, 212, 212, 212, 212, 213, 213,
  213, 213, 214, 214, 214, 214, 215, 215, 215, 215, 221, 221, 221, 221,
  222, 222, 222, 222, 223, 223, 223, 223, 224, 224, 224, 224, 225, 225,
  225, 225, 231, 231, 231, 231, 232, 232, 232, 232, 233, 233, 233, 233,
  234, 234, 234, 234, 235, 235, 235, 235, 311, 311, 311, 311, 312, 312,
  312, 312, 313, 313, 313, 313, 314, 314, 314, 314, 315, 315, 315, 315,
  321, 321, 321, 321, 322, 322, 322, 322, 323, 323, 323, 323, 324, 324,
  324, 324, 325, 325, 325, 325, 331, 331, 331, 331, 332, 332, 332, 332,
  333, 333, 333, 333, 334, 334, 334, 334, 335, 335, 335, 335))
r <- rep(1:4, 45)
dm <- data.frame(tra, r) # Design matrix (a data.frame object)
dfm <- data.frame(dm, pro)

## PARAMETERS ARE ATOMIC VECTORS, DESIGN MATRIX AND THE RESPONSE VARIABLE
## From atomics 'x' and 'y'
sk5 = SK(x=tra, y=pro, model='pro ~ tra', which='tra', sig.level=0.1)
summary(sk5)
plot(sk5, title='Treatments, sig.level=10%')
plot(sk5, title='Treatments, sig.level=10%', col=rainbow(5))
plot(sk5, title='Treatments, sig.level=10%', col=cm.colors(10))

## From design matrix (dm) and response variable (y)
sk6 <- SK(x=dm, y=pro, model='pro ~ tra', which='tra', sig.level=0.005)
summary(sk6)
plot(sk6, title='Treatments, sig.level=0.5%')

## PARAMETER IS DATA.FRAME
## From data.frame 'dfm'
sk7 = SK(x=dfm, model='pro ~ tra', which='tra')
summary(sk7)
plot(sk7, title='Treatments, sig.level=5%')

## PARAMETER IS AOV
sk8 <- SK(x=aov(pro ~ tra , data = dfm), which='tra')
summary(sk8)
plot(sk8, title='Treatments, sig.level=5%', col=heat.colors(4))
plot(sk8, title='Treatments, sig.level=5%', col=heat.colors(5))
plot(sk8, title='Treatments, sig.level=5%', col=heat.colors(7))
plot(sk8, title='Treatments, sig.level=5%', col=heat.colors(10))
