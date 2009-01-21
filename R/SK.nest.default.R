##
## S3 method to design matrix and response variable or data.frame objects
##

SK.nest.default <- function(x, y=NULL, model, which, error, fl2, fl3=0,
                     sig.level=.05, ...)
{ 
  if (is.data.frame(y))
    y <- as.matrix(y[, 1])  # manova is not contemplated
  else
    stopifnot(is.atomic(y))
  if (is.matrix(x) || is.atomic(x))
    x <- as.data.frame(x)
  if(!is.null(y))
    dat <- as.data.frame(cbind(x, y))
  else
    dat <- x
   av <- eval(substitute(aov(fo, x), list(fo = formula(model))))
  if(class(av)[1]=='aov')
    res  <- SK.nest.aov(av, which=which, fl2=fl2,
              fl3=fl3, sig.level=sig.level)
  else
    res  <- SK.nest.aovlist(av, which=which, error=error,
              fl2=fl2, fl3=fl3,
              sig.level=sig.level)
  invisible(res)
 }
