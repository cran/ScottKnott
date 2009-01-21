##
## S3 method to 'aov' object
##

SK.nest.aov <- function(x, which, fl2, fl3=0, sig.level=.05, ...)
{
  mm      <- model.tables(x, "means")  # summary tables for model fits
  if(is.null(mm$n)) stop("No factors in the fitted model!")
  tabs    <- mm$tables[-1]             # all model means
  tabs    <- tabs[which]               # specified group means
  nn      <- mm$n[names(tabs)]         # repetions number of specified groups
  MSE     <- sum(resid(x)^2)/x$df.residual
  tab     <- tabs[[which]]             # tab = means
  if (fl3 == 0)                        # bifactorial
    means <- as.vector(tab[, fl2])
  else
    means <- as.vector(tab[, fl2, fl3]) #trifactorial
  mnumber <- length(means)             # number of means
  nms     <- dimnames(tab)[[1]]
  r       <- nn[[which]]               # groups and its number of replicates
  ord     <- order(means, decreasing=TRUE)
  mMSE    <- MSE/r           
  dfr     <- x$df.residual             # residual degrees of freedom 
  gm      <- mm$tables[[1]]            # grand mean
  means   <- means[ord]                # decreasing ordered means
  g       <- mnumber
  groups  <- MaxValue(g, means, mMSE, dfr, sig.level=sig.level, 1, rep(0, g),
               0, rep(0, g))
  res     <- list(av=x, groups=groups, nms=nms, ord=ord, means=means,
               sig.level=sig.level, mnumber=mnumber, r=r, which=which,
               tab=tab, fl2=fl2, fl3=fl3)
  class(res) <- 'SK.nest'
  invisible(res)
}
