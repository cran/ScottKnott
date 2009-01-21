##
## S3 method to 'aovlist' object
##

SK.aovlist <- function(x, which, error, sig.level=.05, ...)
{
  mm      <- model.tables(x, "means")  # summary tables for model fits
  if(is.null(mm$n)) stop("No factors in the fitted model!")
  tabs    <- mm$tables[-1]             # all model means
  tabs    <- tabs[which]               # specified group means
  nn      <- mm$n[names(tabs)]         # repetions number of specified groups
  MSE     <- sum(resid(x[[error]])^2)/x[[error]][[8]]
  tab     <- tabs[[which]]             # tab=means
  means   <- as.vector(tab)
  mnumber <- length(means)             # number of means
  nms     <- names(tab)
  r       <- nn[[which]]               # groups and its number of replicates
  ord     <- order(means, decreasing=TRUE)
  mMSE    <- MSE/r
  dfr     <- x[[error]][[8]]           # residual degrees of freedom
  gm      <- mm$tables[[1]]            # grand mean
  means   <- means[ord]                # decreasing ordered means
  g       <- mnumber
  groups  <- MaxValue(g, means, mMSE, dfr, sig.level=sig.level, 1, rep(0, g), 0,
               rep(0, g))
  res     <- list(av=x, groups=groups, nms=nms, ord=ord, means=means,
               sig.level=sig.level, mnumber=mnumber, r=r, which=which)
  class(res) <- 'SK'
  invisible(res)
}
