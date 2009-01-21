##
## S3 method to plot 'SK' object
##

plot.SK.nest <- function(x, inches=TRUE, col=NULL, xlab=NULL, ylab=NULL,
                  title="Groups of means divided by colors", ...)
{
  class(x) <- 'SK'
  plot.SK(x, inches, col, xlab, ylab, title, ...)
}
   