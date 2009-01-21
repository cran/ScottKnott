##
## S3 method to plot 'SK' object
##

plot.SK <- function(x, inches=TRUE, col=NULL, xlab=NULL, ylab=NULL,
             title="Groups of means divided by colors", ...)
{
  if(!inherits(x, 'SK'))
    stop("Use only with 'SK' objects!")
  if(!is.null(col))  
  if(length(col) < max(x$groups)){
    stop("Parameter 'col', the number of colors is not enough, it must match 
    the number of groups!")}
  means  <- x$means
  groups <- 1:length(means) 
  if(is.null(col))
    col <- x$groups
  else {
   col <- col[1:max(x$groups)]
   col <- col[x$groups]
  }
  symbols(groups, means, thermometers=cbind(.3, 1, rep(1, length(means))),
    inches=inches, fg=col, xlab=xlab, ylab=ylab)
  title(title)
}
   