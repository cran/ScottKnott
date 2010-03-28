##
## S3 method to sumarize 'SK' object
##

summary.SK.nest <- function(object, ...)
{
  if(!inherits(object, 'SK.nest'))
    stop("Use only with \"SK.nest\" objects!")
  ngroups <- object$groups[length(object$groups)]
  if(ngroups > 26)
    groupletter <- as.vector(t(outer(letters, letters, paste, sep="")))             
  else
    groupletter <- letters
  xgroups <- seq(ngroups)
  for(i in 1 : ngroups)
    object$groups[object$groups == xgroups[i]] <- groupletter[i]
  out <- data.frame(rownames(object$m.inf), object$m.inf[, 1],
           object$groups)
  names(out) <- c('Levels', 'Means', paste('SK(', 100*object$sig.level, '%)',
                  sep=''))
  if(class(object$av)[1]=='aovlist'){
    if(object$fl3 == 0){
      cat('Factor:', names(dimnames(object$tab)[1]), 'for level =',
          dimnames(object$tab)[[2]][object$fl2], 'of factor:',
          names(dimnames(object$tab)[2]), '\n')
    } else {
      cat('Factor:', names(dimnames(object$tab)[1]),
        'for level =', dimnames(object$tab)[[2]][object$fl2],
        'of factor:', names(dimnames(object$tab)[2]),
        'for level=', dimnames(object$tab)[[3]][object$fl3],
        'of factor:', names(dimnames(object$tab)[3]), '\n')
    }
  } else {
    if(object$fl3 == 0){
      cat('Factor:', names(dimnames(object$tab)[1]), 'for level =',
          dimnames(object$tab)[[2]][object$fl2], 'of factor:',
          names(dimnames(object$tab)[2]), '\n')
    } else {
      cat('Factor:', names(dimnames(object$tab)[1]),
          'for level =', dimnames(object$tab)[[2]][object$fl2],
          'of factor:', names(dimnames(object$tab)[2]),
          'for level =', dimnames(object$tab)[[3]][object$fl3],
          'of factor:', names(dimnames(object$tab)[3]), '\n')
    }
  }
  print(out, row.names=FALSE)
}
