
comb.species <- function(x, sel, newname, nametype='LETTERCODE', refl) {
  if(!'veg' %in% class(x)) warning(paste('Object', x, 'not of class "veg"'))
  if(missing(sel)) stop(paste('Either vegetation matrix or the vector of species names to combine missing.'))
  if(missing(refl)) refl <- attr(x, 'taxreflist')
  if(is.null(refl)) warning('Please specify taxonomic reference list')
  nam <- names(x)

  if(missing(newname)) newname <- sel[1]
  occtaxa <- sel[sel %in% names(x)]
#   taxa <- tax(occtaxa, refl=refl)
  if(length(occtaxa) > 1) {
    cat('\n The following names are combined to new name', newname, '\n')
    print(occtaxa)
  n <- x[, names(x) %in% occtaxa]
  y <- x[,!names(x) %in% occtaxa]
  result <- cbind(y, rowSums(n))
  names(result)[ncol(result)] <- newname
  class(result) <- c('veg', 'data.frame')
  attr(result, 'taxreflist') <- attr(x, 'taxreflist')
  } else {
    cat('Nothing to combine for', sel, '\n')
    result <- x
  }
  return(result)
}
