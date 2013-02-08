tv.db <- function() {
  tv_home <- tv.home()
  dir <- list.dirs(path = file.path(tv_home, 'Data'), full.names = TRUE, recursive = TRUE)
  dir <- sapply(dir, function(x) substring(x, nchar(tv_home)+7), USE.NAMES = FALSE)
  return(dir[2:length(dir)])
}

"[.veg" <- function(x, s,...) {
  taxref <- attr(veg, 'taxreflist')
  out <- NextMethod("[,", drop=TRUE)
  class(out) <- c('veg', 'data.frame')
  attr(veg, 'taxreflist') <- taxref
  return(out)
}


first.word <- function (x, i = 1, expr = substitute(x)) {
  words <- if(!missing(x)) as.character(x)[1] else as.character(unlist(expr))[1]
  if (i > 1) stop("i > 1 not implemented")
  chars <- substring(words, 1:nchar(words), 1:nchar(words))
  legal.chars <- c(letters, LETTERS, ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  non.legal.chars <- (1:length(chars))[chars %in% legal.chars]
  if (!any(non.legal.chars)) return(words)
  if (non.legal.chars[1] == 1) return(character(0))
  substring(words, 1, non.legal.chars[1] - 1)
}
