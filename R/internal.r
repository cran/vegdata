tv.db <- function() {
  tv_home <- tv.home()
  wd <- getwd()
  setwd(file.path(tv_home, 'Data'))
  dir <- list.dirs(full.names = TRUE, recursive = TRUE)
#  dir <- sapply(dir, function(x) substring(x, nchar(tv_home)+7), USE.NAMES = FALSE)
  return(dir[2:length(dir)])
}

"[.veg" <- function(x, s,...) {
  taxref <- attr(veg, 'taxreflist')
  out <- NextMethod("[,", drop=TRUE)
  class(out) <- c('veg', 'data.frame')
  attr(veg, 'taxreflist') <- taxref
  return(out)
}

tv.dict <- function(db, tv_home) {
  if(missing(tv_home)) tv_home <- tv.home()
  dbattr <- file.path(tv_home, 'Data', db, 'tvwin.set')
  if(file.access(dbattr)==0) {
    allbytes <- readBin(dbattr, "raw", n = 100, size = 1, endian = "little")
    bin <- sapply(allbytes, readBin, what='character')
    return(readBin(allbytes[(which(bin == 'C')[3]+3):length(allbytes)], what=character()))
  } else warning('tvwin.set not found')
}  

first.word <- function (x, i = 1, expr = substitute(x), add.legal=NULL) {
  words <- if(!missing(x)) as.character(x)[1] else as.character(unlist(expr))[1]
  if (i > 2) stop("only first and second word implemented")
  chars <- substring(words, 1:nchar(words), 1:nchar(words))
  legal.chars <- c(letters, LETTERS, "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", add.legal)
  non.legal.chars <- (1:length(chars))[!chars %in% legal.chars]
  if (i==1 & is.na(non.legal.chars[1])) return(words)
  if(i==1) return(substring(words, 1, non.legal.chars[1] - 1)) else
    if(i==2 & length(non.legal.chars) > 0) return(substring(words, non.legal.chars[1], nchar(words))) else return(character(0))
}

# words <- 'AGRTS;P'
# words <- 'QUERROB.Tree'
# add.legal <- c('-',';')
# (w <- first.word(words, i=1, add.legal = ';'))

