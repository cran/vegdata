
"[.veg" <- function(x, s,...) {
  taxref <- attr(veg, 'taxreflist')
  out <- NextMethod("[,", drop=TRUE)
  class(out) <- c('veg', 'data.frame')
  attr(veg, 'taxreflist') <- taxref
  return(out)
}

first.word <- function (x, i = 1, expr = substitute(x), add.legal=NULL) {
  words <- if(!missing(x)) as.character(x)[1] else as.character(unlist(expr))[1]
  if (i > 2) stop("only first and second word implemented")
  chars <- substring(words, 1:nchar(words), 1:nchar(words))
  legal.chars <- c(letters, LETTERS, '\u00fc','\u00e4','\u00f6','\u00df','\u00d7', "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", add.legal)
  non.legal.chars <- (1:length(chars))[!chars %in% legal.chars]
  if (i==1 & is.na(non.legal.chars[1])) return(words)
  if(i==1) return(substring(words, 1, non.legal.chars[1] - 1)) else
    if(i==2 & length(non.legal.chars) > 0) return(substring(words, non.legal.chars[1], nchar(words))) else return(character(0))
}

# words <- 'AGRTS;P'
# words <- 'QUERROB.Tree'
# add.legal <- c('-',';')
# (w <- first.word(words, i=1, add.legal = ';'))

rbind.df <- function(df1, df2) {
  cols1 <- names(df1); cols2 <- names(df2)
  All <- union(cols1, cols2)
  miss1 <- setdiff(All, cols1)
  miss2 <- setdiff(All, cols2)
  df1[, c(as.character(miss1))] <- NA
  df2[,c(as.character(miss2))] <- NA
  out <- rbind(df1, df2)
  return(out)
}

cbind.df <- function(df1, df2, by) {
  cols1 <- names(df1); cols2 <- names(df2)
  inters <- intersect(cols1, cols2)
  df.m <- df2[match(df1[,by], df2[,by]), ]
  for(i in inters) {
    df1[,i][is.na(df1[,i])] <- df.m[,i][is.na(df1[,i])] 
  }
  return(df1)
}

