
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
  chars <- substring(words, 1:nchar(words, keepNA = FALSE), 1:nchar(words, keepNA = FALSE))
  legal.chars <- c(letters, LETTERS, '\u00fc','\u00e4','\u00f6','\u00df','\u00d7', "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", add.legal)
  non.legal.chars <- (1:length(chars))[!chars %in% legal.chars]
  if (i==1 & is.na(non.legal.chars[1])) return(words)
  if(i==1) return(substring(words, 1, non.legal.chars[1] - 1)) else
    if(i==2 & length(non.legal.chars) > 0) return(substring(words, non.legal.chars[1]+1, nchar(words, keepNA = FALSE))) else return(character(0))
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

#  as.data.frame.list
#' Convert a list of vectors to a data frame.
#' 
#' This function will convert a list of vectors to a data frame. This function
#' will handle three different types of lists of vectors. First, if all the elements
#' in the list are named vectors, the resulting data frame will have have a number
#' of columns equal to the number of unique names across all vectors. In cases
#' where some vectors do not have names in other vectors, those values will be
#' filled with \code{NA}.
#' 
#' The second case is when all the vectors are of the same length. In this case,
#' the resulting data frame is equivalent to applying \code{rbind} across all elements.
#' 
#' The third case handled is when there are varying vector lengths and not all the
#' vectors are named. This condition should be avoided. However, the function will
#' attempt to convert this list to a data frame. The resulting data frame will have
#' a number of columns equal to the length of the longest vector. For vectors with
#' length less than this will fill the row with \code{NA}s. Note that this function
#' will print a warning if this condition occurs.
#' 
#' @author Jason Bryer <<jason@@bryer.org>>
#' @references \url{http://stackoverflow.com/questions/4227223/r-list-to-data-frame}
#' @param x a list to convert to a data frame.
#' @param row.names a vector equal to \code{length(x)} corresponding to the row names.
#'        If \code{NULL}, the row names will be set to \code{names(x)}.
#' @param optional not used.
#' @param ... other parameters passed to \code{\link{data.frame}}.
#' @return a data frame.
#' @S3method as.data.frame list
#' @export
#' @examples
#'     test1 <- list( c(a='a',b='b',c='c'), c(a='d',b='e',c='f'))
#'     as.data.frame(test1)
#'     
#'     test2 <- list( c('a','b','c'), c(a='d',b='e',c='f'))
#'     as.data.frame(test2)
#'     
#'     test3 <- list('Row1'=c(a='a',b='b',c='c'), 'Row2'=c(var1='d',var2='e',var3='f'))
#'     as.data.frame(test3)
#'     
#'     test4 <- list('Row1'=letters[1:5], 'Row2'=letters[1:7], 'Row3'=letters[8:14])
#'     as.data.frame(test4)
#'     
#'     test5 <- list(letters[1:10], letters[11:20])
#'     as.data.frame(test5)
#'     
#'     test6 <- list(list(letters), letters)
#'     as.data.frame(test6)
as.data.frame.list <- function(x, row.names=NULL, optional=FALSE, ...) {
  if(!all(unlist(lapply(x, class)) %in% 
          c('raw','character','complex','numeric','integer','logical'))) {
    warning('All elements of the list must be a vector.')
    NextMethod(x, row.names=row.names, optional=optional, ...)
  }
  allequal <- all(unlist(lapply(x, length)) == length(x[[1]]))
  havenames <- all(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
  if(havenames) { #All the vectors in the list have names we can use
    colnames <- unique(unlist(lapply(x, names)))
    df <- data.frame(matrix(
      unlist(lapply(x, FUN=function(x) { x[colnames] })),
      nrow=length(x), byrow=TRUE), stringsAsFactors = FALSE)
    names(df) <- colnames
  } else if(allequal) { #No names, but are of the same length
    df <- data.frame(matrix(unlist(x), nrow=length(x), byrow=TRUE), stringsAsFactors = FALSE, ...)
    hasnames <- which(unlist(lapply(x, FUN=function(x) !is.null(names(x)))))
    if(length(hasnames) > 0) { #We'll use the first element that has names
      names(df) <- names(x[[ hasnames[1] ]])
    }
  } else { #No names and different lengths, we'll make our best guess here!
    warning(paste("The length of vectors are not the same and do not ",
                  "are not named, the results may not be correct.", sep=''))
    #Find the largest
    lsizes <- unlist(lapply(x, length))
    start <- which(lsizes == max(lsizes))[1]
    df <- x[[start]]
    for(i in (1:length(x))[-start]) {
      y <- x[[i]]
      if(length(y) < length(x[[start]])) {
        y <- c(y, rep(NA, length(x[[start]]) - length(y)))
      }
      if(i < start) {
        df <- rbind(y, df)
      } else {
        df <- rbind(df, y)
      }
    }
    df <- as.data.frame(df, row.names=1:length(x))
    names(df) <- paste('Col', 1:ncol(df), sep='')
  }
  if(missing(row.names)) {
    row.names(df) <- names(x)
  } else {
    row.names(df) <- row.names
  }
  return(df)
}

