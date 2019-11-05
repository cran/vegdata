
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
  # length(non.legal.chars) > 0
  if (i==1 & is.na(non.legal.chars[1])) return(words)
  if (i==1 & !is.na(non.legal.chars[1])) return(substring(words, 1, non.legal.chars[1] - 1))
  if (i==2 & is.na(non.legal.chars[2])) return(substring(words, non.legal.chars[1], nchar(words, keepNA = FALSE)))
  if (i==2 & !is.na(non.legal.chars[2])) return(substring(words, non.legal.chars[1]+1, non.legal.chars[2]-1)) else return(character(0))
}

# x <-  c('Tortula acaulon (With.) R. H.Zander var. acaulon', 'Phascum cuspidatum Hedw. v. cuspidatum', 'Tortula acaulon var. papillosa (Lindb.) R. H. Zander', 'Phascum cuspidatum subsp. papillosum (Lindb.) J. Guerra & Ros', 'Tortula SP.')


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

# as.data.frame.list
# Convert a list of vectors to a data frame.
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

decode <- function(x, search, replace, default = NULL) {
  if(is.factor(x)) x <- as.character(x)
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0L) {function(x) if (is.null(default)) x else rep(default, length(x)) 
    } else {
      function(x) ifelse(x == search[1L], replace[1L],
                         decode.fun(tail(search,  -1L), tail(replace, -1L), default)(x))
    }
  return(decode.fun(search, replace, default)(x))
}
