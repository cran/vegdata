tv.biblio <- function(x='all', site, quiet=FALSE, tv_home, ...) {
  if(missing(tv_home)) tv_home <- tv.home()

  biblio <- read.dbf(file.path(tv_home, 'Popup', 'tvrefenc.dbf'), as.is=TRUE)
  for(i in c('AUTHOR','TITLE','PUBLISHED')) biblio[,i] <- iconv(biblio[,i], "ISO-8859-1", "")  
  if(!missing(site)) {
    freq <- table(site$REFERENCE) 
    biblio$NBREL <- as.integer(freq[match(biblio$REFERENCE, names(freq))] )
    biblio$NBREL[is.na(biblio$NBREL)] <- 0
  }
  if(x[1] != 'all') {
    x <- as.numeric(unique(x))
#   for(i in 1:length(x)) {
      biblio <- biblio[match(x, as.numeric(biblio$REFERENCE)),]
      if(!quiet) print(biblio)
  }
  invisible(biblio)
}
