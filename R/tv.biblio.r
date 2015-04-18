tv.biblio <- function(x='all', site, quiet=FALSE, tv_home, dict ='', iconv = "WINDOWS-1252", ...) {
  if(missing(tv_home)) tv_home <- tv.home()
  bibliopath <- file.path(tv_home, 'Popup', dict, 'tvrefenc.dbf')
  biblio <- read.dbf(bibliopath, as.is=TRUE)
  for(i in c('AUTHOR','TITLE','PUBLISHED')) biblio[,i] <- iconv(biblio[,i], iconv, "")  
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

# sink('umlauteproblem.txt')
# for(i in iconvlist()) try(cat(i, ": ", iconv(bib$PUBLISHED, from = i, to = 'utf-8'), '\n', collapse=''))
# sink()
