tv.biblio <- function(x='all', site, quiet=FALSE, tv_home, ...) {
  if(missing(tv_home)) tv_home <- tv.home()

  biblio <- read.dbf(file.path(tv_home, 'Popup', 'tvrefenc.dbf'), as.is=TRUE)
  if(!missing(site)) {
    freq <- table(site$REFERENCE) 
    biblio$NBREL <- as.integer(freq[match(biblio$REFERENCE, names(freq))] )
    biblio$NBREL[is.na(biblio$NBREL)] <- 0
  }
  if(x != 'all') {
    x <- as.character(unique(x))
#   for(i in 1:length(x)) {
      biblio <- biblio[match(x,biblio$REFERENCE),]
      if(!quiet) print(biblio)
#   cat(iconv(c(b$REFERENCE, '  ', b$AUTHOR, ' (',b$YEAR,') ', b$TITLE, ', ', b$PUBLISHED,' [Nb. of rel.:', b$NBREL, ']'), from='ISO_8859-1', to=''), '\n', sep='')	else
#   cat(iconv(c(b$REFERENCE, '  ', b$AUTHOR, ' (',b$YEAR,') ', b$TITLE, ', ', b$PUBLISHED), from='ISO_8859-1', to=''), '\n', sep='')
  }
  invisible(biblio)
}

