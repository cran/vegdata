if(getRversion() >= "2.15.1")  utils::globalVariables(c("DoWritedbf"))

tv.write <- function(x, site, name, cover=c('code','perc'), overwrite = FALSE, iconv="ISO-8859-1", ...) {
  cover <- match.arg(cover)
  if('veg' %in% class(x)) {
    X <- reShape.veg(x, ...)
    if(cover == 'perc') {
      X$COVER_CODE <- as.character(X$COVER_PERC)
      site$COVERSCALE <- '00'
      X <- X[,c('RELEVE_NR','TaxonUsageID', 'COVER_CODE', 'LAYER')]
    }
  } else {
      if(!any(c('tv.obs','vw.obs') %in% class(x))) stop('Species observations must be of either \"tv.obs\" or \"vw.obs\" class.')
  if(!all(c('RELEVE_NR') %in% names(x))) stop('column names of species observations must contain RELEVE_NR')
    X <- x
  }
  names(X)[2] <- 'SPECIES_NR'
  if(!overwrite) if(file.exists(file.path(options('tv_home'), 'Data', name))) stop('Database ', name, ' already exists.')
  site$DATE <- gsub('-','',site$DATE)
  for(i in names(site)) if(is.character(site[,i])) {
    site[is.na(site[,i]),i] <- ''
    site[,i] <- iconv(site[,i], '', iconv)
  }

### Write
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = TRUE)
  write.dbf(site, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))
  write.dbf(X, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))

  file.copy(from=file.path(options('tv_home'), 'Data/taxatest/remarks.dbf'), 
            to=file.path(options('tv_home'),'Data', name, 'remarks.dbf'), overwrite = overwrite)
  cat('Turboveg database', name, 'written to', file.path(options('tv_home'), 'Data', name), 
      '\n Please specify correct species list when first opening the database in Turboveg.\n')
}

