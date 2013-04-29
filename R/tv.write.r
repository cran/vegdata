
tv.write <- function(x, site, name, cover=c('code','perc'), overwrite = FALSE, ...) {
warning('This function is highly experimental and will most probably not work as expected.')
  cover <- match.arg(cover)
  if('veg' %in% class(x)) {
    X <- reShape.veg(x, ...)
#   cover <- 'perc'
#   names(x)[names(x)=='COVER_PERC'] <- 'COVER_CODE'
# print(names(x))
    if(cover == 'perc') {
      X$COVER_CODE <- as.character(X$COVER_PERC)
      site$COVERSCALE <- '00'
      X <- X[,c('RELEVE_NR','TaxonUsageID', 'COVER_CODE', 'LAYER')]
    }
  } else {
      if(!any(c('tv.obs','vw.obs') %in% class(x))) stop('Species observations must be of either \"tv.obs\" or \"vw.obs\" class.')
  if(!all(c('RELEVE_NR') %in% names(x)))
    stop('column names of species observations must contain RELEVE_NR')
    X <- x}

  if(!overwrite) if(file.exists(file.path(options('tv_home'), 'Data', name))) stop('Database ', name, ' already exists.')
  site$DATE <- gsub('-','',site$DATE)
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = TRUE)
  write.dbf(site, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))
  write.dbf(X, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))
  file.copy(from=file.path(options('tv_home'), 'Data/taxatest/remarks.dbf'), 
            to=file.path(options('tv_home'),'Data', name, 'remarks.dbf'), overwrite = overwrite)
  cat('Turboveg database', name, 'written to', unlist(options('tv_home')), 
      '\n Please specify correct species list when first opening the database in Turboveg.\n', 
      'If Turboveg is not able to open the database, try to exclude the biggest text fields.\n')
}

