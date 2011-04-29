# R
# library(vegdata)
# 
# db <- 'taxatest'
# obs <- tv.coverperc(db, sysPath=TRUE)
# site <- tv.site(db, sysPath=TRUE)
# 
tv.write <- function(obs, site, name, cover=c('code','perc'), overwrite = FALSE, ...) {
  cover <- match.arg(cover)
  if(!any(c('tv.obs','vw.obs') %in% class(obs))) stop('Species observations must be either \"tv.obs\" or \"vw.obs\" class.')
  if(!all(c('SPECIES_NR','RELEVE_NR','COVER_CODE') %in% names(obs))) 
    stop('column names of species observations must contain SPECIES_NR,RELEVE_NR,COVER_CODE')

  if(!overwrite) if(file.exists(file.path(options('tv_home'), 'Data', name))) stop('Database ', name, ' already exists.')
  site$DATE <- gsub('-','',site$DATE)
  if(cover == 'perc') {
      obs$COVER_CODE <- as.integer(ceiling(obs$COVER_PERC))
      site$COVERSCALE <- '00'
  }
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = TRUE)
  write.dbf(site, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))
  write.dbf(obs, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))
  file.copy(from=file.path(options('tv_home'), 'Data/taxatest/remarks.dbf'), to=file.path(options('tv_home'),'Data', name, 'remarks.dbf'), overwrite = overwrite)
  cat('\n Please specify correct species list when first opening the database in Turboveg.\n',
      ' If Turboveg is not able to open the database, try to exclude the biggest text fields.\n')
}

# tv.write(obs, site, 'test2', overwrite=TRUE)
