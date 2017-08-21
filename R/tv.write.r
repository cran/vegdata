if(getRversion() >= "2.15.1")  utils::globalVariables(c("DoWritedbf"))

tv.write <- function(x, site, db, name, db.abs.path, dict, cover=c('code','perc'), overwrite = FALSE, iconv="CP437", newTvAdmin = FALSE, drop = FALSE, ...) {
  if(missing(db.abs.path)) warning('Either db name ("db") or absolute path ("db.abs.path") to the original TV database is necessary to process TvAdmin.dbf, remarks.dbf and tvwin.set/.dbf')
  if(!missing(db)) dict <- tv.dict(db = db, tv_home = tv.home())

  cover <- match.arg(cover)
  if('veg' %in% class(x)) {
    X <- reShape.veg(x, ...)
    if(cover == 'perc') {
      X$COVER_CODE <- as.character(X$COVER_PERC)
      site$COVERSCALE <- '00'
      X <- X[,c('RELEVE_NR','TaxonUsageID', 'COVER_CODE', 'LAYER')]
    }
  } else {
      if(!any(c('tv.obs','vw.obs') %in% class(x))) 
        stop('Species observations must be of either \"tv.obs\" or \"vw.obs\" class.')
  if(!all(c('RELEVE_NR') %in% names(x))) 
    stop('table of species observations must contain a column called RELEVE_NR')
    X <- x
  }

  ## Sanitize
  # Delete empty columns
  na <- apply(site, 2, function(x) all(is.na(x) | x == ''))
  if (any(na)) site <- as.data.frame(site[, !na])
  leer <- apply(site, 2, function(x) all(x == 0 | is.na(x)))
  if (any(leer)) site <- as.data.frame(site[, !leer])
  for (i in c('RELEVE_NR', 'REFERENCE', 'COVERSCALE', 'DATE', 'SURF_AREA')) if (!i %in% names(site)) site[,i] <- NA
  
  if(!drop) {
    ### Add obligatory fields from dictionary
    dbasedic <- read.dbf(file.path(tv.home(), 'Popup', dict, 'dbasedic.dbf'), as.is=TRUE)
    oblig <- dbasedic[dbasedic$FILE_NR == 2, 'FIELD_NAME']
    # oblig <- c('RELEVE_NR','COUNTRY','REFERENCE','TABLE_NR','NR_IN_TAB','COVERSCALE','DATE','SURF_AREA','UTM','ALTITUDE','EXPOSITION','INCLINATIO','COV_TOTAL')
    for(m in oblig[!oblig %in% names(site)])   site[,m] <- ''
    print(names(site))
  } 

  if(!overwrite)
    if(file.exists(file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf')))
      stop('Database ', name, ' already exists. Nothing will be exported.')
  
  site$DATE <- gsub('-','', site$DATE)
  for(i in names(site)) if(is.character(site[,i])) {
    site[is.na(site[,i]),i] <- ''
    site[,i] <- iconv(site[,i], '', iconv)
  }

### Write
  names(X) <- TV.replace(names(X))
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = if(overwrite) FALSE else TRUE)
  write.dbf(site, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))
  write.dbf(X, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))
 # Remarks
 if(!missing(db)) {
  # print(file.path(options('tv_home'), 'Data', db[1], 'remarks.dbf'))
  remarks <- read.dbf(file.path(options('tv_home'), 'Data', db[1], 'remarks.dbf'), as.is=TRUE)
  if(length(db) > 1) 
    for(n in 2:length(db)) remarks <- rbind(remarks, read.dbf(file.path(options('tv_home'), 'Data', db[n], 'remarks.dbf'), as.is=TRUE))
  remarks <- remarks[remarks$RELEVE_NR %in% site$RELEVE_NR,]
  op <- options('warn')
  options(warn=-1)
  suppressWarnings(write.dbf(remarks, file.path(options('tv_home'), 'Data', name, 'remarks.dbf')))
  options(op)
 # TvAdmin
  TvAdmin <- read.dbf(file.path(options('tv_home'), 'Data', db[1], 'TvAdmin.dbf'), as.is = TRUE)
  if(length(db) > 1) 
    for(n in 2:length(db)) TvAdmin <- rbind(TvAdmin, read.dbf(file.path(options('tv_home'), 'Data', db[n], 'TvAdmin.dbf')))
  TvAdmin <- TvAdmin[TvAdmin$RELEVE_NR %in% site$RELEVE_NR,]
  TvAdmin$MOD_USER[is.na(TvAdmin$MOD_USER)] <- Sys.getenv('USER')
  TvAdmin$MOD_DATE[is.na(TvAdmin$MOD_DATE)] <- format(Sys.Date())
  write.dbf(TvAdmin, file.path(options('tv_home'), 'Data', name, 'TvAdmin.dbf'))

  if(!any(db == name))
    file.copy(from = file.path(options('tv_home'), 'Data', db[1], 'tvwin.dbf'), to=file.path(options('tv_home'),'Data', name, 'tvwin.dbf'), overwrite = overwrite)
 }
  cat('Turboveg database', if(!missing(db)) name else db.abs.path, 'written to', file.path(options('tv_home'), 'Data', name),'\n')

if(newTvAdmin) {
  cat('If you want to create a new TvAdmin.dbf, please install a library with uuid capabilities (e.g. UUIDgenerate from package uuid, or uuid.gen from dplR):\n')
  cat('library(uuid)\n')
  cat("tvadmin <- data.frame(RELEVE_NR=site$RELEVE_NR, SOURCE_DB=db,  GUID=replicate(nrow(site), paste('{', UUIDgenerate(), '}', sep='')), CREAT_USER=Sys.getenv('USER'), CREAT_DATE=Sys.Date(), MOD_USER=Sys.getenv('USER'),	MOD_DATE=Sys.Date(), NDFF_QUAL=as.integer(0))\n")
  cat("write.dbf(tvadmin, file.path(options('tv_home'),'Data', db, 'TvAdmin.dbf'))")
  }
}
