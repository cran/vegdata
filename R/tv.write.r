if(getRversion() >= "2.15.1")  utils::globalVariables(c("DoWritedbf"))

tv.write <- function(x, site, name, cover=c('code','perc'), overwrite = FALSE, iconv="WINDOWS-1252", db, tvadmin=FALSE, ...) {
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
    stop('column names of species observations must contain RELEVE_NR')
  names(x)[2] <- 'SPECIES_NR'
    X <- x
  }
#  names(X)[2] <- 'SPECIES_NR'
  if(!overwrite) if(file.exists(file.path(options('tv_home'), 'Data', name))) 
    stop('Database ', name, ' already exists.')
  site$DATE <- gsub('-','',site$DATE)
  for(i in names(site)) if(is.character(site[,i])) {
    site[is.na(site[,i]),i] <- ''
    site[,i] <- iconv(site[,i], '', iconv)
  }

### Write
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = TRUE)
  write.dbf(site, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))
  write.dbf(X, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))

  cat('\nTurboveg database', name, 'written to', file.path(options('tv_home'), 'Data', name))

if(!missing(db)) {
  file.copy(from=file.path(options('tv_home'), 'Data', db, 'remarks.dbf'), 
            to=file.path(options('tv_home'),'Data', name, 'remarks.dbf'), overwrite = overwrite)
  file.copy(from=file.path(options('tv_home'), 'Data', db, 'TvAdmin.dbf'), 
            to=file.path(options('tv_home'),'Data', name, 'TvAdmin.dbf'), overwrite = overwrite)
  file.copy(from=file.path(options('tv_home'), 'Data', db, 'tvwin.set'), 
            to=file.path(options('tv_home'),'Data', name, 'tvwin.set'), overwrite = overwrite)

} else {
  file.copy(from=file.path(options('tv_home'), 'Data/taxatest/remarks.dbf'), 
            to=file.path(options('tv_home'),'Data', name, 'remarks.dbf'), overwrite = overwrite)
  cat("\n Please specify correct species list, dictionary and area when first opening the database in Turboveg. GUID's and remarks might be lost.\n")
 }
if(tvadmin) {
  # tvadmin <- read.dbf(file.path(options('tv_home'), 'Data', db, 'TvAdmin.dbf'))
  # RELEVE_NR,N,6,0  SOURCE_DB,C,38	GUID,C,38	CREAT_USER,C,25	CREAT_DATE,D	MOD_USER,C,25	MOD_DATE,D	NDFF_QUAL,N,1,0
  tvadmin <- data.frame(RELEVE_NR=site$RELEVE_NR, SOURCE_DB=db,	GUID=uuid(n=nrow(site)),	CREAT_USER=Sys.getenv('USER'), CREAT_DATE=Sys.Date(), MOD_USER=Sys.getenv('USER'),	MOD_DATE=Sys.Date(), NDFF_QUAL=as.integer(0))
  write.dbf(tvadmin, file.path(options('tv_home'),'Data', name, 'TvAdmin.dbf'))
  #write.tvdbf(tvadmin, file.path(options('tv_home'),'Data', name, 'TvAdmin.dbf'), dec=c(6,38,38,25,10,25,10,1))
}
}

## Version 4 UUIDs have the form:
##    xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
##    where x is any hexadecimal digit and
##    y is one of 8, 9, A, or B
##    f47ac10b-58cc-4372-a567-0e02b2c3d479
uuid <- function(uppercase=FALSE, n=1) {
  
  hex_digits <- c(as.character(0:9), letters[1:6])
  hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits
  
  y_digits <- hex_digits[9:12]
  u <- vector(length = n)
  for(i in 1:n) {
  u[i] <- paste(
    paste0(sample(hex_digits, 8), collapse=''),
    paste0(sample(hex_digits, 4), collapse=''),
    paste0('4', sample(hex_digits, 3), collapse=''),
    paste0(sample(y_digits,1),
           sample(hex_digits, 3),
           collapse=''),
    paste0(sample(hex_digits, 12), collapse=''),
    sep='-')
  }
  return(u)
}
