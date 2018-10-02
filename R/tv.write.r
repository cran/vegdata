if(getRversion() >= "2.15.1")  utils::globalVariables(c("DoWritedbf"))

tv.write <- function(x, site, name, tvadmin, remarks, dict = '', cover=c('code','perc'), drop = FALSE, obl = TRUE, overwrite = FALSE, iconv="CP437", ...) {
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
      warning('Species observations should be either class \"tv.obs\" or \"vw.obs\".')
    if(!all(c('RELEVE_NR') %in% names(x))) 
      stop('table of species observations must contain a column called RELEVE_NR')
    X <- x
    names(X) <- TV.replace(names(X))
  }
  refl <- if(!is.null(attr(X, "taxreflist"))) attr(X, "taxreflist") else tv.refl()
  
  if(drop) {
    # Delete empty columns
    na <- apply(site, 2, function(x) all(is.na(x) | x == ''))
    if (any(na)) site <- as.data.frame(site[, !na])
    leer <- apply(site, 2, function(x) all(x == 0 | is.na(x)))
    if (any(leer)) site <- as.data.frame(site[, !leer])
  }
  for(i in names(site)) if(is.character(site[,i])) {
    site[is.na(site[,i]),i] <- ''
    site[,i] <- iconv(site[,i], '', iconv)
  }
  
  if(obl) {
    #  for (i in c('RELEVE_NR', 'REFERENCE', 'COVERSCALE', 'DATE', 'SURF_AREA')) if (!i %in% names(site)) site[,i] <- NA
    ### Add obligatory fields from dictionary
    dbasedic <- read.dbf(file.path(tv.home(), 'Popup', dict, 'dbasedic.dbf'), as.is=TRUE)
    oblig <- dbasedic[dbasedic$FILE_NR == 2, 'FIELD_NAME']
    for(m in oblig[!oblig %in% names(site)])   site[,m] <- ''
    site <- site[, match(unique(c(oblig, names(site))), names(site))] # order columns
  }
  
  if(!overwrite)
    if(file.exists(file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf')))
      stop('Database ', name, ' already exists. Nothing will be exported.')
  
  site$DATE <- gsub('-','', site$DATE)
  
  ### Write Turboveg database
  dir.create(file.path(options('tv_home'), 'Data', name), showWarnings = if(overwrite) FALSE else TRUE)
  write.dbf(site, file.path(options('tv_home'), 'Data', name, 'tvhabita.dbf'))
  write.dbf(X, file.path(options('tv_home'), 'Data', name, 'tvabund.dbf'))
  # write TvAdmin
  if(missing(tvadmin)) {
        tvadmin <- data.frame(RELEVE_NR=site$RELEVE_NR, SOURCE_DB='R',  GUID=replicate(nrow(site), paste('{', uuid::UUIDgenerate(), '}', sep='')), CREAT_USER=Sys.getenv('USER'), CREAT_DATE=Sys.Date(), MOD_USER=Sys.getenv('USER'),	MOD_DATE=Sys.Date(), NDFF_QUAL=as.integer(0))
      } else if(is.character(tvadmin))
        tvadmin <- read.dbf(file.path(options('tv_home'), 'Data', tvadmin, 'TvAdmin.dbf'), as.is=TRUE) 
  
  TvAdmin <- tvadmin
  TvAdmin <- TvAdmin[TvAdmin$RELEVE_NR %in% site$RELEVE_NR,]
  TvAdmin$MOD_USER[is.na(TvAdmin$MOD_USER)] <- Sys.getenv('USER')
  TvAdmin$MOD_DATE[is.na(TvAdmin$MOD_DATE)] <- format(Sys.Date())
  write.dbf(TvAdmin, file.path(options('tv_home'), 'Data', name, 'TvAdmin.dbf'))
  
  # write remarks
  if(missing(remarks)) remarks <- data.frame(RELEVE_NR=numeric(), REMARKS=character()) else
  if(nrow(remarks) > 0) remarks <- remarks[remarks$RELEVE_NR %in% site$RELEVE_NR,]
  op <- options('warn')
  options(warn=-1)
  suppressWarnings(write.dbf(remarks, max_nchar = 250, file.path(options('tv_home'), 'Data', name, 'remarks.dbf')))
  options(op)

  # write tvwin.dbf
  write.dbf(data.frame(FLORA=refl, MINALLOW=0, MAXALLOW=0, MINACTUAL= min(site$RELEVE_NR),	MAXACTUAL=max(site$RELEVE_NR), MAP='', DICTIONARY=dict, META=''), file = file.path(options('tv_home'),'Data', name, 'tvwin.dbf'))
  cat('Turboveg database', name, 'written to', file.path(options('tv_home'), 'Data', name),'\n')
}
