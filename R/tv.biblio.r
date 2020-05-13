tv.dict <- function(db, tv_home) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(length(db) > 1) {
    for(i in 1:length(db))
      print(paste(db[i], ':   ', read.dbf(file.path(tv_home, 'Data', db[i], 'tvwin.dbf'), as.is = TRUE)$DICTIONARY))
    warning('Dictionary given by the first database will be used.')
  }
  if(file.exists(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'))) {
    attrib <- read.dbf(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'), as.is = TRUE)
    if(is.na(attrib$DICTIONARY)) attrib$DICTIONARY <- ''
    return(attrib$DICTIONARY)
  } else {
    dbattr <- file.path(tv_home, 'Data', db[1], 'tvwin.set')
    if(file.access(dbattr)==0) {
      allbytes <- readBin(dbattr, "raw", n = 100, size = 1, endian = "little")
      bin <- sapply(allbytes, readBin, what='character')
      return(readBin(allbytes[(which(bin == 'C')[3]+3):length(allbytes)], what=character()))
    } else warning('neither tvwin.set nor tvwin.dbf can be found')
  }
}


tv.db <- function(path='.') {
  ###
  tv.filenames <- c('tvhabita.dbf', 'tvabund.dbf', 'remarks.dbf', 'TvAdmin.dbf')
  valid.TVdb <- function(p) {
    all(tv.filenames %in% list.files(file.path(tv.home(), 'Data', path, p)))
  }
  ###
  dir <- list.dirs(path = file.path(tv.home(), 'Data', path), full.names = FALSE, recursive = TRUE)
  dir <- dir[dir != ""]
  if(length(dir)>0) {
    for(d in dir) {
      fiLe <- list.files(file.path(tv.home(), 'Data', path, d))
      FiLe <- fiLe[which(tolower(fiLe) %in% tolower(tv.filenames) & !fiLe %in% tv.filenames)]
      if(length(FiLe) > 0)
        for(i in 1:length(FiLe))
          file.rename(from = file.path(tv.home(), 'Data', path, d, FiLe[i]), to = file.path(tv.home(), 'Data', path, d, tv.filenames[which(tolower(tv.filenames) %in% tolower(FiLe))][i]))
    }
    valid.dir <- dir[sapply(dir, valid.TVdb)]
    return(file.path(path, valid.dir))
  } else paste('No TV directory found.')
}


tv.metadata <- function (db, refl, tv_home, filename = 'metadata.txt', ...)
{
  if (missing(tv_home)) tv_home <- tv.home()
  if (db[1] == "eco") {
    if(missing(refl)) refl <- tv.refl(db = db[1], tv_home = tv_home, ...)
    shell.exec(file.path(tv_home, "Species", refl, "metadata-eco.txt"))
  } else {
    for(i in 1:length(db)) {
      meta <- file.path(tv_home, "Data", db[i], filename)
      if (file.access(meta)) stop('No metainfo file "',filename, '" available in directory "', db[1], '".') else 
        if(.Platform$OS.type == "windows") shell.exec(meta)  else file.show(meta)
    }}
}

tv.biblio <- function(x='all', db, dict = tv.dict(db), quiet=FALSE, tv_home, ...) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(missing(db) & missing(dict)) {
    message('Using tvrefenc.dbf from default dictionary.')
    dict = ''
  }
  if(dict == 'default') dict <- ''
  bibliopath <- file.path(tv_home, 'Popup', dict, 'tvrefenc.dbf')
  biblio <- read.dbf(bibliopath, as.is=TRUE)
  for(i in c('AUTHOR','TITLE','PUBLISHED', 'ADDRESS')) 
    if(i %in% names(biblio)) biblio[,i] <- iconv(biblio[,i], getOption('tv.iconv'), "")
  if(x[1] != 'all') {
    x <- as.numeric(unique(x))
    biblio <- biblio[match(x, as.numeric(biblio$REFERENCE)),]
    if(!quiet) print(biblio)
  }
  invisible(biblio)
}
