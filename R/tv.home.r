tv.home <- function(check = FALSE) {
  if(is.null(getOption('tv_home')) | check) {
    if(.Platform$OS.type == "unix") 
     if('Turbowin' %in% list.dirs(path=paste(Sys.getenv('HOME'),'/.wine/drive_c', sep=''), full.names=FALSE, recursive = FALSE))
          tv_home <- file.path(Sys.getenv('HOME'),'.wine/drive_c/Turbowin') else
          tv_home <- NA

  if(.Platform$OS.type == "windows") {
    if(file.access('O:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'O:/Turbowin' else 
    	if(file.access('C:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'C:/Turbowin' else 
    	  if(file.access('C:/Programs/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programs/Turbowin' else
    	    if(file.access('C:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
    	      if(file.access('D:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'D:/Programme/Turbowin' else
              tv_home <- NA
  }
  if(is.na(tv_home)) {
   message('\nNo Turbowin installation path found. \n')
   if(interactive()) {
   ANSWER <- readline("Should I use \n 1) the vegdata package path (recommended),  or \n 2) a temporary folder? ")
   tv_home <- switch(substr(ANSWER, 1, 1),
         "1" = file.path(path.package('vegdata'), 'tvdata'),
         tempdir()
   ) } else tv_home <- file.path(path.package('vegdata'), 'tvdata') #tempdir()
   options(tv_home = tv_home)
   if(!file.exists(file.path(tv_home, 'tvdata', 'Popup', 'tvscale.dbf')))
     for(d in c('Popup', 'Data', 'Species')) {
     dir.create(file.path(tv_home, d), showWarnings = FALSE)
     if(d == 'Data') {
       wd <- getwd()
       setwd(file.path(path.package('vegdata'), 'tvdata', 'Data')  )
       dbs <- list.dirs(, recursive=TRUE, full.names=FALSE)
       for(l in 2:length(dbs)) {
         dir.create(file.path(tv_home, 'Data', dbs[l]), showWarnings = FALSE)
       file.copy(from =  list.files(dbs[l], recursive=TRUE, full.names=TRUE, include.dirs=TRUE), to = file.path(tv_home, 'Data', dbs[l]))
       } 
       setwd(wd)
     } else
     file.copy(from =  list.files(file.path(path.package('vegdata'), 'tvdata', d), recursive=TRUE, full.names=TRUE, include.dirs=TRUE), to = file.path(tv_home, d))
   }
  }
  message('############################################################',
      '\nTurboveg root directory is set to "', tv_home, '"',
      '\nIf you want to change this use: options(tv_home=\"<path_to_your_Turbowin_root>\")',
      '\n############################################################')
  options(tv_home=tv_home)
  } else {
  tv_home <- getOption('tv_home')
#  message('Turboveg root directory has already been set to "', tv_home,'".\n', sep='')
  }
  invisible(tv_home)
}


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

tv.biblio <- function(x='all', db, dict = tv.dict(db), quiet=FALSE, tv_home, iconv = "CP437", ...) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(missing(db) & missing(dict)) {
    message('Using tvrefenc.dbf from default dictionary.')
    dict = ''
  }
  if(dict == 'default') dict <- ''
  bibliopath <- file.path(tv_home, 'Popup', dict, 'tvrefenc.dbf')
  biblio <- read.dbf(bibliopath, as.is=TRUE)
  for(i in c('AUTHOR','TITLE','PUBLISHED', 'ADDRESS')) 
    if(i %in% names(biblio)) biblio[,i] <- iconv(biblio[,i], iconv, "")  
  if(x[1] != 'all') {
    x <- as.numeric(unique(x))
    biblio <- biblio[match(x, as.numeric(biblio$REFERENCE)),]
    if(!quiet) print(biblio)
  }
  invisible(biblio)
}
