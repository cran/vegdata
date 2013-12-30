# Load taxonomic reference list
load.taxlist <- function(refl, reflist.type= c('Turboveg', 'EDIT') , verbose=FALSE, ...) {
  reflist.type <- match.arg(reflist.type, c('Turboveg', 'EDIT'))
  if(reflist.type == 'Turboveg') {
    tv_home <- tv.home()
    reflist <- paste(refl, ifelse(verbose,'.verbose',''), sep='')
    if(is.null(store(reflist))) { # load.species(refl=refl, verbose = verbose)
      dbf <- if(verbose) 'tax.dbf' else 'species.dbf'
      supportedReflists <- c('GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2', 'Czsk 0.1')
      supportedReflists <- c(supportedReflists, sub(' ', '', supportedReflists))
      supportedReflists <- c(supportedReflists, tolower(supportedReflists))
      reflist.path <- file.path(tv_home, 'Species', refl, dbf)
      
      if(file.access(reflist.path)) {
        if(refl %in% supportedReflists) {
          message('\nTaxonomic list (',dbf, ') of reflist (version) ', refl, ' not available.\n\n')
            storage <- file.path(tv_home, 'Species')
          tfile <- tempfile()
           if(grepl('GermanSL', refl)) {
            version <- paste("version", substr(refl, 10, nchar(refl)), sep = "")
            download.file(paste('http://geobot.botanik.uni-greifswald.de/download/GermanSL',version,'GermanSL.zip',sep='/'), tfile)
            unzip(tfile, exdir= storage)
           }
           if(grepl('Czsk', refl)) {
            version <- paste("version", substr(refl, 6, nchar(refl)), sep = "")
            download.file(paste('http://geobot.botanik.uni-greifswald.de/download/CZSK',version, 'Czsk.zip',sep='/'), tfile)
            unzip(tfile, exdir= storage)
           }
          }  else stop('\nTaxonomic list ', refl, ' not supported.\n')
      } else storage <- file.path(tv_home, 'Species')

      species <- read.dbf(file.path(storage, refl, dbf), as.is=TRUE)
      names(species) <- TCS.replace(names(species))
      species$TaxonName <- taxname.abbr(species$TaxonName, ...)
      if(verbose) {
        species$TaxonConcept <- taxname.abbr(species$TaxonConcept, ...)
        if('VernacularName' %in% names(species)) species$VernacularName <- iconv(species$VernacularName, from='UTF8', to='')
        if('Author' %in% names(species)) species$AUTHOR <- iconv(species$AUTHOR, from='UTF8', to='')
      }  else {
        if('VernacularName' %in% names(species)) species$VernacularName <- iconv(species$VernacularName, from='CP850', to='')
        if('Author' %in% names(species)) species$AUTHOR <- iconv(species$AUTHOR, from='CP850', to='')
      }
      if(refl %in% supportedReflists && verbose==FALSE) species <- species[,c('TaxonUsageID','LETTERCODE','TaxonName', 'VernacularName','SYNONYM', 'TaxonConceptID')] else {
        include <- !names(species) %in% c('SHORTNAME')
        species <- species[, include]
      }
       store(reflist, species)
    } else  species <- store(reflist)
  } else stop(c('Until now only reference list type "Turboveg" is supported.', if(reflist.type == 'EDIT') 'Use package vegdata.dev to use lists from the EDIT platform.'))
  	
   return(species)
}

store <- local({
  refList <- list()
  function(name, value) {
    if(missing(name)) return(refList)
    if(missing(value)) return(refList[[name]])
    refList[name] <<- list(value)
  }
})


tv.refl <- function(refl, db, tv_home) {

#   capwords <- function(s, strict = FALSE) {
#       cap <- function(s) paste(toupper(substring(s,1,1)), {s <- substring(s,2); if(strict) toupper(s) else s}, sep = "", collapse = " " )
#       sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
#   }
  if(missing(tv_home)) tv_home <- tv.home()
  if(!missing(db)) {
      dbattr <- file.path(tv_home, 'Data', db,'tvwin.set')
      if(file.access(dbattr)==0) refl <-  sub('A\002', '', readBin(dbattr,what='character', n=3)[3]) else 
    stop('Database attribute file tvwin.set from database "', db, '" not available. Please specify name of taxonomic reference list!') 
  } else  
    if(!missing(refl)) {
      rli <- list.dirs(path = file.path(tv_home, "Species"), full.names = TRUE, recursive = FALSE)
      rli <- sapply(rli, function(x) substring(x, nchar(tv_home) + 10), USE.NAMES = FALSE)
      if(length(rli) > 0) refl <- match.arg(refl, rli)
    } else refl <- 'GermanSL 1.2'
#  if(!exists(refl)) refl <- fun(gsub(' ','',refl))
  if(tolower(substr(refl, 1,8)) == 'germansl') refl <- paste('GermanSL', substring(refl, 9, nchar(refl)), sep='')
  return(refl)
 }
