store <- local({
  refList <- list()
  function(name, value) {
    if(missing(name)) return(refList)
    if(missing(value)) return(refList[[name]])
    refList[name] <<- list(value)
  }
})

# Load taxonomic reference list
load.taxlist <- function(refl, reflist.type = c('Turboveg', 'EDIT'), detailed = TRUE, check = FALSE, hybrid, ...) {
  reflist.type <- match.arg(reflist.type, c('Turboveg', 'EDIT'))
  if(missing(hybrid)) hybrid = 'retain'
  if(reflist.type == 'Turboveg') {
    tv_home <- do.call("tv.home", list(check = check))
    dbf <- if(detailed && refl %in% c('GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2', 'GermanSL 1.3')) 'tax.dbf' else 'species.dbf'
    refl.dir <- if(dir.exists(file.path(tv_home, 'species'))) 'species' else 'Species'
    reflist.path <- file.path(tv_home, refl.dir, refl, dbf)
    reflist <- paste(refl, ifelse(detailed,'.detailed',''), sep='')
    if(is.null(store(reflist))) {
      supportedReflists <- c('GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2', 'GermanSL 1.3', 'GermanSL 1.4')
      supportedReflists <- c(supportedReflists, sub(' ', '', supportedReflists))
      supportedReflists <- c(supportedReflists, tolower(supportedReflists))
      if(!file.exists(reflist.path)) {
        message(paste('Taxonomic reference list file', reflist.path, 'does not exist.'))
        if(refl %in% supportedReflists) {
          message('\nTaxonomic list (',dbf, ') of reflist', refl, ' not available.\n\n')
          tfile <- tempfile()
           if(grepl('GermanSL', refl)) {
            version <- substr(refl, 10, nchar(refl))
            m <- try(download.file(paste('https://germansl.infinitenature.org/GermanSL', version, 'GermanSL.zip',sep='/'), tfile), silent=TRUE)
            if(m == 0) unzip(tfile, exdir= file.path(tv_home, refl.dir)) else 
              unzip(file.path(path.package('vegdata'), 'tvdata', refl.dir, 'TaxrefExample.zip'), exdir = file.path(tv_home, refl.dir))
           }
          } else message('\nTaxonomic list ', refl, ' not supported for automatic download.\n')
      }
      
      if(file.exists(reflist.path)) species <- read.dbf(reflist.path, as.is=TRUE) else stop(paste('Reference list file', reflist.path, 'does not exist.'))
      names(species) <- TCS.replace(names(species)) # replaces column names according to the Taxon Concept Scheme, see: tax.names.R
      species$TaxonName <- taxname.abbr(..., x = species$TaxonName, hybrid = hybrid) # standardizes several characters and abbreviations. see: tax.names.R 
      # TaxonName must be ASCII according to Taxonomic Code, therefore iconv is not necesary
      if('NameAuthor' %in% names(species)) species$NameAuthor <- iconv(species$NameAuthor, from=getOption("tv.iconv"), to='')
      if('VernacularName' %in% names(species)) species$VernacularName <- iconv(species$VernacularName, from=getOption("tv.iconv"), to='')
      if(detailed) { # i.e. if tax.dbf is loaded
        species$TaxonConcept <- taxname.abbr( ..., x=species$TaxonConcept, hybrid = hybrid)
        if('NACHWEIS' %in% names(species)) species$NACHWEIS <- iconv(species$NACHWEIS, from='UTF-8', to='')
        if('BEGRUEND' %in% names(species)) species$BEGRUEND <- iconv(species$BEGRUEND, from='UTF-8', to='')
        if('AccordingTo' %in% names(species)) species$AccordingTo <- iconv(species$AccordingTo, from='UTF-8', to='')
      }
      # supported reference list
      if(refl %in% supportedReflists && detailed==FALSE) 
        species <- species[,c('TaxonUsageID','LETTERCODE','TaxonName', 'VernacularName','SYNONYM', 'TaxonConceptID')] else {
        include <- !names(species) %in% c('SHORTNAME')
        species <- species[, include]
      }
      # if(is.character(species$TaxonUsageID)) warning(cat('Taxon ID should be integer or numeric. Please check reference list', refl, ifelse(detailed, 'tax.dbf', 'species.dbf'), '\n'))
       store(reflist, species)
    } else  species <- store(reflist)
  } else stop(c('Until now only reference list type "Turboveg" is supported. If you want to use taxval with other reflists please contact me.'))
  	
   return(species)
}


tv.refl <- function(refl, db, tv_home) {
#   capwords <- function(s, strict = FALSE) {
#       cap <- function(s) paste(toupper(substring(s,1,1)), {s <- substring(s,2); if(strict) toupper(s) else s}, sep = "", collapse = " " )
#       sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
#   }
  if(missing(tv_home)) tv_home <- tv.home()
  if(!missing(db)) {
    if(file.access(file.path(tv_home, 'Data', db[1], 'tvwin.dbf')) == 0) {
      refl <- read.dbf(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'), as.is = TRUE)$FLORA
    } else {
      dbattr <- file.path(tv_home, 'Data', db[1], 'tvwin.set')
      if(file.access(dbattr) == 0) refl <-  sub('A\002', '', readBin(dbattr,what='character', n=3)[3]) else 
    stop('Database attribute file tvwin.set from database "', db[1], '" not available. Please specify name of taxonomic reference list!') 
  } } else  
    if(!missing(refl)) {
      rli <- list.dirs(path = file.path(tv_home, "Species"), full.names = TRUE, recursive = FALSE)
      rli <- sapply(rli, function(x) substring(x, nchar(tv_home) + 10), USE.NAMES = FALSE)
      if(length(rli) > 0) refl <- match.arg(refl, rli)
      options(tv.refl = refl)
    } else refl <- if(!is.null(getOption('tv.refl'))) getOption('tv.refl') else 'GermanSL 1.4'
#  if(!exists(refl)) refl <- fun(gsub(' ','',refl))
  if(tolower(substr(refl, 1,8)) == 'germansl') refl <- paste('GermanSL', substring(refl, 9, nchar(refl)), sep='')
  return(refl)
 }
