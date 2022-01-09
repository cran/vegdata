store <- local({
  refList <- list()
  function(name, value) {
    if(missing(name)) return(refList)
    if(missing(value)) return(refList[[name]])
    refList[name] <<- list(value)
  }
})


load.taxlist <- function(refl, detailed = TRUE, ...) {
    tv_home <- tv.home(...)
    dbf <- if(detailed && refl %in% c('GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2', 'GermanSL 1.3')) 'tax.dbf' else 'species.dbf'
    refl.dir <- if(dir.exists(file.path(tv_home, 'species'))) 'species' else 'Species'
    reflist.path <- file.path(tv_home, refl.dir, refl, dbf)
    reflist <- paste(refl, ifelse(detailed,'.detailed',''), sep='')
    if(is.null(store(reflist))) {
      if(!file.exists(reflist.path)) {
        message(paste('Taxonomic reference list file', refl, 'does not exist. Search path was', reflist.path))
        if(tolower(substr(refl, 1, 8)) %in% c('germansl')) {
          message('Will try to downlaod reflist ', refl, ' (species.dbf) ...\n')
          # if(interactive()) {
          #   ANSWER <- readline("Should I use \n 1) a temporary directory,  or \n 2) the vedata package path (recommended, if you want to reuse the reflist repeatedly)? ")
          #   tv_home <- switch(substr(ANSWER, 1, 1),
          #                     "1" = tempdir(),
          #                     "2" = file.path(path.package('vegdata'), 'tvdata'),
          #                     tempdir())
          #   options(tv_home = tv_home)
             tfile <- tempfile(tmpdir = tv.home())
             version <- substr(refl, 10, nchar(refl))
            remote <- paste('https://germansl.infinitenature.org/GermanSL', version, 'GermanSL.zip',sep='/')
            if(curl::has_internet())
            if (httr::http_error(remote)) { # network is down = message (not an error anymore)
              message("No internet connection or data source broken.")
              return(NULL)
            } else { # network is up = proceed to download via curl
              message("Downloading remote dataset.")
              status <- tryCatch(
                RCurl::getURL(remote, ssl.verifypeer=FALSE, useragent="R"),
                error = function(e) e
              )
              if(inherits(status,  "error", TRUE))
              curl::curl_download(url = paste('https://germansl.infinitenature.org/GermanSL', version, 'GermanSL.zip',sep='/'), destfile = tfile, quiet = T)
              else {
                message("No internet connection.")
                return(invisible(NULL))
              }
            } # /if - network up or down
#            m <- try(download.file(paste('https://germansl.infinitenature.org/GermanSL', version, 'GermanSL.zip',sep='/'), tfile), silent=TRUE)
            if(file.exists(tfile)) unzip(tfile, exdir= file.path(tv_home, refl.dir)) else {
              message('Using example data instead.')
              unzip(file.path(path.package('vegdata'), 'tvdata', refl.dir, 'TaxrefExample.zip'), exdir = file.path(tv_home, refl.dir))
            }
        #   } else {
        #     message('Using example data instead.')
        #     unzip(file.path(path.package('vegdata'), 'tvdata', refl.dir, 'TaxrefExample.zip'), exdir = file.path(tv_home, refl.dir))
        # }
        message('Option "tv_home" is ', tv.home())
        } else stop('\nTaxonomic list ', refl, ' does not exist and is not supported for automatic download.\n')
      }
      if(file.exists(reflist.path)) species <- read.dbf(reflist.path, as.is=TRUE) else stop(paste('Reference list file', reflist.path, 'does not exist.'))
      names(species) <- TCS.replace(names(species)) # replaces column names according to the Taxon Concept Scheme, see: tax.names.R
      # TaxonName must be ASCII according to Taxonomic Code, except hybrid sign
      if('TaxonName' %in% names(species)) species$TaxonName <- iconv(species$TaxonName, from=getOption("tv.iconv"), to='')
      if('NameAuthor' %in% names(species)) species$NameAuthor <- iconv(species$NameAuthor, from=getOption("tv.iconv"), to='')
      if('VernacularName' %in% names(species)) species$VernacularName <- iconv(species$VernacularName, from=getOption("tv.iconv"), to='')
      if(detailed) { # i.e. if tax.dbf is loaded
        if('NACHWEIS' %in% names(species)) species$NACHWEIS <- iconv(species$NACHWEIS, from='UTF-8', to='')
        if('BEGRUEND' %in% names(species)) species$BEGRUEND <- iconv(species$BEGRUEND, from='UTF-8', to='')
        if('AccordingTo' %in% names(species)) species$AccordingTo <- iconv(species$AccordingTo, from='UTF-8', to='')
      }
      # supported reference list
      if(detailed==FALSE)
        species <- species[,c('TaxonUsageID','LETTERCODE','TaxonName', 'VernacularName','SYNONYM', 'TaxonConceptID')] else
          {
            include <- !names(species) %in% c('SHORTNAME')
            species <- species[, include]
          }
      # if(is.character(species$TaxonUsageID)) warning(cat('Taxon ID should be integer or numeric. Please check reference list', refl, ifelse(detailed, 'tax.dbf', 'species.dbf'), '\n'))
       store(reflist, species)
    } else  species <- store(reflist)

   return(species)
}



#' Taxon reference list to be used
#' @name tv.refl
#' @export
#' @param refl name of reference list
#' @param db Turboveg database name
#' @param tv_home Turboveg installation path

tv.refl <- function(refl, db, tv_home) {
  if(missing(tv_home)) tv_home <- tv.home()
  if(!missing(db)) {
    if(file.access(file.path(tv_home, 'Data', db[1], 'tvwin.dbf')) == 0) {
      refl <- read.dbf(file.path(tv_home, 'Data', db[1], 'tvwin.dbf'), as.is = TRUE)$FLORA
    } else {
      dbattr <- file.path(tv_home, 'Data', db[1], 'tvwin.set')
      if(file.access(dbattr) == 0) refl <-  sub('A\002', '', readBin(dbattr,what='character', n=3)[3]) else
    stop('Database attribute file (tvwin.dbf) from database "', db[1], '" not available. Please specify name of taxonomic reference list!')
  } } else
    if(!missing(refl)) {
      rli <- list.dirs(path = file.path(tv_home, "Species"), full.names = TRUE, recursive = FALSE)
      rli <- sapply(rli, function(x) substring(x, nchar(tv_home) + 10), USE.NAMES = FALSE)
      if(length(rli) > 0) refl <- match.arg(refl, rli)
      options(tv.refl = refl)
    } else refl <- if(!is.null(getOption('tv.refl'))) getOption('tv.refl') else 'GermanSL 1.5'
  if(tolower(substr(refl, 1,8)) == 'germansl') refl <- paste('GermanSL', substring(refl, 9, nchar(refl)), sep='')
  return(refl)
 }
