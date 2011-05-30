sub.abbr <- function(x) {
#  loc <- Sys.getlocale(category='LC_CTYPE')
#  Sys.setlocale("LC_ALL","C")
    iconv(x, "latin1", "")
    x <- sub('\ ag[.]', ' agg.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ ssp[.]', ' subsp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ v[.]\ ', ' var. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sv[.]\ ', ' subvar. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Sec[.]\ ', ' sect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Ser[.]\ ', ' ser. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Subs[.]\ ', ' subsect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ spec[.]', ' species', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]l[.]', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]str[.]', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ str[.]', ' sensustricto', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ l[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\ s[.]\ ', ' subsp. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensustricto', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensulato', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ f[.]\ ', ' fo. ', x, perl=TRUE, useBytes=TRUE)
#  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)  
}

tax <- function(x, refl, verbose = FALSE, syn = FALSE, concept = NULL, sysPath=FALSE, ...) {
  tv_home <- tv.home(sysPath=sysPath)
  if(missing(refl)) refl <- tv.refl(tv_home=tv_home, sysPath=sysPath)
  if(!is.null(concept)) {
      cat('\n Taxon concept', concept, 'used.\n\n')
      verbose=TRUE
    }
#   if(is.null(type)) type <- if(is.numeric(x) | is.integer(x)) 'number'
  dbf <- if(verbose) 'tax.dbf' else 'species.dbf'
  supportedReflists <- c('GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2')
  supportedReflists <- c(supportedReflists, sub(' ', '', supportedReflists))
  supportedReflists <- c(supportedReflists, tolower(supportedReflists))
  if(file.access(file.path(tv_home, 'Species', refl, dbf))) {
    if(refl %in% supportedReflists) {
	cat('\nTaxonomic evaluation list (',dbf, ') of', refl, 'not available.\n')
	cat('I will try to download the reference now.\n\n')
	version <- paste('version',substr(refl, 10, nchar(refl)),sep='')
	download.file(paste('http://geobot.botanik.uni-greifswald.de/download/GermanSL',version,'GermanSL.zip',sep='/'), file.path(tv_home, 'Species','GermanSL.zip'))
	unzip(file.path(tv_home, 'Species/GermanSL.zip'), exdir=file.path(tv_home, 'Species'))
    } else cat('\nTaxonomic evaluation list (',dbf, ') of ', refl, 'not available.\n')
  }
  species <- read.dbf(file.path(tv_home, 'Species', refl, dbf))
  species$ABBREVIAT <- sub.abbr(species$ABBREVIAT)
  if(verbose) species$VALID_NAME <- sub.abbr(species$VALID_NAME)
  ## Taxon concepts
  if(!is.null(concept)) {
    species$ABBREVIAT <- as.character(species$ABBREVIAT)
    species$VALID_NAME <- as.character(species$VALID_NAME)
    species$AGG_NAME <- as.character(species$AGG_NAME)
    species$SECUNDUM <- as.character(species$SECUNDUM)
    conc <- read.dbf(file.path(tv_home, 'Species', refl, paste(concept,'dbf',sep='.')), as.is=TRUE)
    co <- conc[match(species$SPECIES_NR, conc$SPECIES_NR, nomatch = 0),]
    species[match(conc$SPECIES_NR,species$SPECIES_NR),c('SYNONYM','VALID_NR','AGG')] <- co[match(conc$SPECIES_NR,co$SPECIES_NR),c('SYNONYM','VALID_NR','AGG')]
#    levels(species$ABBREVIAT) <- c(levels(species$ABBREVIAT), levels(conc$ABBREVIAT))
    species$ABBREVIAT[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$ABBREVIAT[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
#    levels(species$VALID_NAME) <- c(levels(species$VALID_NAME), levels(conc$VALID_NAME))
    species$VALID_NAME[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$VALID_NAME[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$RANG[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$RANG[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$AGG_NAME[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$AGG_NAME[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
#    levels(species$SECUNDUM) <- c(levels(species$SECUNDUM), levels(conc$SECUNDUM))
    species$SECUNDUM[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$SECUNDUM[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    }

  if(refl %in% supportedReflists && verbose==FALSE) species <- species[,c('SPECIES_NR','LETTERCODE','ABBREVIAT','NATIVENAME','SYNONYM', 'VALID_NR')]
  if(x[1] != 'all') {
   if(is.numeric(x) | is.integer(x))   l <- species[match(x, species$SPECIES_NR),]
   if(is.character(x)) {
    x <- sub.abbr(x)
    x <- unique(unlist(strsplit(x, ".", fixed = TRUE)))
    if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  {
	print('Using Lettercodes.')
	l <- species[species$LETTERCODE %in% x,] 
	if(syn == FALSE & !is.numeric(x)) l <- l[l$SYNONYM == FALSE,]
	} else
    if(length(x)==1) l <- species[grep(x, species$ABBREVIAT),] else
    l <- species[species$ABBREVIAT %in% x,]
    }
   if(length(l) == 0) stop('No species found!') 
#   l <- l[!is.na(l$ABBREVIAT),]
   species <- l } else if(!syn) species <- species[species$SYNONYM == FALSE,]

  for(i in which(sapply(species, is.factor))) species[,i] <- as.character(species[,i])
  for(i in which(sapply(species, is.character))) Encoding(species[,i]) <- 'latin1'
  return(species)
} 

spc <- function(...) print('Function spc() is depreacated, please use function tax() instead')


