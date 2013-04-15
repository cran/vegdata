# Load taxonomic reference list
load.taxlist <- function(refl, reflist.type= 'Turboveg', verbose, ...) {
  reflist.type <- match.arg(reflist.type, c('Turboveg'))
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
          cat('\nTaxonomic evaluation list (',dbf, ') of reflist version', refl, 'not available.\n')
          cat('I will try to download the list now.\n\n')
          if(grepl('GermanSL', refl)) {
            version <- paste("version", substr(refl, 10, nchar(refl)), sep = "")
            download.file(paste('http://geobot.botanik.uni-greifswald.de/download/GermanSL',version,'GermanSL.zip',sep='/'), file.path(tv_home, 'Species','TaxRefDownload.zip'))
            unzip(file.path(tv_home, 'Species', 'TaxRefDownload.zip'), exdir=file.path(tv_home, 'Species'))
          }
          if(grepl('Czsk', refl)) {
            version <- paste("version", substr(refl, 6, nchar(refl)), sep = "")
            download.file(paste('http://geobot.botanik.uni-greifswald.de/download/CZSK',version, 'Czsk.zip',sep='/'), file.path(tv_home, 'Species','TaxRefDownload.zip'))
            unzip(file.path(tv_home, 'Species', 'TaxRefDownload.zip'), exdir=file.path(tv_home, 'Species'))
          }
      } else cat('\nTaxonomic evaluation list (',dbf, ') of ', refl, 'not available.\n')
      }
      species <- read.dbf(file.path(tv_home, 'Species', refl, dbf))
      names(species) <- TCS.replace(names(species))
      species$TaxonName <- taxname.abbr(species$TaxonName)
      if(verbose) {
        species$TaxonConcept <- taxname.abbr(species$TaxonConcept)
        if('VernacularName' %in% names(species)) species$VernacularName <- iconv(species$VernacularName, 'ISO-8859-1', '')
      }     
      if(refl %in% supportedReflists && verbose==FALSE) species <- species[,c('TaxonUsageID','LETTERCODE','TaxonName','VernacularName','SYNONYM', 'TaxonConceptID')] else {
        include <- !names(species) %in% c('SHORTNAME')
        species <- species[, include]
      }   
      store(reflist, species)
    } else  species <- store(reflist)       
  } else stop('Only reflisttype Turboveg implemented until now')
  
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

# As dBase is an old DOS format, Umlaute  are  stored  using  a  different  code  table
#    (namely ASCII) than most modern unices (namely ANSI).  If you encounter
#    such a file, I would recommend piping the output through recode(1) with
#    ibmpc:latin1 as its argument.
taxname.abbr <- function(x, enc="ISO-8859-1") {
#  loc <- Sys.getlocale(category='LC_CTYPE')
#  Sys.setlocale("LC_ALL","C")
    x <- iconv(x, enc, "")
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
    x <- sub('\ s[.]lat[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.] lat[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\ s[.]\ ', ' subsp. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensustricto', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensulato', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ f[.]\ ', ' fo. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sp[.]', ' spec.', x, perl=TRUE, useBytes=TRUE)
    #  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)  
}

TCS.replace <- function(x) {
## Turboveg  
  x <- replace(x, x=='ABBREVIAT', 'TaxonName')
  x <- replace(x, x=='taxonName', 'TaxonName')
  x <- replace(x, x=="Species name", 'TaxonUsageID') # temporary shortcut for TV3
  x <- replace(x, x=='SPECIES_NR', 'TaxonUsageID')
  x <- replace(x, x=='VALID_NAME', 'TaxonConcept')
  x <- replace(x, x=='VALID_NR', 'TaxonConceptID')
  x <- replace(x, x=='AGG_NAME', 'IsChildTaxonOf')
  x <- replace(x, x=='AGG', 'IsChildTaxonOfID')
  x <- replace(x, x=='SECUNDUM', 'publishedInCitation')
  x <- replace(x, x=='NATIVENAME', 'VernacularName')
  x <- replace(x, x=='RANG', 'taxonRank')
## Florkart Germany (BfN lists)
  x <- replace(x, x=='TAXNAME', 'TaxonName')
  x <- replace(x, x=='sipnr', 'TaxonConceptID')
  x <- replace(x, x=='SIPNR', 'TaxonConceptID')
  x <- replace(x, x=='namnr', 'TaxonUsageID')
  x <- replace(x, x=='NAMNR', 'TaxonUsageID')
  x <- replace(x, x=='AGG_NAME', 'IsChildTaxonOf')
  x <- replace(x, x=='AGGNR', 'IsChildTaxonOfID')
## ESveg
  x <- replace(x, x=="taxonCode", 'TaxonUsageID')
  x <- replace(x, x=="observationCode", "RELEVE_NR")
  x <- replace(x, x=="ObservationID", "RELEVE_NR")
  x <- replace(x, x=="stratumCode", "LAYER")
  x <- replace(x, x=="Stratum", "LAYER")
  x <- replace(x, x=="Percentage_mean", "COVER_PERC")
  x <- replace(x, x=="coverPercent", "COVER_PERC")
  return(x)
}



#########################################################
## function tax
#########################################################

"tax" <- function(...) UseMethod("tax")

tax.default <- function(x, refl, verbose = FALSE, syn = TRUE, concept = NULL, strict = FALSE, vernacular = FALSE, quiet = FALSE, ...) {
	tv_home <- tv.home()
#########################################################
## internal functions
#########################################################

concept.FUN <- function(species, concept, dbf, ...) {
  cat('\n Will use taxon concept', concept, '.\n\n')
  verbose=TRUE
  species$TaxonName <- as.character(species$TaxonName)
  species$TaxonConcept <- as.character(species$TaxonConcept)
  species$IsChildTaxonOf <- as.character(species$IsChildTaxonOf)
  species$publishedInCitation <- as.character(species$publishedInCitation)
  conc <- read.dbf(file.path(tv_home, 'Species', refl, paste(concept,'dbf',sep='.')), as.is=TRUE)
  co <- conc[match(species$TaxonUsageID, conc$TaxonUsageID, nomatch = 0),]
  species[match(conc$TaxonUsageID,species$TaxonUsageID),c('SYNONYM','TaxonConceptID','IsChildTaxonOfID')] <- co[match(conc$TaxonUsageID,co$TaxonUsageID),c('SYNONYM','TaxonConceptID','IsChildTaxonOfID')]
  species$TaxonName[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$TaxonName[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$TaxonConcept[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$TaxonConcept[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$taxonRank[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$taxonRank[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$IsChildTaxonOf[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$IsChildTaxonOf[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$publishedInCitation[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$publishedInCitation[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
}

# Subsetting
select.taxa <- function(x, species, strict, vernacular, ...) {
  if(is.factor(x)) x <- as.character(x)
  if(is.numeric(x) | is.integer(x))
    l <- species[match(x, species$TaxonUsageID),]  ## Tax numbers
  if(is.character(x)) {
    if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  {  ## Lettercode
      l <- species[species$LETTERCODE %in% x,] 
    } else {		## Taxnames
      l <- species[species$TaxonUsageID==(-1),]
      for(i in 1:length(x))
        if(vernacular) {
          l <- if(!strict) rbind(l, species[grep(x[i], species$VernacularName, useBytes=TRUE), ]) else 
            rbind(l, species[match(species$VernacularName, x[i], nomatch = 0) > 0, ])
        }
      else {
        l <- if(!strict) rbind(l, species[grep(x[i], species$TaxonName, useBytes=TRUE), ]) else 
          rbind(l, species[match(species$TaxonName, x[i], nomatch = 0) > 0, ])
      }}
  }
  if(length(l) == 0) stop('No species found!')
  return(l)
}

##### end of tax internal functions #####

if(missing(refl)) refl <- tv.refl(refl, tv_home=tv_home)
if(!quiet) cat('Reference list used:', refl, '\n')	
species <- load.taxlist(refl, reflist.type='Turboveg', verbose=verbose)

### Filter
# if(!is.null(concept)) species <- concept.FUN(species, concept)
if(x[1] != 'all') species <- select.taxa(x, species, strict, vernacular, ...)
  if(!syn) species <- species[species$SYNONYM == FALSE,]
return(species)
}
#  ls(pos='package:vegdata')

# getS3method('tax', 'default')
tax.veg <- function(veg, ...) {
  if(is.null(attr(veg, 'taxreflist'))) stop('Object must have attribute taxreflist.')
  taxa <- tax.default(names(veg), syn=FALSE, ...)
  cat('Taxa in vegetation matrix:\n\n')
  return(taxa$TaxonName[match(names(veg), taxa$LETTERCODE)])
}

