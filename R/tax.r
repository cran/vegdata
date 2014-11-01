# x <- c('Aconitum vulgare', 'Homalothecium lutescens')
#########################################################
## function tax
#########################################################

"tax" <- function(...) UseMethod("tax")

tax.default <- function(x, refl, verbose = FALSE, syn = TRUE, concept = NULL, strict = FALSE, vernacular = FALSE, simplify=FALSE, quiet = FALSE, reflist.type = 'Turboveg', ...) {
	tv_home <- tv.home()
#########################################################
## internal functions
#########################################################

concept.FUN <- function(species, concept, dbf, ...) {
  message('\n Will use taxon concept', concept, '.\n\n')
  verbose=TRUE
  species$TaxonName <- as.character(species$TaxonName)
  species$TaxonConcept <- as.character(species$TaxonConcept)
  species$IsChildTaxonOf <- as.character(species$IsChildTaxonOf)
  species$AccordingTo <- as.character(species$AccordingTo)
  conc <- read.dbf(file.path(tv_home, 'Species', refl, paste(concept,'dbf',sep='.')), as.is=TRUE)
  co <- conc[match(species$TaxonUsageID, conc$TaxonUsageID, nomatch = 0),]
  species[match(conc$TaxonUsageID,species$TaxonUsageID),c('SYNONYM','TaxonConceptID','IsChildTaxonOfID')] <- co[match(conc$TaxonUsageID,co$TaxonUsageID),c('SYNONYM','TaxonConceptID','IsChildTaxonOfID')]
  species$TaxonName[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$TaxonName[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$TaxonConcept[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$TaxonConcept[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$TaxonRank[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$TaxonRank[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$IsChildTaxonOf[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$IsChildTaxonOf[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
  species$AccordingTo[match(conc$TaxonUsageID,species$TaxonUsageID,nomatch = 0)] <- co$AccordingTo[match(conc$TaxonUsageID,co$TaxonUsageID,nomatch = 0)]
}

# Subsetting
select.taxa <- function(x, species, strict, vernacular = FALSE, simplify = FALSE, genus = TRUE, epithet = TRUE, ...) {
  if(is.factor(x)) x <- as.character(x)
#?  if(simplify) strict <- TRUE
  if(is.numeric(x) | is.integer(x))
    l <- species[match(x, species$TaxonUsageID),]  	## Tax numbers
  if(vernacular) species$TaxonName <- species$VernacularName
  if(is.character(x)) {															## Selecting by string
  	if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  { ## Lettercode
      l <- species[species$LETTERCODE %in% x,]
      if(nrow(l) > 1) l <- l[!l$SYNONYM,]
    } else {
   	if(all(sapply(x, function(x) nchar(x) == 36))) {
   	  message('x is interpreted as 36 character representation of a GUID Taxon ID.')
      l <- species[match(x, species$TaxonUsageID),] ## GUID Tax ID's		
    }   																				## Taxnames
    x <- taxname.abbr(x)
		  if(simplify) {
		  	species$TaxonName <- taxname.simplify(species$TaxonName, genus, epithet, ...)
		 		x <- taxname.simplify(x, genus, epithet, ...) 
		  }
#    	l <- species[match(-1, species$TaxonUsageID),]
#    for(i in 1:length(x)) {
#    	if(!strict) l <- rbind(l, species[grep(x[i], species$TaxonName, useBytes=TRUE), ]) else
#    		l <- rbind(l, species[match(species$TaxonName, x[i], nomatch = 0) > 0, ])
#   	}
			if(!strict) {
				s <- sapply(x, function(f) grep(f, species$TaxonName, useBytes=TRUE))
				l <- species[unlist(s),]
			} else {
				l <- species[match(x, species$TaxonName),]
    }
  } }
 	if(nrow(l) == 0) message('No species found!')
 	return(l)
}

##### end of tax internal functions #####

#### beginning to execute function tax()
if(missing(refl)) refl <- tv.refl(tv_home=tv_home)
if(!quiet) cat('Reference list used:', refl, '\n')	
species <- load.taxlist(refl, reflist.type=reflist.type, verbose=verbose)
# refl='GermanSL 1.2'; verbose=FALSE; vernacular=FALSE;simplify=FALSE; strict=FALSE

### Filter
# if(!is.null(concept)) species <- concept.FUN(species, concept)
if(tolower(x[1]) != 'all') {
	if(simplify) species$originalTaxonName <- species$TaxonName
	species <- select.taxa(x, species, strict, vernacular, simplify, ...)
 }
if(nrow(species) > 0) {
  if(!syn) species <- species[species$SYNONYM == FALSE,]
 }
return(species)
}
#  ls(pos='package:vegdata')

# getS3method('tax', 'default')
tax.veg <- function(veg, ...) {
  if(is.null(attr(veg, 'taxreflist'))) stop('Object must have attribute taxreflist.')
  taxa <- tax.default(names(veg), syn=FALSE, ...)
  message('Taxa in vegetation matrix:\n\n')
  return(taxa$TaxonName[match(names(veg), taxa$LETTERCODE)])
}

