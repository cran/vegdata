# x <- c('Aconitum vulgare', 'Homalothecium lutescens')
#########################################################
## function tax
#########################################################

"tax" <- function(...) UseMethod("tax")

tax.default <- function(x, refl, detailed = FALSE, syn = TRUE, concept = NULL, strict = FALSE, vernacular = FALSE, simplify=FALSE, quiet = FALSE, reflist.type = 'Turboveg', ...) {
	tv_home <- tv.home()
  if(missing(x)) stop('x is missing!')
###------ internal functions
#########################################################
# Subsetting
select.taxa <- function(x, species, strict = FALSE, vernacular = FALSE, simplify = FALSE, genus = TRUE, epithet = TRUE, ...) {
  if(is.factor(x)) x <- as.character(x)
  if(is.numeric(x) | is.integer(x))
    ## Tax numbers
    l <- species[match(x, species$TaxonUsageID),]  else
  if(vernacular) {
    stop()
    if(strict)  l <- species[species$VernacularName %in% x, ] else
      l <- species[grepl(x, species$VernacularName), ]
    } else { 
  if(is.character(x)) {															## Selecting by string
  	if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  { ## Lettercode
      l <- species[species$LETTERCODE %in% x,]
      if(nrow(l) > 1) l <- l[!l$SYNONYM,]
    } else {
#    	if(all(sapply(x, function(x) nchar(x) == 36))) {
#    	  message('x is interpreted as 36 character representation of a GUID Taxon ID.')
#       l <- species[match(x, species$TaxonUsageID),] ## GUID Tax ID's		
#     }   																				## Taxnames
    x <- taxname.abbr(x, hybrid = c('substitute'))
	if(simplify) {
#  		if('simplified' %in% names(species)) species$TaxonName <- species$simplified else {
#		  	species$TaxonName <- taxname.simplify(species$TaxonName, genus, epithet, ...)
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
				l <- species[match(x, species$TaxonName, nomatch=0),]
    }
  } } }
 	if(nrow(l) == 0 & !quiet) message('No species found!')
 	return(l)
}
###--- end of tax internal functions ---###
# refl='GermanSL 1.2'; detailed=FALSE; vernacular=FALSE;simplify=FALSE; strict=FALSE
# if(!is.null(concept)) species <- concept.FUN(species, concept)
  
#### beginning to execute function tax()
if(missing(refl)) {
  refl <- tv.refl(tv_home=tv_home)
  if(!quiet) message('Reference list used:', refl)	
}
species <- load.taxlist(refl, reflist.type=reflist.type, detailed=detailed)
### Filter
if(length(x) == 0 | is.na(x[1])) stop('Input taxon value is missing.')

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

