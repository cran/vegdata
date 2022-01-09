#' Search taxonomic reference lists including concept synonomy and taxonomic hierarchy.
#'
#' @name tax
#' @aliases tax
#'
#' @usage
#'  tax(x, refl, detailed = TRUE, syn = TRUE, concept = NULL, strict = FALSE,
#'                         simplify = FALSE, quiet = FALSE, ...)
#' @export
#' @param x Species number, lettercode or species name(s)
#' @param refl Taxonomic reference list
#' @param detailed In old Turboveg versions detailed taxonomic information could only be given in an extra file which was called tax.dbf in GermanSL. Compatibility mode.
#' @param syn Return also synonym names
#' @param concept Name of the file with an alternative taxon view stored in the reference list directory, see details.
#' @param strict Exact match or partial matching with \code{\link{grep}}
#' @param simplify Should taxname.simplify be applied to find species
#' @param quiet Hide screen messages
#' @param \dots additional attributes for taxname.abbr or taxname.simplify
#'
#' @description Input is either species number (integer), shortletter (7 characters) or full (exact!) species name.
#'
#' @details
#'   \dfn{concept}: GermanSL is a list with a single taxon view according to the standard lists of the different taxon groups (e.g Wisskirchen and Haeupler for higher plants, see).
#'   Nevertheless a huge number of synonyms is included which allows in many cases the transformation into different concepts.
#'   For illustration the concept of \emph{Armeria maritima} from Korneck 1996 is included, which accepts e.g. \emph{Armeria maritima ssp. bottendorfensis}.
#'   \dfn{parse.taxa}: parse genus and epitheta from name strings.
#'   \dfn{taxname.removeAuthors} Remove name authors from full scientific name strings.
#'
#' @references
#'   Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste für Vegetationsdatenbanken. Tuexenia, 28, 239-253.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'

# "tax" <- function(...) UseMethod("tax")

tax <- function(x, refl, detailed = TRUE, syn = TRUE, concept = NULL, strict = FALSE, simplify = FALSE, quiet = FALSE, ...) {
	tv_home <- tv.home()
  if(missing(x)) stop('x is missing!')
###------ internal functions
######################################################## #
# Subsetting
select.taxa <- function(x, species, strict = FALSE, simplify = FALSE, genus = TRUE, epithet = TRUE, ...) {
  if(is.factor(x)) x <- as.character(x)
  if(is.numeric(x) | is.integer(x))
    ## Tax numbers
    l <- species[match(x, species$TaxonUsageID),] else { 															## Selecting by string
  	if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  { ## Lettercode
      l <- species[species$LETTERCODE %in% x,]
      if(nrow(l) > 1) l <- l[!l$SYNONYM,]
    } else {
   	if(all(sapply(x, function(x) nchar(x) == 36))) {
   	  if(is.null(getOption('UUID.taxon'))) {
   	      message('x is interpreted as 36 character representation of a UUID Taxon ID.')
   	      options(UUID.taxon = TRUE)
   	  }
      l <- species[match(x, species$TaxonUsageID), ] ## GUID Tax ID's
    } else {   																				## Taxnames
    x <- taxname.abbr(x, ...)
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

#### beginning to execute function tax() #
if(missing(refl)) {
  refl <- if(!is.null(getOption('tv.refl'))) getOption('tv.refl') else tv.refl(tv_home=tv_home)
  if(!quiet) message('Reference list used: ', refl)
}
if(is.character(refl))
   species <- load.taxlist(refl, detailed=detailed) else
     species <- refl
### Filter
if(length(x) == 0 | is.na(x[1])) stop('Input taxon value is missing.')

if(tolower(x[1]) != 'all') {
	if(simplify) species$originalTaxonName <- species$TaxonName
	species <- select.taxa(x, species, strict, simplify, ...)
 }
if(nrow(species) > 0) {
  if(!syn) species <- species[species$SYNONYM == FALSE,]
 }
return(species)
}
#  ls(pos='package:vegdata')

# # getS3method('tax', 'default')
# tax.veg <- function(veg, ...) {
#   if(is.null(attr(veg, 'taxreflist'))) stop('Object must have attribute taxreflist.')
#   taxa <- tax.default(names(veg), syn=FALSE, ...)
#   message('Taxa in vegetation matrix:\n\n')
#   return(taxa$TaxonName[match(names(veg), taxa$LETTERCODE)])
# }

