#' Standardisation of taxonomic names, especially taxon rank indicators and hybrid signs
#' name taxname.abbr
#'
#' @export
#' @param x (integer or character) Species number, lettercode or species name(s)
#' @param hybrid (logical) remove hybrid markers for comparisons
#' @param concept (logical) remove concept additions like "s. str.", "s. l.
#' @param species (logical) remove "spec.", "sp.", or "species" for genus level taxa
#' @param cf (logical) remove 'in doubt' marker
#' @param \dots additional attributes
#'
#' @author Florian Jansen florian.jansen@uni-rostock.de
#'

taxname.abbr <- function(x, hybrid = TRUE, concept = FALSE, species = TRUE, cf = TRUE, ...) {
    #  loc <- Sys.getlocale(category='LC_CTYPE')
    #  Sys.setlocale("LC_ALL","C")
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    x <- trim(x)
    x <- gsub('\"', '', x)
    x <- sub('\ ag[.]', ' agg.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ aggr[.]', ' agg.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ species group[\ ]', ' agg.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ ssp[.]', ' subsp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ v[.]\ ', ' var. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sv[.]\ ', ' subvar. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Sec[.]\ ', ' sect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sect[.][(]bot[.][)]\ ', ' sect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Ser[.]\ ', ' ser. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Subs[.]\ ', ' subsect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ se?n?s?[.]\\s?st?r?[.]', ' sensustricto', x)
    x <- sub('\ s[.]\\s?la?t?o?[.]', ' sensulato', x)
    x <- sub('\ s[.]\ ', ' subsp. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ f[.]\ ', ' fo. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ species', ' spec.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ nothosubsp[.]' , '\ nothossp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ nothossp[.]' , '\ nothosubsp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ nothovar[.]' , '\ nvar.', x, perl=TRUE, useBytes=TRUE)
    if(concept) {
      x <- sub('\ sensulato', '', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ sensustricto', '', x, perl=TRUE, useBytes=TRUE)
    } else {
      x <- sub('\ sensulato', ' s. l.', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ sensustricto', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    }

    if(hybrid) {
      x <- gsub('\ x ' , '\ ', x, perl=TRUE, useBytes=TRUE)
      x <- gsub('^x ' , '', x, perl=TRUE, useBytes=TRUE)
      x <- gsub('\U00D7', '', x)
      x <- sub('\ nssp[.]' , '\ ssp.', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ nvar[.]' , '\ var.', x, perl=TRUE, useBytes=TRUE)
    } else {
      x <- gsub('\ x\ ' , ' \u00d7', x, perl=TRUE, useBytes=TRUE)
    }
    if(cf) x <- sub('cf.\ ', '', x, ignore.case=TRUE)
		if(species)  {
		  x <- sub('\ sp[.]' , '', x, perl=TRUE, useBytes=TRUE)
		  x <- sub('\ sp$', '', x, perl=TRUE, useBytes=TRUE)
		  x <- sub('\ spec[.]' , '', x, perl=TRUE, useBytes=TRUE)
		  x <- sub('\ species$' , '', x, perl=TRUE, useBytes=TRUE)
			} else {
		  x <- sub('\ sp[.]', ' species', x, perl=TRUE, useBytes=TRUE)
		  x <- sub('\ sp$', ' species', x, perl=TRUE, useBytes=TRUE)
		  x <- sub('\ spec[.]', ' species', x, perl=TRUE, useBytes=TRUE)
		}
    x <- sub("\\s+$", "", x) # trim trailing leading spaces
    #  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)
}



#' @title Simplify name parts for better string matching
#'
#' @name taxname.simpl
#'
#' @export
#' @param x (integer or character) Species number, lettercode or species name(s)
#' @param genus (logical) simplify genus name part
#' @param hybrid (logical) remove hybrid markers
#' @param concept (logical) remove name parts which describe taxon concept size like "s. str.","s. l."
#' @param rank (logical) remove rank specifications
#' @param epithet (logical) simplify epithet(s)
#' @param tax.status (logical) remove taxon status like 'nom. illeg.' or 'auct.'
#' @param \dots additional attributes
#'
#' @details taxname.abbr will be applied beforehand automatically. The function simplifies name parts which are empirically unstable, i.e. sylvatica might alos written as silvatica, or majus s maius. Sex of latin genus or epithet name parts often change and are therefore deleted (us vs. a, ea vs. eos, etc.). taxname.simpl works well for plant names, but be careful with very long name lists or if combined with animal taxa which are sometimes very short and can be confused after applying taxname.simpl
#'
#' @author Florian Jansen florian.jansen@uni-rostock.de
#'

taxname.simplify <- function(x, genus=TRUE, epithet=TRUE, hybrid=TRUE, concept = TRUE, rank=TRUE, tax.status=TRUE, ...) {
  if(tax.status) {
    x <- gsub(' nom[.] illeg[.]', '', x, perl=TRUE, useBytes=TRUE)
    x <- gsub(' nom[.] inval[.]', '', x, perl=TRUE, useBytes=TRUE)
    x <- gsub(' nom[.] rej[.]', '', x, perl=TRUE, useBytes=TRUE)
    x <- gsub(' auct[.]', '', x, perl=TRUE, useBytes=TRUE)
  }
  if(concept) {
    x <- sub('\ sensulato', '', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensustricto', '', x, perl=TRUE, useBytes=TRUE)
    x <- gsub(' s[.] str[.]', '', x, perl=TRUE, useBytes=TRUE)
    x <- gsub(' s[.] l[.]', '', x, perl=TRUE, useBytes=TRUE)
  } else {
    x <- sub('\ sensulato', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensustricto', ' s. str.', x, perl=TRUE, useBytes=TRUE)
  }

    x <- gsub('\U00EB', 'e', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\U00CF', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ii', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ck', 'k', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('nn', 'n', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('fa', 'pha', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ej', 'ei', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('oe', 'ae', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ph', 'p', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('rh', 'h', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('rr', 'r', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('th', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('tt', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub( 'y', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ranum', 'rianum', x, perl=TRUE, useBytes=TRUE)

    if(hybrid) {
      x <- gsub('^X ', '', x)
      x <- gsub('^x ' , '', x)
      x <- gsub(' x ',' ', x)
      x <- gsub('\U00D7 ', '', x)
      x <- gsub('\U00D7', '', x)
    }
    if(rank) {
      x <- gsub(' fo.', '', x, fixed = TRUE)
      x <- gsub(' race', '', x, fixed = TRUE)
      x <- gsub(' var.', '', x, fixed = TRUE)
      x <- gsub(' subsp.', '', x, fixed = TRUE)
      x <- gsub(' [ranglos]', '', x, fixed = TRUE)
    }
    if(genus) {
      x <-	paste(sub('a$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('as$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('e$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('es$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('eus$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('is$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('on$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('u$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('um$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('us$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
    }
    if(epithet) {
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('us\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ae\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('arum\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ea\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ei\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('eos\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ia\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ium\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ius\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('orum\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')

        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('a\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('e\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ens\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('es\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('i\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('is\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('on\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('um\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('-', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
    }
 return(x)
}

#' Standardise taxon list field names to match the Taxonomic Concept Transfer Schema (TCS)
#'
#' @name TCS.replace
#' @description Applies Taxonomic Concept Transfer Schema (TCS) to the different name list conventions of different sources
#'
#' @export
#' @param x (character) string of column names used in data.frames storing taxon lists
#'
#' @author Florian Jansen florian.jansen@uni-rostock.de
#' @references Taxonomic Names and Concepts interest group. 2006. Taxonomic Concept Transfer Schema (TCS), version 1.01. Biodiversity Information Standards (TDWG) http://www.tdwg.org/standards/117

TCS.replace <- function(x) {
## Turboveg & ## Florkart Germany
  x <- replace(x, toupper(x) %in% c('TAXONUSAGEID', 'SPECIES_NR', 'TAXNR', 'NAMNR', 'NAMEID', "NAME_ID", 'TAXONUSAGEID','TAXONID'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('TAXONNAME','ABBREVIAT','TAXONNAME','TAXON','TAXNAME',"WISS_NAME"), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('TAXONCONCEPTID','VALID_NR', 'SIPNR', 'NAMNR_GUELT', 'SYNNAMEID', 'TAXONCONCEPTID',"ACCEPTEDNAMEUSAGEID"), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('TAXONCONCEPT', 'VALID_NAME', 'VALIDNAME', 'TAXONCONCEPT'), 'TaxonConcept')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAXONOFID', 'AGG', 'AGGNR', 'NAMEPARENTID', 'ISCHILDTAXONOFID', "PARENTNAMEUSAGEID"), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAXONOF', 'AGG_NAME', 'AGGNAME', 'ISCHILDTAXONOF'), 'IsChildTaxonOf')
  x <- replace(x, toupper(x) %in% c('ACCORDINGTO', 'SECUNDUM'), 'AccordingTo')
  x <- replace(x, toupper(x) %in% c('VERNACULARNAME', 'NATIVENAME', "COMMONNAME"), 'VernacularName')
  x <- replace(x, toupper(x) %in% c('CLASSIFICATION','CLASSIFICA'), 'Classification')
  x <- replace(x, toupper(x) %in% c('TAXONRANK', 'RANG', 'RANK', "TAXONOMICRANK"), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('NAMEAUTHOR', 'AUTOR', 'AUTHOR',"SCIENTIFICNAMEAUTHORSHIP"), 'NameAuthor')
  x <- replace(x, toupper(x) %in% c('REFERENCE', 'BIBLIO'), 'REFERENCE')

## ESveg & FloraEuropaaea & sPlot
  x <- replace(x, toupper(x) %in% c("TAXONCODE", 'ETAXON', 'SPECIESID'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('TAXON_NAME'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('PARENT'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('RANK_NAME'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('SYNONYM_OF'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('PLOTOBSERVATIONID', "OBSERVATIONCODE", "OBSERVATIONID","PLOTID", "RELEVE_NR"), "PlotObservationID")
  x <- replace(x, toupper(x) %in% c("STRATUMCODE"), "LAYER_ID")
  x <- replace(x, toupper(x) %in% c("STRATUM"), "LAYER")
  x <- replace(x, toupper(x) %in% c("PERCENTAGE_MEAN", "COVERPERCENT"), "Cover_Perc")

## CDM & EuroMed
  x <- replace(x, toupper(x) %in% c('NAME', 'TAXON', 'NAMECACHE'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('TAXONUUID', 'SYNUUID', 'RIDENTIFIER'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('ACCTAXONID', 'ACCEPTEDTAXONFK'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('RANKABBREV'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('PARENTKEY'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('AUTHORSTRING'), 'NameAuthor')

## Veg-X
  x <- replace(x, toupper(x) %in% c('PLOTNAME', 'RELEVE_NR'), 'PlotObservationID')
  x <- replace(x, toupper(x) %in% c('ORGANISMIDENTITYNAME'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('STRATUMNAME'), 'LAYER')
  x <- replace(x, toupper(x) %in% c('AGG_1_VALUE'), 'COVER_CODE')

## APG IV, wfo / taxize(db)
  x <- replace(x, toupper(x) %in% c('SCIENTIFICNAME'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c("TAXONRANK"), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c("PARENTNAMEUSAGE"), 'IsChildTaxonOf')

  return(x)
}

#' Rename data.frame columns to match Turboveg 2 conventions
#'
#' @export
#' @param x (character) string vector of column names
#' @name TV.replace
#'

TV.replace <- function(x) {
  ## Florkart Germany (BfN lists) and others
  x <- replace(x, toupper(x) %in% c("TAXONUSAGE", 'TAXONUSAGEID', 'TAXNR', 'NAMNR', 'NAMEID', "NAME_ID"), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('TAXONNAME','TAXON','TAXNAME',"WISS_NAME"), 'ABBREVIAT')
  x <- replace(x, toupper(x) %in% c('TAXONCONCE', 'VALIDNAME', 'TAXONCONCEPT'), 'VALID_NAME')
  x <- replace(x, toupper(x) %in% c('"TAXONCONC2"', 'TAXONCONCEPTID', 'SIPNR', 'SYNNAMEID','NAMNR_GUELT'), 'VALID_NR')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAX', 'AGGNR', 'NAMEPARENTID', 'ISCHILDTAXONOFID',"TAXON_CHILD_OF"), 'AGG')
  x <- replace(x, toupper(x) %in% c( "ISCHILDTA2", 'ISCHILDTAXONOF'), 'AGG_NAME')
  x <- replace(x, toupper(x) %in% c('ACCORDINGTO'), 'SECUNDUM')
  x <- replace(x, toupper(x) %in% c("COMMONNAME", 'VERNACULAR', 'VERNACULARNAME'), 'NATIVENAME')
  x <- replace(x, toupper(x) %in% c('CLASSIFICATION'), 'CLASSIFICA')
  x <- replace(x, toupper(x) %in% c('RANK', 'TAXONOMICRANK', 'TAXONRANK'), 'RANG')
  x <- replace(x, toupper(x) %in% c('NAMEAUTHOR', 'AUTHOR'), 'AUTHOR')

  ## CDM & EuroMed
  x <- replace(x, toupper(x) %in% c('SYNUUID', 'RIDENTIFIER'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('ACCTAXONID'), 'VALID_NR')
  x <- replace(x, toupper(x) %in% c('AGGNAME'), 'PARNAME')
  x <- replace(x, toupper(x) %in% c('PARENTKEY'), 'PARENT')

  ## Veg-X
  x <- replace(x, toupper(x) %in% c('PLOTNAME', 'PLOTOBSERVATIONID'), 'RELEVE_NR')
  x <- replace(x, toupper(x) %in% c('ORGANISMIDENTITYNAME'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('STRATUMNAME'), 'LAYER')
  x <- replace(x, toupper(x) %in% c('AGG_1_VALUE'), 'COVER_CODE')

  return(x)
}



#' Remove name authors from taxon names
#'
#' @export
#' @param x (character) vector of taxon names
#' @name taxname.removeAuthors
#'

taxname.removeAuthors <- function(x) {
    trimws(sapply(x, function(x) {
      UC <- unlist(gregexpr("[A-Z]", x, perl=TRUE))
      if(length(UC) == 0) stop(paste("Can not detect a valid taxon name in:", x))
      if(length(UC) == 1) x else
         substr(x, 1, unlist(gregexpr("[A-Z]", x, perl=TRUE))[2]-2)
      }
    ))
}




#' Parse taxon strings into genus part and epitheta
#'
#' @export
#' @param x (character) taxon names
#' @param epis (character) vector of separators for epithets (like e.g. "subsp.")
#' @name parse.taxa
#'

parse.taxa <- function(x, epis) {
  warning('Function does not account for "Sect./Ser." etc., nor for intraspecific taxa without specifier ("subsp./var." etc. or aggregates ("agg.")')
  if(length(x) <= 1) stop('Only for vectors of length > 1, i.e. more than one taxon name')
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
          sep="", collapse=" ")
  }

  if(missing(epis)) epis <- c('subsp.', 'var.', 'v.')
  original <- x
  x <- taxname.abbr(x, species = TRUE)
  x <- sub(' sp.', replacement = '', x = x, fixed = TRUE)
  x <- sub(' spec.', replacement = '', x = x, fixed = TRUE)
  x <- sub(' SP.', replacement = '', x = x, fixed = TRUE)
  x <- sub(' SPEC.', replacement = '', x = x, fixed = TRUE)
  genus <- sapply(x, stringr::word, 1)
  genus <- sapply(genus, simpleCap)
  epi1 <- sapply(x, stringr::word, 2)
  intra <- sapply(epis, function(y) grepl(y, x, fixed = TRUE))
  rank.suff <- as.character(apply(intra, 1, function(x) epis[x]))
  epi2 <- character(length = length(x))
  if(any(intra)) for(i in 1:length(x)) if(rank.suff[i] != 'character(0)') epi2[i] <- paste(rank.suff[i], stringr::word(strsplit(x[i], rank.suff[i], fixed = TRUE)[[1]][2],2))
  species <- trimws(paste(genus, tolower(epi1), tolower(epi2)))
  species[species == "NA NA"] <- ''
  species <- sub(' NA', replacement = '', x = species, fixed = TRUE)
  epi2 <-  sub('subsp. ', replacement = '', x = epi2, fixed = TRUE)
  result <- data.frame(original, genus, epi1=tolower(epi1), rank.suff= if(length(rank.suff)>0) rank.suff else NA, epi2, scientificName = species)
  result$rank.suff[result$rank.suff == 'character(0)'] <- NA
  return(result)
}
# parse.taxa(x)
# add.legal <- c('-',';')



#' Recode species names, lettercodes or ID's
#' @name recode.species
#' @export
#' @param x vector of species
#' @param names one of 7digit shortletter codes, species id's or scientific species names
#' @param refl (character) name of taxon reference list
#'
recode.species <- function(x, names = c('shortletters', 'Numbers', 'ScientificNames'), refl) {
  if('veg' %in% class(x)) {
    message('Vegetation matrix detected.\n It is assumed that species identifier are given in column names and consist either of 7 character lettercodes, or integer species IDs, with or without appendices for layer "species.layer' )
    refl <- attr(x, 'taxreflist')
    x <- colnames(x)
  } else  if(missing(refl)) refl <- tv.refl()
  names <- match.arg(names)
  taxonlist <- tax('all', refl, detailed = TRUE)
  sp <- strsplit(x, split = '.', fixed = TRUE)
  taxa <- sapply(sp, '[', 1)
  if(all(nchar(taxa) == 6 | nchar(taxa) == 7)) {
    paste('Shortletters detected.')
    df.names <- data.frame(colnames = x, shortletters = sapply(sp, '[', 1), layer = sapply(sp, '[', 2))
    df.names$TaxonName <- taxonlist$TaxonConcept[match(df.names$shortletters, taxonlist$LETTERCODE)]
    df.names$TaxonUsageID <- taxonlist$TaxonUsageID[match(df.names$TaxonName, taxonlist$TaxonName)]
    if(any(is.na(df.names$TaxonUsageID))) warning('Not all shortletters could be resolved. Check column names and reflist.')
  }
  if(!any(is.na(suppressWarnings(as.integer(taxa))))) {
    paste('Numerical Species IDs detected.')
    df.names <- data.frame(colnames = x, TaxonUsageID = sapply(sp, '[', 1), layer = sapply(sp, '[', 2))
    df.names$TaxonName <- taxonlist$TaxonName[match(df.names$TaxonUsageID, taxonlist$TaxonUsageID)]
    df.names$shortletters <- taxonlist$LETTERCODE[match(df.names$TaxonUsageID, taxonlist$TaxonUsageID)]
    if(any(is.na(df.names$shortletters))) warning('Not all TaxonUsageIDs could be resolved. Check column names and reflist.')
  }
  if(!exists('df.names')) stop('Column names of vegetation matrix must consist either of 7 charachter lettercodes, or integer species IDs, with or without appendices for layer "species.layer"')
  if(names == 'shortletters')
    return(ifelse(is.na(df.names$layer), df.names$shortletters, paste(df.names$shortletters, df.names$layer, sep='.')))
  if(names == 'Numbers')
    return(ifelse(is.na(df.names$layer), df.names$TaxonUsageID, paste(df.names$TaxonUsageID, df.names$layer, sep='.')))
  if(names == 'ScientificNames')
    return(ifelse(df.names$layer == '0', df.names$TaxonName, paste(df.names$TaxonName, df.names$layer, sep='.')))
}

# rename.species(veg, 'N')
