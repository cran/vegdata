
# As dBase is an old DOS format, Umlaute  are stored  using a different code table (CP437)
#    (namely ASCII) than most modern unices (namely ANSI).
taxname.abbr <- function(x, hybrid = c('ignore', 'TRUE', 'preserve', 'FALSE', 'substitute'), species = FALSE, cf = FALSE, ...) {
    hybrid <- as.character(hybrid)
    hybrid <- match.arg(hybrid, c('ignore', 'TRUE', 'preserve', 'FALSE', 'substitute'))
  #  loc <- Sys.getlocale(category='LC_CTYPE')
#  Sys.setlocale("LC_ALL","C")
#  print('Executing taxname.abbr ...')
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
    x <- sub('\ s[.]l[.]', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]str[.]', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ str[.]', ' sensustricto', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ l[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]lat[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.] lat[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ ', ' subsp. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensustricto', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensulato', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ f[.]\ ', ' fo. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ species', ' spec.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ nothosubsp[.]' , '\ nothossp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ nothossp[.]' , '\ nssp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ nothovar[.]' , '\ nvar.', x, perl=TRUE, useBytes=TRUE)
    if(hybrid %in% c('ignore', 'TRUE')) {
      x <- sub('\ x ' , '\ ', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ nssp[.]' , '\ ssp.', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ nvar[.]' , '\ var.', x, perl=TRUE, useBytes=TRUE)
    }
    if(hybrid %in% c('preserve', 'FALSE')) {
    }
    if(hybrid == 'substitute') {
      x <- sub('\ x\ ' , ' \u00d7\ ', x, perl=TRUE, useBytes=TRUE)
    }

    if(cf) x <- sub('^cf.\ ', '', x, ignore.case=TRUE)
		if(species)  {
      x <- sub('\ sp[.]', ' species', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ sp$', ' species', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ spec[.]', ' species', x, perl=TRUE, useBytes=TRUE) 
			} else {
			x <- sub('\ sp[.]' , '', x, perl=TRUE, useBytes=TRUE)
			x <- sub('\ sp$', '', x, perl=TRUE, useBytes=TRUE)
			x <- sub('\ spec[.]' , '', x, perl=TRUE, useBytes=TRUE)
			x <- sub('\ species$' , '', x, perl=TRUE, useBytes=TRUE)
			}
    x <- sub("\\s+$", "", x) # trim trailing leading spaces
    #  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)  
}


taxname.simplify <- function(x, genus=TRUE, epithet=TRUE, hybrid=TRUE, concept='ignore', ...) {
#    x <- 'S\U00EBlixae calcarae subsp. holdae'
    x <- gsub('\U00EB', 'e', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\U00CF', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ii', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('nn', 'n', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('fa', 'pha', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ph', 'p', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('rh', 'h', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('th', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('tt', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('y', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ranum', 'rianum', x, perl=TRUE, useBytes=TRUE)
if(concept == 'ignore') {
  x <- gsub(' s[.] str[.]', '', x, perl=TRUE, useBytes=TRUE)
  x <- gsub(' s[.] l[.]', '', x, perl=TRUE, useBytes=TRUE)
}
if(epithet) {
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
    x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('us\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
    x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ae', 'e', substr(x, regexpr('\ ', x), nchar(x))), sep='')    
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
if(hybrid) {
	x <- gsub(' x ', ' ', x)
	x <- gsub('\U00D7', '', x)
}
return(x)
}
# taxname.simplify(x, Gattungsendung=TRUE, Artendung=TRUE)


TCS.replace <- function(x) {
## Turboveg & ## Florkart Germany (BfN lists)
  x <- replace(x, toupper(x) %in% c('SPECIES_NR', 'TAXNR', 'NAMNR', 'NAMEID', 'TAXONUSAGEID'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('ABBREVIAT','TAXONNAME','TAXON','TAXNAME'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('VALID_NR', 'SIPNR', 'NAMNR_GUELT', 'SYNNAMEID', 'TAXONCONCEPTID'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('VALID_NAME', 'VALIDNAME', 'TAXONCONCEPT'), 'TaxonConcept')
  x <- replace(x, toupper(x) %in% c('AGG', 'AGGNR', 'NAMEPARENTID', 'ISCHILDTAXONOFID'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('AGG_NAME', 'AGGNAME', 'ISCHILDTAXONOF'), 'IsChildTaxonOf')
  x <- replace(x, toupper(x) %in% c('SECUNDUM', 'ACCORDINGTO'), 'AccordingTo')
  x <- replace(x, toupper(x) %in% c('NATIVENAME', "COMMONNAME", 'VERNACULARNAME'), 'VernacularName')
  x <- replace(x, toupper(x) %in% c('CLASSIFICA', 'CLASSIFICATION'), 'Classification')
  x <- replace(x, toupper(x) %in% c('RANG', 'RANK', "TAXONOMICRANK", 'TAXONRANK'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('AUTOR', 'AUTHOR'), 'NameAuthor')

## ESveg & FloraEuropaaea
  x <- replace(x, toupper(x) %in% c("TAXONCODE", 'ETAXON'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('PARENT'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('RANK_NAME'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('SYNONYM_OF'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c("OBSERVATIONCODE"), "RELEVE_NR")
  x <- replace(x, toupper(x) %in% c("OBSERVATIONID"), "RELEVE_NR")
  x <- replace(x, toupper(x) %in% c("STRATUMCODE"), "LAYER_ID")
  x <- replace(x, toupper(x) %in% c("STRATUM"), "LAYER")
  x <- replace(x, toupper(x) %in% c("PERCENTAGE_MEAN", "COVERPERCENT"), "Cover_Perc")

## CDM & EuroMed
  x <- replace(x, toupper(x) %in% c('TAXON', 'NAMECACHE'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('SYNUUID', 'RIDENTIFIER'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('ACCTAXONID', 'ACCEPTEDTAXONFK'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('RANKABBREV'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('PARENTKEY'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('AUTHORSTRING'), 'NameAuthor')

## Veg-X
  x <- replace(x, toupper(x) %in% c('PLOTNAME'), 'RELEVE_NR')
  x <- replace(x, toupper(x) %in% c('ORGANISMIDENTITYNAME'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('STRATUMNAME'), 'LAYER')
  x <- replace(x, toupper(x) %in% c('AGG_1_VALUE'), 'COVER_CODE')
  
  return(x)
}


TV.replace <- function(x) {
  ## Florkart Germany (BfN lists) and others
  x <- replace(x, toupper(x) %in% c('TAXONUSAGEID', 'TAXNR', 'NAMNR', 'NAMEID'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('TAXONNAME','TAXON','TAXNAME'), 'ABBREVIAT')
  x <- replace(x, toupper(x) %in% c('TAXONCONCEPTID', 'SIPNR', 'SYNNAMEID','NAMNR_GUELT'), 'VALID_NR')
  x <- replace(x, toupper(x) %in% c('VALIDNAME', 'TAXONCONCEPT'), 'VALID_NAME')
  x <- replace(x, toupper(x) %in% c('AGGNR', 'NAMEPARENTID', 'ISCHILDTAXONOFID'), 'AGG')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAXONOF'), 'AGG_NAME')
  x <- replace(x, toupper(x) %in% c('ACCORDINGTO'), 'SECUNDUM')
  x <- replace(x, toupper(x) %in% c("CommonName", 'VernacularName'), 'NATIVENAME')
  x <- replace(x, toupper(x) %in% c('Classification'), 'CLASSIFICA')
  x <- replace(x, toupper(x) %in% c('RANK', 'TAXONOMICRANK', 'TAXONRANK'), 'RANG')
  x <- replace(x, toupper(x) %in% c('NAMEAUTHOR', 'AUTHOR'), 'AUTHOR')
    
  ## CDM & EuroMed
  x <- replace(x, toupper(x) %in% c('SYNUUID', 'RIDENTIFIER'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('ACCTAXONID'), 'VALID_NR')
  x <- replace(x, toupper(x) %in% c('AGGNAME'), 'PARNAME')
  x <- replace(x, toupper(x) %in% c('PARENTKEY'), 'PARENT')

  ## Veg-X
  x <- replace(x, toupper(x) %in% c('PLOTNAME'), 'RELEVE_NR')
  x <- replace(x, toupper(x) %in% c('ORGANISMIDENTITYNAME'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('STRATUMNAME'), 'LAYER')
  x <- replace(x, toupper(x) %in% c('AGG_1_VALUE'), 'COVER_CODE')

  return(x)
}

