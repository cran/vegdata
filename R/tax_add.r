
# As dBase is an old DOS format, Umlaute  are  stored  using  a  different  code  table
#    (namely ASCII) than most modern unices (namely ANSI).
taxname.abbr <- function(x, hybrid=FALSE, species=FALSE, cf=FALSE) {
#  loc <- Sys.getlocale(category='LC_CTYPE')
#  Sys.setlocale("LC_ALL","C")
#  print('Executing taxname.abbr ...')
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
    if(hybrid) {
      x <- sub('\ x.' , '\ ', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ nssp[.]' , '\ ssp.', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ nvar[.]' , '\ var.', x, perl=TRUE, useBytes=TRUE)
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
    #  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)  
}


taxname.simplify <- function(x, genus=TRUE, epithet=TRUE, hybrid=TRUE, ...) {
#    x <- 'S\U00EBlixae calcarae subsp. holdae'
    x <- gsub('\U00EB', 'e', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\U00CF', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ii', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('nn', 'n', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ph', 'p', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('rh', 'h', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('th', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('tt', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('y', 'i', x, perl=TRUE, useBytes=TRUE)
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
## Turboveg  
  x <- replace(x, x=='ABBREVIAT', 'TaxonName')
  x <- replace(x, x=='taxonName', 'TaxonName')
  x <- replace(x, x=='Taxon', 'TaxonName')
  x <- replace(x, x=='SPECIES_NR', 'TaxonUsageID')
  x <- replace(x, x=='VALID_NAME', 'TaxonConcept')
  x <- replace(x, x=='VALID_NR', 'TaxonConceptID')
  x <- replace(x, x=='AGG_NAME', 'IsChildTaxonOf')
  x <- replace(x, x=='AGG', 'IsChildTaxonOfID')
  x <- replace(x, x=='SECUNDUM', 'AccordingTo')
  x <- replace(x, x=='NATIVENAME', 'VernacularName')
  x <- replace(x, x=='RANG', 'TaxonRank')
  x <- replace(x, x=='CLASSIFICA', 'Classification')
## Florkart Germany (BfN lists)
  x <- replace(x, x=='TAXNAME', 'TaxonName')
  x <- replace(x, x=='sipnr', 'TaxonConceptID')
  x <- replace(x, x=='SIPNR', 'TaxonConceptID')
  x <- replace(x, x=='namnr', 'TaxonUsageID')
  x <- replace(x, x=='NAMNR', 'TaxonUsageID')
  x <- replace(x, x=='TAXNR', 'TaxonUsageID')
  x <- replace(x, x=='AGG_NAME', 'IsChildTaxonOf')
  x <- replace(x, x=='AGGNR', 'IsChildTaxonOfID')
  x <- replace(x, x=='RANK', 'TaxonRank')  
  x <- replace(x, x=='Rank', 'TaxonRank')  
## ESveg
  x <- replace(x, x=="taxonCode", 'TaxonUsageID')
  x <- replace(x, x=="observationCode", "RELEVE_NR")
  x <- replace(x, x=="ObservationID", "RELEVE_NR")
  x <- replace(x, x=="stratumCode", "LAYER")
  x <- replace(x, x=="Stratum", "LAYER")
  x <- replace(x, x=="Percentage_mean", "COVER_PERC")
  x <- replace(x, x=="coverPercent", "COVER_PERC")
## CDM
  x <- replace(x, x=='Taxon', 'TaxonName')
  x <- replace(x, x=='SynUUID', 'TaxonUsageID')
  x <- replace(x, x=='accTaxonId', 'TaxonConceptID')
  return(x)
}

