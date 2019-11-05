
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
      x <- gsub('\U00D7 ', '', x)
      x <- sub('\ nssp[.]' , '\ ssp.', x, perl=TRUE, useBytes=TRUE)
      x <- sub('\ nvar[.]' , '\ var.', x, perl=TRUE, useBytes=TRUE)
    }
    if(hybrid %in% c('preserve', 'FALSE')) {
    }
    if(hybrid == 'substitute') {
      x <- sub('\ x\ ' , ' \u00d7\ ', x, perl=TRUE, useBytes=TRUE)
    }

    if(cf) x <- sub('cf.\ ', '', x, ignore.case=TRUE)
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


taxname.simplify <- function(x, genus=TRUE, epithet=TRUE, hybrid=TRUE, concept.add='ignore', ...) {
    x <- gsub('\U00EB', 'e', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\U00CF', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ii', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('nn', 'n', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('fa', 'pha', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('oe', 'ae', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ph', 'p', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('rh', 'h', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('rr', 'r', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('th', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('tt', 't', x, perl=TRUE, useBytes=TRUE)
    x <- gsub( 'y', 'i', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('ranum', 'rianum', x, perl=TRUE, useBytes=TRUE)
if(concept.add == 'ignore') {
  x <- gsub(' s[.] str[.]', '', x, perl=TRUE, useBytes=TRUE)
  x <- gsub(' s[.] l[.]', '', x, perl=TRUE, useBytes=TRUE)
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
if(hybrid) {
	x <- gsub(' x ', ' ', x)
	x <- gsub('\U00D7 ', '', x)
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

## APG IV
  x <- replace(x, toupper(x) %in% c('SCIENTIFICNAME'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c("TAXONRANK"), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c("PARENTNAMEUSAGE"), 'IsChildTaxonOf')

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
  x <- replace(x, toupper(x) %in% c("COMMONNAME", 'VERNACULARNAME'), 'NATIVENAME')
  x <- replace(x, toupper(x) %in% c('CLASSIFICATION'), 'CLASSIFICA')
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

taxname.removeAuthors <- function(x) {
  removeAuthor <- function(x) {
    if(length(gregexpr("[A-Z,\U181,\U193, \U044, \U268, \U044, \U381, \U044, \U352]", x)[[1]]) > 2) {
      f <- if(grepl(' sect\\.', x, ignore.case = TRUE) | grepl('X ', x, fixed = TRUE) | grepl(' subg\\. ', x, ignore.case = TRUE)) 3 else 2
      if(length(gregexpr("[A-Z,\U181,\U193, \U044, \U268, \U044, \U381, \U044, \U352]", x)[[1]]) >= f) {
        s <- gregexpr("[A-Z,\U181,\U193, \U044, \U268, \U044, \U381, \U044, \U352]", x)[[1]][f]
        if(s > 1) x <- substr(x, 1, s-2)
        s <- gregexpr("(", x, fixed=TRUE)[[1]][1]
        if(s > 0) x <- substr(x, 1, s-2)
        x <- trimws(x)
      }}
    return(x)
  }
  out <- sapply(x, function(y) removeAuthor(y))
  return(out)
}

parse.taxa <- function(x, epis) {
  warning('Function does not account for "Sect./Ser." etc., nor for intraspecific taxa without specifier ("subsp./var." etc. or aggregates ("agg.")') 
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
  rang.suff <- as.character(apply(intra, 1, function(x) epis[x]))
  epi2 <- character(length = length(x))
  if(any(intra)) for(i in 1:length(x)) if(rang.suff[i] != 'character(0)') epi2[i] <- paste(rang.suff[i], stringr::word(strsplit(x[i], rang.suff[i], fixed = TRUE)[[1]][2],2))
  species <- trimws(paste(genus, tolower(epi1), tolower(epi2)))
  species[species == "NA NA"] <- ''
  species <- sub(' NA', replacement = '', x = species, fixed = TRUE)
  epi2 <-  sub('subsp. ', replacement = '', x = epi2, fixed = TRUE)
  result <- data.frame(original, genus, epi1=tolower(epi1), rang.suff= if(length(rang.suff)>0) rang.suff else NA, epi2, scientificName = species)
  result$rang.suff[result$rang.suff == 'character(0)'] <- NA
  return(result)
}
# parse.taxa(x)
# add.legal <- c('-',';')
