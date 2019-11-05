taxval <- function (obs, refl, db,
ag  = c('conflict', 'adapt', 'preserve'), 
rank, 
mono = c('species','higher','lower','preserve'), 
monolist = "monotypic-D", 
maxtaxlevel = 'AGG', 
check.critical = TRUE, 
interactive = FALSE,
...)
{

###------- config
##############################
#  syn <- match.arg(syn)
#   depr.syn <- function(syn) if(!missing(syn)) warning('Option syn has been removed from function taxval. Synonyms will always be adapted to accepted names.')
#   depr.syn()
  ag <- match.arg(ag)
  mono <- match.arg(mono)
  if(missing(obs)) 
    if(missing(db)) 
      stop('Please specify either an observation dataframe or the name of your Turboveg database.') 
  else  obs <- tv.obs(db=db, tv_home)
  ##
  tv_home <- tv.home()
  # if(missing(arg('maxtaxlevel))) warning('maxtaxlevel set to', maxtaxlevel, '. Occurrence list will be truncated above this taxonomic level.' )
  if(maxtaxlevel == 'AGG') maxtaxlevel <- 'AG1'
  cat("Original number of names:", length(unique(obs$TaxonUsageID)),'\n')
  if(missing(refl)) if(missing(db)) stop('If you do not give a taxonomic reference list name, you have to specify at least a name of a Turboveg database.') else 
     refl <- tv.refl(db = db[1], tv_home=tv_home)
  species <- load.taxlist(refl=refl, detailed=TRUE, ...)
  ranklevels <- factor(taxlevels$level, levels=taxlevels$level, ordered=TRUE)
#  ranklevels <- factor(c('FOR','VAR','ZUS','SSP','SPE','AGG','SEC','SSE','SER','SGE','AG1','GAT','AG2','FAM','ORD','CL3','CL2','UKL','CL1','KLA','UAB','ABT','AG3','ROOT'), levels= c('FOR','VAR','ZUS','SSP','SPE','AGG', 'SGE','SSE','SER','SEC','AG1','GAT','AG2','FAM','ORD','CL3','CL2','UKL','CL1','KLA','UAB','ABT','AG3','ROOT'), ordered=TRUE)

  if(interactive & file.exists('taxvalDecisionTable.csv')) {
    message('File ./taxvalDecisionTable.csv is used for taxonomic harmonization.')
    fr <- read.csv2('taxvalDecisionTable.csv')
    obs <- obs[!obs$TaxonUsageID %in% fr$TaxonUsageID[fr$TaxlevelTooHigh],]
    obs$TaxonUsageID <- fr$NewTaxonID[match(obs$TaxonUsageID, fr$TaxonUsageID)]
    check.critical = FALSE
    } else {  ## see line 197 for ende of this else
      ##-- start taxval functions
  if(ag == 'adapt' & missing(rank)) stop('Please specify to which "rank" the taxa shall be adapted.')
  if(ag != 'adapt' & !missing(rank)) {
    message('Taxonomic levels harmonized to given rank: ', rank, '. Setting option ag to "adapt"!')
    ag <- 'adapt'
  }
##############################
###--- check TaxonID's
if(any(!obs$TaxonUsageID %in% species$TaxonUsageID))
  stop(paste("The following Taxon ID's do not exist in reference list:", paste(obs$TaxonUsageID[which(!obs$TaxonUsageID %in% species$TaxonUsageID)])))
##########################

###------- Decision table
##############################
  fr <- as.data.frame(table(obs$TaxonUsageID), stringsAsFactors = FALSE)
  names(fr)[1] <- 'TaxonUsageID'
  fr$TaxonUsageID <- as.numeric(fr$TaxonUsageID)
  fr$TaxonName <- species$TaxonName[match(fr$TaxonUsageID, species$TaxonUsageID)]
  if(any(is.na(fr$TaxonName))) {
    message('Can not find the following taxon ids in ', refl)
    print(fr$TaxonUsageID[is.na(fr$TaxonName)])
    stop('Wrong taxon ids.')
  }
  fr$Secundum <- species$AccordingTo[match(fr$TaxonUsageID, species$TaxonUsageID)]
  fr$Synonym <- species$SYNONYM[match(fr$TaxonUsageID, species$TaxonUsageID)]
  fr$TaxonRank <- species$TaxonRank[match(fr$TaxonUsageID, species$TaxonUsageID)]
  fr$NewTaxonID <- fr$TaxonUsageID
##############################
### ------ define functions  # 
##############################
agg.conflict <- function(fr, ...) {
  if(maxtaxlevel == 'ROOT') maxtaxlevel <- ranklevels[max(match(unique(fr$TaxonRank), ranklevels))]
  # Subsuming elements into higher rank observations (if necessary) for adapt or conflict .
  origin <- fr$NewTaxonID
  fr$round <- 0
  r <- 1
  repeat{
#    print(unique(fr$NewTaxonID[which(!fr$TaxlevelTooHigh)]))
    ChildsOfOccurringTaxa <- unique(unlist(sapply(fr$NewTaxonID[which(!fr$TaxlevelTooHigh)], function(x) child(x, refl=refl, species=species, gen=4, tree=FALSE, quiet=TRUE)$TaxonUsageID)))
    OccurringChilds <- ChildsOfOccurringTaxa[ChildsOfOccurringTaxa %in% fr$NewTaxonID[which(!fr$TaxlevelTooHigh)]]
    if(length(OccurringChilds) > 0) {
      cat(length(OccurringChilds), 'conflicting child taxa found in dataset.', '\n')
      if(length(OccurringChilds) < 10) print(sort(tax(OccurringChilds, quiet=T, refl = refl)$TaxonName))
      for(i in 1:length(OccurringChilds)) {
        nested.in <- parent(OccurringChilds[i], refl = refl, quiet = TRUE)
        nested.occ <- nested.in[match(nested.in$TaxonRank, ranklevels) <= match(maxtaxlevel, ranklevels),]
        if(nrow(nested.occ) > 0) {
          fr$round[fr$NewTaxonID == OccurringChilds[i] & !is.na(fr$NewTaxonID)] <- r
          fr$NewTaxonID[fr$NewTaxonID == OccurringChilds[i] & !is.na(fr$NewTaxonID)] <- nested.occ$TaxonConceptID[nrow(nested.occ)]
        }
      }
#      write.csv(fr, file=paste('fr', r, 'csv', sep='.'))
      r <- r + 1
    } else break
  }
  return(fr)
}
##############################
### ------ adjust synonyms   #
##############################

synonyms <- if(any(species$SYNONYM[match(fr$NewTaxonID, species$TaxonUsageID)]))
  tax(fr$NewTaxonID[species$SYNONYM[match(fr$NewTaxonID, species$TaxonUsageID)] == TRUE], refl = refl, quiet = TRUE) else NULL
if(length(synonyms) > 0) {
  cat(paste(nrow(synonyms), 'Synonyms found in dataset.', if(!interactive) 'Changed to valid names.', '\n'))
  fr$NewTaxonID[match(synonyms$TaxonUsageID, fr$TaxonUsageID)] <- synonyms$TaxonConceptID
}
##############################

###------ restrict to maximum taxonomic level
##############################
if(maxtaxlevel %in% ranklevels) {
  fr$TaxlevelTooHigh <- species$TaxonRank[match(fr$NewTaxonID, species$TaxonUsageID)] %in% ranklevels[ranklevels > maxtaxlevel]
  if(sum(fr$TaxlevelTooHigh) > 0) {
    cat(sum(fr$TaxlevelTooHigh), 'taxa higher than', maxtaxlevel,'found. Deleted!\n')
#    fr$NewTaxonID[fr$TaxlevelTooHigh] <- NA
    obs <- obs[!obs$TaxonUsageID %in% fr$TaxonUsageID[is.na(fr$NewTaxonID)], ]
  }
 } else stop(paste('The given rank code', maxtaxlevel, 'is not a known rank identifier:', paste(ranklevels, collapse=', ')))
##############################

###------ resolve monotypic taxa
##############################
if (mono %in% c("species", "lower", "higher")) {
  if (file.access(file.path(tv_home, 'Species', refl, paste(monolist, "csv", sep = ".")))) {
    warning("You have chosen to care about monotypic taxa but the specified list of monotypic taxa is not available!") 
  } else {
    Mono <- read.csv(file.path(tv_home, 'Species', refl, paste(monolist, "csv", sep = ".")), sep=';')
    r = 0
    repeat{
      r <- r + 1
      if(refl %in% c('GermanSL 1.3', 'GermanSL 1.4')) names(Mono)[1] <- 'AGG_NR'
      if (mono == "lower")  tmp <- Mono$MEMBER_NR[match(fr$NewTaxonID, Mono$AGG_NR)]
      if (mono == "higher") tmp <- Mono$AGG_NR[match(fr$NewTaxonID, Mono$MEMBER_NR)]
      if (mono == 'species') {
        tmp <- Mono$AGG_NR[match(fr$NewTaxonID, Mono$MEMBER_NR)]
        tmp[Mono$MEMB_Rank[match(tmp, Mono$AGG_NR)] %in% ranklevels[ranklevels >= 'SPE']] <- NA
        tmp <- Mono$MEMBER_NR[match(fr$NewTaxonID, Mono$AGG_NR)]
        tmp[Mono$MEMB_Rank[match(tmp, Mono$MEMBER_NR)] %in% ranklevels[ranklevels <= 'SPE']] <- NA
      }
      if(sum(tmp > 0, na.rm = TRUE) == 0) {break}# cat('\nNo (more) monotypic taxa found.\n'); 
      cat(sum(tmp > 0, na.rm = TRUE), "monotypic taxa found in dataset.")
      fr$Monotypic <- !is.na(tmp)
      fr$NewTaxonID[which(!is.na(tmp))] <- tmp[!is.na(tmp)]
      if(any(is.na(fr$NewTaxonID))) message('Not for all taxa new TaxonIDs could be found. ')
    }
  }
#  cat(sum(fr$Monotypic), "monotypic taxa found in dataset.")
  if(!interactive & sum(fr$Monotypic) > 0) cat("  Will be set to ", mono, " rank", if(mono == 'species') " if possible.", sep='')
  cat('\n')
} else cat('Monotypic taxa preserved!\n')
##############################

###------ harmonize differing taxonomic levels
##############################
fr$adaptHierarchy <- FALSE
fr <- switch(ag,  
    preserve = {cat(' Aggregates preserved! \n'); fr},
    conflict = agg.conflict(fr, quiet=TRUE),
    adapt = {
      if(refl %in% c('GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2')) warning(paste('The taxonomic hierarchy of', refl, 'is inaccurate, please upgrade to version >= 1.3'))
	    if(which(ranklevels==rank) > which(ranklevels==maxtaxlevel)) 
          stop('Maximum allowed taxonomic rank lower than the aggregation level!')
#	for(i in which(!fr$TaxlevelTooHigh)) {
  for(i in 1:nrow(fr)) {
    if(!is.na(fr$NewTaxonID[i])) {
#    if(is.na(fr$NewTaxonID[i])) 
#      stop(paste('Aggregation for', fr$TaxonName[i], 'failed. \nPlease contact the author of the taxonomic reference list!'))
      p <- parents(fr$NewTaxonID[i], refl=refl, species=species, quiet=TRUE)
	    if(rank %in% p$TaxonRank) {
#        fr$NewTaxonID[fr$TaxonUsageID == fr$NewTaxonID[i]] <- p$TaxonUsageID[p$TaxonRank == rank]
        fr$NewTaxonID[i] <- p$TaxonUsageID[p$TaxonRank == rank]
        fr$adaptHierarchy[i] <- TRUE
	    }
	}}
#	fr <- agg.conflict(fr, quiet=TRUE)
  cat('For', sum(fr$adaptHierarchy[!fr$Synonym]), 'taxa the taxonomic hierarchy will be adapted.\n')
  fr
    },
  stop('You need to specify how you want to handle different taxonomic levels in your data: either preserve, adapt, or adapt only in case of conflicts.')
  )
##############################

###--- apply results
##############################
if(interactive) {
  fr$NewTaxonName <- tax(fr$NewTaxonID, refl = refl, quiet=TRUE)$TaxonName
#  fr$FreqNew <- table(fr$TaxonUsageID[fr$TaxonUsageID %in% fr$NewTaxonID])
  fr$willBeAdapted <- ifelse(fr$TaxonUsageID != fr$NewTaxonID | fr$TaxlevelTooHigh, 1, 0)
  message('Interactive mode: Nothing changed. Please check and adapt column "NewTaxonID" in "taxvalDecisionTable.csv" and rerun with interactive = TRUE.')
  write.csv2(fr, file='taxvalDecisionTable.csv')
} else {
  obs <- obs[!obs$TaxonUsageID %in% fr$TaxonUsageID[fr$TaxlevelTooHigh],]
  obs$TaxonUsageID <- fr$NewTaxonID[match(obs$TaxonUsageID, fr$TaxonUsageID)]
}
} # end of line 33: else to "interactive & file.exists('taxvalDecisionTable.csv')"
##############################

cat('Number of taxa after', if(interactive & !file.exists('taxvalDecisionTable.csv')) 'proposed',  'harmonisation:', length(unique(fr$NewTaxonID[!fr$TaxlevelTooHigh])),'\n')

###------ check for Critical species
##############################
if(check.critical) {
  if(!'AccordingTo' %in% names(species)) species$AccordingTo <- '' 
  if(!'IsChildTaxonOf' %in% names(species)) species$IsChildTaxonOf <- species$TaxonName[match(species$IsChildTaxonOfID, species$TaxonUsageID)]
  fr <- as.data.frame(table(obs$TaxonUsageID), stringsAsFactors = FALSE, responseName = 'Count')
  names(fr)[1] <- 'TaxonUsageID'
  # Pseudonyms
  auct <- species[grep("\ auct.", species$TaxonName, perl=TRUE), ] #c(1:5, 11, 13, 14, 15)
  auct$to_check <- sub("\ auct.", "", auct$TaxonName, perl=TRUE)
  auct$check_No <- species$TaxonUsageID[match(auct$to_check, species$TaxonName)]
  auct <- auct[!is.na(auct$check_No), ]
  auct <- auct[,  c('to_check', 'check_No', 'TaxonName','TaxonUsageID', 'AccordingTo')]
  names(auct)[3] <- "check against"
  if (any(fr$TaxonUsageID %in% auct$check_No)) {
    cat('Warning: Potential pseudonyms in dataset, please check.\n')
    print(auct[match(fr$TaxonUsageID, auct$check_No, nomatch = FALSE), ], row.names = FALSE)
  }
  
  ### Extent of taxon interpretation
  sl <- species[grep("\ s.\ l.", species$TaxonName, perl=TRUE), c('TaxonUsageID','TaxonName','TaxonConceptID','TaxonConcept','TaxonRank','IsChildTaxonOfID','IsChildTaxonOf','AccordingTo') ] # c(1:5, 11, 13, 14, 15)
  sl$to_check <- sub("\ s.\ l.$", "", sl$TaxonName, perl=TRUE)
  sstr <- species[grep("\ s.\ str.$", species$TaxonName, perl=TRUE), c('TaxonUsageID','TaxonName','TaxonConceptID','TaxonConcept','TaxonRank','IsChildTaxonOfID','IsChildTaxonOf','AccordingTo')]
  sstr$to_check <- sub("\ s.\ str.$", "", sstr$TaxonName, perl=TRUE)
  ext <- rbind(sl,sstr)
  
  ext$check_No <- species$TaxonUsageID[match(ext$to_check, species$TaxonName)]
  ext <- ext[!is.na(ext$check_No), c('to_check', 'check_No', 'TaxonName','TaxonUsageID', 'AccordingTo')] #  c(10, 11, 2, 1, 5, 4, 6)
  names(ext)[3] <- "check against"
  if (any(fr$TaxonUsageID %in% ext$check_No)) {
    cat('Warning: Critical species in dataset, please check\n')
    u <- ext[match(fr$TaxonUsageID, ext$check_No, nomatch = FALSE), ]
    print(u[order(u$to_check),], row.names = FALSE)
  }
}
##############################

 return(obs)
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("taxlevels"))

tv.taxval <- function(...)  {
cat('tv.taxval is a deprecated function. Use taxval() instead\n
########################################################\n')
taxval(...)
}

