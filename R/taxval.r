taxval <- function (obs, refl, db, 
concept=NULL, 
syn = c('adapt','conflict','preserve'), 
ag = c('conflict', 'adapt', 'preserve'), 
rank, 
mono = c('species','higher','lower','preserve'), 
monolist = "monotypic-D", 
uncertain = NULL, 
maxtaxlevel = 'ROOT', 
check.critical = TRUE, 
sink = TRUE,
...)
{
  syn <- match.arg(syn)
  ag <- match.arg(ag)
  mono <- match.arg(mono)
  tv_home <- tv.home()
  if(ag == 'adapt') syn <- 'adapt' 
  if(missing(obs)) 
  	if(missing(db)) stop('Please specify either an observation dataframe or the name of your Turboveg database.') else  obs <- tv.obs(db=db, tv_home)
  cat("\nOriginal number of names:", length(unique(obs$TaxonUsageID)),'\n\n')
  if(missing(refl)) if(missing(db)) stop('If you do not give a taxonomic reference list name, you have to specify at least a name of a Turboveg database.') else 
  	refl <- tv.refl(db = db[1], tv_home=tv_home)
  species <- 
    load.taxlist(refl=refl, detailed=TRUE, ...)
  taxlevels <- factor(c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), levels= c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), ordered=TRUE)

if(sink) {
	## Store informations in temporary file
	wopt <- getOption("width")
	options(width=256) 
	tmpfile <- tempfile(fileext='txt')
	sink(file=tmpfile, type='output')
}
  fr <- as.data.frame(table(obs$TaxonUsageID))
  ## Adjust synonyms
if(syn=='conflict') {
#   confl <- function(expr, fr) {
    obsTaxa <- unique(obs$TaxonUsageID)
    tem <- species[species$SYNONYM == TRUE & species$TaxonUsageID %in% obsTaxa, ]
    temp <- tem[tem[['TaxonConceptID']] %in% obsTaxa, ]
    tmp <- temp[match(obs$TaxonUsageID, temp$TaxonUsageID),'TaxonConceptID']
    if (nrow(temp)> 0) {
      obs$TaxonUsageID <- replace(obs$TaxonUsageID, which(tmp > 0), tmp[!is.na(tmp)])
	    temp$Freq.1 <- fr[match(temp$TaxonUsageID, fr[,1]),2]
	    temp$Freq.2 <- fr[match(temp[,'TaxonConceptID'], fr[,1]),2]
	    cat(nrow(temp), 'Synonyms found also as standard taxa in dataset. Combined!\n')
	    print(temp[,c("TaxonUsageID", "TaxonName", "Freq.1","TaxonConceptID", "TaxonConcept", "Freq.2")], row.names = FALSE)
      if(sink) message(nrow(temp), 'Synonyms found also as standard taxa in dataset. Combined!')
    } else cat('No conflicting Synonyms.\n')
}

if(syn=='adapt') {
#   adapt <- function(expr, fr) {    
    obsTaxa <- unique(obs$TaxonUsageID)
    species$AGG_TaxonRank <- species$TaxonRank[match(species$IsChildTaxonOfID,species$TaxonUsageID)]
    temp <- species[species$SYNONYM == TRUE & species$TaxonUsageID %in% obsTaxa,]

    vec <- temp[match(obs$TaxonUsageID, temp$TaxonUsageID), 'TaxonConceptID']

    if (sum(vec > 0, na.rm = TRUE) > 0) {
        obs$TaxonUsageID <- replace(obs$TaxonUsageID, which(vec > 0), vec[!is.na(vec)])
        temp$Freq.1 <- fr[match(temp$TaxonUsageID, fr[,1]),2]; temp$Freq.1[is.na(temp$Freq.1)] <- 0
        temp$Freq.2 <- fr[match(temp$IsChildTaxonOfID, fr[,1]),2]; temp$Freq.2[is.na(temp$Freq.2)] <- 0      
        cat(nrow(temp), 'Synonyms found in dataset, adapted \n')
        print(temp[, c("TaxonUsageID", "TaxonName", "Freq.1","TaxonConceptID", "TaxonConcept", "Freq.2")], row.names = FALSE)
        if(sink) message(nrow(temp), ' Synonyms found in dataset, adapted.')
    } else cat('No Synonyms to adapt. \n')
#     obs
}
#   conflict= confl(expr=expression(species$SYNONYM == TRUE), fr),
#   adapt =   adapt(expr=expression(species$SYNONYM == TRUE), fr),
if(syn=='preserve') cat('\nSynonyms preserved! \n')
  
## Monotypic taxa
if (mono %in% c("species", "lower", "higher")) {
     obsTaxa <- sort(unique(obs$TaxonUsageID))
    if (file.access(file.path(tv_home, 'Species', refl, paste(monolist, "dbf", sep = ".")))) { 
        warning("You have chosen to care about monotypic taxa but the specified list of monotypic taxa is not available!") 
	} else {
      Mono <- read.dbf(file.path(tv_home, 'Species', refl, paste(monolist, "dbf", sep = ".")))
      r = 0
      repeat{
      	r <- r +1
      	
        if (mono == "lower")  tmp <- Mono$MEMBER_NR[match(obs$TaxonUsageID, Mono$AGG_NR)]

        if (mono == "higher") tmp <- Mono$AGG_NR[match(obs$TaxonUsageID, Mono$MEMBER_NR)]

        if(mono == 'species') {
	      	tmp <- Mono$AGG_NR[match(obs$TaxonUsageID, Mono$MEMBER_NR)]
	      	tmp <- Mono$MEMBER_NR[match(obs$TaxonUsageID, Mono$AGG_NR)]
	      	tmp[Mono$MEMB_RANG[match(tmp, Mono$AGG_NR)] %in% taxlevels[taxlevels > 'SPE']] <- NA
        }
      
      if(sum(tmp > 0, na.rm = TRUE) == 0) {break}# cat('\nNo (more) monotypic taxa found.\n'); 
      
      cat('Check for monotypic taxa: ', r, '. round.\n', sep='')
      cat(paste(length(unique(tmp))-1, " monotypic taxa found in dataset, set to ", mono, " rank", if(mono == 'species') " where possible.\n"else ".\n", sep=''))
      print(Mono[Mono$AGG_NR %in% tmp[tmp>0] | Mono$MEMBER_NR %in% tmp[tmp>0], ], row.names = FALSE)
#     print(Mono[Mono$AGG_NR %in% obsTaxa, ], row.names = FALSE)
      if(sink) message(paste(length(unique(tmp))-1, "monotypic taxa found in dataset, set to", mono, "rank."))

      obs$TaxonUsageID <- replace(obs$TaxonUsageID, which(tmp > 0), tmp[!is.na(tmp)])
#      obsTaxa <- unique(obs$TaxonUsageID)
      }
  }} else cat('Monotypic taxa preserved!\n')

## Maximum taxonomic level
obsTaxa <- sort(unique(obs$TaxonUsageID))
if(maxtaxlevel %in% taxlevels) {
  toohigh <- obsTaxa[species$TaxonRank[match(obsTaxa, species$TaxonUsageID)] %in% taxlevels[taxlevels > maxtaxlevel]]
  if(length(toohigh) > 0) {
    cat('\n', length(toohigh), 'taxa higher than specified maximal taxonomic level ', maxtaxlevel,'found. Deleted.\n')
    print(species[species$TaxonUsageID %in% toohigh, c('TaxonUsageID','TaxonName','AccordingTo')],row.names=FALSE)
    if(sink) message(length(toohigh), ' taxa higher than specified maximal taxonomic level ', maxtaxlevel,' found. Deleted.')
    obs <- obs[!obs$TaxonUsageID %in% toohigh,]
  } else cat('No taxa higher than', maxtaxlevel,'found.\n')
} else stop(paste('The given rank code', maxtaxlevel, 'is not a known rank code:', taxlevels))


##############################
### define functions   
##############################
agg.conflict <- function(obs, quiet=FALSE, ...) { 
  # Subsuming elements into higher rank observations (when necessary) if adapt or conflict is chosen.
  repeat{
  obsTaxa <- unique(obs$TaxonUsageID)   # Which taxa occur
  if(!exists("ChildsOfOccurringTaxa")) ChildsOfOccurringTaxa <- unique(unlist(sapply(obsTaxa, function(x) childs(x, refl=refl, species=species, gen=4, tree=FALSE, quiet=TRUE)$TaxonUsageID)))
  
  OccurringChilds <- ChildsOfOccurringTaxa[ChildsOfOccurringTaxa %in% obsTaxa]
  if(length(OccurringChilds) != 0) {
    cat(length(OccurringChilds), 'child taxa found in dataset, adapted \n')
    nested <- species[match(OccurringChilds, species$TaxonUsageID),]
    print(nested[,c('TaxonUsageID','TaxonName','Freq.1','IsChildTaxonOfID','IsChildTaxonOf','Freq.2')], row.names = FALSE)
    if(sink) message(length(OccurringChilds), ' child taxa found in dataset, adapted.')
    index <- match(obs$TaxonUsageID, nested$TaxonUsageID)
    repl <- !is.na(index)
    obs$TaxonUsageID[repl] <- nested$IsChildTaxonOfID[index][repl]
    } else break
}
return(obs)
}

##########################
## end of function definition
##########################

## Aggregation
# fr <- table(obs$TaxonUsageID)
species$Freq.1 <- fr$Freq[match(species$TaxonUsageID, fr[,1])]
species$Freq.2 <- fr$Freq[match(species$IsChildTaxonOfID, fr[,1])]

if(syn=='preserve' & ag!='preserve') stop('Harmonisation of taxonomic ranks is working only with valid taxa, please rethink option "syn=preserve".')
if(ag == 'adapt' & missing(rank)) stop('Please specify to which "rank" taxa should be adapted.')
if(ag != 'adapt' & !missing(rank)) warning('Ignoring option "rank", harmonisation to a specified taxonomic rank is working only with option ag="adapt"!')

obs <- switch(ag,
    preserve = {
	  cat(' Aggregates preserved! \n')
	  obs
	  },
    conflict = agg.conflict(obs, quiet=TRUE),
    adapt = {
      if(refl %in% c('GermanSL 1.0', 'GermanSL 1.1')) warning(paste('The taxonomic hierarchy of', refl, 'is inaccurate, please upgrade to version >= 1.2'))
	if(taxlevels[taxlevels==rank] > taxlevels[taxlevels==maxtaxlevel]) 
    warning('Maximum allowed taxonomic rank lower than the aggregation level!')
	obsTaxa <- unique(obs$TaxonUsageID)
	for(i in 1:length(obsTaxa)) {
	    temp <- parents(obsTaxa[i], refl=refl, species=species, quiet=TRUE)
#print(4)
	    if(rank %in% temp$TaxonRank) obs$TaxonUsageID[obs$TaxonUsageID == obsTaxa[i]] <- temp$TaxonUsageID[temp$TaxonRank == rank]
	}
	agg.conflict(obs, quiet = TRUE)
     }
  )

## Uncertainty
  if(!is.null(uncertain)) {
    cat(' Frequency of uncertainty levels')
    print(table(obs[,uncertain[[1]]]), row.names = FALSE)
#    species <- load.taxlist(refl = refl, detailed = TRUE, tv_home = tv_home, ...)

  uncertainty <- function(obs, column, uncrow, ...) {
    un <- match.arg(as.character(uncrow[[2]]),c('aggregate','preserve','ignore'))
    if(un == 'aggregate') {
      cat('\n changing species occurrences to coarser level for uncertainty level ', as.character(uncrow[[1]]))
      if(sink) message('\n changing species occurrences to coarser level for uncertainty level ', as.character(uncrow[[1]]))
      sp <- obs$TaxonUsageID[obs[,column] == uncrow[[1]]]
      taxa <- species[species$TaxonUsageID %in% sp,]
      taxa$IsChildTaxonOfID <- species$IsChildTaxonOfID[match(taxa$TaxonConceptID,species$TaxonUsageID)]
      for(n in 1:nrow(taxa)) obs$TaxonUsageID[obs$TaxonUsageID == taxa[n,'TaxonUsageID'] & obs[,column] == uncrow[[1]]] <- taxa[n,'IsChildTaxonOfID']
  }
    if(un %in% c('preserve','ignore')) cat('\n preserving species occurrences of uncertainty level ',as.character(uncrow[[1]]))
    obs
  }

  for(i in 1:nrow(uncertain[[2]])) obs <- uncertainty(obs, uncertain[[1]], uncertain[[2]][i,])
    cat('\n')
   }

cat('Number of taxa after harmonisation:', length(unique(obs$TaxonUsageID)),'\n\n')

if(check.critical) {
### Critical species
# Pseudonyms
  auct <- species[grep("\ auct.", species$TaxonName, perl=TRUE), ] #c(1:5, 11, 13, 14, 15)
  auct$to_check <- sub("\ auct.", "", auct$TaxonName, perl=TRUE)
  auct$check_No <- species$TaxonUsageID[match(auct$to_check, species$TaxonName)]
  auct <- auct[!is.na(auct$check_No), ]
  auct <- auct[,  c('to_check', 'check_No', 'TaxonName','TaxonUsageID', 'AccordingTo')]
  names(auct)[3] <- "check against"
  if (any(obs$TaxonUsageID %in% auct$check_No)) {
      cat('Warning: Potential pseudonyms in dataset, please check.')
      if(sink) message('Warning: Potential pseudonyms in dataset, please check.')
      u <- unique(obs$TaxonUsageID)
      print(auct[match(u, auct$check_No, nomatch = FALSE), ], row.names = FALSE)
   }

### Extent of taxon interpretation
#species <<- species
#print(names(species))
 sl <- species[grep("\ s.\ l.", species$TaxonName, perl=TRUE), c('TaxonUsageID','TaxonName','TaxonConceptID','TaxonConcept','TaxonRank','IsChildTaxonOfID','IsChildTaxonOf','AccordingTo') ] # c(1:5, 11, 13, 14, 15)
 sl$to_check <- sub("\ s.\ l.$", "", sl$TaxonName, perl=TRUE)
 sstr <- species[grep("\ s.\ str.$", species$TaxonName, perl=TRUE), c('TaxonUsageID','TaxonName','TaxonConceptID','TaxonConcept','TaxonRank','IsChildTaxonOfID','IsChildTaxonOf','AccordingTo')]
 sstr$to_check <- sub("\ s.\ str.$", "", sstr$TaxonName, perl=TRUE)
 ext <- rbind(sl,sstr)

 ext$check_No <- species$TaxonUsageID[match(ext$to_check, species$TaxonName)]
 ext <- ext[!is.na(ext$check_No), c('to_check', 'check_No', 'TaxonName','TaxonUsageID', 'AccordingTo')] #  c(10, 11, 2, 1, 5, 4, 6)
 names(ext)[3] <- "check against"
 if (any(obs$TaxonUsageID %in% ext$check_No)) {
     cat('Warning: Critical species in dataset, please check\n')
     if(sink) message('Warning: Critical species in dataset, please check\n')
     u <- ext[match(unique(obs$TaxonUsageID), ext$check_No, nomatch = FALSE), ]
     print(u[order(u$to_check),], row.names = FALSE)
  }
}
if(sink) {
	sink()
  options(width=wopt) 
}
cat('Number of taxa after validation:', length(unique(obs$TaxonUsageID)),'\n\n')
if(sink) message('Information is written to ', tmpfile, '.\n')  
return(obs)
}


tv.taxval <- function(...)  {
cat('tv.taxval is a deprecated function. Using taxval() instead\n
########################################################\n')
taxval(...)
}
