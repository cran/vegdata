
taxval <- function (obs, refl, db, 
concept=NULL, 
syn = c('adapt','conflict','preserve'), 
ag = c('conflict', 'adapt', 'preserve'), 
rank, 
mono = c('higher','lower','preserve'), 
monolist = "monotypic-D", 
uncertain = NULL, 
maxtaxlevel = 'ROOT', 
check = TRUE,
quiet = FALSE, 
...)
{
  syn <- match.arg(syn)
  ag <- match.arg(ag)
  mono <- match.arg(mono)
  tv_home <- tv.home()
  if(missing(obs))   obs <- tv.obs(db=db, tv_home)
  cat("\nOriginal number of names:", length(unique(obs$SPECIES_NR)),'\n')
  if(missing(refl)) refl <- tv.refl(db[1], tv_home=tv_home)
  species <- tax('all', refl=refl, syn=TRUE, verbose=TRUE, concept=concept, ...)
  taxlevels <- factor(c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), levels= c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), ordered=TRUE)
  fr <- as.data.frame(table(obs$SPECIES_NR))

## Adjust synonyms
  confl <- function(expr, fr) {
    obsspec <- unique(obs$SPECIES_NR)
    tem <- species[eval(expr) & species$SPECIES_NR %in% obsspec, ]
    temp <- tem[tem[['VALID_NR']] %in% obsspec, ]
    tmp <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR),'VALID_NR']
    if (nrow(temp)> 0) {
       obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 0), tmp[!is.na(tmp)])
       if(!quiet) {
        temp$Freq.1 <- fr[match(temp$SPECIES_NR, fr[,1]),2]
        temp$Freq.2 <- fr[match(temp[,'VALID_NR'], fr[,1]),2]
        cat('\n', nrow(temp), 'Synonyms found also as standard taxa in dataset. Combined!\n')
        print(temp[,c("SPECIES_NR", "ABBREVIAT", "Freq.1","VALID_NR", "VALID_NAME", "Freq.2")], row.names = FALSE) 
	}
      } else if(!quiet) cat('\nNo conflicting Synonyms.\n')
    obs
  }
  adapt <- function(expr, fr) {    
    obsspec <- unique(obs$SPECIES_NR)
    species$AGG_RANG <- species$RANG[match(species$AGG,species$SPECIES_NR)]
    temp <- species[eval(expr) & species$SPECIES_NR %in% obsspec,]
#     if(column=='AGG') temp[,c('AGG','AGG_NAME')] <- species[match(temp$VALID_NR,species$SPECIES_NR),c('AGG','AGG_NAME')]
    vec <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR), 'VALID_NR']
    if (sum(vec > 0, na.rm = TRUE) > 0) {
        obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(vec > 0), vec[!is.na(vec)])
        temp$Freq.1 <- fr[match(temp$SPECIES_NR, fr[,1]),2]; temp$Freq.1[is.na(temp$Freq.1)] <- 0
        temp$Freq.2 <- fr[match(temp$AGG, fr[,1]),2]; temp$Freq.2[is.na(temp$Freq.2)] <- 0      
        cat('\n', nrow(temp), 'Synonyms found in dataset, adapted \n')
        if(!quiet) print(temp[, c("SPECIES_NR", "ABBREVIAT", "Freq.1","VALID_NR", "VALID_NAME", "Freq.2")], row.names = FALSE) 
        } else 
          cat('\nNo Synonyms to adapt. \n')
    obs
  }
obs <- switch(syn,
  conflict= confl(expr=expression(species$SYNONYM == TRUE), fr),
  adapt =   adapt(expr=expression(species$SYNONYM == TRUE), fr),
  preserve = {
    cat('\nSynonyms preserved! \n')
    obs
    },
  )

## Monotypic taxa
if (mono %in% c("lower", "higher")) {
     obsspec <- unique(obs$SPECIES_NR)
    if (file.access(file.path(tv_home, 'Species', refl, paste(monolist, "dbf", sep = ".")))) { 
        warning("You have chosen to care about monotypic taxa but the specified list of monotypic taxa is not available!") 
	} else {
      Mono <- read.dbf(file.path(tv_home, 'Species', refl, paste(monolist, "dbf", sep = ".")))
      repeat{
        if (mono == "lower") {
	    tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, Mono$AGG_NR)]
	  }
#	if (mono == "all") 	tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
        if (mono == "higher") {
	    tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
	    tmp[!Mono$MEMB_RANG[match(tmp, Mono$AGG_NR)] %in% taxlevels[taxlevels < 'SPE']] <- NA # Universeller schreiben
	    }
       if(sum(tmp > 0, na.rm = TRUE) == 0) {break}	# cat('\nNo (more) monotypic taxa found.\n'); 
       cat(paste('\n',nrow(Mono[Mono$AGG_NR %in% obsspec, ]), "monotypic taxa found in dataset, set to", mono, "rank.",'\n'))
       if(!quiet) print(Mono[Mono$AGG_NR %in% obsspec, ], row.names = FALSE)
        obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 0), tmp[!is.na(tmp)])
        obsspec <- unique(obs$SPECIES_NR)
      }
  } } else cat('\nMonotypic taxa preserved!\n')


## Maximum taxonomic level
obsspec <- unique(obs$SPECIES_NR)
if(maxtaxlevel %in% taxlevels) {
  toohigh <- obsspec[species$RANG[match(obsspec, species$SPECIES_NR)] %in% taxlevels[taxlevels > maxtaxlevel]]
  if(length(toohigh) > 0) {
    cat('\n', length(toohigh), 'taxa higher than specified maximal taxonomic level',maxtaxlevel,'found. Deleted.\n')
    print(species[species$SPECIES_NR %in% toohigh, c('SPECIES_NR','ABBREVIAT','SECUNDUM')],row.names=FALSE)
    obs <- obs[!obs$SPECIES_NR %in% toohigh,]
  } else cat('\nNo taxa higher than', maxtaxlevel,'found.\n')
}

##############################
### define functions   
##############################
agg.conflict <- function() { # Subsuming elements into higher rank observations when adapt or conflict is chosen.
  repeat{
  obsspec <- unique(obs$SPECIES_NR)
  temp <- unique(sapply(obsspec, function(x) childs(x, species=species, quiet=TRUE, tree=FALSE)$SPECIES_NR))
print(temp)
  ch <- temp[temp %in% obsspec]
  if(length(ch) != 0) {
    cat('\n', length(ch), 'child taxa found in dataset, adapted \n')
    nested <- species[match(ch, species$SPECIES_NR),]
    if(!quiet) print(nested[,c('SPECIES_NR','ABBREVIAT','Freq.1','AGG','AGG_NAME','Freq.2')], row.names = FALSE)
    index <- match(obs$SPECIES_NR, nested$SPECIES_NR)
    repl <- !is.na(index)
    obs$SPECIES_NR[repl] <- nested$AGG[index][repl]
    } else break
}
return(obs)
}

##########################
## end of function definition
##########################


## Aggregation
# fr <- as.data.frame(table(obs$SPECIES_NR))
species$Freq.1 <- fr$Freq[match(species$SPECIES_NR, fr[,1])]
species$Freq.2 <- fr$Freq[match(species$AGG, fr[,1])]

if(syn=='preserve' & ag!='preserve') stop('Harmonisation of taxonomic ranks is working only with valid taxa, please rethink option "syn=preserve".')
if(ag == 'adapt' & missing(rank)) stop('Please specify to which "rank" taxa should be adapted.')
if(ag != 'adapt' & !missing(rank)) warning('Ignoring option "rank", it is working only with ag="adapt"!')

obs <- switch(ag,
    preserve = {
	  cat('\n Aggregates preserved! \n')
	  obs
	  },
    conflict = agg.conflict(),
    adapt = {
	if(rank < maxtaxlevel) warning('Maximum allowed taxonomic rank lower than the aggregation level!')
	obsspec <- unique(obs$SPECIES_NR)
	for(i in 1:length(obsspec)) {
	    temp <- parents(obsspec[i], refl=refl, species=species, quiet=TRUE)
	    if(rank %in% temp$RANG) obs$SPECIES_NR[obs$SPECIES_NR == obsspec[i]] <- temp$SPECIES_NR[temp$RANG == rank]
	}
	agg.conflict()
      }
  )

## Uncertainty
  if(!is.null(uncertain)) {
    cat('\nFrequency of uncertainty levels')
    print(table(obs[,uncertain[[1]]]), row.names = FALSE)
    species <- tax('all', refl = refl, verbose = TRUE, tv_home = tv_home, ...)

  uncertainty <- function(obs, column, uncrow, i, ...) {
    un <- match.arg(as.character(uncrow[[2]]),c('aggregate','preserve','ignore'))
    if(un == 'aggregate') {
      cat('\n changing species occurrences to coarser level for uncertainty level ', as.character(uncrow[[1]]))
      sp <- obs$SPECIES_NR[obs[,column] == uncrow[[1]]]
      taxa <- species[species$SPECIES_NR %in% sp,]
      taxa$AGG <- species$AGG[match(taxa$VALID_NR,species$SPECIES_NR)]
      for(n in 1:nrow(taxa)) obs$SPECIES_NR[obs$SPECIES_NR == taxa[n,'SPECIES_NR'] & obs[,column] == uncrow[[1]]] <- taxa[n,'AGG']
  }
    if(un %in% c('preserve','ignore')) cat('\n preserving species occurrences of uncertainty level ',as.character(uncrow[[1]]))
    obs
  }

  for(i in 1:nrow(uncertain[[2]])) obs <- uncertainty(obs, uncertain[[1]], uncertain[[2]][i,])
    cat('\n')
   }


cat('\nNumber of taxa after validation:', length(unique(obs$SPECIES_NR)),'\n')

if(check) {
### Critical species
# Pseudonyms
 auct <- species[grep("\ auct.", species$ABBREVIAT, perl=TRUE), ] #c(1:5, 11, 13, 14, 15)
  auct$to_check <- sub("\ auct.", "", auct$ABBREVIAT, perl=TRUE)
  auct$check_No <- species$SPECIES_NR[match(auct$to_check, species$ABBREVIAT)]
  auct <- auct[!is.na(auct$check_No), ]
  auct <- auct[,  c('to_check', 'check_No', 'ABBREVIAT','SPECIES_NR', 'SECUNDUM')]
  names(auct)[3] <- "check against"
  if (any(obs$SPECIES_NR %in% auct$check_No)) {
      cat('\nWarning: Critical Pseudonym(s) in dataset, please check\n')
      u <- unique(obs$SPECIES_NR)
      if(!quiet) print(auct[match(u, auct$check_No, nomatch = FALSE), ], row.names = FALSE)
   }

### Extent of taxon interpretation
#species <<- species
  sl <- species[grep("\ s.\ l.$", species$ABBREVIAT, perl=TRUE),c('SPECIES_NR','ABBREVIAT','VALID_NR','VALID_NAME','RANG','AGG','AGG_NAME','SECUNDUM') ] # c(1:5, 11, 13, 14, 15)
  sl$to_check <- sub("\ s.\ l.$", "", sl$ABBREVIAT, perl=TRUE)
  sstr <- species[grep("\ s.\ str.$", species$ABBREVIAT, perl=TRUE), c('SPECIES_NR','ABBREVIAT','VALID_NR','VALID_NAME','RANG','AGG','AGG_NAME','SECUNDUM')]
  sstr$to_check <- sub("\ s.\ str.$", "", sstr$ABBREVIAT, perl=TRUE)
  ext <- rbind(sl,sstr)

  ext$check_No <- species$SPECIES_NR[match(ext$to_check, species$ABBREVIAT)]
  ext <- ext[!is.na(ext$check_No), c('to_check', 'check_No', 'ABBREVIAT','SPECIES_NR', 'SECUNDUM')] #  c(10, 11, 2, 1, 5, 4, 6)
  names(ext)[3] <- "check against"
  if (any(obs$SPECIES_NR %in% ext$check_No)) {
      cat('\nWarning: Critical species in dataset, please check\n')
      u <- ext[match(unique(obs$SPECIES_NR), ext$check_No, nomatch = FALSE), ]
      if(!quiet) print(u[order(u$to_check),], row.names = FALSE)
   }
}
return(obs)
}


tv.taxval <- function(...)  
cat('tv.taxval is a deprecated function. Use taxval() instead\n
########################################################\n')

