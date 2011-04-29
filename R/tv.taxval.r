tv.taxval <- function (db, obs, refl, concept, syn = c('adapt','conflict','preserve'), ag = c('conflict', 'adapt', 'preserve'), rank, mono = c('lower','higher','all','preserve'), monolist = "monotypic-D", uncertain = NULL, maxtaxlevel = 'ROOT', quiet = FALSE, sysPath = FALSE, ...)
{
  syn <- match.arg(syn)
  ag <- match.arg(ag)
  mono <- match.arg(mono)
  tv_home <- tv.home(sysPath, ...)
  if(missing(obs))   obs <- tv.obs(db, tv_home)
  cat("Original number of names:", length(unique(obs$SPECIES_NR)),'\n')
  if(missing(refl)) refl <- tv.refl(db[1], tv_home)
  species <- tax('all', refl=refl, syn=TRUE, tax=TRUE, sysPath=sysPath, tv_home=tv_home, ...)

##############################
### define functions   
##############################
  adapt <- function(expr, mess, column, column2=c("SPECIES_NR", "ABBREVIAT", "Freq_Member","AGG", "AGG_NAME","Freq_Agg")) {    
    obsspec <- unique(obs$SPECIES_NR)
    species$AGG_RANG <- species$RANG[match(species$AGG,species$SPECIES_NR)]
    temp <- species[eval(expr) & species$SPECIES_NR %in% obsspec,]
    if(column=='AGG') temp[,c('AGG','AGG_NAME')] <- species[match(temp$VALID_NR,species$SPECIES_NR),c('AGG','AGG_NAME')]
    vec <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR), column]
    if (sum(vec > 0, na.rm = TRUE) > 0) {
        obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(vec > 0), vec[!is.na(vec)])
        temp$Freq_Member <- fr[match(temp$SPECIES_NR, fr[,1]),2]; temp$Freq_Member[is.na(temp$Freq_Member)] <- 0
        temp$Freq_Agg <- fr[match(temp$AGG, fr[,1]),2]; temp$Freq_Agg[is.na(temp$Freq_Agg)] <- 0      
        cat('\n', nrow(temp), mess, 'found in dataset, adapted \n')
        if(!quiet) print(temp[, column2], row.names = FALSE) 
        } else 
          cat('No', mess, 'to adapt. \n')
    obs	# Subsuming elements into higher rank observations when adapt or confl chosen. Has to be added into code!!
  }

  confl <- function(expr, column, column2=c("SPECIES_NR", "ABBREVIAT", "Freq_Member","AGG", "AGG_NAME","Freq_Agg"), mess, comb='at higher level') {
    obsspec <- unique(obs$SPECIES_NR)
    tem <- species[eval(expr) & species$SPECIES_NR %in% obsspec, ]
    temp <- tem[tem[[column]] %in% obsspec, ]
    tmp <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR),column]
    if (nrow(temp)> 0) {
       obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 0), tmp[!is.na(tmp)])
       if(!quiet) {
        temp$Freq_Member <- fr[match(temp$SPECIES_NR, fr[,1]),2]
        temp$Freq_Agg <- fr[match(temp[,column], fr[,1]),2]
        cat('\n', nrow(temp), mess,'found also',comb,'in dataset, combined',comb, '\n')
        print(temp[,column2], row.names = FALSE) 
	}
      } else if(!quiet) cat('\nNo conflicting', mess, '. \n')
    obs
  }
          
  preserve <- function(mess) {
    cat('\n', mess, 'preserved! \n')
    obs
   }

  findchilds <- function(obsspec, species, quiet=TRUE) {
    unique(unlist(sapply(obsspec, function(x) childs(x, species=species, quiet=quiet)$SPECIES_NR)))
  }
##########################
## end of function definition
##########################

fr <- as.data.frame(table(obs$SPECIES_NR))

## Adjust synonyms
obs <- switch(syn,
  conflict= confl(expr=expression(species$SYNONYM == TRUE), column='VALID_NR', column2=c("SPECIES_NR", "ABBREVIAT", "Freq_Member","VALID_NR", "VALID_NAME", "Freq_Agg"), mess='Synonyms', comb='as standard taxa'),
  adapt =   adapt(expr=expression(species$SYNONYM == TRUE), column='VALID_NR', column2=c("SPECIES_NR", "ABBREVIAT", "Freq_Member","VALID_NR", "VALID_NAME", "Freq_Agg"), mess='Synonyms'),
  preserve = preserve('Synonyms'),
  )

fr <- as.data.frame(table(obs$SPECIES_NR))

## Maximum taxonomic level
taxlevels <- factor(c('ZUS','VAR','SSP','SPE','SGE','SSE','SER','SEC','AGG','AG2','GAT','FAM','ORD','UKL','KLA','UAB','ABT','ROOT'), levels= c('ZUS','VAR','SSP','SPE','SGE','SSE','SER','SEC','AGG','AG2','GAT','FAM','ORD','UKL','KLA','UAB','ABT','ROOT'), ordered=TRUE)

obsspec <- unique(obs$SPECIES_NR)
if(maxtaxlevel %in% taxlevels) {
  toohigh <- obsspec[species$RANG[match(obsspec, species$SPECIES_NR)] %in% taxlevels[taxlevels > maxtaxlevel]]
  if(length(toohigh) > 0) {
    cat('\nTaxa higher than specified maximal taxonomic level:',maxtaxlevel,'found. Deleted.\n')
    print(species[species$SPECIES_NR %in% toohigh, c('SPECIES_NR','ABBREVIAT','SECUNDUM')],row.names=FALSE)
    obs <- obs[!obs$SPECIES_NR %in% toohigh,]
  } else cat('\nNo taxa higher than', maxtaxlevel,'found.\n')
}

## Aggregation
if(syn=='preserve' & ag!='preserve') stop('Harmonisation of taxonomic ranks is only working with valid taxa, please rethink option "syn".')
if(ag == 'adapt' & missing(rank)) stop('Please specify "rank" for rank adaptation.')
if(ag != 'adapt' & !missing(rank)) warning('Option "rank" is only working with option ag="adapt".')

obs <- switch(ag,
    preserve = preserve('Aggregates'),
    conflict = { # Subsuming elements into higher rank observations when adapt or confl chosen.
      mess='child taxa'
      repeat{
      obsspec <- unique(obs$SPECIES_NR)
      temp <- findchilds(obsspec, species)
      ch <- temp[temp %in% obsspec]
      if(length(ch) != 0) {
  	cat('\n', length(ch), mess, 'found in dataset, adapted \n')
	nested <- species[match(ch, species$SPECIES_NR),]
  	if(!quiet) print(nested[,c('SPECIES_NR','ABBREVIAT','AGG','AGG_NAME')], row.names = FALSE) 
	index <- match(obs$SPECIES_NR, nested$SPECIES_NR)
	obs$SPECIES_NR[!is.na(index)] <- replace(obs$SPECIES_NR[!is.na(index)], index[!is.na(index)], nested$AGG )
	} else break 
      }
      obs },
    adapt = {
	cat('\nCode to adapt taxonomic ranks has to be written. Nothing happened with rank and ag=adapt.\n')
	# adapt(expr=expression(species$AGG_RANG %in% taxlevels[taxlevels <= rank]), column='AGG', column2=c("SPECIES_NR", "ABBREVIAT", "Freq_Member","AGG", "AGG_NAME", "Freq_Agg"), mess='Aggregates'),
      obs  }
  ) #c('AGG','AG2','SEC','SGE','SER','SSE')

fr <- as.data.frame(table(obs$SPECIES_NR))

# ## Harmonisation of taxonomic ranks  see aggregation above
# obs <- switch(subdiv,
#     conflict = confl(expr=expression(species$RANG %in% c('FOR','VAR','SGR','SSP','ZUS')), column='AGG', mess="Variants, forms, subspecies etc."),
#     adapt = adapt(expr=expression(species$RANG %in% c('FOR','VAR','SGR','SSP','ZUS')),  column='AGG', mess='Variants, forms, subspecies etc.'),
#     preserve = preserve('Variants, forms, subspecies etc.'),
#   )


## Monotypic taxa     
if (mono %in% c("lower", "higher", "all")) {
    obsspec <- unique(obs$SPECIES_NR)
    if (file.access(paste(tv_home, 'Species', refl, paste(monolist, "dbf", sep = "."), sep = "/"))) 
        warning("List of monotypic taxa not available!") else Mono <- read.dbf(paste(tv_home, 'Species', refl, paste(monolist, "dbf", sep = "."), sep = "/"))
    if (mono == "lower")  tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, Mono$AGG_NR)]
    if (mono == "all") 	tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
    if (mono == "higher") {
	  tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
	  tmp[!Mono$MEMB_RANG[match(tmp, Mono$AGG_NR)] %in% c('FOR','VAR','ZUS','SSP')] <- NA
      }
    if (sum(tmp > 0, na.rm = TRUE) > 0) {
      repeat{
        obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 0), tmp[!is.na(tmp)])
        cat(paste('\n',length(unique(tmp[!is.na(tmp)])), "Monotypic taxa found in dataset, species converted to", mono, "rank.",'\n'))
        if(!quiet) print(Mono[Mono$AGG_NR %in% obsspec, ], row.names = FALSE)
        if (mono == "lower")    tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, Mono$AGG_NR)]
	if (mono == "all") 	tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
        if (mono == "higher") {
	    tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
	    tmp[!Mono$MEMB_RANG[match(tmp, Mono$AGG_NR)] %in% c('FOR','VAR','ZUS','SSP')] <- NA
	    }
        if(sum(tmp > 0, na.rm = TRUE) == 0) break
        obsspec <- unique(obs$SPECIES_NR)
      } } else if(mono=='higher') cat('\nNo monotypic taxa below species level found.\n') else cat('\nNo monotypic taxa found.\n')
  } else cat('\nMonotypic taxa preserved!\n')

## Uncertainty
  if(!is.null(uncertain)) {
    cat('\nFrequency of uncertainty levels')
    print(table(obs[,uncertain[[1]]]), row.names = FALSE)
    species <- tax('all', refl = refl, tax=TRUE, tv_home=tv_home, sysPath=sysPath, ...)

  uncertainty <- function(obs, column, uncrow, i, ...) {
    un <- match.arg(as.character(uncrow[[2]]),c('aggregate','preserve','ignore'))
    if(un == 'aggregate') {
      cat('\n changing species occurrences to coarser level for uncertainty level ', as.character(uncrow[[1]]))
      sp <- obs$SPECIES_NR[obs[,column] == uncrow[[1]]]
      taxa <- species[species$SPECIES_NR %in% sp,]  #tax(sp, refl=refl, tax=TRUE, sysPath=sysPath, ...)
      taxa$AGG <- species$AGG[match(taxa$VALID_NR,species$SPECIES_NR)]
      for(n in 1:nrow(taxa)) obs$SPECIES_NR[obs$SPECIES_NR == taxa[n,'SPECIES_NR'] & obs[,column] == uncrow[[1]]] <- taxa[n,'AGG']
  }
    if(un %in% c('preserve','ignore')) cat('\n preserving species occurrences of uncertainty level ',as.character(uncrow[[1]]))
    obs
  }

  for(i in 1:nrow(uncertain[[2]])) obs <- uncertainty(obs, uncertain[[1]], uncertain[[2]][i,])
    cat('\n')
   }

   

cat('\nNumber of taxa after validation:', length(unique(obs$SPECIES_NR)),'\n\n')

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

return(obs)
}


