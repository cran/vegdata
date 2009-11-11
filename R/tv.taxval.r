tv.taxval <- function (db, obs, refl, tv_home, concept, syn = c('adapt','conflict','preserve'), 
subdiv = c('conflict', 'adapt', 'preserve'), ag = c('conflict', 'preserve', 'adapt'), mono = c('lower','higher', 'preserve'), monolist = "monotypic-D", 
genus = c('delete','preserve'), quiet = FALSE, sysPath = FALSE, ...) 
{

    syn <- match.arg(syn)
    subdiv <- match.arg(subdiv)
#    seg <- match.arg(seg)
#    ssp <- match.arg(ssp)
    ag <- match.arg(ag)
    mono <- match.arg(mono)
    genus <- match.arg(genus)
        
    if(missing(tv_home)) 
    tv_home <- tv.home(sysPath)
    if(missing(obs))   obs <- tv.obs(db, tv_home)
    cat("Original number of taxa:", length(unique(obs$SPECIES_NR)),'\n')
    if(missing(refl)) refl <- tv.refl(db[1], tv_home)
    species <- tax('all', refl=refl, tax=TRUE, sysPath=sysPath, ...)

    ### functions   
adapt <- function(expr, mess, column, column2=c("SPECIES_NR", "ABBREVIAT", "AGG", "AGG_NAME")) {    
    obsspec <- unique(obs$SPECIES_NR)
    temp <- species[eval(expr) & species$SPECIES_NR %in% obsspec,]
    if(column=='AGG') temp[,c('AGG','AGG_NAME')] <- species[match(temp$VALID_NR,species$SPECIES_NR),c('AGG','AGG_NAME')]
    vec <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR), column]
    if (sum(vec > 0, na.rm = TRUE) > 0) {
        obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(vec > 0), vec[!is.na(vec)])
        cat('\n', nrow(temp), mess, 'found in dataset, adapted \n')
        if(!quiet) print(temp[, column2]) } else 
          cat('No', mess, 'to adapt. \n')
    obs
    }

confl <- function(expr, column, column2=c("SPECIES_NR", "ABBREVIAT", "AGG", "AGG_NAME"), mess, comb='at species level') {
    obsspec <- unique(obs$SPECIES_NR)
    tem <- species[eval(expr) & species$SPECIES_NR %in% obsspec, column2]
    temp <- tem[tem[[column]] %in% obsspec, ]
    tmp <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR),column]
    if (nrow(temp)> 0) {
       obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 0), tmp[!is.na(tmp)])
       if(!quiet) {cat('\n', nrow(temp), mess,'found also',comb,'in dataset, combined',comb, '\n')
       print(temp) }
      } else if(!quiet) cat('\n No conflicting', mess, '. \n')
    obs
    }
          
preserve <- function(mess) {
    cat('\n', mess, 'preserved! \n')
    obs
    }

obs <- switch(syn,
  conflict= confl(expr=expression(species$SYNONYM == TRUE), column='VALID_NR', column2=c("SPECIES_NR", "ABBREVIAT", "VALID_NR", "VALID_NAME"), mess='Synonyms', comb='as standard taxa'),
  adapt =   adapt(expr=expression(species$SYNONYM == TRUE), column='VALID_NR', column2=c("SPECIES_NR", "ABBREVIAT", "VALID_NR", "VALID_NAME"), mess='Synonyms'),
  preserve = preserve('Synonyms'),
  )
    
obs <- switch(subdiv,
    conflict = confl(expr=expression(species$RANG %in% c('FOR','VAR','SGR','SSP','ZUS')), column='AGG', mess="Variants, forms, subspecies etc."),
    adapt = adapt(expr=expression(species$RANG %in% c('FOR','VAR','SGR','SSP','ZUS')),  column='AGG', mess='Variants, forms, subspecies etc.'),
    preserve = preserve('Variants, forms, subspecies etc.'),
  )

obs <- switch(ag,
    preserve = preserve('Aggregates'),
    conflict = {
  obsspec <- unique(obs$SPECIES_NR)
  # Valid aggregates in Specieslist
  AG <- species$SPECIES_NR[species$RANG %in% c('AGG','AG2','SEC','SGE','SER','SSE') & species$SYNONYM == FALSE]
  # Obs-Species which are member of aggregates
  agg.member <- species[species$AGG %in% AG & species$SPECIES_NR %in% obsspec, c("SPECIES_NR","ABBREVIAT", "AGG", "AGG_NAME")]
  # agg.members which aggregates are also in dataset
  conf <- agg.member[agg.member$AGG %in% obsspec,]
  fr.member <- as.data.frame(table(match(obs$SPECIES_NR, conf$SPECIES_NR)))
  fr.agg <- as.data.frame(table(match(obs$SPECIES_NR, conf$AGG)))
  if(nrow(conf) == 0) cat("\n No conflicting aggregates found.\n") else {
    cat("\n",  nrow(conf), "members of occurring aggregates in dataset, aggregated: \n")
    if(!quiet) print(conf)
   obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(!is.na(match(obs$SPECIES_NR, conf$SPECIES_NR))), conf$AGG[match(obs$SPECIES_NR, conf$SPECIES_NR)][!is.na(match(obs$SPECIES_NR, conf$SPECIES_NR))])
   }
    obs     },
      adapt = {
  obsspec <- unique(obs$SPECIES_NR)
  AG <- species$SPECIES_NR[species$RANG %in% c('AGG','AG2','SEC','SGE','SER','SSE') & species$SYNONYM == FALSE]
  agg.member <- species[species$AGG %in% AG & species$SPECIES_NR %in% obsspec, c("SPECIES_NR","ABBREVIAT", "AGG", "AGG_NAME")]
    cat("\n",  nrow(agg.member), "members of aggregates in dataset, aggregated: \n")
    if(!quiet) print(agg.member)
   obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(!is.na(match(obs$SPECIES_NR, agg.member$SPECIES_NR))), agg.member$AGG[match(obs$SPECIES_NR, agg.member$SPECIES_NR)][!is.na(match(obs$SPECIES_NR, agg.member$SPECIES_NR))])
    obs}
      )
     
if (mono %in% c("lower", "higher")) {
    obsspec <- unique(obs$SPECIES_NR)
    if (file.access(paste(tv_home, 'Species', refl, paste(monolist, "dbf", sep = "."), sep = "/"))) 
        warning("List of monotypic taxa not available!") else Mono <- read.dbf(paste(tv_home, 'Species', refl, paste(monolist, "dbf", sep = "."), sep = "/"))
    if (mono == "lower")    tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, Mono$AGG_NR)]
    if (mono == "higher")   tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
    if (sum(tmp > 0, na.rm = TRUE) > 0) {
      repeat{
        obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 0), tmp[!is.na(tmp)])
        cat(paste('\n',length(unique(tmp[!is.na(tmp)])), "Monotypic taxa found in dataset, species converted to", mono, "rank.",'\n'))
        if (mono == "lower")    tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, Mono$AGG_NR)]
        if (mono == "higher")   tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
        if(!quiet) print(Mono[Mono$AGG_NR %in% obsspec, ])
        if(sum(tmp > 0, na.rm = TRUE) == 0) break
        obsspec <- unique(obs$SPECIES_NR)
      } } else cat('\n No monotypic taxa found. \n\n')
  } else cat('\n Monotypic taxa preserved! \n\n')

### Critical species
## Pseudonyms
 auct <- species[grep("auct.", species$ABBREVIAT), c(1:5, 11, 13, 14, 15)]
  auct$to_check <- sub(" auct.", "", auct$ABBREVIAT)
  auct$checknr <- species$SPECIES_NR[match(auct$to_check, species$ABBREVIAT)]
  auct <- auct[!is.na(auct$checknr), ]
  auct <- auct[, c(10, 11, 2, 1, 5, 4, 6)]
  names(auct)[3] <- "check against"
  if (any(obs$SPECIES_NR %in% auct$checknr)) {
      cat('\n',"Warning: Critical Pseudonym(s) in dataset, please check",'\n')
      u <- unique(obs$SPECIES_NR)
      if(!quiet) print(auct[match(u, auct$checknr, nomatch = FALSE), ])
   }
### Extent of taxon interpretation
  sl <- species[grep("s. l.", species$ABBREVIAT), c(1:5, 11, 13, 14, 15)]
  sl$to_check <- sub(" s. l.", "", sl$ABBREVIAT)
  sstr <- species[grep("s. str.", species$ABBREVIAT), c(1:5, 11, 13, 14, 15)]
  sstr$to_check <- sub(" s. str.", "", sstr$ABBREVIAT)
  ext <- rbind(sl,sstr)

  ext$checknr <- species$SPECIES_NR[match(ext$to_check, species$ABBREVIAT)]
  ext <- ext[!is.na(ext$checknr),  c(10, 11, 2, 1, 5, 4, 6)]
  names(ext)[3] <- "check against"
  if (any(obs$SPECIES_NR %in% ext$checknr)) {
      cat('\n',"Warning: Critical species in dataset, please check",'\n')
      u <- ext[match(unique(obs$SPECIES_NR), ext$checknr, nomatch = FALSE), ]
      if(!quiet) print(u[order(u$to_check),])
   }

## Delete 
  if(genus == 'delete') {
    obsspec <- unique(obs$SPECIES_NR)
    gat <- species[species$RANG %in% c('GAT','FAM', 'UKL', 'ORD', 'ABT', 'KLA', 'ROOT', 'UAB'), c("SPECIES_NR", "ABBREVIAT")]
    if (any(gat$SPECIES_NR %in% obsspec)) {
      cat('\n',sum(gat$SPECIES_NR %in% obsspec), "Undetermined genera or above found in dataset, deleted:",'\n')
      if(!quiet) print(gat[gat$SPECIES_NR %in% obsspec, ])
      } else cat('\n',"No undetermined genera or above found.",'\n')
    obs <- obs[!obs$SPECIES_NR %in% gat$SPECIES_NR, ]
  } else cat('\n Undetermined genera and above preserved! \n')
    
cat('\n Number of taxa after validation:', length(unique(obs$SPECIES_NR)),'\n\n')
obs
}
