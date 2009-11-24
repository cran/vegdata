tv.veg <- function (db, tv_home, tax = TRUE, convcode = TRUE, lc = c("layer","mean","max","sum","first"), 
pseudo = list(lc.1,'LAYER'), values = "COVER_PERC", concept, names = c('short','long'), dec = 0, obs, refl, spc, site, RelScale, uncertain = NULL, sysPath = FALSE, ...) 
{
### functions  
unc <- function(obs, column, uncrow, i, ...) {
  un <- match.arg(as.character(uncrow[[2]]),c('delete','aggregate','preserve','ignore'))
  if(un == 'delete') {
    cat('\n deleting species occurrences of uncertainty level ',as.character(uncrow[[1]]))
    obs <- obs[obs[,column] != uncrow[[1]],]
   }
  if(un == 'aggregate') {
    cat('\n changing species occurrences to coarser level for uncertainty level ', as.character(uncrow[[1]]))
    sp <- obs$SPECIES_NR[obs[,column] == uncrow[[1]]]
    taxa <- species[species$SPECIES_NR %in% sp,]  #tax(sp, refl=refl, tax=TRUE, sysPath=sysPath, ...)
    taxa$AGG <- species$AGG[match(taxa$VALID_NR,species$SPECIES_NR)] 
    for(n in 1:nrow(taxa)) obs$SPECIES_NR[obs$SPECIES_NR == taxa[n,'SPECIES_NR'] & obs[,column] == uncrow[[1]]] <- taxa[n,'AGG']
   }
  if(un %in% c('preserve','ignore')) {cat('\n preserving species occurrences of uncertainty level ',as.character(uncrow[[1]]))
   }
  obs
 }

    lc <- match.arg(lc)
    names = match.arg(names)
    if(missing(tv_home)) tv_home <- tv.home(sysPath=sysPath)
    if(missing(obs)) obs <- tv.obs(db, tv_home)
    if(suppressWarnings(any(obs < -1e+05, na.rm = TRUE))) 
      cat(paste("\n WARNING! Values less than -100,000. \n WARNING! tvabund.dbf may be corrupt. \n WARNING! Please correct by reexporting e.g. with OpenOffice."), quote = FALSE)
    if(!missing(spc)) { 
      cat('\n Selecting species ... \n')
      obs <- obs[!obs$SPECIES_NR %in% spc,]
      }
    if(missing(refl)) refl <- tv.refl(db[1], tv_home = tv_home, ...)
    cat('Taxonomic reference list: ',refl, '\n')
    if(tax) obs <- tv.taxval(obs=obs, refl = refl, tv_home = tv_home, sysPath = sysPath, ...)
    if(!is.null(uncertain)) {
      cat('frequency of uncertainty levels')
      print(table(obs[,uncertain[[1]]]))
    species <- tax('all', refl = refl, tax=TRUE, tv_home=tv_home, sysPath=sysPath, ...)
      for(i in 1:nrow(uncertain[[2]])) obs <- unc(obs, uncertain[[1]], uncertain[[2]][i,])
      cat('\n')
     }
    if(convcode) {
        cat(paste('\n converting cover code ... \n'))
        if(missing(RelScale)) {      
          RelScale <- tv.site(db, tv_home=tv_home, quiet = TRUE, sysPath)[, c("RELEVE_NR", "COVERSCALE")]
          obs <- tv.coverperc(obs=obs, RelScale = RelScale, tv_home = tv_home) 
          } else  {obs <- tv.coverperc(db[1], obs, tv_home = tv_home, ...)
             }
      } else {
      if (!any(names(obs) == values)) stop(paste(values, " could not be found.")) 
          obs[,values] <- type.convert(as.character(obs[,values]))
          if(is.factor(obs[,values])) { lc='first'
          warning("Multiple occurrences of a species in one releve not supported without cover code conversion. Only first value used!")}
    }
    if(!is.null(pseudo)) {
        cat('\n creating pseudo-species ... \n')
	if(length(pseudo[[2]]) > 1) stop('Function to differentiate more than one species-plot attribute not yet implemented. Please contact jansen@uni-greifswald.de.')
        obs$COMB <- pseudo[[1]][, 2][match(obs[, pseudo[[2]]], pseudo[[1]][,1])]
        collab <- paste(obs$SPECIES_NR, obs$COMB, sep = ".")
    } else     collab <- as.vector(obs$SPECIES_NR)
    rowlab <- as.vector(obs$RELEVE_NR)
    cat('\n combining occurrences using type', toupper(lc), 'and creating vegetation matrix ... \n')
    layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
    results <- switch(lc, 
      layer = tapply(obs[, values], list(rowlab, collab), layerfun),
      mean = tapply(obs[, values], list(rowlab, collab), mean), 
      max = tapply(obs[, values], list(rowlab, collab), max), 
      sum = tapply(obs[, values], list(rowlab, collab), sum),
      first = tapply(obs[, values], list(rowlab, collab), first.word) )
    results[is.na(results)] <- 0
    results <- as.data.frame(results)
    if(names %in% c('short','long') & is.null(pseudo)) {
      cat(paste('\n',"replacing species numbers with ",names, "names ...",'\n',sep=''))
      species <- species[species$SYNONYM==FALSE,]
#tax(as.numeric(colnames(results)), tv_home=tv_home, refl = refl, sysPath=sysPath, ...)
      if(names=='short') colnames(results) <- species$LETTERCODE[match(colnames(results), species$SPECIES_NR)]
      if(names=='long') colnames(results) <- gsub(' ','_', species$ABBREVIAT[match(colnames(results), species$SPECIES_NR)] )
        }
    if(names %in% c('short','long') & !is.null(pseudo)) {
      st <- unlist(strsplit(colnames(results), ".", fixed = TRUE))
      colspc <- st[seq(1, length(st), 2)]
      species <- tax(as.numeric(colspc), tax=FALSE, refl = refl, tv_home=tv_home, sysPath=sysPath, ...)
      ll <- st[seq(2, length(st), 2)]
      if(names=='short') coln <- as.character(species$LETTERCODE[match(as.numeric(colspc), species$SPECIES_NR)])
      if(names=='long') coln <- gsub(' ','_', species$ABBREVIAT[match(as.numeric(colspc), species$SPECIES_NR)]) 
      cn <- replace(coln, which(ll != 0), paste(coln, ll, sep = ".")[ll != 0])
      colnames(results) <- cn
      cat(paste('\n',"replacing species numbers with shortletters ...", '\n'))
    }
   if (any(is.na(colnames(results)))) 
    warning("Some taxa without names, check reference list!")
    class(results) <- c("veg", "data.frame")
    results <- results[, order(names(results))]
    results
}
