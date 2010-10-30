tv.veg <- function (db, tv_home, tax = TRUE, convcode = TRUE, lc = c("layer","mean","max","sum","first"), pseudo = list(lc.1,'LAYER'), values = "COVER_PERC", concept, spcnames = c('short','long','numbers'), dec = 0, obs, refl, spc, site, RelScale, sysPath = FALSE, ...) 
{
## Checks
    lc <- match.arg(lc)
    spcnames = match.arg(spcnames)
    if(missing(tv_home)) tv_home <- tv.home(sysPath=sysPath, ...)
    if(missing(obs)) obs <- tv.obs(db, tv_home)
    if(suppressWarnings(any(obs < -1e+05, na.rm = TRUE))) 
      cat("\n WARNING! Values less than -100,000. \n WARNING! tvabund.dbf may be corrupt. \n WARNING! Please correct by reexporting e.g. with OpenOffice.")
    if(!missing(spc)) { 
      cat('\n Selecting species ... \n')
      obs <- obs[obs$SPECIES_NR %in% spc,]
      }
    if(lc=='first') require(Hmisc)
## Taxa
    if(missing(refl)) refl <- tv.refl(db[1], tv_home = tv_home)
    cat('Taxonomic reference list: ',refl, '\n')
    if(tax) obs <- tv.taxval(obs=obs, refl = refl, tv_home = tv_home, sysPath = sysPath, ...)
## CoverCode
    if(convcode) {
        cat('\n converting cover code ... \n')
        if(missing(RelScale)) {      
          RelScale <- tv.site(db, tv_home=tv_home, quiet = TRUE, sysPath, iconv=NULL)[, c("RELEVE_NR", "COVERSCALE")]
          obs <- tv.coverperc(obs=obs, RelScale = RelScale, tv_home = tv_home, ...) 
          } else  {obs <- tv.coverperc(db[1], obs, tv_home = tv_home, ...)
             }
      } else {
      if (!any(names(obs) == values)) stop(paste(values, " could not be found.")) 
          obs[,values] <- type.convert(as.character(obs[,values]))
          if(is.factor(obs[,values])) { lc='first'
          warning("Multiple occurrences of a species in one releve not supported without cover code conversion. Only first value used!")}
    }
## Pseudo-Species / Layer
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
      mean  = tapply(obs[, values], list(rowlab, collab), mean), 
      max   = tapply(obs[, values], list(rowlab, collab), max), 
      sum   = tapply(obs[, values], list(rowlab, collab), sum),
      first = tapply(obs[, values], list(rowlab, collab), first.word) )
    results[is.na(results)] <- 0
    results <- as.data.frame(results)
## Names
    if(spcnames %in% c('short','long')) {
      cat('\n replacing species numbers with ', spcnames, ' names ... \n')
      if(is.null(pseudo)) {
	species <- tax(as.numeric(colnames(results)), tax=FALSE, syn=FALSE, refl = refl, tv_home=tv_home, sysPath=sysPath, ...)
	if(spcnames=='short') colnames(results) <- species$LETTERCODE[match(colnames(results), species$SPECIES_NR)]
	if(spcnames=='long') colnames(results) <- gsub(' ','_', species$ABBREVIAT[match(colnames(results), species$SPECIES_NR)] )
        } else {
	st <- unlist(strsplit(colnames(results), ".", fixed = TRUE))
	colspc <- st[seq(1, length(st), 2)]
	species <- tax(as.numeric(colspc), tax=FALSE, refl = refl, tv_home=tv_home, sysPath=sysPath, ...)
	ll <- st[seq(2, length(st), 2)]
	if(spcnames=='short') coln <- as.character(species$LETTERCODE[match(as.numeric(colspc), species$SPECIES_NR)])
	if(spcnames=='long') coln <- gsub(' ','_', species$ABBREVIAT[match(as.numeric(colspc), species$SPECIES_NR)]) 
	cn <- replace(coln, which(ll != 0), paste(coln, ll, sep = ".")[ll != 0])
	colnames(results) <- cn
	}
   }
   if (any(is.na(colnames(results)))) warning("Some taxa without names, check reference list!")
## Result
   class(results) <- c("veg", "data.frame")
   results <- results[, order(names(results))]
   results
}
