tv.db <- function() {
  tv_home <- tv.home()
  dir <- list.dirs(path = file.path(tv_home,'Data'), full.names = FALSE)
  for (i in 1:length(dir)) dir[i] <- substring(dir[i], nchar(tv_home)+7)
  return(dir)
}



tv.veg <- function (db, tv_home, taxval = TRUE, convcode = TRUE, lc = c("layer","mean","max","sum","first"), pseudo = list(lc.1,'LAYER'), values = "COVER_PERC", concept, spcnames = c('short','long','numbers'), dec = 0, cover.transform = c('no', 'pa', 'sqrt'), obs, refl, spc, site, RelScale, ...) 
{
## Checks
    lc <- match.arg(lc)
    cover.transform <- match.arg(cover.transform)
    spcnames = match.arg(spcnames)
    if(missing(tv_home)) tv_home <- tv.home()
    if(missing(obs)) obs <- tv.obs(db, tv_home)
#     if(suppressWarnings(any(obs < -1e+05, na.rm = TRUE))) 
#       cat("\n WARNING! Values less than -100,000. \n WARNING! tvabund.dbf may be corrupt. \n WARNING! Please correct by reexporting e.g. with OpenOffice.")
    if(!missing(spc)) {
      cat('\n Selecting species ... \n')
      obs <- obs[obs$SPECIES_NR %in% spc,]
      }
    if(missing(pseudo)) data(lc.1)
## Taxa
    if(missing(refl)) refl <- tv.refl(db[1], tv_home = tv_home)
    cat('Taxonomic reference list: ',refl, '\n')
    if(taxval) 
	obs <- taxval(obs=obs, refl = refl, ...)
## CoverCode
    if(convcode){
        cat('\n converting cover code ... \n')
        if(missing(RelScale)) {
	  if(missing(db)) stop('\nEither database name or a vector with CoverScale per releve has to be permitted, to cope with Cover Scale information\n')
          RelScale <- tv.site(db, tv_home=tv_home, quiet = TRUE, iconv=NULL)[, c("RELEVE_NR", "COVERSCALE")]
          obs <- tv.coverperc(obs=obs, RelScale = RelScale, tv_home = tv_home, ...) 
          } else  obs <- tv.coverperc(obs, RelScale, ...)
      } else {
      if (!any(names(obs) == values)) stop(paste(values, " could not be found.")) 
          obs[,values] <- type.convert(as.character(obs[,values]))
          if(is.factor(obs[,values])) { lc='first'
          warning("Multiple occurrences of a species in one releve not supported without cover code conversion. Only first value used!")}
    }
## Pseudo-Species / Layer
    if(!is.null(pseudo)) {
        cat('\n creating pseudo-species ... \n')
	if(length(pseudo[[2]]) > 1) stop('Possibility to differentiate more than one species-plot attribute not yet implemented. \n
	Please contact <jansen@uni-greifswald.de>.')
        obs$COMB <- pseudo[[1]][, 2][match(obs[, pseudo[[2]]], pseudo[[1]][,1])]
        collab <- paste(obs$SPECIES_NR, obs$COMB, sep = ".")
    } else  collab <- as.vector(obs$SPECIES_NR)
    rowlab <- as.vector(obs$RELEVE_NR)
    cat('\n combining occurrences using type', toupper(lc), 'and creating vegetation matrix ... \n')
    layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
    results <- switch(lc,  # Inflate matrix
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
        print(as.numeric(colnames(results)))
	species <- tax(as.numeric(colnames(results)), verbose=TRUE, syn=FALSE, refl = refl, tv_home=tv_home, ...)
	if(spcnames=='short') colnames(results) <- species$LETTERCODE[match(colnames(results), species$SPECIES_NR)]
	if(spcnames=='long') colnames(results) <- gsub(' ','_', species$ABBREVIAT[match(colnames(results), species$SPECIES_NR)] )
       } else {
	st <- unlist(strsplit(colnames(results), ".", fixed = TRUE))
	colspc <- st[seq(1, length(st), 2)]
	species <- tax(as.numeric(colspc), verbose=FALSE, refl = refl, tv_home=tv_home, ...)
	ll <- st[seq(2, length(st), 2)]
	if(spcnames=='short') coln <- as.character(species$LETTERCODE[match(as.numeric(colspc), species$SPECIES_NR)])
	if(spcnames=='long') coln <- gsub(' ','_', species$ABBREVIAT[match(as.numeric(colspc), species$SPECIES_NR)]) 
	cn <- replace(coln, which(ll != 0), paste(coln, ll, sep = ".")[ll != 0])
	colnames(results) <- cn
	}
   }
   if (any(is.na(colnames(results)))) warning("Some taxa without names, check reference list!")
## Result
#   results <- results[, order(names(results))]
   class(results) <- c("veg", "data.frame")
   if(cover.transform!='no') {
      if(cover.transform == 'pa') results <- as.data.frame(ifelse(results > 0, 1,0))
      if(cover.transform == 'sqrt') results <- as.data.frame(round(sqrt(results),dec))
   }
      class(results) <- c("veg", "data.frame")
      attr(results, 'taxreflist') <- refl
   return(results)
}


first.word <- function (x, i = 1, expr = substitute(x)) {
    words <- if(!missing(x)) as.character(x)[1] else as.character(unlist(expr))[1]
    if (i > 1) stop("i > 1 not implemented")
    chars <- substring(words, 1:nchar(words), 1:nchar(words))
    legal.chars <- c(letters, LETTERS, ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    non.legal.chars <- (1:length(chars))[chars %in% legal.chars]
    if (!any(non.legal.chars)) return(words)
    if (non.legal.chars[1] == 1) return(character(0))
    substring(words, 1, non.legal.chars[1] - 1)
}
