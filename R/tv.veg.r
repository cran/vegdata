if(getRversion() >= "2.15.1")  utils::globalVariables(c("lc.1"))

tv.veg <- function (db, taxval = TRUE, tv_home,  convcode = TRUE, 
  lc = c("layer","mean","max","sum","first"), pseudo, values = "COVER_PERC", 
  spcnames = c('short','long','numbers'), dec = 0, cover.transform = c('no', 'pa', 'sqrt'), 
  obs, site, refl, RelScale, ...) 
{
options(warn=1)
  ## Checks
    lc <- toupper(match.arg(lc))
    cover.transform <- match.arg(cover.transform)
    spcnames = match.arg(spcnames)
    if(missing(tv_home)) tv_home <- tv.home()
    if(missing(obs)) {
      cat('Reading tvabund.dbf\n')
      obs <- tv.obs(db, tv_home) 
      } else names(obs) <- TCS.replace(names(obs))
#     if(suppressWarnings(any(obs < -1e+05, na.rm = TRUE))) 
#       cat("\n WARNING! Values less than -100,000. \n WARNING! tvabund.dbf may be corrupt. \n WARNING! Please correct by reexporting e.g. with OpenOffice.")
    lc.1 <- data.frame(LAYER=0:9, COMB=c(0,rep('Tree',3),rep('Shrub',2),rep(0,4)))
#    data("lc.1", package = 'vegdata', envir = 'vegdata')
    if(missing(pseudo)) pseudo <- list(lc.1, 'LAYER')

## Taxa
    if(missing(refl)) refl <- tv.refl(db = db[1], tv_home = tv_home)
    cat('Taxonomic reference list: ',refl, '\n')
    if(taxval) {
      obs <- taxval(obs=obs, refl = refl, ...)
    }
## CoverCode
    if(missing(site)) {
      cat('Reading tvhabita.dbf\n')
      site <- tv.site(db, tv_home, verbose = FALSE)
    }
    if(convcode){
        cat('converting cover code ... \n')
        if(missing(RelScale)) {
          if(missing(db)) stop('\nEither database name or a vector with CoverScale per releve has to be permitted, to cope with Cover Scale information\n')
          RelScale <- site[, c("RELEVE_NR", "COVERSCALE")]
        }
     	  obs <- tv.coverperc(db=db, obs=obs, RelScale = RelScale, tv_home = tv_home, ...)
    } else {
      if (!any(names(obs) == values)) stop(paste(values, " could not be found.")) 
          obs[,values] <- type.convert(as.character(obs[,values]))
          if(is.factor(obs[,values])) {
            lc='first'
            warning("Multiple occurrences of a species in one layer of a releve can not be supported without cover code conversion. 
                  Only the first occurrence will be used!")}
    }
## empty plots
    noobs <- site$RELEVE_NR[!site$RELEVE_NR %in% obs$RELEVE_NR]
    if(length(noobs) > 0)
      for(i in 1:length(noobs)) {
        obs[(nrow(obs)+1), ] <- NA
        obs$RELEVE_NR[nrow(obs)] <- noobs[i]; obs$TaxonUsageID[nrow(obs)] <- 0; obs$LAYER[nrow(obs)] <- 0
      }
## Pseudo-Species / Layer
    if(!is.null(pseudo)) {
        cat('creating pseudo-species ... \n')
	if(length(pseudo[[2]]) > 1) stop('Possibility to differentiate more than one plot-species attribute not yet implemented. \n
	Please contact <florian.jansen@uni-rostock.de>.')
        obs$COMB <- pseudo[[1]][, 2][match(obs[, pseudo[[2]]], pseudo[[1]][,1])]
        collab <- paste(obs$TaxonUsageID, obs$COMB, sep = ".")
    } else  collab <- as.vector(obs$TaxonUsageID)
    rowlab <- as.vector(obs$RELEVE_NR)
    cat('combining occurrences using type', toupper(lc), 'and creating vegetation matrix ... \n')
    gc(TRUE, verbose = FALSE)
    layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
    results <- switch(lc,  # Inflate matrix
      LAYER = tapply(obs[, values], list(rowlab, collab), layerfun),
      MEAN  = tapply(obs[, values], list(rowlab, collab), mean), 
      MAX   = tapply(obs[, values], list(rowlab, collab), max), 
      SUM   = tapply(obs[, values], list(rowlab, collab), sum),
      FIRST = tapply(obs[, values], list(rowlab, collab), first.word) )
    results[is.na(results)] <- 0
    results <- as.data.frame(results)
    colnames(results)
    
## Names
    if(spcnames %in% c('short','long')) {
      cat('replacing species numbers with ', spcnames, ' names ... \n')
      if(is.null(pseudo)) {
      	species <- tax(as.numeric(colnames(results)), detailed=TRUE, syn=FALSE, refl = refl, tv_home=tv_home)
      	if(spcnames=='short') colnames(results) <- species$LETTERCODE[match(colnames(results), species$TaxonUsageID)]
      	if(spcnames=='long') colnames(results) <- gsub(' ','_', species$TaxonName[match(colnames(results), species$TaxonUsageID)] )
       } else {
	st <- unlist(strsplit(colnames(results), ".", fixed = TRUE))
	colspc <- st[seq(1, length(st), 2)]
	species <- tax(as.numeric(colspc), detailed=FALSE, refl = refl, tv_home=tv_home)

	if(spcnames=='short') coln <- as.character(species$LETTERCODE[match(as.numeric(colspc), species$TaxonUsageID)])
	if(spcnames=='long') coln <- gsub(' ','_', species$TaxonName[match(as.numeric(colspc), species$TaxonUsageID)]) 

  ll <- st[seq(2, length(st), 2)]
  cn <- replace(coln, which(ll != 0), paste(coln, ll, sep = ".")[ll != 0])
	colnames(results) <- cn
	}
   }
   if (any(is.na(colnames(results)))) warning("Some taxa without names, check reference list!")
#  rownames(results) <- unique(rowlab)
## Result
#   results <- results[, order(names(results))]
   if(cover.transform!='no') {
      if(cover.transform == 'pa') results <- as.data.frame(ifelse(results > 0, 1,0))
      if(cover.transform == 'sqrt') results <- as.data.frame(round(sqrt(results),dec))
   }
  results <- results[, colSums(results)> 0]
  class(results) <- c("veg", "data.frame")
  attr(results, 'taxreflist') <- refl
  return(results)
}

