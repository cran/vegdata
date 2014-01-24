syntab <- function (veg, clust, type = c('rel','abs','mean.cover'), fullnames=FALSE, mupa=NULL, dec = 0, refl, ...)
{
#    if(fullnames & missing(refl)) stop('Please specify taxonomic reference list for fullnames.')
    type <- match.arg(type)
    if (missing(clust)) clust <- rep(1,nrow(veg))
    ncl <- length(unique(clust))
    cat(' Number of clusters: ', ncl, '\n')
    nb.rel.clust <- as.numeric(table(clust))
    cat('Cluster frequency', nb.rel.clust,'\n')
    if(is.null(levels(clust))) levels(clust) <- 1:length(table(clust))
    sp.veg <- split(veg, clust, drop=FALSE)
    if(any(colSums(veg)==0)) stop('Some species without occurrence.')
    if(type=='rel') {
    	tmp <- lapply(sp.veg, function(x) colSums(x > 0))
    	temp <- vector('list', length=ncl)
	    for(i in 1:length(nb.rel.clust))
	      temp[[i]] <- round(tmp[[i]] / nb.rel.clust[i] * 100, dec)
    }
    if(type=='mean.cover') {
      temp <- lapply(sp.veg, function(x) {x[x==0] <- NA; round(colMeans(x, na.rm=TRUE),dec)})
      is.na(temp) <- 0
    }
    if(type=='abs') 
      temp <- lapply(sp.veg, function(x) colSums(x > 0))

    st <- as.data.frame(temp)
    st[is.na(st)] <- 0
    names(st) <- levels(clust)

    if(!is.null(mupa) | class(mupa)=='multipatt' & ncl < 1) {
      if(class(mupa)!='multipatt') {
      	 require(indicspecies) || stop("Needs package indicspecies (function multipatt)")
    	    mu <- multipatt(veg, clust, ...) } else mu <- mupa
      # st <- st[rownames(st) %in% rownames(sig),]     
      # o <- order(mu$sign[,'index'])
      df <- mu$sign
      df[, 1:ncl] <- st # mu$sign[,1:ncl] * st
      colnames(df)[1:ncl] <- levels(clust)
      st <- df
    }
    if(fullnames) {
      if(missing(refl) & is.null(attr(veg, 'taxreflist'))) stop('Either "taxreflist" attribute must be given in veg object or refl must be specified.')
      if(missing(refl)) refl <- attr(veg, 'taxreflist')
      if(length(grep('.', rownames(st), fixed=TRUE)) > 0) stop('Fullname substitution only possible without pseudo-species separation (e.g. layers).')
      nam <- tax(rownames(st), verbose=FALSE, syn=FALSE, refl, ...)
      rownames(st) <- nam$TaxonName[match(rownames(st), nam$LETTERCODE)]
      }
    class(st) <- c('syntab', 'data.frame')
#    print(st)
    return(st)
}

print.syntab <- function(x, zero.print='.', trait, limit = 1, minstat = 0, alpha = 0.05, ...) {
  if(any(c('stat','index','p.value') %in% names(x))) {
    if(any(is.na(x[1:(ncol(x)-3)]))) stop('NA values in frequency table. Do you have species without occurrences in your matrix?')
    mu=TRUE } else {
      if(any(is.na(x))) stop('NA values in frequency table. Do you have species without occurrences in your matrix?')
      mu=FALSE
  }
  if(mu) {
    stat <- x[,'stat']; x <- x[,-which(names(x)=='stat')]
    index <- x[,'index']; x <- x[,-which(names(x)=='index')]
    p.value <- x[,'p.value']; x <- x[,-which(names(x)=='p.value')]
    select <- stat > minstat & !is.na(stat) & p.value < alpha & !is.na(p.value) & apply(x, 1, function(y) max(y) >= limit)
    } else  select <- rep(TRUE, length = nrow(x))
  x <- x[select, ]
  if(zero.print != "0" && any(i0 <- x == 0)) {
      x[i0] <- sub("0", zero.print, x[i0])
      x[i0] <- sub("0.0", zero.print, x[i0])
      x[i0] <- sub("0.00", zero.print, x[i0]) 
  }
  if(mu) {
#     <- index[select], stat[select], 
    x <- cbind(x, index=index[select], stat=stat[select], p.value=p.value[select])
    x <- x[order(x$index),]
    }

  if(!missing(trait)) {
    if(is.null(names(trait))) stop('Trait vector must have names of taxa according to the vegetation matrix.')
    traitname <- as.character(substitute(trait))
    trait <- as.data.frame(trait[match(rownames(x), rownames(trait)),])
		x <- cbind(x, trait)
#		names(x)[names(x)=='trait'] <- traitname
	}
  print.data.frame(x, ...)
 }

