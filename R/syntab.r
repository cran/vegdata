syntab <- function (veg, clust, freq = c('rel','abs','mean.cover'), fullnames=FALSE, limit=0, dec = 0, mupa=FALSE, alpha=0.05, minstat=0,...)
{
    freq <- match.arg(freq)
    if (missing(clust)) clust <- rep(1,nrow(veg))
    ncl <- length(unique(clust))
    if(freq=='assoc') mupa=TRUE
    cat(' Number of clusters: ', ncl, '\n')
    cat('Cluster frequency', as.vector(table(clust)),'\n')
    sp.veg <- split(veg, clust)
    nb.rel.clust <- as.numeric(table(clust))

    if(freq=='rel') {
	tmp <- lapply(sp.veg, function(x) colSums(x > 0))
	temp <- vector('list', length=ncl)
	for(i in 1:length(nb.rel.clust))
	    temp[[i]] <- round(tmp[[i]] / nb.rel.clust[i] * 100, dec)
    }
    if(freq=='mean.cover') {
      temp <- lapply(sp.veg, function(x) {x[x==0] <- NA; round(colMeans(x, na.rm=TRUE),dec)})
      is.na(temp) <- 0
    }
    if(freq=='abs') temp <- lapply(sp.veg, function(x) colSums(x > 0))

    st <- as.data.frame(temp)
    st[is.na(st)] <- 0
    names(st) <- levels(as.factor(clust))

    if(mupa) {
      require(indicspecies)
      mu <- multipatt(veg, clust, ...)
      sig <- mu$sign[mu$sign$p.value <= alpha & !is.na(mu$sign$p.value) & mu$sign$stat > minstat,]

      st <- st[rownames(sig),]
      o <- order(sig[,'index'])
      st <- st[o,]
      st$cl <- attr(mu$str,'dimnames')[[2]][sig[o,'index']]
      class(st) <- c('syntab','data.frame')
    }

    st <- st[apply(st[,-ncol(st)], 1, function(x) max(x, na.rm=TRUE)>limit),]

    if(fullnames) {
      nam <- tax(rownames(st), tax=FALSE, ...)
      rownames(st) <- nam$ABBREVIAT[match(rownames(st), nam$LETTERCODE)]
      }
    return(st)
}

print.syntab <- function(x, zero.print='.', ...) {
  if (zero.print != "0" && any(i0 <- x == 0))
      x[i0] <- sub("0", zero.print, x[i0])
      x[i0] <- sub("0.0", zero.print, x[i0])
      x[i0] <- sub("0.00", zero.print, x[i0])
    print.data.frame(x, zero.print='', ...)
  }



