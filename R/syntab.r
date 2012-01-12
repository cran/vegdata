syntab <- function (veg, clust, type = c('rel','abs','mean.cover'), fullnames=FALSE, limit=0, mupa=NULL, alpha=0.05, minstat=0, dec = 0, ...)
{
#    if(mupa & !'indicspecies' %in% installed.packages()[,1]) stop('Please install package \'indicspecies\' before using option mupa=TRUE')
    type <- match.arg(type)
    if (missing(clust)) clust <- rep(1,nrow(veg))
    ncl <- length(unique(clust))
    cat(' Number of clusters: ', ncl, '\n')
    cat('Cluster frequency', as.vector(table(clust)),'\n')
    if(is.null(levels(clust))) levels(clust) <- 1:length(table(clust))
    sp.veg <- split(veg, clust)
    nb.rel.clust <- as.numeric(table(clust))

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
    if(type=='abs') temp <- lapply(sp.veg, function(x) colSums(x > 0))

    st <- as.data.frame(temp)
    st[is.na(st)] <- 0
    names(st) <- levels(clust)
    if(ncl>1) st <- st[apply(st, 1, function(x) max(x, na.rm=TRUE) > limit),]
    if(!is.null(mupa) | class(mupa)=='multipatt' & ncl<1) {
      require(indicspecies)
      if(class(mupa)!='multipatt')
	mu <- multipatt(veg, clust, ...) else 
	mu <- mupa
      sig <- mu$sign[mu$sign$p.value <= alpha & !is.na(mu$sign$p.value) & mu$sign$stat >= minstat,]
      st <- st[rownames(sig),]
      o <- order(sig[,'index'])

      st <- st[o,]
      st$cl <- attr(mu$str,'dimnames')[[2]][sig[o,'index']]
    }
    class(st) <- c('syntab','data.frame')

    if(fullnames) {
      nam <- tax(rownames(st), verbose=FALSE, ...)
      rownames(st) <- nam$ABBREVIAT[match(rownames(st), nam$LETTERCODE)]
      }
    print(st)
    invisible(st)
}

print.syntab <- function(x, zero.print='.', ...) {
class(x) <- 'data.frame'
if(ncol(x)>1) {
  if(names(x)[ncol(x)] == 'cl') mu=TRUE else mu=FALSE
  if(mu) {cl <- x[ncol(x)]; x <- x[-ncol(x)]}
   if(zero.print != "0" && any(i0 <- x == 0)) {
      x[i0] <- sub("0", zero.print, x[i0])
      x[i0] <- sub("0.0", zero.print, x[i0])
      x[i0] <- sub("0.00", zero.print, x[i0]) }
  if(mu) x <- cbind(x, cl)
 }
   print.data.frame(x, zero.print='', ...)
 }

