freqtab <- function (veg, clust, relfr = TRUE, sort, limit = 0, dec = 0, ...)
# pltord, spcord, pltlbl)
{
    if('RELEVE_NR'%in% names(veg) & 'SPECIES_NR' %in% names(veg)) {
          stop('freqtab for raw Turboveg format is yet to be written!')
     } else {
    if (missing(clust)) clust <- rep(1,nrow(veg))
    cat(' Number of clusters: ', length(unique(clust)), '\n')
    sp.veg <- split(veg, clust)
    abs.fr <- as.data.frame(lapply(sp.veg, function(x) colSums(x > 0)))
    if(relfr) {
       clust.fr <- as.numeric(table(clust))
       tmp <- abs.fr
       for(i in 1:length(clust.fr))
           tmp[,i] <- round(abs.fr[,i] / clust.fr[i] * 100,dec)
      } else tmp <- abs.fr
    }
    tmp[is.na(tmp)] <- 0
    nam <- tax(rownames(tmp), tax=FALSE, ...)
    rownames(tmp) <- nam$ABBREVIAT[match(rownames(tmp), nam$LETTERCODE)]
    lim <- apply(tmp, 1, function(x) max(x)>limit)
    if(!missing(sort)) tmp <- tmp[,sort]
    tmp[lim,]
}
