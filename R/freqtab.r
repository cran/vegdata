freqtab <- function (veg, clust, relfr = TRUE, limit = 0, dec = 0, ...)
# pltord, spcord, pltlbl)
{
    if('RELEVE_NR'%in% names(veg) & 'SPECIES_NR' %in% names(veg)) {
          stop('freqtab from Turboveg format is to be written!')
      obs <- veg
      if (missing(clust)) clust <- unique(obs$RELEVE_NR)     
      print(paste('Number of clusters: ', length(unique(clust))))
      obs$cluster <- clust[,2][match(obs$RELEVE_NR,clust[,1])]
      abs.fr <- tapply(obs$SPECIES_NR>0, list(obs$SPECIES_NR, obs$cluster), sum)
          if(relfr) {
             clust.fr <- as.numeric(table(clust[,2]))
             tmp <- abs.fr
             for(i in 1:length(clust.fr))
               tmp[,i] <- abs.fr[,i] / clust.fr[i] * 100
          } else tmp <- abs.fr
     } else {

    if (missing(clust)) clust <- rep(1,nrow(veg)); names(clust) <- rownames(veg)
    print(paste('Number of clusters: ', length(unique(clust))))
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
    tmp[lim,]
}
