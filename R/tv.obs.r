"tv.obs" <- function(db, tv_home, ...) {
    if(missing(tv_home)) tv_home <- tv.home(...)
    # Observations
#    if(is.list(db)) obs <- vw.query(db, 'obs') else
    obs <- read.dbf(file.path(tv_home, 'Data', db[1],'tvabund.dbf'))
    cat(paste('reading observations ...', '\n'))
    # Combine multiple databases
    if(length(db)>1) {
    refl.1 <- tv.refl(db[1])
      for(i in 2:length(db)) {
	refl.i <- tv.refl(db[i])
	if(refl.1 != refl.i) stop('Differing taxonomic reference lists used in the databases!')
	obs.tmp <- read.dbf(file.path(tv_home, 'Data', db[i],'tvabund.dbf'))
	if(any(unique(obs$RELEVE_NR) %in% unique(obs.tmp$RELEVE_NR))) stop('Overlap of releve numbers between the databases!')
	if(any(!names(obs) %in%  names(obs.tmp))) {
	  ind.obs <- match(names(obs), names(obs.tmp), nomatch = 0)
	  ind.tmp <- match(names(obs.tmp), names(obs),nomatch=0)
	obs <- rbind(obs[,ind.obs], obs.tmp[,ind.tmp]) } else obs <- rbind(obs, obs.tmp)
      }
    }
    class(obs) <- c('tv.obs','data.frame')
    obs
}
