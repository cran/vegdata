"tv.obs" <- function(db, tv_home, ...) {
    if(missing(tv_home)) tv_home <- tv.home()
    # Observations
#    if(is.list(db)) obs <- vw.query(db, 'obs') else
    obs <- read.dbf(file.path(tv_home, 'Data', db[1],'tvabund.dbf'))
    names(obs) <- TCS.replace(names(obs))
    cat(paste('reading observations ...', '\n'))
    # Combine multiple databases
    if(length(db)>1) {
      cat('More than 1 database, trying to combine.\n')
    refl.1 <- tv.refl(db = db[1])
      for(i in 2:length(db)) {
      	refl.i <- tv.refl(db = db[i])
      	if(refl.1 != refl.i) stop('You are using different taxonomic reference lists in your databases!')
      	obs.tmp <- read.dbf(file.path(tv_home, 'Data', db[i],'tvabund.dbf'))
        names(obs.tmp) <- TCS.replace(names(obs.tmp))
        if(any(unique(obs$RELEVE_NR) %in% unique(obs.tmp$RELEVE_NR))) stop('Overlap of releve numbers between the databases!')
    	  if(any(!names(obs) %in%  names(obs.tmp))) {
    	  ind.obs <- match(names(obs), names(obs.tmp), nomatch = 0)
    	  ind.tmp <- match(names(obs.tmp), names(obs), nomatch=0)
    	  obs <- rbind(obs[,ind.obs], obs.tmp[,ind.tmp]) 
        } else obs <- rbind(obs, obs.tmp)
      }
    }
    class(obs) <- c('tv.obs','data.frame')
    obs
}
