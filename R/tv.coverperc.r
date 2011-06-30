tv.coverperc <- function (db, obs, RelScale, tv_home, tvscale, ...) 
{
  if (missing(tv_home)) 
      tv_home <- tv.home(...)
  if(missing(tvscale)) 
      tvscale <- read.dbf(file.path(tv_home, "Popup", "tvscale.dbf"))
  tvscale <- tvscale[!is.na(tvscale$SCALE_NR),]
  rownames(tvscale) <- tvscale[, 1]
  if (missing(RelScale)) {
      ow <- options('warn')
      options(warn = -1)
      RelScale <- tv.site(db, tv_home, quiet = TRUE)[, c("RELEVE_NR", "COVERSCALE")]
      options(ow)
      }
  if (missing(obs))
      obs <- tv.obs(db, tv_home)
  obs$COVERSCALE <- RelScale$COVERSCALE[match(obs$RELEVE_NR, RelScale$RELEVE_NR)]
  g <- obs$COVERSCALE
  if(any(is.na(g)))  {
    print(obs[is.na(g),])
    stop('These releve numbers are without valid CoverScale value.')
    }
  obs <- split(obs, g, drop = FALSE)
  for (i in names(obs)) {
    if (i == "00") 
      obs[[i]] <- data.frame(obs[[i]], COVER_PERC = as.numeric(as.character(obs[[i]][, "COVER_CODE"])))
    else {
      p <- which(is.na(tvscale[i,]))[1]
      if(is.na(p)) p <- ncol(tvscale)
      scala <- tvscale[i,]
      if(is.na(scala[1])) stop('Can not find cover scale', i, 'in Turbowin/Popup/tvscale.dbf')
      code <- t(scala[seq(4,(p-1),2)])
      perc <- scala[seq(5,p,2)][1,]
      d.f <- data.frame(code=code[,1], perc = as.numeric(perc))
      cat('\n Cover code used: ', as.character(tvscale[i, 2]), '\n')
      write.table(t(d.f), col.names = FALSE, sep = "\t", quote = FALSE)
      obs[[i]]["COVER_PERC"] <- d.f$perc[match(obs[[i]][,"COVER_CODE"], d.f$code)]
  }
  }
  obs <- unsplit(obs, g)
  if(any(is.na(obs$COVER_PERC))) {
      print(obs[is.na(obs$COVER_PERC),])
      stop("Invalid cover codes, please check tvabund.dbf and tvscale.dbf!")
  }
  obs
}

