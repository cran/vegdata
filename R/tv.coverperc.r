tv.coverperc <- function (db, obs, RelScale, tv_home, tvscale, ...) 
{
  if (missing(tv_home)) 
      tv_home <- tv.home(...)
  if(missing(tvscale)) 
      tvscale <- read.dbf(paste(tv_home, "Popup", "tvscale.dbf", sep = "/"))
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
  if(any(is.na(g))) stop('Some occurences without valid Coverscale value.')
  obs <- split(obs, g, drop = FALSE)
  for (i in names(obs)) {
      if (i == "00") 
          obs[[i]] <- data.frame(obs[[i]], COVER_PERC = as.numeric(as.character(obs[[i]][, "COVER_CODE"])))
      else {
    p <- which(is.na(tvscale[i,]))[1]
    if(is.na(p)) p <- ncol(tvscale)
    scala <- tvscale[i,]
#    code <- apply(scala[,seq(4,p-1,2)],1,function(x) levels(factor(x)))
    code <- t(scala[seq(4,(p-1),2)])
#match(
    perc <- scala[seq(5,p,2)][1,]
    if (any(is.na(perc))) stop("TVScale.dbf is incorrect, contains NA values!")
          d.f <- data.frame(code=code[,1], perc = as.numeric(perc))
          cat('\n Cover code used: ', as.character(tvscale[i, 2]), '\n')
          write.table(t(d.f), col.names = FALSE, sep = "\t", quote = FALSE)
          obs[[i]]["COVER_PERC"] <- d.f$perc[match(obs[[i]][,"COVER_CODE"], d.f$code)]
      }
  }
  unsplit(obs, g)
}

