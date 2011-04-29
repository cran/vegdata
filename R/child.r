childs <- function (x, refl, species, gen=NULL, quiet=FALSE, ...) {
#maxlevel = c('GAT')
 if(missing(species)) species <- tax("all", tax = TRUE, refl = refl, syn = TRUE, ...)
 if(is.character(x)) {
      if(nchar(x)[1]== 7) x <- species$SPECIES_NR[species$LETTERCODE %in% x] else
	x <- species$SPECIES_NR[grep(x, species$ABBREVIAT)]
    }
  if(length(x)>1) warning('More than one match, using only first.')
  x <- x[1]
 # Valid:
 x <- species[match(x, species$SPECIES_NR),'VALID_NR']
 x <- species[match(x, species$SPECIES_NR),]

 Agg <- species[which(species$AGG == x$SPECIES_NR),]
 if(nrow(Agg)==0) {if(!quiet) cat(x$ABBREVIAT, 'has no childs.\n') } else{
 Agg$GENERATION <- 1
# table(species$RANG)
# ROOT  ABT  UAB  KLA  UKL  ORD FAM  GAT  AG2  AGG  SEC  SER  SSE  SGE  SPE  SSP  VAR  ZUS
  ag2 <- Agg
  t <- 1
  repeat {
    t <- t+1
    ag2 <- species[which(species$AGG %in% ag2$SPECIES_NR),]
    if(nrow(ag2)== 0 ) break
    ag2$GENERATION <- t
    Agg <- rbind(Agg, ag2)
  }
  if(!is.null(gen)) Agg <- Agg[Agg$GENERATION <= gen,]
  if(!quiet) {
      cat('Childs of', x$ABBREVIAT, '(', x$SPECIES_NR, '):\n')
      print(Agg[,c('SPECIES_NR','ABBREVIAT','SECUNDUM','AGG','GENERATION','EDITSTATUS')])
    }
  invisible(Agg)
}}

parents <- function (x, refl, species, quiet=FALSE, ...) {
#maxlevel = c('GAT')
 if(missing(species)) species <- tax("all", tax = TRUE, refl = refl, syn = TRUE, ...)
 if(is.character(x)) {
      if(nchar(x)[1]== 7) x <- species$SPECIES_NR[species$LETTERCODE %in% x] else {
	x <- species$SPECIES_NR[grep(x, species$ABBREVIAT)]
    }}
  if(length(x)>1) warning('More than one match, using only first.')
  x <- x[1]
 x <- species[match(x, species$SPECIES_NR),'VALID_NR']
 x <- species[match(x, species$SPECIES_NR),]

 p <- species[match(unique(x$AGG),species$SPECIES_NR),]
 p$GENERATION <- 1
 if(nrow(p)==0) cat(x$ABBREVIAT, 'has no parents.\n') else{
# table(species$RANG)
# ROOT  ABT  UAB  KLA  UKL  ORD FAM  GAT  AG2  AGG  SEC  SER  SSE  SGE  SPE  SSP  VAR  ZUS
  p2 <- p
  t <- 1
  repeat {
  t <- t+1
  p2 <- species[match(p2$AGG,species$SPECIES_NR),]
  p2$GENERATION <- t
  p <- rbind(p, p2)
  if(p2$SPECIES_NR == 0 ) break
  }

  if(!quiet) {
    cat('Parents of', x$ABBREVIAT, '(', x$SPECIES_NR, '):\n')
    print(p[,c('SPECIES_NR','ABBREVIAT','SECUNDUM','AGG','GENERATION','EDITSTATUS')])
#    print(p[,c(1,3,8,9,12,21)])
   }
 invisible(p)
}}

agg <- function(x, refl, species, ...) {
  cat('Deprecated function. Use childs(x, gen=1) instead\n')
  childs(x, refl=refl, species=species, gen=1, ... )
  }
