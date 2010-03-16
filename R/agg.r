agg <- function (member = "all", refl, tv_home, ...) {
 if (missing(tv_home)) tv_home <- tv.home()
 taxa <- tax("all", refl = refl, tax = TRUE, syn = FALSE, ...)
 if(is.character(member)) {
    if(member == 'all') 
      m <- taxa$SPECIES_NR[taxa$RANG %in% agg & taxa$SYNONYM == FALSE] else 
      if(nchar(member)[1]== 7) m <- taxa$SPECIES_NR[taxa$LETTERCODE %in% member] else {
	member <- sub.abbr(member)
	m <- taxa$SPECIES_NR[taxa$ABBREVIAT %in% member]
  #    Agg$ABBREVIAT <- sub.abbr(Agg$ABBREVIAT)
  #    Agg$SECUNDUM <- sub.abbr(Agg$SECUNDUM)     
      }
 }
 if(is.numeric(member)) m <- member 
 Agg <- taxa[!is.na(taxa$AGG) & taxa$AGG %in% m,]
 Agg <- Agg[,-c(8,9)] #, c("SPECIES_NR", "ABBREVIAT", "AGG", "AGG_NAME")]
 print(paste('Members of', taxa$ABBREVIAT[taxa$SPECIES_NR==m], ':'))
 print(Agg)
 invisible(Agg)  
}
