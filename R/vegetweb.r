# library(vegdata)
## Zugriff auf VegetWeb

vw.con <- function() {
    require(RMySQL)
    db <- list(user='vegbank', password='vw', host='141.53.8.94', dbname='vegetweb')
    con <- dbConnect(MySQL(), user = db$user, password = db$password, dbname = db$dbname, host = db$host)
# dbListTables(con)
    return(con)
  }

## Header data
vw.site <- function(query=NULL, quiet=TRUE, ...) {
    if(missing(query)) {
    cat('\n No query string specified. \n You can select vegetation plots from VegetWeb with queries like
	  query=\"Projekt=\'T271\'\" \n This will select all releves from project T271, i.e Dengler 2007 Tuexenia.\n')
    cat(' If you want to see which selection parameters are available, try: \n',
      'con <- vw.con() \n', 
      'dbListFields(con, \"beobachtung\")\n', 
#      'dbListTables(con)\n', 
      'summary(dbGetQuery(con, "SELECT pH FROM beobachtung")) \n',
      'dbGetQuery(con, "SELECT * FROM projekt") \n etc. \n\n')
 } else {
  con <- vw.con()
  res <- dbSendQuery(con, statement = paste(
    'SELECT b.*, p.*',
    'FROM beobachtung AS b LEFT JOIN plot AS p',
    'ON p.Plot_ID = b.Plotcode',
     paste('WHERE b.', query, sep='')
     ))
  site <- fetch(res, n=-1)
 dbDisconnect(con)

#  for(i in c("SYNTAXON", "Erheber", "Subassoziation", "Lokalit\xe4t")) site[,i] <- iconv(site[,i], 'ISO8859-15', "")
  site$Datum <- as.Date(site$Datum, '%Y-%m-%d')
#  site$UTM <- paste(site$'UTM-GK Zone',site$Ostkoordinate, site$Nordkoordinate,sep='')

### Turboveg order and naming
# site$COVER_TOTAL <- NA
# site$COVER_LICHEN <- NA
  tv <- c('Beobachtungs_ID', 'Land', "Zitat", 'Zitattabelle', 'Zitataufnahme', 'Deckungsmethode', 'Datum', "Gesellschaftsbezeichnung", 'Fl\xe4che', 'H\xf6he \xfcNN',"Exposition", "Neigung", "Deckung Baumschicht","Deckung Strauchschicht","Deckung Feldschicht",'Anteil Streuschicht', 'Anteil offene Wasserfl\xe4che','Anteil Fels', 'H\xf6he Baumschicht','H\xf6he Strauchschicht','H\xf6he Feldschicht','H\xf6he Kryptogamenschicht', "Moosidentifikation","Flechtenidentifikation") # 'Geografische Breite', 'Geografische L\xe4nge'
  site <- cbind(site[,tv], site[,!names(site) %in% tv])
  names(site)[1:length(tv)] <- c('RELEVE_NR', 'COUNTRY', 'REFERENCE', 'TABLE_NR', 'NR_IN_TAB', 'COVERSCALE', 'DATE', 'SYNTAXON', 'SURF_AREA', 'ALTITUDE', 'EXPOSITION', 'INCLINATIO', 'COV_TREES', 'COV_SHRUBS', 'COV_HERBS', 'COV_LITTER', 'COV_WATER', 'COV_ROCK', 'TREE_HIGH', 'SHRUB_HIGH', 'HERB_HIGH', 'CRYPT_HIGH', 'MOSS_IDENT', 'LICH_IDENT') #'LATITUDE', 'LONGITUDE'


### Empty variables
  fun <- function(x) all(is.na(x))
  na <- apply(site, 2, fun)
      if(any(na) & !quiet) {
  cat('\n The following columns contain no data and are omitted \n')
  print(names(site)[na], quote = FALSE)
  }
  site <- site[, !na]
  names(site) <- iconv(names(site), 'ISO8859-15', "")
  return(site)
  }
}


vw.obs <- function(query=NULL, refl='GermanSL 1.2', ...) {
    con <- vw.con()
    if(missing(query)) {
    cat('\n No query string specified. \n You can select vegetation plots from VegetWeb with queries like
	  query=\"Projekt=\'T271\'\" \n This will select all releves from project T271, i.e Dengler 2007 Tuexenia.\n')
    cat(' If you want to see which selection parameters are available, try: \n',
      'con <- vw.con() \n', 
      'dbListFields(con, \"beobachtung\")\n', 
      'dbListTables(con)\n', 
      'summary(dbGetQuery(con, "SELECT pH FROM beobachtung")) \n',
      'dbGetQuery(con, "SELECT * FROM projekt") \n etc. \n\n')
 } else {
#  paste('SELECT * FROM taxonbeobachtung as taxbeob LEFT JOIN _fundtaxa as tax ON taxbeob.Taxonname = tax.Taxonname_ID LEFT JOIN beobachtung as beob ON beob.Beobachtungs_ID = taxbeob.Beobachtung WHERE', query)
# SELECT Deckungsprozent FROM deckungsindex as COVER_PERC LEFT JOIN schichtzusammensetzung as layer ON Deckungsmethode = 
 
# dbSendQuery(con, paste('CREATE TABLE tmp_beo SELECT b.Beobachtungs_ID FROM beobachtung AS b WHERE b.', query,sep=''))   
# tmp <- dbReadTable(con, 'tmp_beo')

# query <- "Projekt='T271'"
# res <- dbSendQuery(con, paste("SELECT o.Beobachtung, o.Taxonname FROM taxonbeobachtung AS o INNER JOIN beobachtung ON o.Beobachtung = beobachtung.Beobachtungs_ID WHERE beobachtung.", query, sep='')
#  )
# obs <- fetch(res, n = -1)

obs <- dbGetQuery(con, statement=paste(
    'SELECT s.Beobachtung, k.Taxonkonzept_ID, k.Taxonname, n.Taxonname, k.GermanSLNr, sz.Schichttaxondeckung, s.Schichttyp, d.Deckungsprozent',
    'FROM taxonkonzept k, taxonname n, schichtzusammensetzung sz, schicht s, beobachtung b, deckungsindex d',
    'JOIN taxoninterpretation i',
    'LEFT JOIN taxonrelation r ON r.Taxonkonzept1 = i.Taxonkonzept',
    'AND r.Relationsqualit\xe4t = 1',
    'AND r.Referenz = 1014',
    'WHERE k.Taxonname = n.Taxonname_ID',
    'AND i.Taxonbeobachtung = sz.Taxonbeobachtung',
    'AND sz.Schicht = s.Schicht_ID',
    'AND i.Aktuell = 1',
    'AND IF( r.Taxonkonzept2 IS NULL , i.Taxonkonzept, r.Taxonkonzept2 ) = k.Taxonkonzept_ID',
    'AND b.Beobachtungs_ID = s.Beobachtung',
    'AND b.Deckungsmethode = d.Deckungsmethode',
    'AND sz.Schichttaxondeckung = d.Deckungscode',
     paste('AND b.',query, sep='') )
)
#obs <- fetch(qu, n = -1)
#dbClearResult(qu)
  dbDisconnect(con)
  taxa <- tax('all', refl=refl)
  taxa$ABBREVIAT <- sub.abbr(taxa$ABBREVIAT)
  obs$Taxonname <- sub.abbr(obs$Taxonname)
  obs$NAME_Match <- taxa$SPECIES_NR[match(obs$Taxonname, taxa$ABBREVIAT)]
  names(obs) <- c('RELEVE_NR','Taxonkonzept_ID','Taxonname_ID','TAXNAME', 'SPECIES_NR','COVER_CODE','LAYER','COVER_PERC','NAME_Match')
  obs <- obs[order(obs$RELEVE_NR),]
  nomatch <- obs[is.na(obs$SPECIES_NR),]
  if(nrow(nomatch)>0) {
  tmp <- nomatch[!duplicated(nomatch[,c('TAXNAME','Taxonkonzept_ID')]),c("Taxonkonzept_ID", "Taxonname_ID", "TAXNAME")]
  Freq <- table(nomatch[,'TAXNAME'])
  tmp$Freq <- Freq[match(tmp$TAXNAME, names(Freq))]
  cat('The following name oberservations are not compatible with', refl, 'and are therefore deleted.\n')
  print(tmp)
  obs <- obs[!obs$Taxonkonzept_ID %in% tmp$Taxonkonzept_ID,]
}
  class(obs) <- c('vw.obs', 'data.frame')
  return(obs)
  }
}

vw.veg <- function(query, obs, taxval=TRUE, ...){
    if(missing(obs)) obs <- vw.obs(query)
    refl <- tv.refl(refl='GermanSL 1.2')
    result <- tv.veg(db=NULL, obs=obs, refl=refl, taxval=taxval, convcode=FALSE, ...)
    result
}

###################################################################################
