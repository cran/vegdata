#' Reads a Turboveg XML file and writes a Turboveg dbase database
#'
#' @param file the filename of an XML file using Turboveg format (Turboveg version xx)
#'
tv.readXML <- function(file) {
  veg = xmlRoot(xmlTreeParse(file, useInternalNodes = T))
  if(xmlAttrs(veg)[['Dictionary']] == 'default') 
    XML::xmlAttrs(veg)['Dictionary'] <- ''
    #attr <- xmlAttrs(veg); attr[names(attr) == 'Dictionary'] <- ''
    #xmlAttrs(veg) <- attr
  if(!xmlAttrs(veg)['Dictionary'] %in% list.dirs(file.path(tv.home(), 'Popup'), full.names = FALSE))
    stop(paste('Dictionary', xmlAttrs(veg)['Dictionary'], 'not in Turboveg Popup directory. XML file can not be converted.'))
  refl <- xmlAttrs(veg)[['SpeciesList']]
  if(!tolower(refl) %in% tolower(list.dirs(file.path(tv.home(), 'Species'), full.names = FALSE)))
    stop(paste('Species list', xmlAttrs(veg)['SpeciesList'], 'not in Turboveg Species directory. XML file can not be converted.'))
    # read tvadmin
    out <- xmlApply(veg[["Plots"]], xmlAttrs) #    names(out[[1]])
    tmp <- data.frame(matrix(unlist(out), nrow=length(out), byrow=T), stringsAsFactors = FALSE)
    tvadmin <- tmp[,c(2,1,3:8)]
    names(tvadmin) <- c('RELEVE_NR', 'SOURCE_DB', 'GUID',	'CREAT_USER',	'CREAT_DATE', 'MOD_USER',	'MOD_DATE', 'NDFF_QUAL')
    # read plot header
    .readPlotHeader <- function(x) xmlAttrs(x[['header_data']][['standard_record']])
    out <- xmlApply(veg[["Plots"]], .readPlotHeader)
    names(out) <- tvadmin$RELEVE_NR
    site <- as.data.frame.list(out)
    # site <- data.frame(matrix(unlist(out), nrow=length(out), byrow=T), stringsAsFactors = FALSE)
    names(site) <- toupper(names(site))
#    if(any(site$RELEVE_NR %in% site.tmp$RELEVE_NR)) warning('Found duplicate Turboveg RELEVE_NR!')
    # user defined header data
    doc <- getNodeSet(veg, '//Plots/Plot/header_data')[[1]]
    udf <- as.data.frame(t(sapply(xmlChildren(doc), function(x) xmlAttrs(x)['value'])), stringsAsFactors = FALSE)
    names(udf) <- sapply(xmlChildren(doc), function(x) xmlAttrs(x)['name'])
    udf <- udf[,-1]
    for(i in 2:nrow(site)) { # i=2
      doc <- getNodeSet(veg, '//Plots/Plot/header_data')[[i]]
      tmp <- as.data.frame(t(sapply(xmlChildren(doc), function(x) xmlAttrs(x)['value'])), stringsAsFactors = FALSE)
      names(tmp) <- sapply(xmlChildren(doc), function(x) xmlAttrs(x)['name'])
      tmp <- tmp[,-1]
      cols1 <- names(udf)
      cols2 <- names(tmp)
      common <- intersect(cols1, cols2)
      udf <- rbind(udf[, common], tmp[, common])
    }
    udf$RELEVE_NR = site$RELEVE_NR
    site <- merge(site, udf, all = TRUE)
    site[site=='null'] <- NA
    site$RELEVE_NR <- as.integer(site$RELEVE_NR)
    # tvwin
    tvwin <- data.frame(FLORA = xmlAttrs(veg)[['SpeciesList']], MINALLOW = 1, MAXALLOW = 999999, MINACTUAL = min(as.integer(site$RELEVE_NR)), MAXACTUAL = max(as.integer(site$RELEVE_NR)), MAP = 'EUROPE', DICTIONARY = xmlAttrs(veg)[['Dictionary']], META = '')
    #########################
    # read plant observations
    # .readPlotObs <- function(x) xmlAttrs(x[['species_data']][['species']][['standard_record']])
    # out <- xmlApply(veg[["Plots"]], .readPlotObs)
    # names(out) <- 1:length(out)
    # obs <- as.data.frame.list(out)
    # names(obs) <- c('TaxonUsageID', 'COVER_CODE', 'LAYER')
    # .children <- function(x){
    #   xname <- xmlName(x)
    #   xattrs <- c('nr', 'cover', 'layer') # xmlAttrs(x)
    #   c(sapply(xmlChildren(x), xmlAttrs), name = xname, xattrs)
    # }
    # x <- getNodeSet(veg, '//Plots/Plot/species_data')[[6]]
    obs <- data.frame(RELEVE_NR=NULL,SPECIES_NR=NULL,COVER=NULL,LAYER=NULL)
    for(i in 1:nrow(site)) { # i=6
      releve_nr <- xmlAttrs(getNodeSet(veg, '//Plots/Plot')[[i]])[['releve_nr']]
      doc <- getNodeSet(veg, '//Plots/Plot/species_data')[[i]]
#      tmp <- xpathSApply(doc, "*/species", .children)
      tmp <- xpathSApply(doc, "species", function(x) xmlAttrs(x[['standard_record']]))
      o <- as.data.frame(t(tmp), stringsAsFactors = FALSE)
      names(o)[1:3] <- c('SPECIES_NR', 'COVER', 'LAYER')
      obs <- rbind(obs, data.frame(RELEVE_NR = rep(releve_nr, nrow(o)), o[,1:3], stringsAsFactors = FALSE))
    }
    names(obs) <- TCS.replace(names(obs))
    obs$RELEVE_NR <- as.integer(obs$RELEVE_NR)
    obs$TaxonUsageID <- as.integer(obs$TaxonUsageID)
    class(obs) <- c('tv.obs', 'data.frame')
    return(list(tvwin=tvwin, tvadmin=tvadmin, site=site, obs=obs))
# tv.write(obs, site, name = dbname, tvadmin = tvadmin, dict = xmlAttrs(veg)[['Dictionary']], refl= xmlAttrs(veg)[['SpeciesList']], overwrite = TRUE)
}


