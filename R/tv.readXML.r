# # Reads a Turboveg XML file
# library(vegdata)
# library(xml2)
# library(plyr)
# # library(data.table)
# require(doMC)
# registerDoMC(detectCores()-1)
#file='~/Projekte/vegetweb2.0/Daten/05_vegetwebExport/warenkorb_leyer.xml'
#file='~/Projekte/vegetweb2.0/Daten/05_vegetwebExport/warenkorb_CarexBrachystachys.xml'

tv.readXML <- function(file) {
  options(stringsAsFactors=FALSE)
  veg = read_xml(file)
  plots <- xml_find_all(veg, "//Plots")
#  xml_find_all(veg, "//Lookup_tables")
#  xml_find_all(veg, "//Template")
#  xml_children(plots)
#  xml_attrs(xml_children(plots)[[1]]) 
  guid <- xml_attr(xml_children(plots), 'guid')
  releve_nr <- as.integer(xml_attr(xml_children(plots), 'releve_nr'))
#  xml_find_all(plots, '//header_data')[[2]]
#  xml_attr(xml_find_all(plots, '//header_data//standard_record'), 'reference')
  
  ## tvadmin  ####
  tvadmin <- data.frame(RELEVE_NR=releve_nr, SOURCE_DB=xml_attr(xml_children(plots), 'database'), GUID=guid,	CREAT_USER=xml_attr(xml_children(plots), 'create_user'),	CREAT_DATE=xml_attr(xml_children(plots), 'create_date'), MOD_USER=xml_attr(xml_children(plots), 'mod_user'),	MOD_DATE=xml_attr(xml_children(plots), 'mod_date'), NDFF_QUAL=xml_attr(xml_children(plots), 'ndff_qual'), stringsAsFactors = FALSE)
  if(any(duplicated(tvadmin$RELEVE_NR))) {
    dup = TRUE
    warning('Duplicated Turboveg RELEVE_NR detected. New identifiers (consecutive) have been created.')
    tvadmin$ORIGINAL_RELEVE_NR <- tvadmin$RELEVE_NR
    tvadmin$RELEVE_NR <- 1:nrow(tvadmin)
  } else dup = FALSE
  cat('tvadmin extracted ...\n')
  ## header ####
  site.list <- xml_attrs(xml_find_all(plots, '//header_data//standard_record'))
  site <- ldply(site.list, rbind, .parallel = TRUE)
  site$guid <- tvadmin$GUID
  # extract user defined header data  ####
  l <- xml_length(xml_find_all(plots, '//header_data')) - 1
  if(any(l > 0)) {
    udf.list <- xml_attrs(xml_find_all(plots, '//header_data//udf_record'))
    f <- rep.int(1:nrow(site), l)
    sv <- split(sapply(udf.list, '[', 'value'), rep.int(1:nrow(site), l))
    sn <- split(sapply(udf.list, '[', 'name'), rep.int(1:nrow(site), l))
    for(i in 1:length(sv)) names(sv[[i]]) <- sn[[i]]
    site.udf <- ldply(sv, rbind, .parallel = TRUE)
    site.udf$guid <- site$guid[l != 0]  # take care of partially missing udf plots
    site <- merge(site, site.udf, by='guid', all.x=TRUE)
  }
  names(site) <- toupper(names(site))
  site$RELEVE_NR <- tvadmin$RELEVE_NR[match(site$GUID, tvadmin$GUID)]
  cat('header extracted ...\n')
  
  ## tv.win  ####
  dict <- xml_attr(veg, 'Dictionary')
  if(!dict %in% list.dirs(file.path(tv.home(), 'Popup'), full.names = FALSE))
    stop(paste('Dictionary', dict, 'not in Turboveg Popup directory. XML file can not be converted.'))
  refl <- xml_attr(veg, 'SpeciesList')
  if(!tolower(refl) %in% tolower(list.dirs(file.path(tv.home(), 'Species'), full.names = FALSE)))
    stop(paste('Species list', refl, 'not in Turboveg Species directory. XML file can not be converted.'))
  tvwin <- data.frame(FLORA = refl, MINALLOW = 1, MAXALLOW = 999999, MINACTUAL = min(as.integer(site$RELEVE_NR)), MAXACTUAL = max(as.integer(site$RELEVE_NR)), MAP = 'EUROPE', DICTIONARY = dict, META = '', stringsAsFactors = FALSE)
  cat('tvwin extracted ...\n')
  
  ## obs  ####
  xml_length(plots) #xml_find_all(plots, '//species_data//species/standard_record'), 'nr')
  obs <- data.frame(
    RELEVE_NR=rep.int(guid, xml_length(xml_find_all(plots, '//species_data'))), 
    SPECIES_NR=as.integer(xml_attr(xml_find_all(plots, '//species_data//species//standard_record'), 'nr')), 
    COVER=xml_attr(xml_find_all(plots, '//species_data//species/standard_record'), 'cover'), 
    LAYER=xml_attr(xml_find_all(plots, '//species_data//species/standard_record'), 'layer'), stringsAsFactors = FALSE)
  cat('obs extracted ...\n')
  obs$RELEVE_NR <- tvadmin$RELEVE_NR[match(obs$RELEVE_NR, tvadmin$GUID)]
  # return ####
  cat(paste('number of plots: ', nrow(site), '\n'))
  cat(paste('number of references: ', length(unique(site$REFERENCE)), '\n'))
  class(obs) <- c('tv.obs', 'data.frame')
  return(list(tvwin=tvwin, tvadmin=tvadmin, site=site, obs=obs))
}

