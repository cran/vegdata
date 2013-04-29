# path <- '~/workspace/vegdata/inst'
# db <- file.path(path,'tvdata', 'Data', 'elbaue.xml')
# obs <- ESveg.obs(db)

ESveg.obs <- function (db, ...) {
  enc <- "ISO-8859-1"
#  data(lc.1)
  doc <- xmlParse(db)
 # names(xmlChildren(doc))
 # table(names(xmlChildren(xmlRoot(doc))))
  taxrefnode <- getNodeSet(doc, "//reference")
  taxref <- xmlValue(taxrefnode[[1]])
  userdefined <- xmlToDataFrame(getNodeSet(doc, "//userdefined"))
  ind <- userdefined$userDefinedTableName == "cover"
  if(any(ind)) cat('User defined plot-species attributes not used:', as.character(userdefined$userDefinedName[ind]), '\n') 
  taxa <- xmlToDataFrame(getNodeSet(doc, "//taxon"))
  obs <- xmlToDataFrame(getNodeSet(doc, "//cover"), stringsAsFactors=FALSE)
  obs$stratumCode <- as.character(obs$stratumCode)
  obs$stratumCode[obs$stratumCode == ''] <- 0
  coverindex <- xmlToDataFrame(getNodeSet(doc, "//coverindex"))
#  head(coverindex)
  samples <- xmlToDataFrame(getNodeSet(doc, "//observation"))
#  head(samples) # "observation" should be called "samples"
  obs$coverMethod <- samples$coverMethod[match(obs$observationCode, samples$observationCode)]
#  head(cover)
    obs$coverPercent <- NA
    covM <- levels(coverindex$coverMethod)
    for(i in unique(covM)) {
      index <- coverindex$coverMethod == i
      coindex <- obs$coverMethod == i  
    obs[coindex,'coverPercent'] <- 
      as.numeric(as.character(coverindex$coverPercent[match(obs[coindex,'taxonCover'] , coverindex$coverCode[index])]))
    }
  obs$coverPercent[is.na(obs$coverPercent)] <- as.character(obs$taxonCover[is.na(obs$coverPercent)])
#  head(obs)
#  obs$coverPercent
  names(obs)<- TCS.replace(names(obs))
  cat(paste('reading observations ...', '\n'))  
  class(obs) <- c('tv.obs','data.frame')
#  cat('Taxonomic reference list:', taxref, '\n')
  return(obs)  
}





ESveg.site <- function (db, ...) {
  # path <- system.file(package = "vegdata")
  # db <- file.path(path,'tvdata', 'Data', 'tvexport.xml')
  # db <- file.path(path,'tvdata', 'Data', 'elbaue.xml')
  # site <- ESveg.site(db)
  doc <- xmlParse(db)
  
  r <- xmlRoot(doc)
  if(xmlName(r) != 'ESVeg') stop('File not in ESVeg XML format.')
  # xmlSize(r)
  
  ### plot attributes
  # plot - sample - observation
  Samples <- xmlToDataFrame(getNodeSet(doc, "//observation"))
  userdefined <- xmlToDataFrame(getNodeSet(doc, "//userdefined"))
  ind <- userdefined$userDefinedTableName == "observation"
  if(any(ind))  {
    plotAttr <- as.character(userdefined$userDefinedName[ind])
    cat('User defined plot attributes:', plotAttr)
    samplenames <- unique(Samples[, 1:ncol(Samples) %in% grep('Name', names(Samples))])
    plotAttr <- plotAttr[which(plotAttr %in% unlist(samplenames))]
    samples <- Samples[, !1:ncol(Samples) %in% grep('Name', names(Samples))]
    names(samples)[(ncol(samples)-(length(plotAttr)-1)) : ncol(samples)] <- plotAttr   
    #  head(samples) # "observation" should better be called "samples"
  }
  
  plotNodes <- getNodeSet(doc, "//plot")
  plot <- xmlToDataFrame(getNodeSet(doc, "//plot"))
  if(nrow(plot) != nrow(samples)) cat('Multiple observations of plots. This function handles observations. Site conditions will be recycled.')
  site <- cbind(plot[match(as.character(samples$plotCode), plot$plotCode),], samples)
  
  ### Time
  if(any(site$obsEndDate == '1901-01-01')) warning(sum(site$obsEndDate == '1901-01-01'), ' releves without date.')
    #  site$obsEndDate <- gsub('/','',site$obsEndDate)
    #      Date <- rep('no date', nrow(site))
  site$obsEndDate <- as.Date(site$obsEndDate, '%Y-%m-%d')

  ### Survey Area
#   n <- sum(site$area == 0 | is.na(site$area))
#   if(n>0) warning(paste(n, ' releves without survey area'))
  
  ### Conversion of factors
  # fact <- sapply(site, is.factor)
  for (i in (1:ncol(site))) # [fact & !(1:ncol(site)) %in% c(3,6,9)]) 
    site[,i] <- type.convert(as.character(site[, i]),...)
  # site$SURF_AREA[site$SURF_AREA==0] <- NA        
  ### 
  fun <- function(x) all(is.na(x))
  na <- apply(site, 2, fun)
  if(any(na))  cat('\n The following columns contain no data and are omitted \n')
  print(names(site)[na], quote = FALSE)
  
  site <- site[, !na]
  fun.2 <- function(x) all(x == 0 | is.na(x))
  leer <- apply(site, 2, fun.2)
  if (any(leer)) {
    cat('\n The following numeric columns contain only 0 values and are omitted \n')
    print(names(site)[leer], quote = FALSE)
  }
  site <- site[, !leer]
  fun.3 <- function(x) is.numeric(x) & any(x == 0, na.rm = TRUE)
  null <- logical()
  for (i in 1:length(site)) null[i] <- fun.3(site[, i])
  site <- site[order(site$plotNumber),]
  return(site)
}


### VegX
# path <- file.path(tv.home(), 'Data','taxatest')
# site <- vegX.site(file.path(path, 'tvexport.xml'))
# veg <- vegX.veg(file.path(path, 'tvexport.xml'))
