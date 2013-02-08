ESveg.veg <- function (db, refl, ...) {
  enc <- "ISO-8859-1"
  require(XML)
#  data(lc.1)
  doc <- xmlParse(db)
 # names(xmlChildren(doc))
 # table(names(xmlChildren(xmlRoot(doc))))
  taxrefnode <- getNodeSet(doc, "//reference")
  taxref <- xmlSApply(taxrefnode[[1]], xmlValue)['referenceCode']
  cat('Taxonomic reference list used:', taxref, '\n')
  userdefined <- xmlToDataFrame(getNodeSet(doc, "//userdefined"))
  ind <- userdefined$userDefinedTableName == "cover"
  if(any(ind)) cat('User defined plot-species attributes not used:', as.character(userdefined$userDefinedName[ind]), '\n') 
  taxa <- xmlToDataFrame(getNodeSet(doc, "//taxon"))
#  head(taxa)
  cover <- xmlToDataFrame(getNodeSet(doc, "//cover"))
#  head(cover)
  coverindex <- xmlToDataFrame(getNodeSet(doc, "//coverindex"))
#  head(coverindex)
  samples <- xmlToDataFrame(getNodeSet(doc, "//observation"))
#  head(samples) # "observation" should be called "samples"
  cover$coverMethod <- samples$coverMethod[match(cover$observationCode, samples$observationCode)]
#  head(cover)
    cover$coverPercent <- NA
    covM <- levels(coverindex$coverMethod)
    for(i in unique(covM)) {
      index <- coverindex$coverMethod == i
      coindex <- cover$coverMethod == i  
    cover[coindex,'coverPercent'] <- 
      as.numeric(as.character(coverindex$coverPercent[match(cover[coindex,'taxonCover'] , coverindex$coverCode[index])]))
    }
#  head(cover)
#  cover$coverPercent
  
  ## vegmatrix
  cover$stratumCode[cover$stratumCode == ''] <- '0'
#  cover$COMB <- pseudo[[1]][, 2][match(obs[, pseudo[[2]]], pseudo[[1]][,1])]
#  collab <- paste(obs$TaxonUsageID, obs$COMB, sep = ".")
  collab <- paste(cover$taxonCode, cover$stratumCode, sep = ".")
  rowlab <- as.vector(cover$observationCode)
  
  dec = 0
  layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
  lc <- 'layer'; values <- 'coverPercent'
  results <- switch(lc,  # Inflate matrix
                    layer = tapply(cover[, values], list(rowlab, collab), layerfun),
                    mean  = tapply(cover[, values], list(rowlab, collab), mean), 
                    max   = tapply(cover[, values], list(rowlab, collab), max), 
                    sum   = tapply(cover[, values], list(rowlab, collab), sum),
                    first = tapply(cover[, values], list(rowlab, collab), first.word) )
  results[is.na(results)] <- 0
  
  st <- unlist(strsplit(colnames(results), ".", fixed = TRUE))
  colspc <- st[seq(1, length(st), 2)]
  # taxa <- tax(as.numeric(colspc), verbose=FALSE)
  ll <- st[seq(2, length(st), 2)]
  coln <- make.names(taxa$interpretationName[match(as.numeric(colspc), taxa$taxonCode)])
  cn <- replace(coln, which(ll != 0), paste(coln, ll, sep = ".")[ll != 0])
  colnames(results) <- cn
  # colnames(results) <- species$LETTERCODE[match(colnames(results), species$TaxonUsageID)]
  #(veg <- results)
  out <- as.data.frame(results)
  class(out) <- c('veg', 'data.frame')
  attr(out, 'taxreflist') <- as.character(unique(taxa$interpretationReference))
  return(out)
}




# path <- file.path(tv.home(), 'Data','taxatest')
# veg <- vegX.veg(file.path(path, 'tvexport.xml'))

### plot attributes
# plot - sample - observation
# samples <- xmlToDataFrame(getNodeSet(doc, "//observation"))
# head(samples) # "observation" should better be called "samples"
# userdefined <- xmlToDataFrame(getNodeSet(doc, "//userdefined"))
# cat('User defined plot attributes:', as.character(userdefined$userDefinedName[userdefined$userDefinedTableName == "observation"]))
# names(samples)[6:ncol(samples)] <- as.character(userdefined$userDefinedName[userdefined$userDefinedTableName == "observation"])
# 
# plotNodes <- getNodeSet(doc, "//plot")
# plot <- xmlToDataFrame(getNodeSet(doc, "//plot"))
# if(nrow(plot) != nrow(samples)) cat('Multiple observations of plots. This function handles observations. Site conditiins will be recycled.')
# sampleobs <- cbind(plot[match(as.character(samples$plotCode), plot$plotCode),], samples)
# (site <- sampleobs)

ESveg.site <- function (db, ...) {
  require(XML)
  #  require(vegdata)
  # path <- file.path(tv.home(), 'Data','taxatest')
  # db <- file.path(path, 'tvexport.xml')
  doc <- xmlParse(db)
  
  ### plot attributes
  # plot - sample - observation
  samples <- xmlToDataFrame(getNodeSet(doc, "//observation"))
  samples <- samples[, !1:ncol(samples) %in% grep('Name', names(samples))]
  #  head(samples) # "observation" should better be called "samples"
  userdefined <- xmlToDataFrame(getNodeSet(doc, "//userdefined"))
  ind <- userdefined$userDefinedTableName == "observation"
  if(any(ind))  {
    plotAttr <- as.character(userdefined$userDefinedName[ind])
    cat('User defined plot attributes:', plotAttr)
    names(samples)[7:ncol(samples)] <- plotAttr
  # names(samples)[6:ncol(samples)] <- as.character(userdefined$userDefinedName[userdefined$userDefinedTableName == "observation"])
  }
  plotNodes <- getNodeSet(doc, "//plot")
  plot <- xmlToDataFrame(getNodeSet(doc, "//plot"))
  if(nrow(plot) != nrow(samples)) cat('Multiple observations of plots. This function handles observations. Site conditions will be recycled.')
  site <- cbind(plot[match(as.character(samples$plotCode), plot$plotCode),], samples)
  
  # for(i in names(site)[lapply(site, class) == 'factor']) 
  #  site[,i] <- base:::iconv(site[,i], enc, '')
  ### Time
  if(any(site$obsEndDate == '1901-01-01')) warning(sum(site$obsEndDate == '1901-01-01'), ' releves without date. Not converted from factor to date format.') 
  else {
    #  site$obsEndDate <- gsub('/','',site$obsEndDate)
    #      Date <- rep('no date', nrow(site))
    site$obsEndDate <- as.Date(site$obsEndDate, '%Y-%m-%d')
  }
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

# path <- file.path(tv.home(), 'Data','taxatest')
# site <- vegX.site(file.path(path, 'tvexport.xml'))
