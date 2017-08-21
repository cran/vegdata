vw.survey <- function(searchstring, server, ...) {
  #  require(httr)
  # require(jsonlite)
  if(missing(server)) server <- 'botanik3.botanik.uni-greifswald.de'
  surveys <- fromJSON(paste('http://', server, '/floradb-rs/service/v1/surveys', sep=''))$survey
  if(is.numeric(searchstring)) {
    df <- data.frame(Projekt_ID = surveys[searchstring == surveys$id,'id'], Projekttitel = surveys[searchstring ==  surveys$id,'title'], Container = surveys[surveys$id == searchstring,'container'], Anzahl_Plots = surveys[surveys$id == searchstring,'sampleCount'])
    # Kustoden = paste(surveys$owner[searchstring == surveys$title, ], surveys[searchstring == surveys$title,'owner']$lastName)
  } else {
    df <- data.frame(Projekt_ID = surveys[grep(searchstring, surveys$title),'id'], Projekttitel = surveys[grep(searchstring, surveys$title),'title'], Container = surveys[grep(searchstring, surveys$title),'container'], Anzahl_Plots = surveys[grep(searchstring, surveys$title),'sampleCount'] )
    # Kustoden = paste(surveys$owner[surveys$owner$id == searchstring, ], surveys[grepl(searchstring, surveys$title),'owner']$lastName), 
  }
  df <- df[grep('JUNIT-TESTS', df$Projekttitel, invert = TRUE),]
  return(df)
}

vw.basket <- function(user, password, server, ...) {
  #  require(httr)
  # require(jsonlite)
  if(missing(server)) server <- 'botanik3.botanik.uni-greifswald.de'
    r <- GET(paste('http:/', server, 'floradb-rs/service/v1/shoppingCarts/mine', sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
    httr::stop_for_status(x=r, 'access the dataset.')
  baskets <- content(r, "parsed", "application/json")$shoppingCart
  out <- data.frame(Title = sapply(baskets, function(x) x$title), Status = sapply(baskets, function(x) x$status), NbPlots = sapply(baskets, function(x) x$numberOfPlots), ID = sapply(baskets, function(x) x$id))
  return(out)
}

################################

vw.site <- function(user, password, id, server, ...) {
# require(httr)
  if(missing(server)) server <- 'botanik3.botanik.uni-greifswald.de'
  if(is.character(id)) {
    if(nchar(id) >= 36 & nchar(id) <= 38) {
      cat('Accessing ', paste('http:/', server, 'floradb-rs/service/v1/shoppingCarts', id, 'snapshot', sep='/'), '\n')
    r <- GET(paste('http:/', server, 'floradb-rs/service/v1/shoppingCarts', id[1], 'snapshot', sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
    
  } else 
    if(nchar(id) >= 36 & nchar(id) <= 38) {
    cat('Accessing ', paste('http:/', server, 'floradb-rs/service/v1/snapshots', id[1], sep='/'), '\n')
    r <- GET(paste('http:/', server, 'floradb-rs/service/v1/snapshots', id[1], sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
  }} else if(is.numeric(id)) {
    cat('Accessing ', paste('http:/', server, 'floradb-rs/service/v1/snapshots', id[1], '?occurrenceAttribute=COVERAGE_MEAN', sep='/'), '\n')
    r <- GET(paste('http:/', server, 'floradb-rs/service/v1/snapshots', id[1], '?occurrenceAttribute=COVERAGE_MEAN', sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
  } else stop('id must be a character or numeric')
  httr::warn_for_status(x = r)
  if(r$status_code != 500) {
  data <- content(r, "parsed", "application/json")
  # saveRDS(data, file = 'data.RDS')
  nbplots <- length(unique(sapply(data$data, '[[', 'sampleUUID')))
  message('Number of plots: ', nbplots)
  if(nbplots > 0) {
    # Custom attributes
   if('customAttribute' %in% names(data$header[[1]])) {
     fields <- data$header[[1]]$customAttribute
     attributnames <- gsub('udf_', '', sapply(fields, `[[`, 1))
     message('Custom attributes: ', paste(attributnames, collapse = ', '))
     # attributes <- lapply(fields, `[[`, which(names(data$header[[1]]) == 'customAttribute') )
     custom <- vector(mode = 'list', length = length(data$header))
     cust <- setNames(data.frame(matrix(ncol = length(attributnames), nrow = 0)), attributnames)
     # data <- readRDS('data.RDS')
     for(i in 1:length(data$header)) {
       custom[[i]] <- data$header[[i]][which(names(data$header[[i]]) %in% c("customAttribute"))][[1]]
       values <- sapply(custom[[i]], '[[', 2)
        cust[i,] <- sapply(custom[[i]], '[[', 2)
       data$header[[i]][which(names(data$header[[i]]) %in% c("customAttribute"))] <- NULL
     }
   }
  site <- do.call(rbind.data.frame, data$header)
  site <- cbind.data.frame(site, cust)
  #df <- data.frame(matrix(unlist(data$header), nrow=nbplots, byrow=T), stringsAsFactors = FALSE)
  #df <- data.frame(t(sapply(data$header, c)))
  # Type conversion
  site$startDate <- as.Date(site$startDate)
  site$endDate <- as.Date(site$endDate)
  site$sampleUUID <- as.character(site$sampleUUID)
  return(site)
  }
 } else warning('Failed to access the dataset.')
}


vw.veg <- function(user, password, id, taxeval = TRUE, server, refl = 'GermanSL 1.2', ...) {
  if(missing(refl)) refl <- tv.refl()
#  require(httr)
# require(jsonlite)
  if(missing(server)) server <- 'botanik3.botanik.uni-greifswald.de'
  # message('This is a test implementation. Passwords will be send through http, i.e. unsecured.')
  if(is.character(id))  {
    if(nchar(id) < 36 | nchar(id) > 38) {
      surveyid <- vw.survey(id, server = server)
      if(nrow(surveyid) > 1) {stop("More than one survey found, please restrict.")}
#      id <- surveyid$Projekt_ID
      cat('Accessing ', paste('http:/', server, 'floradb-rs/service/v1/snapshots', surveyid, sep='/'), '\n')
      r <- GET(paste('http:/', server, 'floradb-rs/service/v1/snapshots', surveyid, sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
  } else
    if(nchar(id) >= 36 & nchar(id) <= 38) {
    cat('Accessing ', paste('http:/', server, 'floradb-rs/service/v1/shoppingCarts', id, 'snapshot', sep='/'), '\n')
    r <- GET(paste('http:/', server, 'floradb-rs/service/v1/shoppingCarts', id[1], 'snapshot', sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
  }} else {
    if(is.numeric(id)) {
  cat('Accessing ', paste('http:/', server, 'floradb-rs/service/v1/snapshots', id, sep='/'), '\n')
    r <- GET(paste('http:/', server, 'floradb-rs/service/v1/snapshots', id, sep='/'),  authenticate(user, password), add_headers("Accept : application/json"))
    } else stop('id must be a character or numeric')
  }
  httr::warn_for_status(x=r)
  if(r$status_code != 500) {
  data <- content(r, "parsed", "application/json")
  nbplots <- length(unique(sapply(data$data, '[[', 'sampleUUID')))
  message('Number of plots: ', nbplots)
  
  obs <- data.frame(RELEVE_NR = sapply(data$data, '[[', 'sampleUUID'), TaxonUsageID = sapply(data$data, '[[', 'germanSLNo'), COVER_PERC = sapply(data$data, '[[', 'value'), LAYER = sapply(data$data, '[[', 'layer'), stringsAsFactors = FALSE)
  if(taxeval)  
    obs <- taxval(obs, refl = refl, check.critical = FALSE, ...)
  lc = c("layer"); values = "COVER_PERC"; dec=1
  collab <- as.vector(obs$TaxonUsageID)
  rowlab <- as.vector(obs$RELEVE_NR)
  cat('combining occurrences using type', toupper(lc), 'and creating vegetation matrix ... \n')
  layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
  results <- tapply(as.numeric(obs[, values]), list(rowlab, collab), layerfun)
  results[is.na(results)] <- 0
  veg <- as.data.frame(results)
  class(veg) <- c("veg", "data.frame")
  attr(veg, 'taxreflist') <- refl
  return(veg)
  } else message('Failed to access the dataset.')
}

