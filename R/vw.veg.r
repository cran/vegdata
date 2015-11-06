vw.survey <- function(searchstring, ...) {
  surveys <- fromJSON('http://botanik4.botanik.uni-greifswald.de/floradb-rs/service/v1/surveys?portalId=3')$survey
  message('Projekt ID: ', unique(surveys[grep(searchstring, surveys$title),'id']))
  message('Kustode: ', unique(paste(surveys[grepl(searchstring, surveys$title),'owner']$firstName, surveys[grepl(searchstring, surveys$title),'owner']$lastName)))
  invisible(surveys[grep(searchstring, surveys$title),])
}


vw.veg <- function(user, password, survey, basket, taxeval = TRUE, ...) {
  if(is.character(survey)) {
    survey <- vw.survey(survey)
    if(nrow(survey) > 1) {print(survey); stop('More than one survey found, please restrict.')}
  }
  if(!missing(basket)) stop('Webservices for vegetation plot baskets are not yet implemented in vegetweb.de')
  r <- GET(paste('http://botanik4.botanik.uni-greifswald.de/floradb-rs/service/v1/snapshots', survey, '', sep='/'),  authenticate(user, password), add_headers("Accept : application/json")) 
  stop_for_status(r)
  data <- content(r, "parsed", "application/json")
  nbplots <- length(unique(sapply(data$data, '[[', 'sampleUUID')))
  message('Number of plots: ', nbplots)
  obs <- data.frame(RELEVE_NR = sapply(data$data, '[[', 'sampleUUID'), TaxonUsageID = sapply(data$data, '[[', 'germanSLNo'), COVER_PERC = sapply(data$data, '[[', 'coverage'), LAYER = sapply(data$data, '[[', 'layer'))
  if(taxeval)  obs <- taxval(obs, refl = 'GermanSL 1.2', check.critical = FALSE)
  lc = c("layer"); values = "COVER_PERC"; dec=1
  collab <- as.vector(obs$TaxonUsageID)
  rowlab <- as.vector(obs$RELEVE_NR)
  cat('combining occurrences using type', toupper(lc), 'and creating vegetation matrix ... \n')
  layerfun <- function(x) round((1 - prod(1 - x/100)) * 100, dec)
  results <- tapply(as.numeric(obs[, values]), list(rowlab, collab), layerfun)
  results[is.na(results)] <- 0
  veg <- as.data.frame(results)
  class(veg) <- c("veg", "data.frame")
  attr(veg, 'taxreflist') <- 'GermanSL 1.3'
  return(veg)
}

# user='jansen@uni-greifswald.de'
# password = 'freieDaten'
# survey=13529
# veg <- vw.veg(survey, user, password)