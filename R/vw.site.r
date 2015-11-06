vw.site <- function(user, password, survey, basket, ...) {
  if(!missing(basket)) stop('Webservices for vegetation plot baskets are not yet implemented in vegetweb.de')
  r <- GET(paste('http://botanik4.botanik.uni-greifswald.de/floradb-rs/service/v1/snapshots', survey[1], '', sep='/'),  authenticate(user, password), add_headers("Accept : application/json")) 
  stop_for_status(r)
  data <- content(r, "parsed", "application/json")
  nbplots <- length(unique(sapply(data$data, '[[', 'sampleUUID')))
  message('Number of plots: ', nbplots)
  message('Available information: ', paste(names(data$header[[1]]), collapse=' '))
  site <- data.frame(RELEVE_NR = sapply(data$header, '[[', 'sampleUUID'), loc = sapply(data$header, '[[', 'centroid'), Date = sapply(data$header, '[[', 'endDate'))
  
if(length(survey) > 1)
   for(i in 2:length(survey)) {
     r <- GET(paste('http://botanik4.botanik.uni-greifswald.de/floradb-rs/service/v1/snapshots', survey[1], '', sep='/'),  authenticate(user, password), add_headers("Accept : application/json")) 
     stop_for_status(r)
     data <- content(r, "parsed", "application/json")
     site.tmp <- data.frame(RELEVE_NR = sapply(data$header, '[[', 'sampleUUID'), loc = sapply(data$header, '[[', 'centroid'), Date = sapply(data$header, '[[', 'endDate'))
  	cols1 <- names(site)
  	cols2 <- names(site.tmp)
   	All <- union(cols1, cols2)
  	miss1 <- setdiff(All, cols1)
  	miss2 <- setdiff(All, cols2)
  	site[, c(as.character(miss1))] <- NA
  	site.tmp[,c(as.character(miss2))] <- NA
  	site <- rbind(site, site.tmp)
  }

  ### Survey Area
  if(!'SURF_AREA' %in% names(site)) site$SURF_AREA <- NA
  n <- sum(site$SURF_AREA == 0 | is.na(site$SURF_AREA))
  if(n>0) message(paste(n, ' releves without survey area information.'))
  return(site)
}

# user='jansen@uni-greifswald.de'
# password = 'bruni23'
# survey = 14817
# site <- vvw.site(survey, user, password)