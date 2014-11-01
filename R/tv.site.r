tv.site <- function (db, tv_home, drop = TRUE, common.only = TRUE, iconv="ISO-8859-1", ...) 
{
ow <- options('warn')
# if(quiet) { options(warn=-1) }
# if (is.list(db)) site <- tv.mysql(db, "tvhabita") else {
  if (missing(tv_home)) tv_home <- tv.home()
  site <- read.dbf(file.path(tv_home, "Data", db[1], "tvhabita.dbf"), as.is=TRUE)
  if (suppressWarnings(any(site < -1e+05, na.rm = TRUE))) 
    print(c("WARNING! Values less than -100,000. \n", "WARNING! tvhabita.dbf may be corrupt. \n", "WARNING! Please correct by im- / exporting e.g. with OpenOffice."), quote = FALSE)
  if(length(db)>1) for(i in 2:length(db)) {
	site.tmp <- read.dbf(file.path(tv_home, 'Data', db[i],'tvhabita.dbf'))
	if(any(site$RELEVE_NR %in% site.tmp$RELEVE_NR)) stop('Found duplicate releve numbers, aborting!')
	cols1 <- names(site)
	cols2 <- names(site.tmp)
	if (common.only){
		common <- intersect(cols1, cols2)
		site <- rbind(site[, common], site.tmp[, common])
	} else {
 		All <- union(cols1, cols2)
		miss1 <- setdiff(All, cols1)
		miss2 <- setdiff(All, cols2)
		site[, miss1] <- NA
		site.tmp[, miss2] <- NA
		site <- rbind(site, site.tmp)
	} 
  }

### Conversion of factors
# fac <- sapply(site, is.factor)
    for(i in names(site)) if(is.character(site[,i])) site[,i] <- iconv(site[,i], iconv, '')

    ### Time
    if(any(is.na(site$DATE))) warning(sum(is.na(site$DATE)), ' releves without date. Not converted from factor to date format.') else {
    site$DATE <- gsub('/','',site$DATE)
#      Date <- rep('no date', nrow(site))
    index <- nchar(as.character(site$DATE))==4
   fun <- function(x) paste(x,'0101',sep='')
    site$DATE[index] <- fun(site$DATE[index])
#      Date[!index] <- as.character(site$DATE[!index])
    index <- nchar(as.character(site$DATE))==6
   fun <- function(x) paste(x,'01',sep='')
    site$DATE[index] <- fun(site$DATE[index])
    site$DATE <- as.Date(site$DATE, '%Y%m%d')
    }
  ### Survey Area
  n <- sum(site$SURF_AREA == 0 | is.na(site$SURF_AREA))
  if(n>0) warning(paste(n, ' releves without survey area'))
  site$SURF_AREA[site$SURF_AREA==0] <- NA

### 
  fun <- function(x) all(is.na(x))
  na <- apply(site, 2, fun)
  if (drop) {
      cat('\n The following columns contain no data and are omitted \n')
      print(names(site)[na], quote = FALSE)
      site <- site[, !na]
  }
  fun.2 <- function(x) all(x == 0 | is.na(x))
  leer <- apply(site, 2, fun.2)
  if (drop) 
      if (any(leer)) {
	  cat('\n The following numeric columns contain only 0 values and are omitted \n')
	  print(names(site)[leer], quote = FALSE)
	  site <- site[, !leer]
  }
  fun.3 <- function(x) is.numeric(x) & any(x == 0, na.rm = TRUE)
  null <- logical()
  for (i in 1:length(site)) null[i] <- fun.3(site[, i])

    if (any(null)) {
      cat(paste('\n', "The following numeric fields contain 0 values:", '\n'))
    print(names(site)[null], quote = FALSE)
    cat(' Please check if these are really measured as 0 values or if they are not measured \n and wrongly assigned because of Dbase restrictions. \n')
    cat(" If so, use something like: \n site$Column_name[site$Column_name==0] <- NA \n summary(site[,c('", paste(names(site)[null], 
	  collapse = "','"), "')]) \n", sep = "")       
          }
  site <- site[order(site$RELEVE_NR),]
  site
}

