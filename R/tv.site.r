tv.site <- function (db, tv_home, quiet = FALSE, sysPath = FALSE, iconv, common.only = TRUE, ...) 
{
ow <- options('warn')
if(quiet) { options(warn=-1) }
# if (is.list(db)) site <- tv.mysql(db, "tvhabita") else {
      if (missing(tv_home)) tv_home <- tv.home(sysPath=sysPath)
  site <- read.dbf(paste(tv_home, "Data", db[1], "tvhabita.dbf", sep = "/"))
  if (suppressWarnings(any(site < -1e+05, na.rm = TRUE))) 
    print(c("WARNING! Values less than -100,000. \n", "WARNING! tvhabita.dbf may be corrupt. \n", "WARNING! Please correct by reexporting e.g. with OpenOffice."), quote = FALSE)
         
  if(length(db)>1) for(i in 2:length(db)) {
	site.tmp <- read.dbf(paste(tv_home, 'Data', db[i],'tvhabita.dbf',sep='/'))
	if(any(site$RELEVE_NR %in% site.tmp$RELEVE_NR)) stop('Datasets are using equal releve number(s), aborting!')
	cols1 <- names(site)
	cols2 <- names(site.tmp)
	if (common.only){
		common <- intersect(cols1, cols2)
		site <- rbind(site[, common], site.tmp[, common])
	}
	else {
		all <- union(cols1, cols2)
		miss1 <- setdiff(all, cols1)
		miss2 <- setdiff(all, cols2)
		site[, miss1] <- NA
		site.tmp[, miss2] <- NA
		site <- rbind(site, site.tmp)
	} 
  }

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

  ### Conversion of factors
  fac <- sapply(site, is.factor)
  if (!missing(iconv)) 
      if (!is.null(iconv)) 
	  for (i in (1:ncol(site))[fac & !(1:ncol(site)) %in% c(3,6,9)]) site[,i] <- type.convert(iconv(as.character(site[,i]), iconv[1], iconv[2]))
  if (missing(iconv)) 
      for (i in c(1, 2, 4, 5, 7, 8, 10:ncol(site))) site[,i] <- type.convert(as.character(site[, i]),...)
  site$SURF_AREA[site$SURF_AREA==0] <- NA        
### 
  fun <- function(x) all(is.na(x))
  na <- apply(site, 2, fun)
  if (!quiet) {
      cat('\n The following columns contain no data and are omitted \n')
      print(names(site)[na], quote = FALSE)
  }
  site <- site[, !na]
  fun.2 <- function(x) all(x == 0 | is.na(x))
  leer <- apply(site, 2, fun.2)
  if (!quiet) 
      if (any(leer)) {
	  cat('\n The following numeric columns contain only 0 values and are omitted \n')
	  print(names(site)[leer], quote = FALSE)
      }
  site <- site[, !leer]
  fun.3 <- function(x) is.numeric(x) & any(x == 0, na.rm = TRUE)
  null <- logical()
  for (i in 1:length(site)) null[i] <- fun.3(site[, i])
  if (!quiet) 
    if (any(null)) {
      cat(paste('\n', "The following numeric fields contain 0 values:", '\n'))
    print(names(site)[null], quote = FALSE)
    cat('\n Please check if these are really measured as 0 values or if they are not measured \n and wrongly assigned because of Dbase restrictions. \n')
    cat(" If so, use something like: \n site$Column_name[site$Column_name==0] <- NA \n summary(site[,c('", paste(names(site)[null], 
	  collapse = "','"), "')]) \n", sep = "")       
          }
options(ow)
    site <- site[order(site$RELEVE_NR),]
    site
}

