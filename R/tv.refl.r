tv.refl <- function(db, refl, tv_home) {

#   capwords <- function(s, strict = FALSE) {
#       cap <- function(s) paste(toupper(substring(s,1,1)), {s <- substring(s,2); if(strict) toupper(s) else s}, sep = "", collapse = " " )
#       sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
#   }
  fun <- function(tmprefl) {
    try(refl <- match.arg(tmprefl,list.files(file.path(tv_home,'Species')) ), silent=TRUE)
    if(exists('refl')) refl
  }

  if(missing(tv_home)) tv_home <- tv.home()
  if(!missing(db)) {
      dbattr <- file.path(tv_home, 'Data', db,'tvwin.set')
      if(file.access(dbattr)==0) refl <-  sub('A\002', '', readBin(dbattr,what='character', n=3)[3]) else 
    stop('Database attribute file tvwin.set from database "', db, '" not available. Please specify name of taxonomic reference list!') 
  } else  refl <- 'GermanSL 1.2'
  if(!exists(refl)) refl <- fun(gsub(' ','',refl))
  if(tolower(substr(refl, 1,8)) == 'germansl') refl <- paste('GermanSL', substring(refl,9,nchar(refl)), sep='')
  return(refl)
 }
