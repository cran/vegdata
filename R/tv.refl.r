tv.refl <- function(db, tv_home, refl='Germansl 1.1', sysPath = FALSE) {

  capwords <- function(s, strict = FALSE) {
      cap <- function(s) paste(toupper(substring(s,1,1)), {s <- substring(s,2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
  fun <- function(tmprefl) {
    try(refl <- match.arg(tmprefl,list.files(paste(tv_home,'Species',sep='/')) ), silent=TRUE)
    if(exists('refl')) refl
  }

  if(missing(tv_home)) tv_home <- tv.home(sysPath)
  if(!missing(db)) {
      dbattr <- paste(tv_home, 'Data', db,'tvwin.set',sep='/')
      if(file.access(dbattr)==0) refl <-  sub('A\002', '', readBin(dbattr,what='character', n=3)[3]) else 
    stop('Database attribute file tvwin.set from database "', db, '" not available. Please specify name of taxonomic reference list!') 
  } else  refl <- fun(refl)
  if(!exists(refl)) refl <- fun(gsub(' ','',refl))
  refl
 }
