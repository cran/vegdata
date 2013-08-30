tv.home <- function() {

  if(is.null(getOption('tv_home'))) {
    if(.Platform$OS.type == "unix") if(file.access(paste(Sys.getenv('HOME'),'/.wine/drive_c/Turbowin/Popup/tvscale.dbf', sep='')) == 0)   tv_home <- file.path(Sys.getenv('HOME'),'.wine/drive_c/Turbowin') else
      tv_home <- NA

  if(.Platform$OS.type == "windows") {
    if(file.access('O:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'O:/Turbowin' else 
    	if(file.access('C:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'C:/Turbowin' else 
    	  if(file.access('C:/Programs/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programs/Turbowin' else
    	    if(file.access('C:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
    	      if(file.access('D:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'D:/Programme/Turbowin' else
              tv_home <- NA
  }
  if(is.na(tv_home)) {
   cat('\nNo Turbowin installation path found. \n')
   ANSWER <- readline("Should I use 1) the vegdata package path, or 2) a temporary folder? ")
   tv_home <- switch(substr(ANSWER, 1, 1),
         "1" = file.path(path.package('vegdata'), 'Species'),
         tempdir()
   )
   for(d in c('Popup', 'Data', 'Species')) {
     dir.create(file.path(tv_home, d))
     if(d=='Data') {
       wd <- getwd()
       setwd(file.path(path.package('vegdata'), 'tvdata', 'Data')  )
       dbs <- list.dirs(, recursive=TRUE, full.names=FALSE)
       for(l in 2:length(dbs)) {
         dir.create(file.path(tv_home, 'Data', dbs[l]))
       file.copy(from =  list.files(dbs[l], recursive=TRUE, full.names=TRUE, include.dirs=TRUE), to = file.path(tv_home, 'Data', dbs[l]))
       }
       setwd(wd)
     } else
     file.copy(from =  list.files(file.path(path.package('vegdata'), 'tvdata', d), recursive=TRUE, full.names=TRUE, include.dirs=TRUE), to = file.path(tv_home, d))
   }
  }
  cat('\n##########################################################',
      '\nTurboveg root directory is set to', tv_home,
      '\nIf you want to change this use: options(tv_home=\"<path_to_your_Turbowin_root>\")',
      '\n##########################################################\n')
  options(tv_home=tv_home)
  } else {
  tv_home <- getOption('tv_home')
#  cat('Turboveg root directory has already been set to "', tv_home,'".\n', sep='')
  }
  invisible(tv_home)
}



