tv.home <- function(check = FALSE) {
  if(is.null(getOption('tv_home')) | check) {
    if(.Platform$OS.type == "unix") {
     if('Turbowin' %in% list.dirs(path=paste(Sys.getenv('HOME'),'/.wine/drive_c', sep=''), full.names=FALSE, recursive = FALSE))
        tv_home <- file.path(Sys.getenv('HOME'),'.wine/drive_c/Turbowin') else {
          if('GermanSL' %in% list.dirs(file.path(path.package('vegdata'), 'tvdata'))) 
            tv_home <- file.path(path.package('vegdata'), 'tvdata') else {
              message('\nNo Turbowin installation path found. \n')  
            if(interactive()) {
              ANSWER <- readline("Should I use \n 1) a temporary directory,  or \n 2) the vedata package path (recommended, if you want to use the reflist repeatedly)? ")
              tv_home <- switch(substr(ANSWER, 1, 1),
                      "1" = tempdir(),	     
                      "2" = file.path(path.package('vegdata'), 'tvdata'),
                      tempdir())
            } else tv_home <- tempdir()
          }
      }
    }
    if(.Platform$OS.type == "windows") {
    	if(file.access('C:/Turbowin/Popup/tvscale.dbf')==0) tv_home <- 'C:/Turbowin' else 
    	  if(file.access('C:/Programs/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programs/Turbowin' else
    	    if(file.access('C:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'C:/Programme/Turbowin' else
    	      if(file.access('D:/Programme/Turbowin/Popup/tvscale.dbf') ==0) tv_home <- 'D:/Programme/Turbowin' else {
    	        message('\nNo Turbowin installation path found. \n')
    	        if(interactive()) {
    	          ANSWER <- readline("Should I use \n 1) a temporary directory,  or \n 2) the vedata package path (recommended, if you want to use the reflist repeatedly)? ")
    	          tv_home <- switch(substr(ANSWER, 1, 1),
    	                            "1" = tempdir(),	     
    	                            "2" = file.path(path.package('vegdata'), 'tvdata'),
    	                            tempdir())
    	        } else tv_home <- tempdir()
    	     }
    }
    options(tv_home = tv_home)
    message('############################################################',
            '\nTurboveg root directory is set to "', getOption('tv_home'), '"',
            '\nIf you want to change this use: options(tv_home=\"<path_to_your_Turbowin_root>\")',
            '\n############################################################')
   }
   if(getOption('tv_home') == path.package('vegdata'))
    if(!file.exists(file.path(getOption('tv_home'), 'tvdata', 'Popup', 'tvscale.dbf')))
     for(d in c('Popup', 'Data', 'Species')) {
     dir.create(file.path(getOption('tv_home'), d), showWarnings = FALSE)
     if(d == 'Data') {
       wd <- getwd()
       setwd(file.path(path.package('vegdata'), 'tvdata', 'Data')  )
       dbs <- list.dirs('.', recursive=TRUE, full.names=FALSE)
       for(l in 2:length(dbs)) {
         dir.create(file.path(getOption('tv_home'), 'Data', dbs[l]), showWarnings = FALSE)
       file.copy(from =  list.files(dbs[l], recursive=TRUE, full.names=TRUE, include.dirs=TRUE), to = file.path(getOption('tv_home'), 'Data', dbs[l]))
       }
       setwd(wd)
     } else
     file.copy(from =  list.files(file.path(path.package('vegdata'), 'tvdata', d), recursive=TRUE, full.names=TRUE, include.dirs=TRUE), to = file.path(getOption('tv_home'), d))
    }
  invisible(getOption('tv_home'))
}


