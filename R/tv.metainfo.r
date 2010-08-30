tv.metainfo <- function (db, refl='GermanSL 1.1', tv_home, filename = 'metainfo.txt', ...)
{
    if (missing(tv_home)) tv_home <- tv.home(...)
    if (db[1] == "eco") {
	if(missing(refl)) if(length(db)>1) stop('More than one database name specified!') else refl <- tv.refl(db, tv_home, ...)
	shell.exec(file.path(tv_home, "Species", refl, "metainfo-eco.txt"))
      } else {
	for(i in 1:length(db)) {
	  meta <- file.path(tv_home, "Data", db[i], filename)
	  if (file.access(meta)) stop('No metainfo file "',filename, '" available in directory "', db[1], '".') else 
	  if(.Platform$OS.type == "windows") shell.exec(meta)  else file.show(meta)
  }}
}
