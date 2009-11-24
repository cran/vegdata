tv.metainfo <- function (db, refl='GermanSL 1.1', tv_home, filename = 'metainfo.txt', ...)
{
    if (missing(tv_home)) tv_home <- tv.home(...)
    if (db == "eco") {
        shell.exec(file.path(tv_home, "Species", refl, "metainfo-eco.txt"))
    }
    else {
      meta <- file.path(tv_home, "Data", db, filename)
        if (file.access(meta)) 
            stop('No metainfo file "',filename, '" available in directory "', db, '".')
        else 
    if(.Platform$OS.type == "windows") shell.exec(meta)  else file.show(meta)
  }
}
