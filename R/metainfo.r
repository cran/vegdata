metainfo <- function (db, refl='GermanSL 1.1', tv_home, ...)
{
    if (missing(tv_home))  tv_home <- tv.home(...)
    if (db == "eco") {
        shell.exec(paste(tv_home, "/Species/", refl, "/metainfo-eco.txt", sep = ""))
    }
    else {         meta <- paste(tv_home, "/Data/", db, "/metainfo.", "txt", sep = "")
    if(.Platform$OS.type == "windows") {
        if (file.access(meta)) 
            stop("No metainfo available")
        else shell.exec(meta)
    } else   if (file.access(meta)) stop("No metainfo available")
        else file.show(meta)
  }
}
