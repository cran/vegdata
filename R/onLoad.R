tdb_cache <- NULL

.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set("vegdata")
  tdb_cache <<- x
}

utils::globalVariables(c(".", "multipatt", "write.dbf","gwindow", "gtree", "addHandlerDoubleclick", "svalue")) # Needed for use of . in magrittr pipelines
