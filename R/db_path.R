#' database path
#' @export
#' @param db (character) db name. one of: eurosl, germansl
db_path <- function(db) {
  file <- switch(
    db,
    eurosl = "eurosl.sqlite",
    germansl = "germansl.sqlite",
    stop("must be one of eurosl, germansl",
      call. = FALSE)
  )
  file.path(tdb_cache$cache_path_get(), file)
}
