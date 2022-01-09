#' Download taxonomic databases
#'
#' @export
#' @name db_download
#' @param version (character) desired version number of the list
#' @param verbose (logical) Print messages. Default: `TRUE`
#' @param overwrite (logical) If `TRUE` force an update by overwriting
#' previously downloaded data. Default: `FALSE`
#' @return (character) path to the downloaded SQL database
#' @details Downloads sql database, cleans up unneeded files, returns path
#' to sql file
#' @seealso [tdb_cache]
#' @examples \dontrun{
#' # EuroSL
#' # db_download_eurosl()
#' # src_eurosl()
#'
#' # GermanSL
#' # db_download_germansl()
#' # db_download_germansl(overwrite=TRUE) # overwrite - download again
#' # src_germansl()
#'
#' }

#' @export
#' @rdname db_download
db_download_eurosl <- function(version = 'latest', verbose = TRUE, overwrite = FALSE) {
  # set paths
    db_url <- paste('https://euromed.infinitenature.org/EuroSL', version, 'EuroSL.zip', sep='/')
    # https://euromed.infinitenature.org/EuroSL/latest/EuroSL.sqlite
    db_path <- file.path(tdb_cache$cache_path_get(), 'euroslSqlite.zip')
    db_path_file <- file.path(tdb_cache$cache_path_get(), 'euroslSqlite')
    final_file <- file.path(tdb_cache$cache_path_get(), 'eurosl.sqlite')

    assert(verbose, "logical")
    assert(overwrite, "logical")
    if (file.exists(final_file) && !overwrite) {
      mssg(verbose, "Database already exists, returning old file")
      return(final_file)
    }
    unlink(final_file, force = TRUE)

    # make home dir if not already present
    tdb_cache$mkdir()
    # download data
    mssg(verbose, 'downloading...')
    curl::curl_download(db_url, db_path, quiet = TRUE)
    # unzip
    mssg(verbose, 'unzipping...')
    utils::unzip(db_path, exdir = db_path_file)
    # get file path
    dirs <- list.dirs(db_path_file, full.names = TRUE)
    dir_date <- dirs[ dirs != db_path_file ]
    sql_path <- list.files(dir_date, pattern = ".sqlite", full.names = TRUE)
    # move database
    file.rename(sql_path, final_file)
    # cleanup
    mssg(verbose, 'cleaning up...')
    unlink(db_path)
    unlink(db_path_file, recursive = TRUE)
    # return path
    return(final_file)
  }


#' @export
#' @rdname db_download
db_download_germansl <- function(version = 'latest', verbose = TRUE, overwrite = FALSE) {
  # paths
  db_url <- paste('https://germansl.infinitenature.org/GermanSL', version, 'GermanSL.zip', sep='/')
  db_path <- file.path(tdb_cache$cache_path_get(), 'germanslSqlite.zip')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'germanslSqlite')
  final_file <- file.path(tdb_cache$cache_path_get(), 'germansl.sqlite')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(final_file) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }
  unlink(final_file, force = TRUE)

  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = db_path_file)
  # get file path
  dirs <- list.dirs(db_path_file, full.names = TRUE)
  dir_date <- dirs[ dirs != db_path_file ]
  sql_path <- list.files(dir_date, pattern = ".sqlite", full.names = TRUE)
  # move database
  file.rename(sql_path, final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # return path
  return(final_file)
}
