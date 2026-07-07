#' Internal vegdata functions
#' @name vegdata-internal
#' @aliases reShape.veg bin2word word2bin
#' @noRd
#' @description  Internal vegdata functions.
#' @details These are not intended to be called directly by the user.
#'  tv.home tries to guess the default tv_home directory (\code{'C:\Turbowin'} or \code{'C:\Programme\Turbowin'} or \code{'O:\Turbowin'} on Windows systems and \code{'~/.wine/drive_c/Turbowin'} on Unix systems.
#' @keywords internal


# #'  As dBase is an old DOS format, Umlaute have been stored in Turboveg using the CP437 code table. Change options('tv.iconv') if you run into problems
# gracefully_fail <- function(remote_file) {
#   try_GET <- function(x, ...) {
#     tryCatch(
#       GET(url = x, timeout(1), ...),
#       error = function(e) conditionMessage(e),
#       warning = function(w) conditionMessage(w)
#     )
#   }
#   is_response <- function(x) {
#     class(x) == "response"
#   }
#   # First check internet connection
#   if (!curl::has_internet()) {
#     message("No internet connection.")
#     return(invisible(NULL))
#   }
#   # Then try for timeout problems
#   resp <- try_GET(remote_file)
#   if (!is_response(resp)) {
#     message(resp)
#     return(invisible(NULL))
#   }
#   # Then stop if status > 400
#   if (httr::http_error(resp)) {
#     message_for_status(resp)
#     return(invisible(NULL))
#   }
#
#   # # If you are using rvest as I do you can easily read_html in the response
#   # xml2::read_html(resp)
# }

# remote <- "https://germansl.infinitenature.org/GermanSL/1.5/GermanSL.zip"
# remote2 <- "https://german.infinitenature.org/GermanSL/1.5/GermanSL.zip"
# remote3 <- "https://germansl.infinitenature.org/GermanSL/1.6/GermanSL.zip"
#
# # gracefully_fail(remote) #OK
#
# status <- tryCatch(
#   RCurl::getURL(url, ssl.verifypeer=FALSE, useragent="R"),
#   error = function(e) e
# )
# # inherits(status,  "error")
#
#
# f <- function(url) {
#   if (!curl::has_internet()) {
#     message("No internet connection")
#     return(NULL)
#   }
#   if (httr::http_error(url)) {
#     message("Data source broken.")
#     return(NULL)
#   }
#   url(remote2)
#   if(as.integer(tmp) == 5)
#   tryCatch(http_error(GET(url)),
#            http_404 = function(c) "That url doesn't exist",
#            http_403 = function(c) "You need to authenticate!",
#            http_400 = function(c) "You made a mistake!",
#            http_500 = function(c) "The server screwed up"
#   )
# }

# f(remote2)

asc <- function(char) sapply(char, function(x) strtoi(charToRaw(x), 16L), simplify = TRUE, USE.NAMES = FALSE)
chr <- function(ascii) sapply(ascii, function(x) rawToChar(as.raw(x)), USE.NAMES = FALSE)

bin2word <- function(x) {
  c1 <- substr(x, 1,1)
  c2 <- substr(x, 2,2)
  return(255 * (asc(c1) -1) + asc(c2) - 1)
}
word2bin <- function(x) {
  c1 <- floor(x/255+1)
  c2 <- x - (c1-1)*255
  paste (chr(c1), chr(c2 + 1), sep='')
}

#' @export
"[.veg" <- function(x, s,...) {
  taxref <- attr(x, 'taxreflist')
  out <- NextMethod()
  class(out) <- c('veg', 'data.frame')
  attr(out, 'taxreflist') <- taxref
  return(out)
}
