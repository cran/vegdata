#'
#'
.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is vegdata ",
    utils::packageDescription("vegdata", field="Version"), paste(' - build: '),
    utils::packageDate('vegdata'),
    appendLF = TRUE)
    if(is.null(getOption('tv.iconv'))) options(tv.iconv = 'ISO-8859-15')
}

mssg <- function(v, ...) if (v) message(...)

assert <- function(x, y) {
    if (!is.null(x)) {
        if (!inherits(x, y)) {
            stop(deparse(substitute(x)), " must be of class ",
                 paste0(y, collapse = ", "), call. = FALSE)
        }
    }
}
