.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is vegdata ",
    utils::packageDescription("vegdata", field="Version"), paste(' - build: '),
#    utils::packageDate('vegdata'),
    appendLF = TRUE)
    options(tv.iconv = 'ISO-8859-15')
    # As dBase is an old DOS format, characters have been stored in Turboveg using the dbase specific CP437 code table. This has been changed and Turboveg seems to use a country specific code page now. Please change getOptions('tv.iconv') if you run into problems.
}
