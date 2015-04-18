.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is vegdata ",
    utils::packageDescription("vegdata", field="Version"),
    appendLF = TRUE)
}
