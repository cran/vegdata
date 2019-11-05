tv.eco <- function (...) stop('This function is deprecated, use tv.traits instead.')

tv.traits <- function (db, trait.db = 'ecodbase.dbf', refl, ...) {
    tv_home <- tv.home()
    if(missing(refl))  refl <- if(missing(db)) tv.refl() else tv.refl(db = db)
    if(tools::file_ext(trait.db) != 'dbf') stop('Only implemented for dBas file format.')
    ecodb <- read.dbf(file.path(tv_home, 'Species', refl, trait.db), as.is = TRUE)
    names(ecodb) <- TCS.replace(names(ecodb))
    empty <- function(x) all(is.na(x) | x == 0)
    na <- apply(ecodb, 2, empty)
    if(any(na)) {
#       if(!quiet) {
#         cat("\n The following columns contain no data and are omitted: \n")
#         cat(names(ecodb)[na])}
        ecodb <- ecodb[, !na]
                }
#    if(!quiet) message("Changing character fields into logical, integer or numericals if appropriate.")
# ecoDB <- apply(ecodb, 2, function(x) type.convert(as.character(x))) # doesnt work 
    ecoDB <- ecodb
#    for(i in 1:ncol(ecodb)) if(is.factor(ecodb[,i])) {
#       ecoDB[,i] <- type.convert(ecoDB[,i]) }
#     for(i in 1:ncol(ecoDB))  if(class(ecodb[,i]) != class(ecoDB[,i]))
#       ecoDB$ABBREVIAT <- as.character(ecoDB$ABBREVIAT)
#       ecoDB$LETTERCODE <- as.character(ecoDB$LETTERCODE)
#       if(!quiet) message('Data type of ', names(ecoDB)[i], ' changed to ', class(ecoDB[,i]))    
    return(ecoDB)
}


