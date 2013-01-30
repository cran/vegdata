tv.eco <- function (...) stop('This function is deprecated, use tv.traits instead.')
  
tv.traits <- function (db, trait.db = 'ecodbase.dbf', refl, quiet = FALSE, ...) {
    tv_home <- tv.home()
    if(missing(refl))  refl <- if(missing(db)) tv.refl() else tv.refl(db = db)
    ecodb <- read.dbf(file.path(tv_home, 'Species', refl, trait.db))
    empty <- function(x) all(is.na(x) | x == 0)
    na <- apply(ecodb, 2, empty)
    if(any(na)) {
      if(!quiet) {
        cat("\n The following columns contain no data and are omitted: \n")
        cat(names(ecodb)[na])}
        ecodb <- ecodb[, !na]
                }
    if(!quiet) cat("\n Changing character fields into logical, integer or numericals if appropriate: \n")
# ecoDB <- apply(ecodb, 2, function(x) type.convert(as.character(x)))
# doesnt work 
    ecoDB <- ecodb
    for(i in 1:ncol(ecodb)) if(is.factor(ecodb[,i])) {
      ecoDB[,i] <- iconv(as.character(ecodb[,i], "ISO-8859-1", ""))
      ecoDB[,i] <- type.convert(ecoDB[,i]) }
    if(!quiet) cat('\n')
    
    for(i in 1:ncol(ecoDB))  if(class(ecodb[,i]) != class(ecoDB[,i])) if(!quiet) cat('Class of', names(ecoDB)[i], 'changed to ', class(ecoDB[,i]), '\n')
#     if(rm.dupl) {
#       taxa <- load.taxlist(refl=refl, ...)
#       ecoDB$LETTERCODE <- taxa$LETTERCODE[match(ecoDB$TAXNR, taxa$TaxonUsageID)]
#       ecoDB$SYNONYM <- taxa$SYNONYM[match(ecoDB$TAXNR, taxa$TaxonUsageID)]
#       tab <- table(ecoDB$LETTERCODE)
#       ind <- ecoDB$LETTERCODE %in% names(tab)[tab>1] & ecoDB$SYNONYM
#       ecoDB <- ecoDB[!ind,]
#       rownames(ecoDB) <- ecoDB$LETTERCODE
#     }
    ecoDB
}


meanTraits <- function(trait, veg, trait.db = 'ecodbase.dbf', join = 'LETTERCODE', refl, zero.is.NA = TRUE, ...) {
  cat('Maximum performance value:', max(veg, na.rm=TRUE),'\n')
  if(missing(refl)) refl <- attr(veg, 'taxreflist')
  if(is.null(refl)) refl <- tv.refl()
  if(missing(trait.db)) {
    trait.db <- 'ecodbase.dbf'
    cat('Using trait database:', trait.db, '\n') 
    }
  eco <- tv.traits(trait.db = trait.db, refl=refl, quiet=TRUE)
  if(!trait %in% names(eco)) stop(paste('Trait name does not occur in column names of', trait.db))
  IV <- eco[,trait][match(names(veg), eco[,join])]
  names(IV) <- names(veg)
  if(zero.is.NA) IV[IV == 0] <- NA
  ind <- !is.na(IV)
  veg <- veg[,ind]; IV <- IV[ind]
  out <- rowSums(t((t(veg) * IV)) / rowSums(veg))
  return(out)
}


# veg <- matrix(c(9,0,6,0,68,0,20,4,33), ncol=3)
# IV <- c(4,6,NA)



