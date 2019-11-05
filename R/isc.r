#### community weighted trait means
# isc(veg, trait.db, ivname, keyname = 'LETTERCODE', species <- tax('all', refl = refl, quiet = TRUE, 'mean'), db, ...)
# isc(veg.perc, trait.db = eco, ivname = 'OEK_F', keyname = 'TaxonName', method = 'mean')
isc <- function(veg,
                refl,
                trait.db = 'ecodbase.dbf', 
                ivname,
                keyname = 'LETTERCODE',
                method = c('mean', 'mode'),
                weight,
                db,
                ...
) {
  if(missing(refl) & missing(veg) & missing(db)) stop('Either refl, db, or a class "veg" object have to be provided.')
  if(!missing('veg')) 
    if('veg' %in% class(veg)) refl <- attr(veg, 'taxreflist')
  species <- tax('all', refl = refl, quiet = TRUE, ...)
  if(is.character(trait.db)) iv <- tv.traits(trait.db = trait.db, refl = refl, ...) else iv = trait.db
  if(missing(veg)) veg <- tv.veg(db, ...) else if(!'data.frame' %in% class(veg)) veg <- as.data.frame(veg)
  if(!all(ivname %in% names(iv))) stop('Not all ivname in table of indicators.')
  method <- match.arg(method)
  if(missing(weight)) {
    iv$weight <- 1
  #  weight <- 'weight'
  } else names(iv)[names(iv) == weight] <- 'weight'
  iv <- as.data.frame(cbind(iv[, match(ivname, names(iv))], iv[, keyname], iv[, 'weight']))
  names(iv) <- c(ivname, keyname, 'weight')
  # workaround
  for(i in 1:length(ivname)) {
    colnames(iv)[i] <- as.character(ivname[i])
    iv[,i] <- as.integer(as.character(iv[,i]))
  }
  if(!keyname %in% names(iv)) stop(paste(keyname, 'not in column names of trait dataframe.'))
  if(ivname %in% c('OEK_F', 'OEK_L', 'OEK_K', 'OEK_N', 'OEK_T') & any(iv[,ivname] == 0 & !is.na(iv[,ivname]))) print('Warning: Detecting 0 values, please check if these are really menat to be zeros.')
  if(all(is.na(match(colnames(veg), iv[, keyname])))) stop('Taxon names in trait table and names in vegetation matrix do not match, please check.') else
  v <- as.matrix(iv[match(colnames(veg), iv[, keyname]), ivname])
  rownames(v) <- colnames(veg)
  if(length(ivname) == 1) {
#    print(table(is.na(v)))
    veg <- veg[,!is.na(v)] #?
    v <- as.matrix(v[!is.na(v),]) #?
  } else  v[is.na(v)] <- 0
  # Species * indicator Matrix of available Species
  w <- as.character(iv$weight[match(names(veg), iv[, keyname])])
  w[is.na(w)] <- "1"
  w <- as.numeric(w)
  veg <- t(t(veg) * w)
  # Method == mean
  if(method == 'mean') {
    io <- matrix(0, nrow = nrow(veg), ncol = ncol(v)) # Plots * sum of WS indicators for WS
    io <- apply(v, 2, function(x) rowSums(as.matrix(veg/apply(veg, 1, sum)) %*% x, na.rm=TRUE) )
    # io <- apply(v, 2, function(x) rowSums(as.matrix(veg) %*% x, na.rm=TRUE) )
    rS <- rowSums(io, na.rm = TRUE)
    IV <- io
    names(IV) <- rownames(veg)
  }
  # Method == max
  if(method == 'mode') {
    funMode <- function(x) {
        tab <- table(v[rownames(v) %in% dimnames(veg)[[2]][x > 0],])
        return(names(tab[which.max(tab)]))
    }
  IV <- apply(veg, 1, funMode)
  }
  if(any(IV == 0) & ivname != 'OEK_S') {
    cat('The following plots seem to be without a single indicator species:\n')
    print(names(IV)[IV == 0])
  }
  return(IV)
}
### end of function

showplot <- function(veg, plotids)  for(i in 1:length(plotids)) print(veg[plotids[i], veg[plotids[i],]>0])
# showplot(veg, c("361", "362", "363"))

showindiplot <- function(veg, trait.db, plotid, weight, keyname = 'LETTERCODE') {
  if(length(plotid) > 1) {
    print('warning: more than one plot selected. using only the first.')
    plotid <- plotid[1]
  }
  if(missing(weight)) { trait.db$weight <- 1 } else names(trait.db)[names(trait.db) == weight] <- 'weight'
  pl <- veg[plotid, veg[plotid,]>0] * trait.db[,'weight']
  indi <- trait.db[match(names(pl), trait.db[, keyname]) , 3:8]
  rownames(indi) <- colnames(pl)
  
  indsum <- matrix(unlist(sapply(indi, function(x) x * pl)), nrow=nrow(indi))
  rbind(sapply(indi, function(x) x * pl), SUM = colSums(indsum, na.rm = TRUE))
}
# showindiplot(veg, wsingo, which(ingo == '5+/4+/3+'))


meanTraits <- function (...) stop('This function is deprecated, use isc with method "mean" instead.')
# meanTraits <- function(trait, veg, refl, trait.db = 'ecodbase.dbf', join = 'LETTERCODE', zero.is.NA = TRUE, ...) {
#   cat('Maximum performance value:', max(veg, na.rm=TRUE),'\n')
#   if(missing(refl)) refl <- attr(veg, 'taxreflist')
#   if(is.null(refl)) refl <- tv.refl()
#   if(missing(trait.db)) {
#     trait.db <- 'ecodbase.dbf'
#     cat('Using trait database:', trait.db, '\n') 
#     }
#   eco <- tv.traits(trait.db = trait.db, refl=refl, quiet=TRUE)
#   if(!trait %in% names(eco)) stop(paste('This trait name does not occur in column names of', trait.db))
#   IV <- as.numeric(eco[,trait][match(names(veg), eco[,join])])
#   names(IV) <- names(veg)
#   if(zero.is.NA) IV[IV == 0] <- NA
#   ind <- !is.na(IV)
#   veg <- veg[,ind]; IV <- IV[ind]
#   out <- rowSums(t((t(veg) * IV)) / rowSums(veg))
#   return(out)
# }
# 
