
##### Child taxa of a taxon
childs <- function (x, refl, species, gen=4, tree=FALSE, quiet=FALSE, syn=FALSE, ...) {
  refl <- tv.refl(refl = refl)
  species <- load.taxlist(refl = refl, verbose = TRUE, syn = FALSE)
  if(length(x)>1) warning('More than one species selected, using only the first.')
  x <- x[1]
  # if(is.character(x)) x <- species[match(x, species$TaxonName),'TaxonUsageID']
  x <- tax(x, refl = tv.refl(refl = refl), syn = FALSE, quiet = quiet)$TaxonConceptID
  
  if(tree) {
    require(gWidgets)
    if(length(find.package('gWidgetstcltk', quiet=TRUE)) == 0) warning('Please install gWidgetstcltk.')
    root <- childs(x, gen=1, ...)
    if(!is.null(root)) {
      offspring <- function(path, ...) {
        ll <- root
        for(i in path)
          ll <- childs(i, gen=1, tree=FALSE, quiet=TRUE, syn=syn)
        off <- logical(nrow(ll))
        for(n in 1:nrow(ll)) off[n] <- !is.null(childs(ll$TaxonUsageID[n], quiet=TRUE))
        if(syn) out <- data.frame(
          Name=ll$TaxonName,
          hasOffspring=off,
          Rang=ll$taxonRank,
          Synonym=ll$SYNONYM,
          stringsAsFactors=FALSE
        ) else
          out <- data.frame(
            Name=ll$TaxonName,
            hasOffspring=off,
            Rang=ll$taxonRank,
            Nr=ll$TaxonUsageID,
            stringsAsFactors=FALSE
          )
        out
      }
      w <- gwindow(paste("Taxonomic Tree of", species$TaxonName[species$TaxonUsageID==x]))
      tr <- gtree(offspring=offspring, container=w)  
      addHandlerDoubleclick(tr, handler=function(h,...) {
        print(childs(svalue(h$obj), gen=1, syn=syn , quiet=TRUE)[, c('TaxonUsageID' , 'LETTERCODE' , 'TaxonName' , 'GRUPPE' , 'taxonRank' , 'SYNONYM', 'IsChildTaxonOfID' , 'publishedInCitation' , 'EDITSTATUS')])
      })
    }} else {
      x <- species[match(x, species$TaxonUsageID),'TaxonConceptID']
      x <- species[match(x, species$TaxonUsageID),]
      if(syn) {
        ch <- species[which(species$IsChildTaxonOfID == x$TaxonUsageID),'TaxonUsageID']
        ch <- do.call(rbind, lapply(ch, function(x) syn(x, quiet=TRUE)))
      } else ch <- species[which(species$IsChildTaxonOfID == x$TaxonUsageID),]
      if(is.null(ch)) { if(!quiet) cat(x$TaxonName, 'has no childs.\n')
      } else
        if(nrow(ch)==0) {
          if(!quiet) cat(x$TaxonName, 'has no childs.\n') 
        } else {
          ch$GENERATION <- 1
          ch2 <- ch
          t <- 1
          repeat {
            t <- t+1
            if(syn) {
              ch2 <- species[which(species$IsChildTaxonOfID == x$TaxonUsageID),'TaxonUsageID']
              ch2 <- do.call(rbind, lapply(ch2, function(x) syn(x, quiet=TRUE)))
            } else  ch2 <- species[which(species$IsChildTaxonOfID %in% ch2$TaxonUsageID),]
            if(nrow(ch2)== 0 ) break
            ch2$GENERATION <- t
            ch <- rbind(ch, ch2)
            if(gen <= t) break
          }
          if(!is.null(gen)) ch <- ch[ch$GENERATION <= gen,]
          if(!quiet) {
            cat('Childs of', x$TaxonName, '(', x$TaxonUsageID, '):\n')
            print(ch[,c('TaxonUsageID','TaxonName','taxonRank','publishedInCitation','IsChildTaxonOfID','GENERATION','SYNONYM','EDITSTATUS')])
          }
          invisible(ch)
        }
    }
}


## Parents of a taxon
parents <- function (x, refl = 'GermanSL 1.2', species, rank, quiet = FALSE, ...) {
  if(!is.numeric(x) & !is.integer(x)) x <- tax(x, strict=TRUE, syn=FALSE, refl, quiet = TRUE, ...)['TaxonUsageID']
  # stop('x must be numeric or integer (use tax() to find Species numbers).')
  taxlevels <- factor(c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), levels= c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), ordered=TRUE)
  refl <- tv.refl(refl = refl)
  if(missing(species)) species <- tax("all", verbose = TRUE, refl = refl, syn = TRUE, quiet =TRUE)
  #  x <- tax(x, refl=refl, strict=TRUE, syn = FALSE)$TaxonUsageID
  if(length(x)>1) warning('More than one match, using only first.')
  x <- x[1]
  x <- species[match(x, species$TaxonUsageID),'TaxonConceptID']
  y <- species[match(x, species$TaxonUsageID),]
  y$GENERATION <- 0
  p <- species[match(unique(y$IsChildTaxonOfID),species$TaxonUsageID),]
  p$GENERATION <- 1
  
  lo <- function(y,p) {
    if(nrow(p)==0) cat(y$TaxonName, 'has no parents.\n') 
    else {
      p2 <- p
      t <- 1
      repeat {
        t <- t+1
        p2 <- species[match(p2$IsChildTaxonOfID,species$TaxonUsageID),]
        p2$GENERATION <- t
        p <- rbind(p, p2)
        if(p2$TaxonUsageID == 0 ) break
      }}
    return(p)
  }
  
  if(!missing(rank)) {
    if(!rank %in% taxlevels) stop(c('Rank must be one of', rank))
    if(taxlevels[match(rank, taxlevels)] <= taxlevels[match(y$taxonRank, taxlevels)]) {
      warning('Species is equal oo higher rank than specified parent level.')
      p <- c(TaxonName='')
    } else {
      p <- lo(y,p)
      # oblig.taxlevels <- factor(c('SPE','GAT','FAM','ORD','KLA','ABT','ROOT'), levels= c('SPE','GAT','FAM','ORD','KLA','ABT','ROOT'), ordered=TRUE)
      #  p$TAXLEVEL <- as.integer(oblig.taxlevels[match(p$taxonRank, oblig.taxlevels)])
      p <- p[which(p$taxonRank == rank), ]
      if(nrow(p) == 0) p <- c(TaxonName='Incertae_sedis')
      #    tv <- oblig.taxlevels[(which(oblig.taxlevels == y$taxonRank)+1):length(oblig.taxlevels)]
      #    if(!all(tv %in% p$taxonRank)) 
      cat('Parents of', y$TaxonName, '(', y$TaxonUsageID, '):\n')
      print(p[,c('TaxonUsageID','TaxonName','publishedInCitation','taxonRank','GENERATION')])
    }
  }  else p <- lo(y, p)
  
  return(p)
}


# Synonymy swarm of a taxon
syn <- function (x, refl = 'GermanSL 1.2', species, quiet=FALSE, ...) {
  if(missing(species)) 
    species <- tax('all', verbose = TRUE, refl = refl, syn = TRUE, strict = TRUE, quiet = TRUE, ...)
  x <- tax(x, refl=refl, strict=TRUE, quiet = quiet)$TaxonUsageID
  if(length(x)>1) {
    warning('More than one match, using only first.')
    x <- x[1]
  }
  v <- species[match(x, species$TaxonUsageID),'TaxonConceptID']
  if(length(v)==0) stop('No matching species.')
  s <- species[which(species$TaxonConceptID == v),]
  if(!quiet) {
    cat('Name swarm of', s$TaxonName[s$TaxonUsageID == x],':\n')
    print(s[, c('TaxonUsageID','TaxonName','SYNONYM','EDITSTATUS')])
    #    print(p[,c(1,3,8,9,12,21)])
  }
  invisible(s)
}


agg <- function(x, refl = 'GermanSL 1.2', species, ...) {
  cat('Deprecated function. Using childs(x, gen=1) instead\n')
  childs(x, refl=refl, species=species, gen=1, ... )
}
