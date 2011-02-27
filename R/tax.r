store <- local({
    refList <- list()
    function(name, value) {
	if(missing(name)) return(refList)
	if(missing(value)) return(refList[[name]])
	refList[name] <<- list(value)
    }
})

# As dBase is DOS, Umlaute  are  stored  using  a  different  code  table
#    (namely ASCII) than most modern unices (namely ANSI).  If you encounter
#    such a file, I would recommend piping the output through recode(1) with
#    ibmpc:latin1 as its argument.
sub.abbr <- function(...) tax.abbr(...)
tax.abbr <- function(x) {
#  loc <- Sys.getlocale(category='LC_CTYPE')
#  Sys.setlocale("LC_ALL","C")
    x <- iconv(x, "437", "")
    x <- sub('\ ag[.]', ' agg.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ ssp[.]', ' subsp.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ v[.]\ ', ' var. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sv[.]\ ', ' subvar. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Sec[.]\ ', ' sect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Ser[.]\ ', ' ser. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ Subs[.]\ ', ' subsect. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ spec[.]', ' species', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]l[.]', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]str[.]', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ str[.]', ' sensustricto', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ s[.]\ l[.]', ' sensulato', x, perl=TRUE, useBytes=TRUE)
    x <- gsub('\ s[.]\ ', ' subsp. ', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensustricto', ' s. str.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ sensulato', ' s. l.', x, perl=TRUE, useBytes=TRUE)
    x <- sub('\ f[.]\ ', ' fo. ', x, perl=TRUE, useBytes=TRUE)
#  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)  
}

tax <- function(x, refl, verbose = FALSE, syn = TRUE, concept = NULL, strict = FALSE, ...) {
  tv_home <- tv.home()
  if(missing(refl)) refl <- tv.refl(tv_home=tv_home)
  reflist <- paste(refl, ifelse(verbose,'.verbose',''), sep='')
  dbf <- if(verbose) 'tax.dbf' else 'species.dbf'
  supportedReflists <- c('latest', 'GermanSL 1.0', 'GermanSL 1.1', 'GermanSL 1.2')
  supportedReflists <- c(supportedReflists, sub(' ', '', supportedReflists))
  supportedReflists <- c(supportedReflists, tolower(supportedReflists))

###### Internal Functions #####
# Taxon concepts
concept.FUN <- function(species, concept, ...) {
    cat('\n Will use taxon concept', concept, '.\n\n')
    verbose=TRUE
    species$ABBREVIAT <- as.character(species$ABBREVIAT)
    species$VALID_NAME <- as.character(species$VALID_NAME)
    species$AGG_NAME <- as.character(species$AGG_NAME)
    species$SECUNDUM <- as.character(species$SECUNDUM)
    conc <- read.dbf(file.path(tv_home, 'Species', refl, paste(concept,'dbf',sep='.')), as.is=TRUE)
    co <- conc[match(species$SPECIES_NR, conc$SPECIES_NR, nomatch = 0),]
    species[match(conc$SPECIES_NR,species$SPECIES_NR),c('SYNONYM','VALID_NR','AGG')] <- co[match(conc$SPECIES_NR,co$SPECIES_NR),c('SYNONYM','VALID_NR','AGG')]
    species$ABBREVIAT[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$ABBREVIAT[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$VALID_NAME[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$VALID_NAME[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$RANG[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$RANG[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$AGG_NAME[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$AGG_NAME[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$SECUNDUM[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$SECUNDUM[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
}

# Subsetting
select.taxa <- function(x, species, strict) {
  if(is.factor(x)) x <- as.character(x)
  if(is.numeric(x) | is.integer(x))  
	l <- species[match(x, species$SPECIES_NR),]  ## Tax numbers
  if(is.character(x)) {
#    x <- sub.abbr(x)
#   x <- unique(unlist(strsplit(x, ".", fixed = TRUE))) # Pseudo-Species
    if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  {	## Lettercode
	l <- species[species$LETTERCODE %in% x,] 
	} else {		## Taxnames
	l <- species[species$SPECIES_NR==(-1),]
	for(i in 1:length(x))
	  l <- if(!strict) rbind(l, species[grep(x[i], species$ABBREVIAT, useBytes=TRUE), ]) else 
			   rbind(l, species[match(species$ABBREVIAT, x[i], nomatch = 0) > 0, ])
      }
   }
   if(length(l) == 0) stop('No species found!')
   return(l)
}
##### end internal functions #####

# Get reflist
if(is.null(store(reflist))) { # load.species(refl=refl, verbose = verbose)
  if(file.access(file.path(tv_home, 'Species', refl, dbf))) {
    if(refl %in% supportedReflists) {
	cat('\nTaxonomic evaluation list (',dbf, ') of version', refl, 'not available.\n')
	cat('I will try to download the reference now.\n\n')
# 	version <- gsub(' ', '', refl)
	version <- paste("version", substr(refl, 10, nchar(refl)), sep = "")
	download.file(paste('http://geobot.botanik.uni-greifswald.de/download/GermanSL',version,'GermanSL.zip',sep='/'), file.path(tv_home, 'Species','GermanSL.zip'))
	unzip(file.path(tv_home, 'Species/GermanSL.zip'), exdir=file.path(tv_home, 'Species'))
    } else cat('\nTaxonomic evaluation list (',dbf, ') of ', refl, 'not available.\n')
  }
  species <- read.dbf(file.path(tv_home, 'Species', refl, dbf))
  for(i in which(sapply(species, is.factor))) species[,i] <- as.character(species[,i]) # ob nötig?
  for(i in which(sapply(species, is.character))) Encoding(species[,i]) <- 'latin1' # ob nötig?
  species$ABBREVIAT <- sub.abbr(species$ABBREVIAT)
  if(verbose) {
    species$VALID_NAME <- sub.abbr(species$VALID_NAME)
    if('NATIVENAME' %in% names(species)) species$NATIVENAME <- iconv(species$NATIVENAME, iconvlist()[5], '')
  }
  store(reflist, species)
  } else {
#     cat('Using stored list', reflist, '\n')
    species <- store(reflist)
}

# Filter
if(!is.null(concept)) species <- concept.FUN(species, concept)
if(x[1] != 'all') species <- select.taxa(x, species, strict)
if(!syn) species <- species[species$SYNONYM == FALSE,]

if(refl %in% supportedReflists && verbose==FALSE) species <- species[,c('SPECIES_NR','LETTERCODE','ABBREVIAT','NATIVENAME','SYNONYM', 'VALID_NR')]

return(species)
}
#  ls(pos='package:vegdata')

##### Child taxa of a taxon
childs <- function (x, refl, species, gen=4, tree=FALSE, quiet=FALSE, syn=FALSE, ...) {
 if(missing(species)) species <- tax("all", verbose = TRUE, refl = refl, syn = FALSE)
 if(length(x)>1) warning('More than one species selected, using only the first.')
  x <- x[1]
  # if(is.character(x)) x <- species[match(x, species$ABBREVIAT),'SPECIES_NR']
  x <- tax(x, refl=tv.refl(refl=refl), syn = FALSE)$SPECIES_NR	# too many ressources used

  if(tree) {
    require(gWidgets)
   if(length(find.package('gWidgetstcltk', quiet=TRUE)) == 0) warning('Please install gWidgetstcltk.')
    root <- childs(x, gen=1)
    if(!is.null(root)) {
    offspring <- function(path, ...) {
      ll <- root
      for(i in path)
  	    ll <- childs(i, gen=1, tree=FALSE, quiet=TRUE, syn=syn)
      off <- logical(nrow(ll))
      for(n in 1:nrow(ll)) off[n] <- !is.null(childs(ll$SPECIES_NR[n], quiet=TRUE))
      if(syn) out <- data.frame(
                Name=ll$ABBREVIAT,
    		        hasOffspring=off,
    		        Rang=ll$RANG,
                Synonym=ll$SYNONYM,
    		        stringsAsFactors=FALSE
               ) else
  	  out <- data.frame(
              Name=ll$ABBREVIAT,
  		        hasOffspring=off,
  		        Rang=ll$RANG,
  		        Nr=ll$SPECIES_NR,
  		        stringsAsFactors=FALSE
             )
      out
    }
    w <- gwindow(paste("Taxonomic Tree of", species$ABBREVIAT[species$SPECIES_NR==x]))
    tr <- gtree(offspring=offspring, container=w)  
    addHandlerDoubleclick(tr, handler=function(h,...) {
      print(childs(svalue(h$obj), gen=1, syn=syn , quiet=TRUE)[, c('SPECIES_NR' , 'LETTERCODE' , 'ABBREVIAT' , 'GRUPPE' , 'RANG' , 'SYNONYM', 'AGG' , 'SECUNDUM' , 'EDITSTATUS')])
      })
  }} else {
    x <- species[match(x, species$SPECIES_NR),'VALID_NR']
    x <- species[match(x, species$SPECIES_NR),]
    if(syn) {
          ch <- species[which(species$AGG == x$SPECIES_NR),'SPECIES_NR']
          ch <- do.call(rbind, lapply(ch, function(x) syn(x, quiet=TRUE)))
         } else ch <- species[which(species$AGG == x$SPECIES_NR),]
    if(is.null(ch)) { if(!quiet) cat(x$ABBREVIAT, 'has no childs.\n')
                    } else
    if(nrow(ch)==0) {
        if(!quiet) cat(x$ABBREVIAT, 'has no childs.\n') 
  } else {
    ch$GENERATION <- 1
    ch2 <- ch
    t <- 1
    repeat {
      t <- t+1
      if(syn) {
        ch2 <- species[which(species$AGG == x$SPECIES_NR),'SPECIES_NR']
        ch2 <- do.call(rbind, lapply(ch2, function(x) syn(x, quiet=TRUE)))
     } else  ch2 <- species[which(species$AGG %in% ch2$SPECIES_NR),]
      if(nrow(ch2)== 0 ) break
      ch2$GENERATION <- t
      ch <- rbind(ch, ch2)
      if(gen <= t) break
    }
    if(!is.null(gen)) ch <- ch[ch$GENERATION <= gen,]
    if(!quiet) {
      	cat('Childs of', x$ABBREVIAT, '(', x$SPECIES_NR, '):\n')
      	print(ch[,c('SPECIES_NR','ABBREVIAT','RANG','SECUNDUM','AGG','GENERATION','SYNONYM','EDITSTATUS')])
    }
    invisible(ch)
  }
 }
}


## Parents of a taxon
parents <- function (x, refl, species, rank, ...) {
  if(!is.numeric(x) & !is.integer(x)) x <- tax(x, strict=TRUE, syn=FALSE, refl, ...)['SPECIES_NR']
  # stop('x must be numeric or integer (use tax() to find Species numbers).')
  taxlevels <- factor(c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), levels= c('FOR','VAR','ZUS','SSP','SPE','SGE','SSE','SER','SEC','AGG','GAT','FAM','ORD','UKL','KLA','UAB','ABT','AG2','ROOT'), ordered=TRUE)

  if(missing(species)) species <- tax("all", verbose = TRUE, refl = refl, syn = TRUE)
#  x <- tax(x, refl=refl, strict=TRUE, syn = FALSE)$SPECIES_NR
  if(length(x)>1) warning('More than one match, using only first.')
  x <- x[1]
  x <- species[match(x, species$SPECIES_NR),'VALID_NR']
  y <- species[match(x, species$SPECIES_NR),]
  y$GENERATION <- 0
  p <- species[match(unique(y$AGG),species$SPECIES_NR),]
  p$GENERATION <- 1

lo <- function(y,p) {
    if(nrow(p)==0) cat(y$ABBREVIAT, 'has no parents.\n') 
    else {
      p2 <- p
      t <- 1
      repeat {
        t <- t+1
        p2 <- species[match(p2$AGG,species$SPECIES_NR),]
        p2$GENERATION <- t
        p <- rbind(p, p2)
        if(p2$SPECIES_NR == 0 ) break
    }}
    return(p)
}
    
if(!missing(rank)) {
    if(!rank %in% taxlevels) stop(c('Rank must be one of', rank))
      if(taxlevels[match(rank, taxlevels)] <= taxlevels[match(y$RANG, taxlevels)]) {
        warning('Species is equal oo higher rank than specified parent level.')
        p <- c(ABBREVIAT='')
      } else {
        p <- lo(y,p)
        # oblig.taxlevels <- factor(c('SPE','GAT','FAM','ORD','KLA','ABT','ROOT'), levels= c('SPE','GAT','FAM','ORD','KLA','ABT','ROOT'), ordered=TRUE)
        #  p$TAXLEVEL <- as.integer(oblig.taxlevels[match(p$RANG, oblig.taxlevels)])
        p <- p[which(p$RANG == rank), ]
        if(nrow(p) == 0) p <- c(ABBREVIAT='Incertae_sedis')
        #    tv <- oblig.taxlevels[(which(oblig.taxlevels == y$RANG)+1):length(oblig.taxlevels)]
        #    if(!all(tv %in% p$RANG)) 
        cat('Parents of', y$ABBREVIAT, '(', y$SPECIES_NR, '):\n')
        print(p[,c('SPECIES_NR','ABBREVIAT','SECUNDUM','RANG','GENERATION')])
      }
}  else p <- lo(y, p)

return(p)
}

# Synonymy swarm of a taxon
syn <- function (x, refl, species, quiet=FALSE, ...) {
 if(missing(species)) 
      species <- tax('all', verbose = TRUE, refl = refl, syn = TRUE, strict = TRUE, ...)
  x <- tax(x,refl=refl, strict=TRUE)$SPECIES_NR
  if(length(x)>1) {
      warning('More than one match, using only first.')
      x <- x[1]
    }
  v <- species[match(x, species$SPECIES_NR),'VALID_NR']
  if(length(v)==0) stop('No matching species.')
  s <- species[which(species$VALID_NR == v),]
  if(!quiet) {
    cat('Name swarm of', s$ABBREVIAT[s$SPECIES_NR == x],':\n')
    print(s[, c('SPECIES_NR','ABBREVIAT','SYNONYM','SECUNDUM','EDITSTATUS')])
#    print(p[,c(1,3,8,9,12,21)])
   }
 invisible(s)
}


agg <- function(x, refl, species, ...) {
  cat('Deprecated function. Using childs(x, gen=1) instead\n')
  childs(x, refl=refl, species=species, gen=1, ... )
  }

