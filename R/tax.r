store <- local({
    refList <- list()
    function(name, value) {
	if(missing(name)) return(refList)
	if(missing(value)) return(refList[[name]])
	refList[name] <<- list(value)
    }
  })

# , envir=as.environment(which(search()=='package:vegdata'))
# store <- local({
#        assign('GermanSL', NULL)
#        function(value, refl) {
#           if(missing(value)) get(refl)
# 	  else get(refl) <<- value
#        }
#    })
# 
sub.abbr <- function(x) {
#  loc <- Sys.getlocale(category='LC_CTYPE')
#  Sys.setlocale("LC_ALL","C")
    iconv(x, "latin1", "")
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
   if(is.numeric(x) | is.integer(x))   l <- species[match(x, species$SPECIES_NR),]
   if(is.character(x)) {
    x <- sub.abbr(x)
    x <- unique(unlist(strsplit(x, ".", fixed = TRUE)))
    if(nchar(x[1]) == 7 & x[1] == toupper(x[1]))  {
	l <- species[species$LETTERCODE %in% x,] 
# 	if(syn == FALSE & !is.numeric(x)) l <- l[l$SYNONYM == FALSE,]
	} else {
	l <- character()
	for(i in 1:length(x)) 
	l <- if(!strict) c(l, species[grep(x[i], species$ABBREVIAT, useBytes=TRUE), 'ABBREVIAT']) else c(l, species$ABBREVIAT[species$ABBREVIAT == x[i]])
        l <- species[match(l, species$ABBREVIAT),]
      }
    }
   if(length(l) == 0) stop('No species found!')
   return(l)
}
##### end internal functions #####
# Type
# reflist <- paste(refl, dbf, sep='.')
 
# Get
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
  species$ABBREVIAT <- sub.abbr(species$ABBREVIAT)
  if(verbose) species$VALID_NAME <- sub.abbr(species$VALID_NAME)
  store(reflist, species)
  } else {
#     cat('Using stored list', reflist, '\n')
    species <- store(reflist)
  }

# Filter
for(i in which(sapply(species, is.factor))) species[,i] <- as.character(species[,i])
for(i in which(sapply(species, is.character))) Encoding(species[,i]) <- 'latin1'
if(!is.null(concept)) species <- concept.FUN(species, concept)
if(x[1] != 'all') species <- select.taxa(x, species, strict)
if(!syn) species <- species[species$SYNONYM == FALSE,]

if(refl %in% supportedReflists && verbose==FALSE) species <- species[,c('SPECIES_NR','LETTERCODE','ABBREVIAT','NATIVENAME','SYNONYM', 'VALID_NR')]

return(species)
}

#  ls(pos='package:vegdata')


childs <- function (x, refl, species, gen=5, tree=FALSE, quiet=FALSE, ...) {
 if(missing(species)) species <- tax("all", verbose = TRUE, refl = refl, syn = FALSE)
  x <- tax(x, refl=refl, strict=TRUE, syn = FALSE)$SPECIES_NR
#  if(length(x)>1) warning('More than one match, using only first.')
  x <- x[1]
  if(tree==TRUE) {
    require(gWidgets)
    root <- childs(x, gen=1)
    offspring <- function(path, ...) {
    ll <- root
    for(i in path)
	ll <- childs(i, gen=1, quiet=TRUE)
	out <- data.frame(Name=ll$ABBREVIAT,
                    hasOffspring=!is.null(childs(ll$SPECIES_NR, quiet=TRUE)),
                    Rang=ll$RANG,
#                     Edit=ll$EDITSTATUS,
                    Nr=ll$SPECIES_NR,
                   stringsAsFactors=FALSE)
    out	}
    w <- gwindow(paste("Taxonomic Tree of", species$ABBREVIAT[species$SPECIES_NR==x]))
    tr <- gtree(offspring=offspring, container=w)  
    addHandlerDoubleclick(tr, handler=function(h,...) {
      print(childs(svalue(h$obj), gen=1)[,c('SPECIES_NR','LETTERCODE','ABBREVIAT','GRUPPE','RANG','AGG','SECUNDUM','EDITSTATUS')])
      })
  } else {
 x <- species[match(x, species$SPECIES_NR),'VALID_NR']
 x <- species[match(x, species$SPECIES_NR),]
 Agg <- species[which(species$AGG == x$SPECIES_NR),]
  if(nrow(Agg)==0) {if(!quiet) cat(x$ABBREVIAT, 'has no childs.\n') } else {
    Agg$GENERATION <- 1
    ag2 <- Agg
    t <- 1
    repeat {
      t <- t+1
      ag2 <- species[which(species$AGG %in% ag2$SPECIES_NR),]
      if(nrow(ag2)== 0 ) break
      ag2$GENERATION <- t
      Agg <- rbind(Agg, ag2)
      if(gen <= t) break
    }
    if(!is.null(gen)) Agg <- Agg[Agg$GENERATION <= gen,]
    if(!quiet) {
	cat('Childs of', x$ABBREVIAT, '(', x$SPECIES_NR, '):\n')
	print(Agg[,c('SPECIES_NR','ABBREVIAT','RANG','SECUNDUM','AGG','GENERATION','EDITSTATUS')])
      }
    invisible(Agg)
  }
 }
}

parents <- function (x, refl, species, quiet=FALSE,  ...) {
  if(missing(species)) species <- tax("all", verbose = TRUE, refl = refl, syn = TRUE)
  x <- tax(x, refl=refl, strict=TRUE, syn = FALSE)$SPECIES_NR
  if(length(x)>1) warning('More than one match, using only first.')
  x <- x[1]
  x <- species[match(x, species$SPECIES_NR),'VALID_NR']

  x <- species[match(x, species$SPECIES_NR),]
  p <- species[match(unique(x$AGG),species$SPECIES_NR),]
  p$GENERATION <- 1
  if(nrow(p)==0) cat(x$ABBREVIAT, 'has no parents.\n') else{
# table(species$RANG)
# ROOT  ABT  UAB  KLA  UKL  ORD FAM  GAT  AG2  AGG  SEC  SER  SSE  SGE  SPE  SSP  VAR  ZUS
  p2 <- p
  t <- 1
  repeat {
  t <- t+1
  p2 <- species[match(p2$AGG,species$SPECIES_NR),]
  p2$GENERATION <- t
  p <- rbind(p, p2)
  if(p2$SPECIES_NR == 0 ) break
 }

  if(!quiet) {
    cat('Parents of', x$ABBREVIAT, '(', x$SPECIES_NR, '):\n')
    print(p[,c('SPECIES_NR','ABBREVIAT','SECUNDUM','RANG','GENERATION','EDITSTATUS')])
   }
 invisible(p)
}}


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

