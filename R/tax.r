tax <- function(x, refl='Germansl 1.1', tv_home, syn = FALSE, tax = FALSE, concept = NULL, ...) {
  if(missing(tv_home)) tv_home <- tv.home(...)
  if(missing(refl)) refl <- tv.refl(...)
  if(!is.null(concept))  tax=TRUE
  dbf <- if(tax) 'tax.dbf' else 'species.dbf'
 if(file.access(paste(tv_home, 'Species', refl, dbf, sep='/'))) stop(paste('Taxonomic evaluation list (',dbf, ') of ', refl, 'not available')) else 
  species <- read.dbf(paste(tv_home, 'Species', refl, dbf, sep='/'))
  # Taxon concepts
  if(!is.null(concept)) {
    conc <- read.dbf(paste(tv_home, 'Species', refl, paste(concept,'dbf',sep='.'), sep='/'))
    co <- conc[match(species$SPECIES_NR, conc$SPECIES_NR, nomatch = 0),]
    species[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0),c('SYNONYM','VALID_NR','AGG')] <- co[match(conc$SPECIES_NR,co$SPECIES_NR),c('SYNONYM','VALID_NR','AGG')]
    levels(species$ABBREVIAT) <- c(levels(species$ABBREVIAT), levels(conc$ABBREVIAT))
    species$ABBREVIAT[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$ABBREVIAT[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    levels(species$VALID_NAME) <- c(levels(species$VALID_NAME), levels(conc$VALID_NAME))
    species$VALID_NAME[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$VALID_NAME[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$RANG[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$RANG[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    species$AGG_NAME[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$AGG_NAME[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    levels(species$SECUNDUM) <- c(levels(species$SECUNDUM), levels(conc$SECUNDUM))
    species$SECUNDUM[match(conc$SPECIES_NR,species$SPECIES_NR,nomatch = 0)] <- co$SECUNDUM[match(conc$SPECIES_NR,co$SPECIES_NR,nomatch = 0)]
    }

  if(refl=='Germansl 1.1' && tax==FALSE) species <- species[,c(1,2,4,5,7,8)]
  if(x[1] != 'all') {
   if(is.numeric(x)) l <- species[match(x, species$SPECIES_NR),]
   if(is.character(x) & nchar(x)[1]== 7) 
        l <- species[species$LETTERCODE %in% x,]
   if(is.character(x) & nchar(x)[1] > 7) 
        l <- species[species$ABBREVIAT %in% x,] 
   if(syn == FALSE & !is.numeric(x)) l <- l[l$SYNONYM == FALSE,]
   l <- l[!is.na(l$ABBREVIAT),]
   if(length(l) == 0) stop('No species found!') 
   l } else species 
} 
spc <- function(...) print('spc() is depreacated, please use function tax() instead')
