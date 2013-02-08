pkgname <- "vegdata"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('vegdata')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ESveg")
### * ESveg

flush(stderr()); flush(stdout())

### Name: ESveg.veg
### Title: Load vegetation data from ESveg formatted data files
### Aliases: ESveg.veg ESveg.site
### Keywords: misc,manip,survey

### ** Examples

path <- system.file(package = "vegdata")
veg <- ESveg.veg(file.path(path,'tvdata', 'Data', 'tvexport.xml'))
names(veg)



cleanEx()
nameEx("elbaue")
### * elbaue

flush(stderr()); flush(stdout())

### Name: elbaue
### Title: Species Data and Altitude from floodplains of the river Elbe,
###   Germany.
### Aliases: elbaue elbaue.env
### Keywords: datasets

### ** Examples

elbaue <- tv.veg('elbaue')
elbaue.env <- tv.site('elbaue')



cleanEx()
nameEx("syntab")
### * syntab

flush(stderr()); flush(stdout())

### Name: syntab
### Title: Frequency tables
### Aliases: syntab print.syntab freqtab
### Keywords: misc

### ** Examples

## Not run: 
##D elbaue <- tv.veg('elbaue')
##D elbaue.env <- tv.site('elbaue')
##D clust <- vector('integer', nrow(elbaue.env))
##D clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1
##D clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2
##D clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3
##D clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4
##D levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')
##D syntab(elbaue, clust, limit=30, mupa=TRUE, fullnames=TRUE)
## End(Not run)



cleanEx()
nameEx("tax")
### * tax

flush(stderr()); flush(stdout())

### Name: tax
### Title: Query of taxonomic reference list including concept synonomy and
###   taxonomic hierarchy.
### Aliases: tax tax.default tax.veg childs parents syn
### Keywords: misc

### ** Examples

## Not run: 
##D ## GermanSL in Turboveg installation path needed
##D tax(27)
##D tax('Achillea millefolium')
##D tax('ACHIMILL')
## End(Not run)
## Not run: 
##D childs(0, gen=1)
##D childs(94419, tree=TRUE)
## End(Not run)



cleanEx()
nameEx("taxval")
### * taxval

flush(stderr()); flush(stdout())

### Name: taxval
### Title: Handling of taxonomy in vegetation data.
### Aliases: taxval tv.taxval comb.species
### Keywords: misc,manip

### ** Examples

## Not run: 
##D # Turboveg installation needed
##D obs <- taxval(db='taxatest')
##D ## For explanations see vignette('vegdata').
##D 
##D veg <- tv.veg('taxatest')
##D veg <- comb.species(veg, c('ARMEM-E','ARMEM-H'))
## End(Not run)



cleanEx()
nameEx("tv.coverperc")
### * tv.coverperc

flush(stderr()); flush(stdout())

### Name: tv.coverperc
### Title: Cover code translation
### Aliases: tv.coverperc
### Keywords: misc

### ** Examples

## For examples see in vignette('vegdata').



cleanEx()
nameEx("tv.obs")
### * tv.obs

flush(stderr()); flush(stdout())

### Name: tv.obs
### Title: Dataframe of plot-species observations directly from Turboveg
### Aliases: tv.obs
### Keywords: misc, survey

### ** Examples

## Not run: 
##D # Turboveg installation needed
##D obs <- tv.obs('taxatest')
##D head(obs)
## End(Not run)



cleanEx()
nameEx("tv.traits")
### * tv.traits

flush(stderr()); flush(stdout())

### Name: tv.traits
### Title: Load species traits from Turboveg reference list
### Aliases: tv.traits meanTraits tv.eco
### Keywords: misc

### ** Examples

## Not run: 
##D veg <- tv.veg('elbaue', cover.transform='pa')
##D # mEIV <- meanTraits('OEK_F', veg, 'ecodbase.dbf')
##D site <- tv.site('elbaue')
##D # plot(site$MGL, mEIV)
## End(Not run)


cleanEx()
nameEx("tv.veg")
### * tv.veg

flush(stderr()); flush(stdout())

### Name: tv.veg
### Title: Tabulates vegetation tables from Turboveg database
### Aliases: tv.veg tv.db
### Keywords: misc,manip,survey

### ** Examples

## Not run: 
##D vignette("vegdata")
##D # If you have Turboveg installed on your computer try for a beginning 
##D # tv.veg('databasename', tax=FALSE).
##D args(tv.veg)
##D help('taxval')
##D 
##D veg <- tv.veg('taxatest')
##D names(veg)
##D tv.veg('taxatest', uncertain=list('DET_CERT', data.frame(0:2,c('pres','agg','agg'))), pseudo=list(lc.0,'LAYER'), genus = 'delete')
## End(Not run)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
