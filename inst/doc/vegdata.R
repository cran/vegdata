## ----prep, echo=FALSE, results='hide'-----------------------------------------
library(knitr)
options(stringsAsFactors=FALSE)
opts_chunk$set(concordance = TRUE, comment = "", warning = FALSE, message = TRUE, echo = TRUE, results = 'tex', size="footnotesize")
tmp <- tempdir(check = T)
suppressPackageStartupMessages(library(vegdata))
options(tv_home = tmp)
dir.create(file.path(tmp, 'Species'))
dir.create(file.path(tmp, 'Popup'))
dir.create(file.path(tmp, 'Data'))
file.copy(from = file.path(path.package("vegdata"), 'tvdata', 'Popup'), to = tmp, recursive = TRUE)
file.copy(from = file.path(path.package("vegdata"), 'tvdata', 'Species'), to = tmp, recursive = TRUE)
file.copy(from = file.path(path.package("vegdata"), 'tvdata', 'Data'), to = tmp, recursive = TRUE)
# dim(tax('all'))

## ----load, results='hide'-----------------------------------------------------
library(vegdata)

## ----eval=FALSE---------------------------------------------------------------
#  tv_home <- tv.home()

## ----eval=FALSE---------------------------------------------------------------
#  options(tv_home="path_to_your_Turboveg_root_directory")
#  options(tv_home="/home/jansen/aGitRepos/vegdata/vegdata/inst/tvdata/")

## ----dblisting----------------------------------------------------------------
tv.db()

## -----------------------------------------------------------------------------
tv.refl()

## ----eval=FALSE---------------------------------------------------------------
#  tv.refl('your_preferred_list')

## ----tax, eval =TRUE----------------------------------------------------------
tax('Quercus robur')

## ----syn----------------------------------------------------------------------
tax('Elytrigia repens')$TaxonName
syn('Elytrigia repens')

## ----childs, eval=FALSE-------------------------------------------------------
#  child(27, quiet=TRUE)$TaxonName
#  parent(32)
#  parent(32, rank = 'FAM')

## ----db-----------------------------------------------------------------------
db <- 'taxatest'

## ----meta, eval=FALSE---------------------------------------------------------
#  tv.metadata(db)

## ----obs----------------------------------------------------------------------
getOption('tv_home')
obs <- tv.obs(db)
# Adding species names
species <- tax('all', refl=tv.refl(db=db))
obs$TaxonName <-  species$TaxonName[match(obs$TaxonUsageID, species$TaxonUsageID)]
head(obs[,c('PlotObservationID','TaxonUsageID','COVER_CODE','LAYER','TaxonName')])

## ----data---------------------------------------------------------------------
library(vegdata)
obs <- tv.obs('taxatest')
sort(tax(unique(obs$TaxonUsageID))$TaxonName)

## ----adapt--------------------------------------------------------------------
obs.tax <- taxval(obs, db='taxatest', ag='adapt', rank='SPE', check.critical = FALSE, taxlevels = taxlevels, mono = 'pre')
sort(tax(unique(obs.tax$TaxonUsageID), db=db)$TaxonName)

## ----conflict-----------------------------------------------------------------
obs.tax <- taxval(obs, db='taxatest', ag='conflict', check.critical = FALSE)
sort(tax(unique(obs.tax$TaxonUsageID), db=db)$TaxonName)

## ----maxtax, eval=FALSE-------------------------------------------------------
#  obs.tax <- taxval(obs, db='taxatest', ag='conflict', maxtaxlevel = 'AGG', check.critical = FALSE, interactive = TRUE)
#  obs.tax <- taxval(obs.tax, db='taxatest', ag='adapt', rank='SPE', check.critical = FALSE)
#  sort(tax(unique(obs.tax$TaxonUsageID), db=db)$TaxonName)

## ----coverperc, echo=2:4, eval=TRUE-------------------------------------------------------------------------
options(width=120)
obs <- tv.obs(db)
# obs <- tv.coverperc(db, obs)
tail(obs)
options(width=110)

## ----pseudo1, eval=FALSE------------------------------------------------------------------------------------
#  data(lc.0)
#  obs <- tv.obs(db)
#  tv.veg(db, pseudo = list(lc.0, c("LAYER")), lc = "layer")

## ----lc0, warning=FALSE-------------------------------------------------------------------------------------
tmp <- tv.veg(db, tax=FALSE, pseudo = list(lc.0, "LAYER"), lc = "layer", quiet=TRUE)
names(tmp)

## ----Season-------------------------------------------------------------------------------------------------
comb <- list(data.frame(SEASON=0:4, COMB=c(0,'Spring','Summer','Autumn','Winter')),'SEASON')
names(tv.veg(db, tax=FALSE, pseudo=comb, quiet=TRUE))

## ----layer, results='hide', warning=FALSE-------------------------------------------------------------------
data(lc.1)
veg <- tv.veg(db, lc = "sum", pseudo = list(lc.1, 'LAYER'), dec = 1, check.critical = FALSE)
veg[,1:10]

## -----------------------------------------------------------------------------------------------------------
obs.tax$TaxonUsageID[obs.tax$TaxonUsageID == 27] <- 31

## ----replace------------------------------------------------------------------------------------------------
taxon.repl <- data.frame(old=c(27), new=c(31))
obs.tax$TaxonUsageID <- replace(obs.tax$TaxonUsageID,
                                match(taxon.repl$old, obs.tax$TaxonUsageID), taxon.repl$new)

## ----comb.spec, eval=FALSE----------------------------------------------------------------------------------
#  comb.species(veg, sel=c('QUERROB','QUERROB.Tree'))

## ----site.echo, eval=TRUE-----------------------------------------------------------------------------------
site <- tv.site(db)

## ----elbaue, results='hide'---------------------------------------------------------------------------------
elbaue <- tv.veg('elbaue', check.critical = FALSE)
elbaue.env <- tv.site('elbaue')

## ----cluster------------------------------------------------------------------------------------------------
clust <- vector('integer', nrow(elbaue.env))
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1		# dry sites, low deviation
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2	# dry sites, high deviation
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3	# wet sites, high deviation
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4	# wet sites, low deviation
levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')

## ----syntab.mupa--------------------------------------------------------------------------------------------
require(indicspecies)
synt <- syntab(elbaue, clust, mupa=TRUE)
synt

## ----nmds, quiet=TRUE, results='hide'-----------------------------------------------------------------------
## Data analyses
if (requireNamespace('vegan', quietly = TRUE) ) {
  library(vegan)
veg.nmds <- metaMDS(elbaue, distance = "bray", trymax = 5, autotransform =FALSE,
                    noshare = 1, expand = TRUE, trace = 2)
#eco <- tv.traits()
#eco$OEK_F <- as.numeric(eco$OEK_F)
F <- cwm(veg = elbaue, trait.db = 'ecodbase.dbf', ivname = 'OEK_F', method = 'mean')
N <- cwm(veg = elbaue, trait.db = 'ecodbase.dbf', ivname = 'OEK_N', method = 'mean')
env <- envfit(veg.nmds, env = data.frame(F, N))
} else
  message("package vegan not available")

## ----nmdsplot, quiet=TRUE, results='hide', warning=FALSE, eval=TRUE-----------------------------------------
if (requireNamespace('interp', quietly = TRUE) & requireNamespace('labdsv', quietly = TRUE) & requireNamespace('vegan', quietly = TRUE) ) {
  library(labdsv)
  library(interp)
color = function(x)rev(topo.colors(x))
nmds.plot <- function(ordi, site, var1, var2, disp, plottitle =  'NMDS', env = NULL, ...) {
lplot <- nrow(ordi$points);  lspc <- nrow(ordi$species)
filled.contour(interp(ordi$points[, 1], ordi$points[, 2], site[, var1], duplicate = 'strip'),
           ylim = c(-1, 1.1), xlim = c(-1.4, 1.4),
           color.palette = color, xlab = var1, ylab = var2, main = plottitle,
           key.title = title(main = var1, cex.main = 0.8, line = 1, xpd = NA),
           plot.axes = { axis(1);  axis(2)
             points(ordi$points[, 1], ordi$points[, 2], xlab = "", ylab = "", cex= .5, col = 2, pch = '+')
             points(ordi$species[, 1], ordi$species[, 2], xlab = "", ylab = "", cex=.2, pch = 19)
             ordisurf(ordi, site[, var2], col = 'black', choices = c(1, 2), add = TRUE)
             orditorp(ordi, display = disp, pch = " ")
             legend("topright", paste("GAM of ", var2), col = 'black', lty = 1)
             if(!is.null(env)) plot(env, col='red')
           }
           ,...)
}

nmds.plot(veg.nmds, elbaue.env, disp='species', var1="MGL", var2="SDGL", env=env, plottitle = 'Elbaue floodplain dataset')
} else {
  message("packages interp and/or labdsv not available")
}

