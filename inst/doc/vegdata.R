
## ----prep, echo=FALSE----------------------------------------------------
options(width=90,digits=2)


## ----eval=FALSE----------------------------------------------------------
## vignette('vegdata')


## ----load, results='hide'------------------------------------------------
library(vegdata)


## ----eval=TRUE-----------------------------------------------------------
tv.home()


## ----eval=FALSE----------------------------------------------------------
## options(tv_home="path_to_your_Turboveg_root_directory")


## ----echo=FALSE----------------------------------------------------------
options(tv_home=system.file('tvdata', package='vegdata'))


## ----dblisting-----------------------------------------------------------
tv.db()


## ------------------------------------------------------------------------
tv.refl()


## ----tax-----------------------------------------------------------------
tax('Achillea millefolium')


## ------------------------------------------------------------------------
tax('Achillea millefolium', strict=TRUE, verbose=TRUE) 


## ----syn-----------------------------------------------------------------
tax('Elytrigia repens')$TaxonName
syn('Elytrigia repens')


## ----childs--------------------------------------------------------------
childs(27, quiet=TRUE)$TaxonName
parents('ACHIMIL')


## ----db------------------------------------------------------------------
db <- 'taxatest'


## ----meta, eval=FALSE----------------------------------------------------
## tv.metainfo(db)


## ----obs-----------------------------------------------------------------
obs.tax <- tv.obs(db)
# Adding species names
species <- tax('all')
obs.tax$TaxonName <-  species$TaxonName[match(obs.tax$TaxonUsageID, species$TaxonUsageID)]
head(obs.tax[,c('RELEVE_NR','TaxonUsageID','COVER_CODE','LAYER','TaxonName')])


## ----taxval, eval=TRUE---------------------------------------------------
obs.taxval <- taxval(obs.tax, db=db, mono='lower')


## ----Taxon---------------------------------------------------------------
obs.taxval$TaxonName <-  species$TaxonName[match(obs.taxval$TaxonUsageID, species$TaxonUsageID)]
obs.taxval[,c('RELEVE_NR', 'COVER_CODE', 'TaxonName')]


## ----coarsen, eval=TRUE, results='hide'----------------------------------
tmp <- taxval(obs.tax, refl='GermanSL 1.2', ag='adapt', rank='FAM')
tmp$oldTaxon <- tax(obs.tax$TaxonUsageID, refl='GermanSL 1.2')$TaxonName
tmp$newTaxon <- tax(tmp$TaxonUsageID, refl='GermanSL 1.2')$TaxonName


## ----print.coarsen-------------------------------------------------------
head(tmp[,c('oldTaxon','newTaxon')], 10)


## ----taxonviews, eval=FALSE----------------------------------------------
## newconcept <- taxval(obs, db=db, concept='korneck1996')


## ----coverperc-----------------------------------------------------------
obs <- tv.obs(db)
obs <- tv.coverperc(db, obs)
head(obs)


## ----pseudo1, eval=FALSE-------------------------------------------------
## data(lc.0)
## tv.veg(db, pseudo = list(lc.0, c("LAYER")), lc = "layer")


## ----lc0, echo=FALSE-----------------------------------------------------
data(lc.0)
tmp <- tv.veg(db, tax=FALSE, pseudo = list(lc.0, "LAYER"), lc = "layer", quiet=TRUE)
names(tmp)


## ----Season--------------------------------------------------------------
comb <- list(data.frame(SEASON=0:4, COMB=c(0,'Spring','Summer','Autumn','Winter')),'SEASON')
names(tv.veg(db, tax=FALSE, pseudo=comb, quiet=TRUE))


## ----layer, results='hide'-----------------------------------------------
data(lc.1)
veg <- tv.veg(db, lc = "sum", pseudo = list(lc.1, 'LAYER'), dec = 1, quiet=TRUE)


## ----layerdiff-----------------------------------------------------------
veg[,1:10]


## ------------------------------------------------------------------------
obs.tax$TaxonUsageID[obs.tax$TaxonUsageID == 27] <- 31


## ----replace-------------------------------------------------------------
taxon.repl <- data.frame(old=c(27), new=c(31))
obs.tax$TaxonUsageID <- replace(obs.tax$TaxonUsageID, 
    match(taxon.repl$old, obs.tax$TaxonUsageID), taxon.repl$new)


## ----comb.spec, eval=TRUE------------------------------------------------
veg <- tv.veg('taxatest', quiet=TRUE)
comb.species(veg, sel=c('QUERROB','QUERROB.Tree'))


## ----site.echo, eval=TRUE------------------------------------------------
site <- tv.site(db)


## ----VegetWeb, eval=FALSE------------------------------------------------
## source('http://geobot.botanik.uni-greifswald.de/download/r_package/vegetweb.r')


## ----ESVeg---------------------------------------------------------------
download.file('http://geobot.botanik.uni-greifswald.de/download/data/T302.xml', "T302.xml")
T302.site <- ESveg.site('T302.xml')
T302.site <- T302.site[!is.na(T302.site$LONGITUDE),]


## ----eval=FALSE----------------------------------------------------------
## tv.compRefl('taxref1', 'taxref2')


## ----elbaue, results='hide'----------------------------------------------
elbaue <- tv.veg('elbaue')
elbaue.env <- tv.site('elbaue')


## ----cluster-------------------------------------------------------------
clust <- vector('integer', nrow(elbaue.env))
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1		# dry sites, low deviation
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2	# dry sites, high deviation
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3	# wet sites, high deviation
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4	# wet sites, low deviation
levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')


## ----syntab.mupa---------------------------------------------------------
require(indicspecies)
traits <- tv.traits()
trait <- data.frame(EIV_F = traits$OEK_F, EIV_N = traits$OEK_N)
rownames(trait) <- traits$ABBREVIAT
st <- syntab(elbaue, clust, mupa=TRUE, fullnames=TRUE)
print(st, limit=30, trait=trait)


## ----eval=FALSE----------------------------------------------------------
## library(rgdal)
## library(googleVis)
## coord <- data.frame(lat=T302.site$LATITUDE, long=T302.site$LONGITUDE)
## coordinates(coord) <- c("long", "lat")
## proj4string(coord) <- CRSargs(CRS("+init=epsg:31468")) # GK, 4. Stripe
## coord <- spTransform(coord, CRS("+init=epsg:4326")) # WGS 84, geographical coordinates, decimal degrees
## T302.site$long <- coordinates(coord)[,1]
## T302.site$lat <- coordinates(coord)[,2]


## ----eval=FALSE----------------------------------------------------------
## T302.site$loc <- paste(T302.site$LATITUDE, T302.site$LONGITUDE, sep=':')
## T302.site$tip <- paste(paste('Releve_NR:', T302.site$plotCode), paste('Table:',
##   T302.site$referenceTable), paste('Nr. in table:', T302.site$referencePlot),
##   paste('Date:', T302.site$obsEndDate), paste('Landuse:', T302.site$NUTZUNG),
##   paste('Author:', T302.site$ERHEBER), paste('Locality:', T302.site$LOKALIT__T),
##   paste('Longitude:', T302.site$LONGITUDE), paste('Latitude:', T302.site$LATITUDE),
##   paste('geogr. Uncertaintity:', T302.site$GENAUIGKEI), sep='<BR>')


## ----eval=FALSE, fig.keep='none'-----------------------------------------
## places <- gvisMap(T302.site[,c('loc','tip')], 'loc', 'tip', options=list(showTip=TRUE,
##   showLine=FALSE, enableScrollWheel=TRUE, mapType='hybrid', useMapTypeControl=TRUE,
##   width=1000, height=800))
## plot(places)


## ----nmds, quiet=TRUE, results='hide', eval=TRUE-------------------------
## Data analyses
library(vegan)
veg.nmds <- metaMDS(elbaue, distance = "bray", trymax = 5, autotransform =FALSE, 
     noshare = 1, expand = TRUE, trace = 2)
mT.F <- meanTraits('OEK_F', elbaue)
mT.N <- meanTraits('OEK_N', elbaue)
env <- envfit(veg.nmds, data.frame(mT.F,mT.N))


## ----nmdsplotfun, quiet=TRUE, results='hide'-----------------------------
library(labdsv)
library(akima)
color = function(x)rev(topo.colors(x))
nmds.plot <- function(ordi, site, var1, var2, disp, plottitle =  'NMDS', env = NULL, ...) {
 lplot <- nrow(ordi$points);  lspc <- nrow(ordi$species)
 filled.contour(interp(ordi$points[, 1], ordi$points[, 2], site[, var1]), 
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


## ----nmdsplot, quiet=TRUE, results='hide', eval=TRUE---------------------
print(nmds.plot(veg.nmds, elbaue.env, disp='species', 
                var1="MGL", var2="SDGL", env=env, 
                plottitle = 'NMDS of Elbaue floodplain vegetation'))


