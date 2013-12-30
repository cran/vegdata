### vegdata-tests: unit tests for vegan functions

### This file contains unit tests for vegdata functions. This file is
### run in R CMD check and its results are compared against previously
### saved results in vegdata-tests.Rout.save. If you change tests, you
### must generate new vegdata-tests.Rout.save in this directory.

### The current plan is that tests/ are not included in the CRAN
### release, but only in the development versin of vegan in R-Forge.

### The tests here are not intended for human reading. The tests need
### not be ecological or biologically meaningful, but they are only
### intended for testing strange arguments, protect against
### regressions and test correctness of results.

### The tests are in a single file and you should clean work space
### after the unit test. You should set random number seed (if needed)
### for comparison against vegdata-tests.Rout.save, and you should
### remove the seed after the unit test. If possible, avoid very long
### lasting tests.

###<--- BEGIN TESTS --->
suppressPackageStartupMessages(require(vegdata))

###<--- BEGIN tv.veg test --->
### tv.taxval test: should work with (1) Turboveg data set taxatest, (2) with all available options and their combinations
# set.seed(3252)
db <- 'taxatest'

### taxval
obs <- tv.obs(db)
sort(tax(unique(obs$TaxonUsageID), syn=F)$TaxonName)

obs.v <- taxval(db=db)
sort(tax(unique(obs.v$TaxonUsageID), syn=F)$TaxonName)

obs.spe <- taxval(db=db, obs=obs, ag='adapt', rank='SPE')
sort(tax(unique(obs.spe$TaxonUsageID), syn=F)$TaxonName)

obs.m <- taxval(db=db, maxtaxlevel='AGG')
sort(tax(unique(obs.m$TaxonUsageID), syn=F)$TaxonName)

obs.mh <- taxval(db=db, mono='higher')
sort(tax(unique(obs.mh$TaxonUsageID), syn=F)$TaxonName)

obs.ml <- taxval(db=db,  mono='lower')
sort(tax(unique(obs.ml$TaxonUsageID), syn=F)$TaxonName)

### tax, syn, child, parent (results depend heavely on taxonomic reference list)
tax('Elytrigia repens')$TaxonName
syn('Elytrigia repens')$TaxonName
child('Elytrigia repens')
parent('Elytrigia repens')
tax(27778)
syn(27778)
child(27778)
parent(27778)
tax('ELYMREP')
syn('ELYMREP')
child('ELYMREP')
parent('ELYMREP')
