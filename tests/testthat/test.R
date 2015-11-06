# library(vegdata)
# library(testthat)
context("Defaults")
tmp <- tempdir()
options(tv_home = tmp)

test_that("reference list", {
  expect_equal(tv.refl(), 'GermanSL 1.3')
  taxa <- load.taxlist(refl = 'GermanSL 1.3')
})

context("Taxonomy")

test_that("Taxa", {
  expect_equal(sort(tax('Elytrigia repens', quiet = TRUE)$TaxonName), sort(c("Elytrigia repens subsp. arenosa", "Elytrigia repens","Elytrigia repens var. caesia","Elytrigia repens var. littoralis","Elytrigia repens var. repens")))
  
  x <- c('Aconitum vulgare', 'Homalothecium lutescens')
  expect_equal(sort(tax(x, strict=TRUE, quiet = TRUE)$TaxonUsageID), c(14261,81891))
  expect_equal(sort(tax(x, detailed=TRUE, strict=TRUE, syn=FALSE, quiet = TRUE)$IsChildTaxonOfID), 81898)
  expect_equal(sort(syn('Elytrigia repens', quiet = TRUE)$TaxonName), sort(c("Agropyron repens subsp. caesium", "Elymus repens subsp. repens s. l.", "Elymus repens subsp. caesium", "Agropyron caesium", "Agropyron repens subsp. repens",  "Elytrigia repens", "Triticum repens", "Elymus repens", "Agropyron repens")))
  
  expect_equal(child('Elytrigia repens', quiet = TRUE)[,1:7], structure(list(TaxonUsageID = c(27780, 27781, 91150, 91151), LETTERCODE = c("ELYMR-L", "ELYMR-R", "ELYMR-P", "ELYMR-E"), TaxonName = c("Elymus repens subsp. littoreus", "Elymus repens subsp. repens", "Elymus repens subsp. repens * caesium", "Elymus repens subsp. repens * repens"), AUTHOR = c("(Schumach.) Conert", "s. str.", "-", "-"), SYNONYM = c(FALSE, FALSE, FALSE, FALSE), TaxonConceptID = c(27780, 27781, 91150, 91151), TaxonConcept = c("Elymus repens subsp. littoreus", "Elymus repens subsp. repens", "Elymus repens subsp. repens * caesium", "Elymus repens subsp. repens * repens")), .Names = c("TaxonUsageID", "LETTERCODE", "TaxonName", "AUTHOR", "SYNONYM", "TaxonConceptID", "TaxonConcept"), class = "data.frame", row.names = c("14021", "14022", "27364", "27365")))

  expect_equal(parent('Elytrigia repens', quiet = TRUE)[,c(1:7,18)], structure(list(TaxonUsageID = c(61389, 60522, 60506, 60469, 60465, 60049, 60000, 94419, 0), LETTERCODE = c('ELYM-SP','POAC-SP','CYPF-SP','COMA-SP','LILC-SP','MAGN-SP','SPEI-SP','GEFA-SP','GRUEETW'), TaxonName = c("Elymus", "Poaceae", "Cyperales", "Commelinidae", "Liliopsida", "Magnoliophytina", "Spermatophyta", "\"Gefaesspflanze\"", "\"Gruenliches etwas\""), AUTHOR = c("L.", "Barnhart", "Burnett", "Takht.", "Dc.", "A. Braun & Doell", NA, "-", "-"), SYNONYM = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), TaxonConceptID = c(61389, 60522, 60506, 60469, 60465, 60049, 60000, 94419, 0), TaxonConcept = c("Elymus", "Poaceae", "Cyperales", "Commelinidae", "Liliopsida", "Magnoliophytina", "Spermatophyta", "\"Gefaesspflanze\"", "\"Gruenliches etwas\""), GENERATION = c(1, 2, 3, 4, 5, 6, 7, 8, 9)), .Names = c("TaxonUsageID", "LETTERCODE", "TaxonName", "AUTHOR", "SYNONYM", "TaxonConceptID", "TaxonConcept", "GENERATION"), class = "data.frame", row.names = c("21433", "20697", "20692", "20673", "20670", "20506", "20480", "29573", "1")) )
})

context('taxval')
### tv.taxval test: should work with (1) Turboveg data set taxatest, (2) with all available options and their combinations

test_that("Taxa1", {
  options(tv_home = file.path(path.package('vegdata'), 'tvdata'))
  db <- 'taxatest'
  obs <- tv.obs(db, tv_home = getOption('tv_home'))
  expect_equal(sort(tax(unique(obs$TaxonUsageID), syn=FALSE, quiet = TRUE)$TaxonName), c("Acer pseudoplatanus", "Achillea", "Achillea millefolium", "Achillea millefolium agg.", "Achillea millefolium subsp. sudetica", "Acoraceae", "Adonis aestivalis", "Agrostis stolonifera var. palustris", "Armeria maritima subsp. elongata", "Armeria maritima subsp. halleri", "Dactylis glomerata", "Galium mollugo", "Hieracium pilosella", "Hieracium subg. Pilosella", "Picea abies", "Quercus robur"))
})
# 
test_that("Taxa2", {
  db <- 'taxatest'
  expect_equal_to_reference(taxval(tv.obs(db), refl='GermanSL 1.3', check.critical = FALSE), file='obs.rds')
#   
#   # saveRDS(obs, file='./vegdata/tests/output/obs.csv')
#   path_expected <- base::file.path(devtools::inst(name="vegdata"), "tests/expected/obs.rds")
#   #path_expected <- base::file.path(devtools::inst(name="vegdata"), "data/obs.rds")
#   expected <- readRDS(path_expected)
#   expect_equal(actual, expected, label="The returned data.frame should be correct")  
})
# 
# 
