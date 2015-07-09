context("Defaults")

test_that("reference list", {
  expect_equal(tv.refl(), 'GermanSL 1.2')
})

context("Taxonomy")

test_that("Taxa", {
  expect_equal(sort(tax('Elytrigia repens')$TaxonName), sort(c("Elytrigia repens subsp. arenosa", "Elytrigia repens","Elytrigia repens var. caesia","Elytrigia repens var. littoralis","Elytrigia repens var. repens")))
  
  x <- c('Aconitum vulgare', 'Homalothecium lutescens')
  expect_equal(sort(tax(x, strict=TRUE)$TaxonUsageID), c(14261,81891))
  expect_equal(sort(tax(x, detailed=TRUE, strict=TRUE, syn=FALSE)$IsChildTaxonOfID), 81898)
  expect_equal(sort(syn('Elytrigia repens')$TaxonName), sort(c("Agropyron repens subsp. caesium", "Elymus repens subsp. repens s. l.", "Elymus repens subsp. caesium", "Agropyron caesium", "Agropyron repens subsp. repens",  "Elytrigia repens", "Triticum repens", "Elymus repens", "Agropyron repens")))
  
  expect_equal(child('Elytrigia repens')[,1:7], 
               structure(list(TaxonUsageID = c(27780, 27781, 91150, 91151), LETTERCODE = c("ELYMR-L", "ELYMR-R", "ELYMR-E", "ELYMR-P"), TaxonName = c("Elymus repens subsp. littoreus", "Elymus repens subsp. repens", "Elymus repens subsp. repens * caesium", "Elymus repens subsp. repens * repens"), AUTHOR = c("(Schumach.) Conert", "s. str.", "-", "-"), SYNONYM = c(FALSE, FALSE, FALSE, FALSE), TaxonConceptID = c(27780, 27781, 91150, 91151), TaxonConcept = c("Elymus repens subsp. littoreus", "Elymus repens subsp. repens", "Elymus repens subsp. repens * caesium", "Elymus repens subsp. repens * repens")), .Names = c("TaxonUsageID", "LETTERCODE", "TaxonName", "AUTHOR", "SYNONYM", "TaxonConceptID", "TaxonConcept"), class = "data.frame", row.names = c("13918", "13919", "27169", "27170")))

  expect_equal(parent('Elytrigia repens')[,c(1:7,19)], structure(list(TaxonUsageID = c(61389, 60522, 60506, 60469, 60465, 60049, 60000, 94419, 0), LETTERCODE = c("ELYM-SP", "POA -SP", "CYPR-SP", "COML-SP", "LILO-SP", "MAGO-SP", "SPEA-SP", "\"GEF-SP", "\"GRUETW"), TaxonName = c("Elymus", "Poaceae", "Cyperales", "Commelinidae", "Liliopsida", "Magnoliophytina", "Spermatophyta", "\"Gefaesspflanze\"", "\"Gruenliches etwas\""), AUTHOR = c("L.", "Barnhart", "Burnett", "Takht.", "Dc.", "A. Braun & Doell", NA, "-", "-"), SYNONYM = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), TaxonConceptID = c(61389, 60522, 60506, 60469, 60465, 60049, 60000, 94419, 0), TaxonConcept = c("Elymus", "Poaceae", "Cyperales", "Commelinidae", "Liliopsida", "Magnoliophytina", "Spermatophyta", "\"Gefaesspflanze\"", "\"Gruenliches etwas\""), GENERATION = c(1, 2, 3, 4, 5, 6, 7, 8, 9)), .Names = c("TaxonUsageID", "LETTERCODE", "TaxonName", "AUTHOR", "SYNONYM", "TaxonConceptID", "TaxonConcept", "GENERATION"), class = "data.frame", row.names = c("21237", "20502", "20497", "20478", "20475", "20312", "20286", "29377", "1")) )
})

context('taxval')
### tv.taxval test: should work with (1) Turboveg data set taxatest, (2) with all available options and their combinations

messageToText <- function(expr) {
  con <- textConnection("messages", "w")
  sink(con, type="message")
  eval(expr)
  sink(NULL, type="message")
  close(con)
  messages
}

# test_that("Taxa1", {
#   db <- 'taxatest'
#   obs <- tv.obs(db)
#   expect_equal(sort(tax(unique(obs$TaxonUsageID), syn=FALSE, quiet = TRUE)$TaxonName), c("Acer pseudoplatanus", "Achillea", "Achillea millefolium", "Achillea millefolium agg.", "Achillea millefolium subsp. sudetica", "Acoraceae", "Adonis aestivalis", "Agrostis stolonifera var. palustris", "Armeria maritima subsp. elongata", "Armeria maritima subsp. halleri", "Dactylis glomerata", "Galium mollugo", "Hieracium pilosella", "Hieracium subg. Pilosella", "Picea abies", "Quercus robur"))
# })


