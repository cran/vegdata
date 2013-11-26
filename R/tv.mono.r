tv.mono <- function(refl, write = FALSE, filename='monotypic-D.dbf', nr.member = 1, tv_home, ...)
{
 if(missing(tv_home)) tv_home <- tv.home()
 taxa <- load.taxlist(refl = refl, verbose = TRUE, syn = FALSE)   
 AG <- table(taxa$IsChildTaxonOfID)
 AG <- AG[AG == nr.member]
 AG <- as.integer(names(AG))
# names(taxa)[names(taxa)=='TAXONRANK'] <- 'TaxonRank'
 mono <- data.frame(AGG_NR = AG, AGG_TaxonRank = taxa$TaxonRank[match(AG,taxa$TaxonUsageID)], MEMBER_NR=as.integer(taxa$TaxonUsageID[match(AG,taxa$IsChildTaxonOfID)]), 
MEMB_NAME=taxa$TaxonName[match(AG,taxa$IsChildTaxonOfID)], MEMB_TaxonRank=taxa$TaxonRank[match(AG,taxa$IsChildTaxonOfID)])
 #IsChildTaxonOf = taxa$IsChildTaxonOf[match(AG, taxa$IsChildTaxonOfID)],
#for(i in 1:nrow(mono)) {
# mono$MEMBER_NR[i] <- taxa[taxa$IsChildTaxonOfID == AG[i],1]
# mono$MEMBER_NAME[i] <- as.character(taxa[taxa$IsChildTaxonOfID == AG[i],2])
# mono$AGG_TaxonRank[i] <- as.character(taxa[taxa$IsChildTaxonOfID == AG[i],7])
# }
if(write) {
  write.dbf(mono, filename) } else 
return(mono)
}

