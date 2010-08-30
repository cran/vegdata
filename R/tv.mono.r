tv.mono <- function(refl, write=FALSE, nr.member = 1, tv_home, ...)
{
 if(missing(tv_home)) tv_home <- tv.home(...)
 taxa <- tax('all', refl=refl, tax=TRUE, syn=FALSE, ...)   
 AG <- table(taxa$AGG)
 AG <- AG[AG == nr.member]
 AG <- as.integer(names(AG))
 
 mono <- data.frame(AGG_NR = AG, AGG_NAME = taxa$AGG_NAME[match(AG, taxa$AGG)], AGG_RANG = taxa$RANG[match(AG,taxa$SPECIES_NR)], MEMBER_NR=as.integer(taxa$SPECIES_NR[match(AG,taxa$AGG)]), 
MEMB_NAME=taxa$ABBREVIAT[match(AG,taxa$AGG)], MEMB_RANG=taxa$RANG[match(AG,taxa$AGG)])
#for(i in 1:nrow(mono)) {
# mono$MEMBER_NR[i] <- taxa[taxa$AGG == AG[i],1]
# mono$MEMBER_NAME[i] <- as.character(taxa[taxa$AGG == AG[i],2])
# mono$AGG_RANG[i] <- as.character(taxa[taxa$AGG == AG[i],7])
#  }
#csv(mono, paste(tv_home,'species',refl,'monotypic-D.csv',sep='/'))
if(write) write.dbf(mono, paste(tv_home,'species',refl,'monotypic-D.dbf',sep='/')) else mono
}
# head(mono)


