compRefl <- function(refl1, refl2, tv_home, var = 'ABBREVIAT', keyvar = 'SPECIES_NR', p=TRUE, dif=FALSE, Sink=FALSE) {
  if(missing(tv_home)) tv_home <- tv.home()
  df.1 <- read.dbf(paste(tv_home,'Species',refl1,'species.dbf',sep='/'))
  df.2 <- read.dbf(paste(tv_home,'Species',refl2,'species.dbf',sep='/'))
    eva <- 1
    ### Identical Taxon Names with different Taxnr in list 1 and 2?
    df <- merge(df.1,df.2, by=var, all.x=FALSE)
    nomatch1 <- df[as.character(df$SPECIES_NR.x) != as.character(df$SPECIES_NR.y),c('ABBREVIAT','SPECIES_NR.x','SPECIES_NR.y')]
    nomatch1 <- nomatch1[!is.na(nomatch1[,1]),]
    if(nrow(nomatch1)>0) {print(paste(nrow(nomatch1),'taxon names with different numbers'), quote=FALSE); if(p) print(nomatch1)} else eva <- eva - 0.5
    ### Identical Taxon Numbers with different Names in list 1 and 2?
    df <- merge(df.1,df.2, by=keyvar, all.x=FALSE)
    nomatch2 <- df[as.character(df$ABBREVIAT.x) != as.character(df$ABBREVIAT.y),c('SPECIES_NR','ABBREVIAT.x','ABBREVIAT.y')]
    nomatch2 <- nomatch2[!is.na(nomatch2[,1]),]
    if(nrow(nomatch2)>0) {print(paste(nrow(nomatch2),'taxon numbers with different names'), quote=FALSE); if(p) print(nomatch2)} else eva <- eva - 0.5
    
if(eva==0) print('Hurray! All TaxNr <-> TaxName combinations are identical') else print('!!! Reference lists do NOT match !!!')

diff.1 <- as.character(df.1[!df.1[,var] %in% df.2[,var],var])
diff.2 <- as.character(df.2[!df.2[,var] %in% df.1[,var],var])
if(length(diff.1)==0 & length(diff.2)==0) print('Species Lists are identical') else {
if(length(diff.1)>0) {
  print(paste(length(diff.1),'TaxNames of' ,refl1,'not occurring in', refl2), quote=FALSE)
  if(dif) print(diff.1, quote=FALSE)
}
if(length(diff.2)>0) {
  print(paste(length(diff.2),'TaxNames of' ,refl2,'not occurring in', refl1, ':'), quote=FALSE)
  if(dif) print(diff.2, quote=FALSE)
} }
if(Sink) {
  write.table(nomatch1, file='newnr.csv' ,sep=';',row.names=TRUE, col.names=NA)
  write.table(df.1$SPECIES_NR[!df.1$SPECIES_NR %in% df.2$SPECIES_NR],file='lostnr.csv' ,sep=';',row.names=TRUE, col.names=NA)
  tmp.wid = getOption("width")  
  options(width=10000)
  sink('compRefl.txt')
  print(paste(nrow(nomatch1),'taxon names with different numbers'), quote=FALSE)
  print(nomatch1)
  print(paste(nrow(nomatch2),'taxon numbers with different names'), quote=FALSE)
  print(nomatch2)
  options(width=tmp.wid)        # restore linewidth
  print(paste(length(diff.1),'TaxNames of' ,refl1,'not occurring in', refl2), quote=FALSE)
  print(diff.1, quote=FALSE)
  print(paste(length(diff.2),'TaxNames of' ,refl2,'not occurring in', refl1, ':'), quote=FALSE)
  print(diff.2, quote=FALSE)
  sink()
  }
}
