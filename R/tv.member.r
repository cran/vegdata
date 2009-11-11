tv.member <- function(x, tv_home, refl='GermanSL 1.1', syn = FALSE, ...) {
  if(missing(tv_home)) tv_home <- tv.home()
  tax <- read.dbf(paste(tv_home, 'Species', refl, 'tax.dbf',sep='/'))
  if(is.numeric(x)) a <- tax$SPECIES[tax$AGG %in% x]
  if(is.character(x) & nchar(x)[1]== 7) 
        if(syn) l <- tax[tax$LETTERCODE %in% x,] else
                l <- tax[tax$LETTERCODE %in% x & tax$SYNONYM == FALSE,]
  tmp <- tax[tax$SPECIES_NR %in% a,c(1:7)]
  print(paste('Aggregate: ',tax$ABBREVIAT[tax$SPECIES_NR == x]))
  print('Members:')
  tmp
}
   
   
# 4223