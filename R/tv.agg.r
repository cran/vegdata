tv.agg <- function(nr.member = 'ALL', refl, tv_home, agg=c('AGG','SER','SEC','SSE','SGE','SGR'), ...) {
 if(missing(tv_home)) tv_home <- tv.home()
 taxa <- tax('all', refl=refl, tax=TRUE, syn=FALSE, ...)

 if(nr.member==1) {
  AG <- table(tax$AGG)
  AG <- AG[AG == nr.member]
  AG <- as.integer(names(AG))
mono <- data.frame(AGG_NR = AG, AGG_NAME = tax$AGG_NAME[match(AG,tax$AGG)], AGG_RANG = tax$RANG[match(AG,tax$SPECIES_NR)], MEMBER_NR=tax$SPECIES_NR[match(AG,tax$AGG)], MEMBER_NAME=tax$ABBREVIAT[match(AG,tax$AGG)], MEMBER_RANG=tax$RANG[match(AG,tax$AGG)])
  write.table(mono, paste(tv_home,'species',refl,'monotypic-D.csv',sep='/') ,sep=';',row.names=TRUE, col.names=NA)
  } else {
  AG <- tax[tax$RANG %in% agg & tax$SYNONYM==FALSE, c('SPECIES_NR','ABBREVIAT','AGG','AGG_NAME')]
  o <- order(AG$ABBREVIAT)
  AG <- AG[o,]
  aggregates <- vector("list", nrow(AG))
  names(aggregates) <- AG$ABBREVIAT
  for(i in 1:length(AG$SPECIES_NR))
  aggregates[[i]] <- tax[tax$AGG == AG$SPECIES_NR[i] & !is.na(tax$AGG),1:2]
  if(nr.member !='ALL'){aggregates <- aggregates[lapply(aggregates,nrow)==nr.member]}

  list2file <- function(x, file = paste(paste(tv_home,'species',refl,'',sep='/'), nr.member, paste(agg,collapse=''),'.txt',sep='')) {
     sink(file)                    # redirect output to file
     print(x)                      # print the object
     sink()                        # cancel redirection
     return(invisible(NULL))       # return (nothing) from function
  }
  list2file(aggregates)
  }
  }
