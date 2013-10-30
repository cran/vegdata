tv.compRefl <- function (refl1, refl2, tv_home, check.nr = FALSE, verbose = FALSE, Sink = TRUE, filter.1, filter.2, new = FALSE, file="compRefl.txt", ...)  {
  if (missing(tv_home)) tv_home <- tv.home()
    refl.1 <- if(is.character(refl1)) read.dbf(file.path(tv_home, "Species", refl1, "tax.dbf")) else refl1
    refl.2 <- if(is.character(refl2)) read.dbf(file.path(tv_home, "Species", refl2, "species.dbf")) else refl2
    names(refl.1) <- TCS.replace(names(refl.1))
    names(refl.2) <- TCS.replace(names(refl.2))
    refl1 <- deparse(substitute(refl1))
    refl2 <- deparse(substitute(refl2))
    refl.1[, "TaxonName"] <- taxname.abbr(refl.1[, "TaxonName"])
    refl.2[, "TaxonName"] <- taxname.abbr(refl.2[, "TaxonName"])
  diff.A <- sort(as.character(refl.2[!refl.2[, "TaxonName"] %in% refl.1[, "TaxonName"], "TaxonName"]))
  diff.B <- sort(as.character(refl.1[!refl.1[, "TaxonName"] %in% refl.2[, "TaxonName"], "TaxonName"]))
    if (check.nr) {
      merged.df <- merge(refl.1, refl.2, by = "TaxonName", all.x = FALSE)
      selectedcolumns <- if('EDITSTATUS' %in% names(merged.df)) c("TaxonName", "TaxonUsageID.x", "TaxonUsageID.y", 'BEGRUEND', 'EDITSTATUS') else c("TaxonName", "TaxonUsageID.x", "TaxonUsageID.y")
      nonmatchingNumbers <- merged.df[as.character(merged.df$TaxonUsageID.x) != as.character(merged.df$TaxonUsageID.y), selectedcolumns]
      nonmatchingNumbers <- nonmatchingNumbers[!is.na(nonmatchingNumbers[, 1]), ]
      #   nonmatchingNumbers <-  if('EDITSTATUS' %in% names(merged.df)) nonmatchingNumbers[order(nonmatchingNumbers$EDITSTATUS, nonmatchingNumbers[, 2]), ] else nonmatchingNumbers[order(nonmatchingNumbers[, 2]), ]
    }
      merged.df <- merge(refl.1, refl.2, by = 'TaxonUsageID', all.x = FALSE)
      selectedcolumns <- if('EDITSTATUS' %in% names(merged.df)) c("TaxonUsageID", 'TaxonName.x', "TaxonName.y", 'BEGRUEND', 'EDITSTATUS') else c("TaxonUsageID", 'TaxonName.x', "TaxonName.y")
      nonmatchingNames <- merged.df[merged.df$TaxonName.x != merged.df$TaxonName.y, selectedcolumns]
      nonmatchingNames <- nonmatchingNames[!is.na(nonmatchingNames[, 1]), ]
      nonmatchingNames <- if('EDITSTATUS' %in% names(merged.df)) nonmatchingNames[order(nonmatchingNames[, 'EDITSTATUS'], nonmatchingNames[, 2]), ] else  nonmatchingNames <- nonmatchingNames[order(nonmatchingNames[, 2]), ]

  if (check.nr) {
    if (nrow(nonmatchingNumbers) == 0 & nrow(nonmatchingNames) == 0) 
            cat("\n Hurray! All TaxNr <-> TaxName combinations are identical. Species lists are identical or can be used as a combined list. \n")
        else cat("\n###############################################\n!!! Reference lists are not congruent !!!\n###############################################\n")
        if (nrow(nonmatchingNumbers) > 0) {
            cat("\n", nrow(nonmatchingNumbers), "identical taxon names with different numbers \n")
            if (verbose) 
                print(nonmatchingNumbers, row.names=FALSE)
        }
  }
        if (nrow(nonmatchingNames) > 0) {
            cat("\n", nrow(nonmatchingNames), "identical taxon numbers with different names \n")
            if (verbose) 
                print(nonmatchingNames, row.names=FALSE)
        }

        reflmerge <- merge(refl.1, refl.2, by = "TaxonName", all = TRUE)
#        refl <- reflmerge[is.na(reflmerge$TaxonUsageID.x) | is.na(reflmerge$TaxonUsageID.y),]
        combnames <- reflmerge$TaxonName # cat(reflmerge$TaxonName, ' ', reflmerge$Author)
        auct <- data.frame(Taxname = sort(grep("auct.", combnames, value = TRUE, fixed = TRUE, useBytes = TRUE)))
        auct$to_check_against <- sub(" auct.", "", auct$Taxname)
        if (nrow(auct) > 0) {
            cat("\n", "Warning: Critical Pseudonyms in dataset, please check","\n")
            print(auct, row.names=FALSE)
        }
        sl <- data.frame(Taxname = sort(grep("s. l.", combnames, value = TRUE, fixed = TRUE, useBytes = TRUE)))
        sl$to_check_against <- sub(" s. l.", "", sl$Taxname)
        sstr <- data.frame(Taxname = sort(grep("s. str.", combnames, value = TRUE, fixed = TRUE, useBytes = TRUE)))
        sstr$to_check_against <- sub(" s. str.", "", sstr$Taxname)
        ext <- rbind(sl, sstr)
        if (nrow(ext) > 0) {
            cat("\n", "Warning: Critical names/concepts in the lists, please check", "\n")
            print(ext, row.names=FALSE)
        }
  
    if(length(diff.B) == 0 & length(diff.A) == 0)
        cat("\n Species names are identical \n")
    else {
      if(!missing(filter.1)) diff.B <- diff.B[!diff.B %in% filter.1]
      if (length(diff.B) > 0) {
            cat("\n", length(diff.B), "TaxNames of", refl1, "not occurring in", refl2, "\n")
            if (verbose) 
                print(diff.B, quote = FALSE, row.names=FALSE)
      }
      if(!missing(filter.2)) {
        diff.A <- diff.A[!diff.A %in% filter.2]
      }
    
      if (length(diff.A) > 0) {
          cat("\n", length(diff.A), "TaxNames of", refl2, "not occurring in", refl1, ": \n")
          if (verbose) 
              print(diff.A, quote = FALSE, row.names=FALSE)
      }
    }
    if (Sink) {
        tmp.wid = getOption("width")
        options(width = 5000)
        sink(file)
        print(paste(".x =", refl1, ".y =", refl2))
        if (check.nr) {
           print(paste(nrow(nonmatchingNumbers), "taxon names with different numbers"), quote = FALSE)
           print(nonmatchingNumbers, row.names=FALSE)
#         write.csv(cbind(nonmatchingNumbers, refl.1[match(nonmatchingNumbers[,1], refl.1$TaxonName), c("BEGRUEND","EDITSTATUS")]), file='differentNumbers.csv')
           print(paste(nrow(nonmatchingNames), "taxon numbers with different names"), quote = FALSE)
           print(nonmatchingNames, row.names=FALSE)
        }
        options(width = tmp.wid)
        cat('\n', length(diff.B), "TaxNames of", refl1, "not occurring in", refl2, ':\n')
        print(paste(diff.B, collapse = ', '))
        cat('\n', length(diff.A), "TaxNames of", refl2, "not occurring in", refl1, ":\n")
        print(paste(diff.A, collapse = ', '))
        sink()
        cat("\n Report is written to file \"", file, " \n")
        if (check.nr) write.csv(nonmatchingNumbers, file='differentNumbers.csv')
        if(!missing(filter.1)) write.csv(nonmatchingNames[!nonmatchingNames[,2] %in% filter.1,], file='differentNames.csv')
        write.csv(diff.B[!diff.B %in% nonmatchingNames], file='noMatches_inRefl_2.csv')
        write.csv(diff.A[!diff.A %in% nonmatchingNames], file='noMatches_inRefl_1.csv')
    }
    if (new) {
      names(refl.1) <- replace(names(refl.1), names(refl.1)=='TaxonName','ABBREVIAT')
      names(refl.1) <- replace(names(refl.1), names(refl.1)=='TaxonUsageID','SPECIES_NR')
      names(refl.1) <- replace(names(refl.1), names(refl.1)=='TaxonConcept','VALID_NAME')
      names(refl.1) <- replace(names(refl.1), names(refl.1)=='TaxonConceptID','VALID_NR')
      names(refl.1) <- replace(names(refl.1), names(refl.1)=='IsChildTaxonOf','AGG_NAME')
      names(refl.1) <- replace(names(refl.1), names(refl.1)=='VernacularName','NATIVENAME')

      names(refl.2) <- replace(names(refl.2), names(refl.2)=='TaxonName','ABBREVIAT')
      names(refl.2) <- replace(names(refl.2), names(refl.2)=='TaxonUsageID','SPECIES_NR')
      names(refl.2) <- replace(names(refl.2), names(refl.2)=='TaxonConcept','VALID_NAME')
      names(refl.2) <- replace(names(refl.2), names(refl.2)=='TaxonConceptID','VALID_NR')
      names(refl.2) <- replace(names(refl.2), names(refl.2)=='IsChildTaxonOf','AGG_NAME')
      names(refl.2) <- replace(names(refl.2), names(refl.2)=='VernacularName','NATIVENAME')
      
      inter <- intersect(names(refl.1),names(refl.2))
      comb <- rbind(refl.1[,inter], refl.2[refl.2$TaxonName %in% diff.A, inter])
      comb$Attention <- comb$ABBREVIAT %in% auct | comb$ABBREVIAT %in% ext
      cat("\n New names in refl2 added to refl1. Reference list \"combrefl\" saved in TURBOVEG species directory. Please check for critical species names before use. \n")
      dir.create(file.path(tv_home, "/Species/combrefl"), showWarnings = TRUE)
      write.dbf(comb, file.path(tv_home, "/Species/combrefl/species.dbf"))
    }
}
