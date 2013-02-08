tv.compRefl <- function (refl1, refl2, tv_home, check.nr = FALSE, verbose = FALSE, Sink = TRUE, filter.1, filter.2, new = FALSE, file="compRefl.txt", ...)  {
  if (missing(tv_home)) 
      tv_home <- tv.home()
    refl.1 <- if(is.character(refl1)) read.dbf(file.path(tv_home, "Species", refl1, "species.dbf")) else refl1
    refl.2 <- if(is.character(refl2)) read.dbf(file.path(tv_home, "Species", refl2, "species.dbf")) else refl2
    names(refl.1) <- TCS.replace(names(refl.1))
    names(refl.2) <- TCS.replace(names(refl.2))
    refl1 <- deparse(substitute(refl1))
    refl2 <- deparse(substitute(refl2))
    refl.1[, "TaxonName"] <- taxname.abbr(refl.1[, "TaxonName"])
    refl.2[, "TaxonName"] <- taxname.abbr(refl.2[, "TaxonName"])
    diff.1 <- sort(as.character(refl.1[!refl.1[, "TaxonName"] %in% refl.2[, "TaxonName"], "TaxonName"]))
    diff.2 <- sort(as.character(refl.2[!refl.2[, "TaxonName"] %in% refl.1[, "TaxonName"], "TaxonName"]))
    if (check.nr) {
      df <- merge(refl.1, refl.2, by = "TaxonName", all.x = FALSE)
      selectedcolumns <- if('EDITSTATUS' %in% names(df)) c("TaxonName", "TaxonUsageID.x", "TaxonUsageID.y", 'EDITSTATUS') else c("TaxonName", "TaxonUsageID.x", "TaxonUsageID.y")
      nomatch1 <- df[as.character(df$TaxonUsageID.x) != as.character(df$TaxonUsageID.y), selectedcolumns]
      nomatch1 <- nomatch1[!is.na(nomatch1[, 1]), ]
      
      nomatch1 <-  if('EDITSTATUS' %in% names(df)) nomatch1[order(nomatch1$EDITSTATUS, nomatch1[, 2]), ] else nomatch1[order(nomatch1[, 2]), ]
      df <- merge(refl.1, refl.2, by = 'TaxonUsageID', all.x = FALSE)
      selectedcolumns <- if('EDITSTATUS' %in% names(df)) c("TaxonUsageID", 'TaxonName.x', "TaxonName.y", 'EDITSTATUS') else c("TaxonUsageID", 'TaxonName.x', "TaxonName.y")
      nomatch2 <- df[df$TaxonName.x != df$TaxonName.y, selectedcolumns]
      nomatch2 <- nomatch2[!is.na(nomatch2[, 1]), ]
      nomatch2 <- if('EDITSTATUS' %in% names(df)) nomatch2[order(nomatch2[, 'EDITSTATUS'], nomatch2[, 2]), ] else  nomatch2 <- nomatch2[order(nomatch2[, 2]), ]
      if (nrow(nomatch1) == 0 & nrow(nomatch2) == 0) 
            cat("\n Hurray! All TaxNr <-> TaxName combinations are identical. Species lists are identical or can be used as combined list. \n")
        else cat("\n###############################################\n!!! Reference lists are not identical !!!\n###############################################\n")
        if (nrow(nomatch1) > 0) {
            cat("\n", nrow(nomatch1), "identical taxon names with different numbers \n")
            if (verbose) 
                print(nomatch1, row.names=FALSE)
        }
        if (nrow(nomatch2) > 0) {
            cat("\n", nrow(nomatch2), "identical taxon numbers with different names \n")
            if (verbose) 
                print(nomatch2, row.names=FALSE)
        }
    }
    else {
        reflmerge <- merge(refl.1, refl.2, by = "TaxonName", all = TRUE)
#        refl <- reflmerge[is.na(reflmerge$TaxonUsageID.x) | is.na(reflmerge$TaxonUsageID.y),]
        combnames <- reflmerge$TaxonName
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
            cat("\n", "Warning: Critical names in dataset, please check", "\n")
            print(ext, row.names=FALSE)
        }
    }
    if (length(diff.1) == 0 & length(diff.2) == 0) 
        cat("\n Species names are identical \n")
    else {
        if (length(diff.1) > 0) {
	  
            cat("\n", length(diff.1), "TaxNames of", refl1, "not occurring in", refl2, "\n")
            if (verbose) 
                print(diff.1, quote = FALSE, row.names=FALSE)
        }
        if (length(diff.2) > 0) {
            cat("\n", length(diff.2), "TaxNames of", refl2, "not occurring in", refl1, ": \n")
            if (verbose) 
                print(diff.2, quote = FALSE, row.names=FALSE)
        }
    }
    if (Sink) {
        tmp.wid = getOption("width")
        options(width = 5000)
        sink(file)
        print(paste(".x =", refl1, ".y =", refl2))
        if (check.nr) {
            print(paste(nrow(nomatch1), "taxon names with different numbers"), quote = FALSE)
           print(nomatch1, row.names=FALSE)
           print(paste(nrow(nomatch2), "taxon numbers with different names"), quote = FALSE)
           print(nomatch2, row.names=FALSE)
        }
        options(width = tmp.wid)
        if(!missing(filter.1)) for(i in 1:length(filter.1)) diff.1 <- diff.1[!grepl(filter.1[i], diff.1)]
        cat('\n', length(diff.1), "TaxNames of", refl1, "not occurring in", refl2, ':\n')
        print(paste(diff.1, collapse = ', '))
        if(!missing(filter.2)) for(i in 1:length(filter.2)) diff.2 <- diff.2[!grepl(filter.2[i], diff.2)]
        cat('\n', length(diff.2), "TaxNames of", refl2, "not occurring in", refl1, ":\n")
        print(paste(diff.2, collapse = ', '))
        sink()
        cat("\n Report is written to file \"", file, " \n")
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
      comb <- rbind(refl.1[,inter], refl.2[refl.2$TaxonName %in% diff.2, inter])
      comb$Attention <- comb$ABBREVIAT %in% auct | comb$ABBREVIAT %in% ext
      cat("\n New names in refl2 added to refl1. Reference list \"combrefl\" saved in TURBOVEG species directory. Please check for critical species names before use. \n")
      dir.create(file.path(tv_home, "/Species/combrefl"), showWarnings = TRUE)
      write.dbf(comb, file.path(tv_home, "/Species/combrefl/species.dbf"))
    }
}
