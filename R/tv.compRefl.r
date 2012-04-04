tv.compRefl <- function (refl1, refl2, tv_home, check.nr = FALSE, verbose = FALSE, Sink = TRUE, new = FALSE, ...) 
{
    Names = "ABBREVIAT"
    Numbers = "SPECIES_NR"
    if (missing(tv_home)) 
        tv_home <- tv.home()
    refl.1 <- if(is.character(refl1)) read.dbf(paste(tv_home, "Species", refl1, "species.dbf", sep = "/")) else refl1
    refl.2 <- if(is.character(refl2)) read.dbf(paste(tv_home, "Species", refl2, "species.dbf", sep = "/")) else refl2
    refl1 <- deparse(substitute(refl1))
    refl2 <- deparse(substitute(refl2))

    refl.1[, Names] <- sub.abbr(refl.1[, Names])
    refl.2[, Names] <- sub.abbr(refl.2[, Names])

    diff.1 <- sort(as.character(refl.1[!refl.1[, Names] %in% refl.2[, Names], Names]))
    diff.2 <- sort(as.character(refl.2[!refl.2[, Names] %in% refl.1[, Names], Names]))
    if (check.nr) {
        df <- merge(refl.1, refl.2, by = Names, all.x = FALSE)
        nomatch1 <- df[as.character(df$SPECIES_NR.x) != as.character(df$SPECIES_NR.y), c("ABBREVIAT", "SPECIES_NR.x", "SPECIES_NR.y")]
        nomatch1 <- nomatch1[!is.na(nomatch1[, 1]), ]
        nomatch1 <- nomatch1[order(nomatch1[, 2]), ]
        df <- merge(refl.1, refl.2, by = Numbers, all.x = FALSE)
        nomatch2 <- df[as.character(df$ABBREVIAT.x) != as.character(df$ABBREVIAT.y), c("SPECIES_NR", "ABBREVIAT.x", "ABBREVIAT.y")]
        nomatch2 <- nomatch2[!is.na(nomatch2[, 1]), ]
        nomatch2 <- nomatch2[order(nomatch2[, 2]), ]
        if (nrow(nomatch1) == 0 & nrow(nomatch2) == 0) 
            cat("\n Hurray! All TaxNr <-> TaxName combinations are identical. Species lists are identical or can be used as combined list. \n")
        else cat("\n###############################################\n!!! Reference lists might be not compatible !!!\n###############################################\n")
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
        reflmerge <- merge(refl.1, refl.2, by = Names, all = TRUE)
        refl <- reflmerge[is.na(reflmerge$SPECIES_NR.x) | is.na(reflmerge$SPECIES_NR.y),]
        combnames <- reflmerge$ABBREVIAT
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
        options(width = 10000)
        sink("compRefl.txt")
        if (check.nr) {
            print(paste(nrow(nomatch1), "taxon names with different numbers"), 
                quote = FALSE)
            print(nomatch1, row.names=FALSE)
            print(paste(nrow(nomatch2), "taxon numbers with different names"), 
                quote = FALSE)
            print(nomatch2, row.names=FALSE)
        }
        options(width = tmp.wid)
        cat('\n', length(diff.1), "TaxNames of", refl1, "not occurring in", refl2, ':\n')
        print(diff.1, row.names=FALSE)
        cat('\n', length(diff.2), "TaxNames of", refl2, "not occurring in", refl1, ":\n")
        print(diff.2, row.names=FALSE)
        sink()
        cat("\n Report is written to file \"comprefl.txt\" \n")
    }
    if (new) {
        comb <- rbind(refl.1, refl.2[refl.2$ABBREVIAT %in% diff.2, 
            ])
        comb$Attention <- comb$ABBREVIAT %in% auct | comb$ABBREVIAT %in% 
            ext
        cat("\n New names in refl2 added to refl1. Reference list \"combrefl\" saved in TURBOVEG species directory. Please check for critical species names before use. \n")
        dir.create(file.path(tv_home, "/Species/combrefl"), showWarnings = FALSE)
        write.dbf(comb, file.path(tv_home, "/Species/combrefl/species.dbf"))
    }
}
