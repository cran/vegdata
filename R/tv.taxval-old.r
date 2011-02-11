tv.taxval <- function (db, obs, refl, tv_home, concept, syn = c("adapt", "conflict", 
    "preserve"), subdiv = c("conflict", "adapt", "preserve"), 
    ag = c("conflict", "adapt", "preserve"), mono = c("lower", 
        "higher", "all", "preserve"), monolist = "monotypic-D", 
    uncertain = NULL, genus = c("preserve", "delete"), quiet = FALSE, 
    sysPath = FALSE, ...) 
{
    syn <- match.arg(syn)
    subdiv <- match.arg(subdiv)
    ag <- match.arg(ag)
    mono <- match.arg(mono)
    genus <- match.arg(genus)
    if (missing(tv_home)) 
        tv_home <- tv.home(sysPath)
    if (missing(obs)) 
        obs <- tv.obs(db, tv_home)
    cat("Original number of taxa:", length(unique(obs$SPECIES_NR)), 
        "\n")
    if (missing(refl)) 
        refl <- tv.refl(db[1], tv_home)
    species <- tax("all", refl = refl, tax = TRUE, sysPath = sysPath, 
        ...)
    adapt <- function(expr, mess, column, column2 = c("SPECIES_NR", 
        "ABBREVIAT", "Freq_Member", "AGG", "AGG_NAME", "Freq_Agg")) {
        obsspec <- unique(obs$SPECIES_NR)
        species$AGG_RANG <- species$RANG[match(species$AGG, species$SPECIES_NR)]
        temp <- species[eval(expr) & species$SPECIES_NR %in% 
            obsspec, ]
        if (column == "AGG") 
            temp[, c("AGG", "AGG_NAME")] <- species[match(temp$VALID_NR, 
                species$SPECIES_NR), c("AGG", "AGG_NAME")]
        vec <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR), column]
        if (sum(vec > 0, na.rm = TRUE) > 0) {
            obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(vec > 
                0), vec[!is.na(vec)])
            temp$Freq_Member <- fr[match(temp$SPECIES_NR, fr[, 
                1]), 2]
            temp$Freq_Member[is.na(temp$Freq_Member)] <- 0
            temp$Freq_Agg <- fr[match(temp$AGG, fr[, 1]), 2]
            temp$Freq_Agg[is.na(temp$Freq_Agg)] <- 0
            cat("\n", nrow(temp), mess, "found in dataset, adapted \n")
            if (!quiet) 
                print(temp[, column2])
        }
        else cat("No", mess, "to adapt. \n")
        obs
    }
    confl <- function(expr, column, column2 = c("SPECIES_NR", 
        "ABBREVIAT", "Freq_Member", "AGG", "AGG_NAME", "Freq_Agg"), 
        mess, comb = "at higher level") {
        obsspec <- unique(obs$SPECIES_NR)
        tem <- species[eval(expr) & species$SPECIES_NR %in% obsspec, 
            ]
        temp <- tem[tem[[column]] %in% obsspec, ]
        tmp <- temp[match(obs$SPECIES_NR, temp$SPECIES_NR), column]
        if (nrow(temp) > 0) {
            obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 
                0), tmp[!is.na(tmp)])
            if (!quiet) {
                temp$Freq_Member <- fr[match(temp$SPECIES_NR, 
                  fr[, 1]), 2]
                temp$Freq_Agg <- fr[match(temp[, column], fr[, 
                  1]), 2]
                cat("\n", nrow(temp), mess, "found also", comb, 
                  "in dataset, combined", comb, "\n")
                print(temp[, column2])
            }
        }
        else if (!quiet) 
            cat("\n No conflicting", mess, ". \n")
        obs
    }
    preserve <- function(mess) {
        cat("\n", mess, "preserved! \n")
        obs
    }
    unc <- function(obs, column, uncrow, i, ...) {
        un <- match.arg(as.character(uncrow[[2]]), c("aggregate", 
            "preserve", "ignore"))
        if (un == "aggregate") {
            cat("\n changing species occurrences to coarser level for uncertainty level ", 
                as.character(uncrow[[1]]))
            sp <- obs$SPECIES_NR[obs[, column] == uncrow[[1]]]
            taxa <- species[species$SPECIES_NR %in% sp, ]
            taxa$AGG <- species$AGG[match(taxa$VALID_NR, species$SPECIES_NR)]
            for (n in 1:nrow(taxa)) obs$SPECIES_NR[obs$SPECIES_NR == 
                taxa[n, "SPECIES_NR"] & obs[, column] == uncrow[[1]]] <- taxa[n, 
                "AGG"]
        }
        if (un %in% c("preserve", "ignore")) {
            cat("\n preserving species occurrences of uncertainty level ", 
                as.character(uncrow[[1]]))
        }
        obs
    }
    fr <- as.data.frame(table(obs$SPECIES_NR))
    obs <- switch(syn, conflict = confl(expr = expression(species$SYNONYM == 
        TRUE), column = "VALID_NR", column2 = c("SPECIES_NR", 
        "ABBREVIAT", "Freq_Member", "VALID_NR", "VALID_NAME", 
        "Freq_Agg"), mess = "Synonyms", comb = "as standard taxa"), 
        adapt = adapt(expr = expression(species$SYNONYM == TRUE), 
            column = "VALID_NR", column2 = c("SPECIES_NR", "ABBREVIAT", 
                "Freq_Member", "VALID_NR", "VALID_NAME", "Freq_Agg"), 
            mess = "Synonyms"), preserve = preserve("Synonyms"), 
        )
    fr <- as.data.frame(table(obs$SPECIES_NR))
    obs <- switch(subdiv, conflict = confl(expr = expression(species$RANG %in% 
        c("FOR", "VAR", "SGR", "SSP", "ZUS")), column = "AGG", 
        mess = "Variants, forms, subspecies etc."), adapt = adapt(expr = expression(species$RANG %in% 
        c("FOR", "VAR", "SGR", "SSP", "ZUS")), column = "AGG", 
        mess = "Variants, forms, subspecies etc."), preserve = preserve("Variants, forms, subspecies etc."), 
        )
    fr <- as.data.frame(table(obs$SPECIES_NR))
    obs <- switch(ag, preserve = preserve("Aggregates"), conflict = {
        obsspec <- unique(obs$SPECIES_NR)
        AG <- species$SPECIES_NR[species$RANG %in% c("AGG", "AG2", 
            "SEC", "SGE", "SER", "SSE") & species$SYNONYM == 
            FALSE]
        agg.member <- species[species$AGG %in% AG & species$SPECIES_NR %in% 
            obsspec, c("SPECIES_NR", "ABBREVIAT", "AGG", "AGG_NAME")]
        conf <- agg.member[agg.member$AGG %in% obsspec, ]
        conf$Freq_Member <- fr[match(conf$SPECIES_NR, fr[, 1]), 
            2]
        conf$Freq_Agg <- fr[match(conf$AGG, fr[, 1]), 2]
        conf <- conf[, c(1, 2, 5, 3, 4, 6)]
        if (nrow(conf) == 0) 
            cat("\n No conflicting aggregates found.\n")
        else {
            cat("\n", nrow(conf), "members of occurring aggregates in dataset, aggregated: \n")
            if (!quiet) 
                print(conf)
            obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(!is.na(match(obs$SPECIES_NR, 
                conf$SPECIES_NR))), conf$AGG[match(obs$SPECIES_NR, 
                conf$SPECIES_NR)][!is.na(match(obs$SPECIES_NR, 
                conf$SPECIES_NR))])
        }
        obs
    }, adapt = adapt(expr = expression(species$AGG_RANG == "AGG"), 
        column = "AGG", column2 = c("SPECIES_NR", "ABBREVIAT", 
            "Freq_Member", "AGG", "AGG_NAME", "Freq_Agg"), mess = "Aggregates"), 
        )
    if (mono %in% c("lower", "higher", "all")) {
        obsspec <- unique(obs$SPECIES_NR)
        if (file.access(paste(tv_home, "Species", refl, paste(monolist, 
            "dbf", sep = "."), sep = "/"))) 
            warning("List of monotypic taxa not available!")
        else Mono <- read.dbf(paste(tv_home, "Species", refl, 
            paste(monolist, "dbf", sep = "."), sep = "/"))
        if (mono == "lower") 
            tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, Mono$AGG_NR)]
        if (mono == "all") 
            tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
        if (mono == "higher") {
            tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
            tmp[!Mono$MEMB_RANG[match(tmp, Mono$AGG_NR)] %in% 
                c("FOR", "VAR", "ZUS", "SSP")] <- NA
        }
        if (sum(tmp > 0, na.rm = TRUE) > 0) {
            repeat {
                obs$SPECIES_NR <- replace(obs$SPECIES_NR, which(tmp > 
                  0), tmp[!is.na(tmp)])
                cat(paste("\n", length(unique(tmp[!is.na(tmp)])), 
                  "Monotypic taxa found in dataset, species converted to", 
                  mono, "rank.", "\n"))
                if (!quiet) 
                  print(Mono[Mono$AGG_NR %in% obsspec, ])
                if (mono == "lower") 
                  tmp <- Mono$MEMBER_NR[match(obs$SPECIES_NR, 
                    Mono$AGG_NR)]
                if (mono == "all") 
                  tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
                if (mono == "higher") {
                  tmp <- Mono$AGG_NR[match(obs$SPECIES_NR, Mono$MEMBER_NR)]
                  tmp[!Mono$MEMB_RANG[match(tmp, Mono$AGG_NR)] %in% 
                    c("FOR", "VAR", "ZUS", "SSP")] <- NA
                }
                if (sum(tmp > 0, na.rm = TRUE) == 0) 
                  break
                obsspec <- unique(obs$SPECIES_NR)
            }
        }
        else if (mono == "higher") 
            cat("\n No monotypic taxa below species level found. \n\n")
        else cat("\n No monotypic taxa found. \n\n")
    }
    else cat("\n Monotypic taxa preserved! \n\n")
    if (!is.null(uncertain)) {
        cat("\n Frequency of uncertainty levels")
        print(table(obs[, uncertain[[1]]]))
        species <- tax("all", refl = refl, tax = TRUE, tv_home = tv_home, 
            sysPath = sysPath, ...)
        for (i in 1:nrow(uncertain[[2]])) obs <- unc(obs, uncertain[[1]], 
            uncertain[[2]][i, ])
        cat("\n")
    }
    obsspec <- unique(obs$SPECIES_NR)
    gat <- species[species$RANG %in% c("GAT", "FAM", "UKL", "ORD", 
        "ABT", "KLA", "ROOT", "UAB"), c("SPECIES_NR", "ABBREVIAT")]
    if (any(gat$SPECIES_NR %in% obsspec)) {
        if (genus == "delete") {
            cat("\n", sum(gat$SPECIES_NR %in% obsspec), "Undetermined genera or above found in dataset, deleted:", 
                "\n")
            if (!quiet) 
                print(gat[gat$SPECIES_NR %in% obsspec, ])
            obs <- obs[!obs$SPECIES_NR %in% gat$SPECIES_NR, ]
        }
        else cat("\n Undetermined genera and above preserved! \n")
    }
    else cat("\n", "No undetermined genera or above found.", 
        "\n")
    cat("\n Number of taxa after validation:", length(unique(obs$SPECIES_NR)), 
        "\n\n")
    auct <- species[grep(" auct.", species$ABBREVIAT, perl = TRUE), 
        c(1:5, 11, 13, 14, 15)]
    auct$to_check <- sub(" auct.", "", auct$ABBREVIAT, perl = TRUE)
    auct$checknr <- species$SPECIES_NR[match(auct$to_check, species$ABBREVIAT)]
    auct <- auct[!is.na(auct$checknr), ]
    auct <- auct[, c(10, 11, 2, 1, 5, 4, 6)]
    names(auct)[3] <- "check against"
    if (any(obs$SPECIES_NR %in% auct$checknr)) {
        cat("\n", "Warning: Critical Pseudonym(s) in dataset, please check", 
            "\n")
        u <- unique(obs$SPECIES_NR)
        if (!quiet) 
            print(auct[match(u, auct$checknr, nomatch = FALSE), 
                ])
    }
    sl <- species[grep(" s. l.$", species$ABBREVIAT, perl = TRUE), 
        c(1:5, 11, 13, 14, 15)]
    sl$to_check <- sub(" s. l.$", "", sl$ABBREVIAT, perl = TRUE)
    sstr <- species[grep(" s. str.$", species$ABBREVIAT, perl = TRUE), 
        c(1:5, 11, 13, 14, 15)]
    sstr$to_check <- sub(" s. str.$", "", sstr$ABBREVIAT, perl = TRUE)
    ext <- rbind(sl, sstr)
    ext$checknr <- species$SPECIES_NR[match(ext$to_check, species$ABBREVIAT)]
    ext <- ext[!is.na(ext$checknr), c(10, 11, 2, 1, 5, 4, 6)]
    names(ext)[3] <- "check against"
    if (any(obs$SPECIES_NR %in% ext$checknr)) {
        cat("\n", "Warning: Critical species in dataset, please check", 
            "\n")
        u <- ext[match(unique(obs$SPECIES_NR), ext$checknr, nomatch = FALSE), 
            ]
        if (!quiet) 
            print(u[order(u$to_check), ])
    }
    obs
}
