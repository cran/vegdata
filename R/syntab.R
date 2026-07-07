#' Syntaxonomic frequency tables
#'
#' @rdname syntab
#' @aliases syntab print.syntab
#'
#' @description Calculate and display relative or absolute frequency tables with or without use of function multipatt from package indicspecies
#'
#' @param veg Vegetation dataframe
#' @param clust Vector with cluster information with length equal to number of rows of veg
#' @param type Relative (species or type) or absolute frequency, mean species response values or strength of association.
#' @param mupa Either logical for (not) using multipatt from package indicspecies to detect significance of cluster association strength or supply output from previous use of multipatt.
#' @param x Object from function syntab
#' @param zero.print Replacement for zero values.
#' @param trait Optional vector of trait values to be plotted behind the species.
#' @param limit Minimum value to display.
#' @param minstat Minimal indicator value
#' @param alpha Significance threshold.
#' @param dec Number of decimals in result.
#' @param refl Name of Turboveg taxonomic reference list to use for fullnames.
#' @param ... additional arguments
#'
#' @seealso Package indicspecies with function \link[indicspecies]{multipatt} for indicator species analysis along multiple cluster combinations
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @importFrom data.table .N ":="
#' @importFrom forcats as_factor fct_count
#'
#' @examples
#' \dontrun{
#'  elbaue <- tv.veg('elbaue')
#'  elbaue.env <- tv.site('elbaue')
#'  clust <- vector('integer', nrow(elbaue.env))
#'  clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1
#'  clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2
#'  clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3
#'  clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4
#'  levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')
#'  st <- syntab(elbaue, clust, mupa=TRUE)
#'  traits <- tv.traits()
#'  m <- match(rownames(st$syntab), traits$LETTERCODE, nomatch = 0)
#'  trait <- traits[m, c("OEK_F","OEK_N")]
#'  rownames(trait) <- traits$LETTERCODE[m]
#'  print(st, limit=30, trait=trait)
#'  #'  Configure the syntaxonomic table
#'  sttable <- st$syntab
#'  sttable <- sttable[sttable$p.value < 0.05 & !is.na(sttable$p.value),
#'  !names(sttable) %in% c('stat')]
#'  taxa <- tax(rownames(sttable))
#'  rownames(sttable) <- taxa[match(rownames(sttable), taxa$LETTERCODE, nomatch = 0),'TaxonName']
#'  write.csv(sttable, 'sttable.csv')
#' }
#'

#' @export
"syntab" <- function(veg, clust, type = c('rel','abs','mean.cover'), mupa, dec = 0, refl, ...) UseMethod("syntab")

#' @export
syntab.veg <- function (veg, clust, type = c('rel','abs','mean.cover'), mupa = FALSE, dec = 0, refl, ...) {
    type <- match.arg(type)
    clust <- if (missing(clust))
      as_factor(sample(c('one','two','three'), size = nrow(veg), replace = TRUE)) else
      as_factor(clust)
    if(!missing('refl'))
      if('veg' %in% class(veg)) refl <- attr(veg, 'taxreflist') else
        tax.refl(refl)
    refl = tax.refl()

    ncl <- fct_count(clust)
    cat(' Number of clusters: ', nrow(ncl), '\n')
    nb.rel.clust <- as.numeric(table(clust))
    cat(' Cluster frequency', nb.rel.clust,'\n')
    if(any(nb.rel.clust == 0)) stop("All cluster levels must be represented by plots.")

    if(any(colSums(veg)==0)) stop('Some species without occurrence.')
    sp.veg <- split(veg, clust, drop=FALSE)
    if(type=='rel') {
    	tmp <- lapply(sp.veg, function(x) colSums(x > 0))
    	temp <- vector('list', length = nrow(ncl))
	    for(i in 1:length(nb.rel.clust))
	      temp[[i]] <- round(tmp[[i]] / nb.rel.clust[i] * 100, dec)
    } else
    if(type=='mean.cover') {
      temp <- lapply(sp.veg, function(x) {x[x==0] <- NA; round(colMeans(x, na.rm=TRUE),dec)})
      is.na(temp) <- 0
    } else
    if(type=='abs')  temp <- lapply(sp.veg, function(x) colSums(x > 0))

    df <- as.data.frame(temp)
    names(df) <- names(sp.veg)

  # Multipatt analysis
    if(is.logical(mupa)) {
      if(mupa) {
          requireNamespace("indicspecies", quietly = TRUE)
          mu <- indicspecies::multipatt(veg, clust, ...)
          mu$sign[, 1:nrow(ncl)] <- df
          df <- mu$sign
          names(df) <- gsub('s\\.', '', names(df))
        }
      } else
          if(inherits(mupa, 'multipatt')) {
            df <- mupa$sign
            names(df) <- gsub('s\\.', '', names(df))
          } else
             stop('Give multipatt object or set mupa to true or false.')
    df[is.na(df)] <- 0
#    colnames(df)[1:ncl] <- levels(clust)
    out <- list(clust=clust, syntab=df)
    class(out) <- c('syntab', 'list')
   invisible(out)
}

#' @export
syntab.data.table <- function (veg, clust,
                               type = c('rel', 'relspec', 'abs', 'mean.cover'),
                               mupa = FALSE, dec = 0, refl, ...) {
  requireNamespace("data.table", quietly = TRUE)
  type <- match.arg(type)

  # defensive checks
  if (!inherits(veg, "data.table")) veg <- data.table::as.data.table(veg)
  if (!("TaxonName" %in% names(veg))) stop("veg must have a column 'TaxonName'.")
  if (!("RELEVE_NR" %in% names(veg))) stop("veg must have a column 'RELEVE_NR' (Plot-ID).")

  clust <- as.factor(clust)
  if (missing(clust)) clust <- sample(1:2, size = length(unique(veg$RELEVE_NR)), replace = TRUE)

  # map cluster to plots (RELEVE_NR)
  if (is.null(names(clust))) {
    # fallback: assume clust is in the same order as unique plots
    plots <- sort(unique(veg$RELEVE_NR))
    if (length(clust) != length(plots)) {
      stop("If clust has no names(), length(clust) must be == Number of RELEVE_NR.")
    }
    names(clust) <- plots
  }

  veg[, clust := clust[match(RELEVE_NR, names(clust))]]
  if (any(is.na(veg$clust))) stop("Einige RELEVE_NR konnten keinem Cluster zugeordnet werden (names(clust) pr\u00fcfen).")

  # ncl <- length(levels(veg$clust))
  # cat(" Number of clusters: ", ncl, "\n")
  # nb.rel.clust <- as.numeric(table(veg$clust))
  # cat(" Cluster frequency", nb.rel.clust, "\n")
  # if (any(nb.rel.clust == 0)) stop("All cluster levels must be represented by plots.")
  rel_clust <- unique(veg[, .(RELEVE_NR, clust)])
  ncl <- nlevels(rel_clust$clust)
  cat("Number of clusters:", ncl, "\n")
  nb.rel.clust <- rel_clust[, .N, by = clust][order(clust)]
  cat("Cluster frequency:", nb.rel.clust$N, "\n")

  # --- Build the basic syntab (cluster columns only) ---
  if (type == "relspec") {
    counts <- veg[, .N, by = .(TaxonName, clust)]
    counts[, Total := sum(N), by = TaxonName]
    counts[, RelativeFreq := round(N / Total * 100, dec)]
    st <- data.table::dcast(counts, TaxonName ~ clust, value.var = "RelativeFreq", fill = 0)
  } else if (type == "rel") {
    pres <- unique(veg[, .(RELEVE_NR, TaxonName, clust)])
    nplots_cl <- pres[, .(Nplots = data.table::uniqueN(RELEVE_NR)), by = clust]
    counts <- pres[, .(NplotsWith = data.table::uniqueN(RELEVE_NR)), by = .(TaxonName, clust)]
    counts <- merge(counts, nplots_cl, by = "clust", all.x = TRUE)
    counts[, RelativeFreq := round(NplotsWith / Nplots * 100, dec)]
    st <- data.table::dcast(counts, TaxonName ~ clust, value.var = "RelativeFreq", fill = 0)
  } else if (type == "mean.cover") {
    # try to find a cover/abundance column
    valcol <- intersect(c("Cover", "COVER", "cover", "Abundance", "abundance", "value"), names(veg))
    if (length(valcol) == 0) stop("F\u00fcr type='mean.cover' brauche ich eine Spalte wie Cover/COVER/value.")
    valcol <- valcol[1]
    st <- data.table::dcast(veg, TaxonName ~ clust, value.var = valcol, fun.aggregate = mean, fill = 0)
  } else if (type == "abs") {
    st <- data.table::dcast(veg, TaxonName ~ clust, fun.aggregate = length, fill = 0)
  }

  st_df <- as.data.frame(st)
  rownames(st_df) <- st_df$TaxonName
  st_df$TaxonName <- NULL

  # --- Multipatt analysis (optional) ---
  if (is.logical(mupa)) {
    if (mupa) {
      requireNamespace("indicspecies", quietly = TRUE)

      # build community matrix: plots x taxa
      # choose abundance if available, else presence/absence (1)
      valcol <- intersect(c("Cover", "COVER", "Cover_Perc", "Abundance", "abundance", "value"), names(veg))
      if (length(valcol) > 0) {
        valcol <- valcol[1]
        comm_dt <- data.table::dcast(
          veg, RELEVE_NR ~ TaxonName, value.var = valcol,
          fun.aggregate = sum, fill = 0
        )
      } else {
        # presence/absence fallback
        comm_dt <- data.table::dcast(
          veg, RELEVE_NR ~ TaxonName,
          fun.aggregate = length, fill = 0
        )
        comm_dt[, (names(comm_dt)[-1]) := lapply(.SD, function(x) as.numeric(x > 0)), .SDcols = names(comm_dt)[-1]]
      }

      plots <- comm_dt$RELEVE_NR
      comm <- as.matrix(comm_dt[, -1, with = FALSE])
      rownames(comm) <- plots

      cl_plot <- clust[match(plots, names(clust))]
      cl_plot <- as.factor(cl_plot)

      mu <- indicspecies::multipatt(comm, cl_plot, ...)

      # integrate: replace single-cluster membership columns (s.<level>) with our frequency table
      sign <- mu$sign

      # which columns correspond to single clusters?
      lev <- levels(cl_plot)
      s_cols <- paste0("s.", lev)
      hit <- intersect(s_cols, colnames(sign))

      # align species order and inject cluster frequencies
      # st_df has taxa as rownames
      common_taxa <- intersect(rownames(sign), rownames(st_df))
      if (length(common_taxa) == 0) stop("Keine \u00dcberlappung zwischen multipatt taxa und syntab taxa (TaxonName pr\u00fcfen).")

      sign2 <- sign[common_taxa, , drop = FALSE]

      # inject only those single-cluster columns that exist
      if (length(hit) > 0) {
        # map hit columns to levels
        lev_hit <- sub("^s\\.", "", hit)
        for (j in seq_along(hit)) {
          cl_name <- lev_hit[j]
          if (cl_name %in% colnames(st_df)) {
            sign2[, hit[j]] <- st_df[common_taxa, cl_name]
          }
        }
      }

      # nicer names: remove leading s.
      colnames(sign2) <- gsub("^s\\.", "", colnames(sign2))

      st_df <- as.data.frame(sign2)
    }
  } else if (inherits(mupa, "multipatt")) {
    sign <- mupa$sign
    colnames(sign) <- gsub("^s\\.", "", colnames(sign))
    st_df <- as.data.frame(sign)
  } else {
    stop("Give multipatt object or set mupa to TRUE or FALSE.")
  }

  st_df[is.na(st_df)] <- 0

  out <- list(clust = clust, syntab = st_df)
  class(out) <- c("syntab", "list")
  invisible(out)
}

#
#' @param zero.print character to fill empty cells
#' @param trait prints ecological indicators of species
#' @param limit minimum frequency to display
#' @param minstat minimum multipatt statistic
#' @param alpha Significance level
#' @export
#' @rdname syntab
print.syntab <- function(x,
                         zero.print='.',
                         trait,
                         limit = 1,
                         minstat = 0,
                         alpha = 0.05,
                         ...) {
  clust <- x$clust
  ncl <- length(unique(clust))
  cll <- levels(factor(clust))
  ntc <- as.numeric(table(clust))
  x <- x$syntab
  #suppressWarnings(colnames(x)[2:ncl] <- cll)
  if(any(c('stat','index','p.value') %in% names(x))) {
    if(any(is.na(x[1:(ncol(x)-3)]))) stop('NA values in frequency table. Do you have species without occurrences in your matrix?')
    mu = TRUE
    } else {
      if(any(is.na(x))) stop('NA values in frequency table. Do you have species without occurrences in your matrix?')
      mu = FALSE
    }
  if(mu) {
    stat <- x[,'stat']; x <- x[,-which(names(x)=='stat')]
    index <- x[,'index']; x <- x[,-which(names(x)=='index')]
    p.value <- x[,'p.value']; x <- x[,-which(names(x)=='p.value')]
    select <- stat > minstat & !is.na(stat) & p.value < alpha & !is.na(p.value) & apply(x, 1, function(y) max(y) >= limit)
    } else  select <- apply(x, 1, function(y) max(y) >= limit)
  if(sum(select)>0) {
    x <- x[select, ]
  if(zero.print != "0" && any(i0 <- x == 0)) {
      x[i0] <- sub("0", zero.print, x[i0])
      x[i0] <- sub("0.0", zero.print, x[i0])
      x[i0] <- sub("0.00", zero.print, x[i0])
  }

  if(mu) {
    if(sum(select) > 0)
       x <- cbind(x, index=index[select], stat=stat[select], p.value=p.value[select])
    x <- x[order(x$index),]
    }

  if(!missing(trait)) {
    if(is.null(names(trait))) stop('Trait vector must have names of taxa according to the vegetation matrix.')
    traitname <- names(trait) #as.character(substitute(trait))
    trait.df <- as.data.frame(trait[match(rownames(x), rownames(trait)),])
		x <- cbind(x, trait.df)
	}
  } else warning('No species exceed the chosen significance threshold.')
  cat('Number of clusters: ', ncl, '\n\n')
#  cat(' Cluster names           ', cll,'\n')
  cl <- t(data.frame(ntc)) #, matrix(nrow=2, ncol = (ncol(x)-ncl)))
  dimnames(cl) <- list(c('Cluster frequ:  '), cll)
  print(cl, row.names = FALSE, quote = FALSE)
  cat('\n')
  if(sum(select)>0) print.data.frame(x, ...)
  invisible(x)
 }

# ---- data.table version
syntab.data.table <- function (veg, clust,
                               type = c('rel', 'relspec', 'abs', 'mean.cover'),
                               mupa = FALSE, dec = 0, refl, ...,
                               dt_threads = NULL) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  type <- match.arg(type)
  if(length(unique(clust)) > 10 & mupa == TRUE) warning('Calculation time rises exponentially with the number of clusters!')
  # data.table in place
  data.table::setDT(veg)
  # after setDT(veg) and before map_dt creation:
  veg[, RELEVE_NR := as.character(RELEVE_NR)]

  if (is.null(names(clust))) {
    plots_u <- sort(unique(veg$RELEVE_NR))
    if (!missing(clust) && length(clust) != length(plots_u)) stop(...)
    names(clust) <- plots_u
  } else {
    names(clust) <- as.character(names(clust))
  }

  map_dt <- data.table::data.table(RELEVE_NR = names(clust), clust = as.factor(clust))
  veg <- map_dt[veg, on="RELEVE_NR"]

  # optional: set threads for data.table
  old_threads <- NULL
  if (!is.null(dt_threads)) {
    old_threads <- data.table::getDTthreads()
    data.table::setDTthreads(dt_threads)
    on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  }

  # defensive checks
  if (!("TaxonName" %in% names(veg))) stop("veg must have a column 'TaxonName'.")
  if (!("RELEVE_NR" %in% names(veg))) stop("veg must have a column 'RELEVE_NR' (Plot-ID).")

  if (missing(clust)) {
    plots_u <- sort(unique(veg$RELEVE_NR))
    clust <- sample(1:2, size = length(plots_u), replace = TRUE)
    names(clust) <- plots_u
  } else {
    clust <- as.factor(clust)
    if (is.null(names(clust))) {
      plots_u <- sort(unique(veg$RELEVE_NR))
      if (length(clust) != length(plots_u)) {
        stop("If clust has no names(), length(clust) must be == Number of RELEVE_NR.")
      }
      names(clust) <- plots_u
    }
  }

  # map cluster to plots using a join (fast, avoids match per row)
  map_dt <- data.table::data.table(RELEVE_NR = names(clust), clust = as.factor(clust))
  veg <- map_dt[veg, on = "RELEVE_NR"]
  if (anyNA(veg$clust)) stop("Einige RELEVE_NR konnten keinem Cluster zugeordnet werden (names(clust) pr\u00fcfen).")

  rel_clust <- unique(veg[, .(RELEVE_NR, clust)])
  ncl <- nlevels(rel_clust$clust)
  cat("Number of clusters:", ncl, "\n")
  nb.rel.clust <- rel_clust[, .N, by = clust][order(clust)]
  cat("Cluster frequency (per RELEVE_NR):", nb.rel.clust$N, "\n")

  # --- Build syntab (cluster columns only) ---
  if (type == "relspec") {
    counts <- veg[, .N, by = .(TaxonName, clust)]
    counts[, Total := sum(N), by = TaxonName]
    counts[, RelativeFreq := round(N / Total * 100, dec)]
    st <- data.table::dcast(counts, TaxonName ~ clust, value.var = "RelativeFreq", fill = 0)

  } else if (type == "rel") {
    # number of plots per cluster (unique plots!)
    nplots_cl <- veg[, .(Nplots = data.table::uniqueN(RELEVE_NR)), by = clust]

    # plots-with-taxon per cluster: use unique plot-taxon pairs, then count plots
    pt <- unique(veg[, .(RELEVE_NR, TaxonName, clust)])
    counts <- pt[, .(NplotsWith = data.table::uniqueN(RELEVE_NR)), by = .(TaxonName, clust)]
    counts <- nplots_cl[counts, on = "clust"]
    counts[, RelativeFreq := round(NplotsWith / Nplots * 100, dec)]
    st <- data.table::dcast(counts, TaxonName ~ clust, value.var = "RelativeFreq", fill = 0)

  } else if (type == "mean.cover") {
    valcol <- intersect(c("Cover", "COVER", "cover", "Abundance", "abundance", "value"), names(veg))
    if (length(valcol) == 0) stop("F\u00fcr type='mean.cover' brauche ich eine Spalte wie Cover/COVER/value.")
    valcol <- valcol[1]
    st <- data.table::dcast(veg, TaxonName ~ clust, value.var = valcol, fun.aggregate = mean, fill = 0)

  } else if (type == "abs") {
    # counts of records per (TaxonName, clust)
    st <- veg[, .N, by = .(TaxonName, clust)]
    st <- data.table::dcast(st, TaxonName ~ clust, value.var = "N", fill = 0)
  }

  st_df <- as.data.frame(st)
  rownames(st_df) <- st_df$TaxonName
  st_df$TaxonName <- NULL

  # --- Multipatt analysis (optional) ---
  if (is.logical(mupa)) {
    if (mupa) {
      stopifnot(requireNamespace("indicspecies", quietly = TRUE))

      # build community matrix: plots x taxa
      valcol <- intersect(c("Cover", "COVER", "Cover_Perc", "Abundance", "abundance", "value"), names(veg))
      if (length(valcol) > 0) {
        valcol <- valcol[1]
        comm_dt <- data.table::dcast(
          veg, RELEVE_NR ~ TaxonName, value.var = valcol,
          fun.aggregate = sum, fill = 0
        )
      } else {
        comm_dt <- data.table::dcast(
          veg, RELEVE_NR ~ TaxonName,
          fun.aggregate = length, fill = 0
        )
        comm_dt[, (names(comm_dt)[-1]) := lapply(.SD, \(x) as.numeric(x > 0)),
                .SDcols = names(comm_dt)[-1]]
      }

      plots <- comm_dt$RELEVE_NR
      comm <- as.matrix(comm_dt[, -1, with = FALSE])
      rownames(comm) <- plots

      cl_plot <- clust[match(plots, names(clust))]
      cl_plot <- as.factor(cl_plot)

      mu <- indicspecies::multipatt(comm, cl_plot, ...)

      sign <- mu$sign
      lev <- levels(cl_plot)
      s_cols <- paste0("s.", lev)
      hit <- intersect(s_cols, colnames(sign))

      common_taxa <- intersect(rownames(sign), rownames(st_df))
      if (length(common_taxa) == 0) stop("Keine \u00dcberlappung zwischen multipatt taxa und syntab taxa (TaxonName pr\u00fcfen).")

      sign2 <- sign[common_taxa, , drop = FALSE]

      # inject cluster frequencies into single-cluster columns
      if (length(hit) > 0) {
        lev_hit <- sub("^s\\.", "", hit)
        for (j in seq_along(hit)) {
          cl_name <- lev_hit[j]
          if (cl_name %in% colnames(st_df)) {
            sign2[, hit[j]] <- st_df[common_taxa, cl_name]
          }
        }
      }

      colnames(sign2) <- gsub("^s\\.", "", colnames(sign2))
      st_df <- as.data.frame(sign2)
    }
  } else if (inherits(mupa, "multipatt")) {
    sign <- mupa$sign
    colnames(sign) <- gsub("^s\\.", "", colnames(sign))
    st_df <- as.data.frame(sign)
  } else {
    stop("Give multipatt object or set mupa to TRUE or FALSE.")
  }

  st_df[is.na(st_df)] <- 0

  out <- list(clust = clust, syntab = st_df)
  class(out) <- c("syntab", "list")
  invisible(out)
}
