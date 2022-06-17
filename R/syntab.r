#' Syntaxonomic frequency tables
#'
#' @rdname syntab
#' @aliases syntab print.syntab
#'
#' @description Calculate and display relative or absolute frequency tables with or without use of function multipatt from package indicspecies
#'
#' @usage syntab(veg, clust, type = c('rel','abs','mean.cover'), mupa, dec=0, refl, ...)
#' @usage print.syntab(x, zero.print = ".", trait, limit = 1, minstat = 0, alpha = 0.05, ...)
#'
#' @export syntab
#' @param veg Vegetation dataframe
#' @param clust Vector with cluster information with length equal to number of rows of veg
#' @param type Relative or absolute frequency, mean species response values or strength of association.
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
#'  traits <- tv.traits()
#'  m <- match(rownames(st$syntab), traits$LETTERCODE, nomatch = 0)
#'  trait <- traits[m, c("OEK_F","OEK_N")]
#'  rownames(trait) <- traits$LETTERCODE[m]
#'  st <- syntab(elbaue, clust, mupa=TRUE)
#'  print(st, limit=30, trait=trait)
#'  #'  Manipulation of the syntaxonomic table
#'  sttable <- st$syntab
#'  sttable <- sttable[sttable$p.value < 0.05 & !is.na(sttable$p.value),
#'  !names(sttable) %in% c('stat')]
#'  taxa <- tax(rownames(sttable))
#'  rownames(sttable) <- taxa[match(rownames(sttable), taxa$LETTERCODE, nomatch = 0),'TaxonName']
#'  write.csv(sttable, 'sttable.csv')
#' }
#'

syntab <- function (veg, clust, type = c('rel','abs','mean.cover'), mupa, dec = 0, refl, ...)
{
type <- match.arg(type)
    if (missing(clust)) clust <- sample(1:2, size = nrow(veg), replace = TRUE)
    ncl <- length(unique(clust))
    cat(' Number of clusters: ', ncl, '\n')
    nb.rel.clust <- as.numeric(table(clust))
    cat(' Cluster frequency', nb.rel.clust,'\n')
    if(any(nb.rel.clust == 0)) stop("All cluster levels must be represented by plots.")
    if(is.null(levels(clust))) levels(clust) <- 1:length(table(clust))
    if(any(colSums(veg)==0)) stop('Some species without occurrence.')
    sp.veg <- split(veg, clust, drop=FALSE)
    if(type=='rel') {
    	tmp <- lapply(sp.veg, function(x) colSums(x > 0))
    	temp <- vector('list', length=ncl)
	    for(i in 1:length(nb.rel.clust))
	      temp[[i]] <- round(tmp[[i]] / nb.rel.clust[i] * 100, dec)
    } else
    if(type=='mean.cover') {
      temp <- lapply(sp.veg, function(x) {x[x==0] <- NA; round(colMeans(x, na.rm=TRUE),dec)})
      is.na(temp) <- 0
    } else
    if(type=='abs')
      temp <- lapply(sp.veg, function(x) colSums(x > 0))

    st <- t(as.data.frame(temp))
    st[is.na(st)] <- 0

    if(!missing(mupa) | inherits(mupa, 'multipatt') & ncl < 1) {
      if(!inherits(mupa, 'multipatt')) {
          requireNamespace("indicspecies", quietly = TRUE)
          mu <- indicspecies::multipatt(veg, clust, ...)
        }  else mu <- mupa
    #   st <- st[match(rownames(mu$sign), rownames(st)),]
      # o <- order(mu$sign[,'index'])
    df <- mu$sign
    }
    df[, 1:ncl] <- t(st) # mu$sign[,1:ncl] * st
    colnames(df)[1:ncl] <- levels(clust)
    out <- list(clust=clust, syntab=df)
    class(out) <- c('syntab', 'list')
   invisible(out)
}

#--------------

if(getRversion() >= "2.15.1")  utils::globalVariables(c("st"))


print.syntab <- function(x, zero.print='.', trait, limit = 1, minstat = 0, alpha = 0.05, ...) {
  clust <- x$clust
  ncl <- length(unique(clust))
  cll <- levels(factor(clust))
  ntc <- as.numeric(table(clust))
  x <- x$syntab
  colnames(x)[1:ncl] <- cll
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
#		names(x)[names(x)=='trait'] <- traitname
	}
  } else warning('NO species exceed the chosen significance threshold.')
  cat('Number of clusters: ', ncl, '\n\n')
#  cat(' Cluster names           ', cll,'\n')
  cl <- t(data.frame(ntc)) #, matrix(nrow=2, ncol = (ncol(x)-ncl)))
  dimnames(cl) <- list(c('Cluster frequency:  '), cll)
  print(cl, row.names = FALSE, quote = FALSE)
  cat('\n')
  if(sum(select)>0) print.data.frame(x, ...)
  invisible(x)
 }

