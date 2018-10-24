\name{syntab}
\alias{syntab}
\alias{print.syntab}
\alias{freqtab}
\title{Syntaxonomic frequency tables}

\description{
Calculate and display relative or absolute frequency tables with or without use of function multipatt from package indicspecies}

\seealso{
Package indicspecies with function \link[indicspecies]{multipatt} for indicator species analysis along multiple cluster combinations
}

\usage{
syntab(veg, clust, type = c('rel','abs','mean.cover'), mupa=NULL, dec=0, refl, ...)
\method{print}{syntab}(x, zero.print = ".", trait, limit = 1, minstat = 0, alpha = 0.05, \dots)
}

\arguments{
\item{veg}{Vegetation dataframe}
\item{clust}{Vector with cluster information with length equal to number of rows of veg}
\item{type}{Relative or absolute frequency, mean species response values or strength of association.}
\item{mupa}{Either logical for (not) using multipatt from package indispecies to detect significance of cluster association strength or supply output from previous use of multipatt.}
\item{x}{Object from function syntab}
\item{zero.print}{Replacement for zero values.}
\item{trait}{Optional vector of trait values to be plotted behind the species.}
\item{limit}{Minimum value to display.}
\item{minstat}{Minimal indicator value}
\item{alpha}{Significance threshold.}
\item{dec}{Number of decimals in result.}
\item{refl}{Name of Turboveg taxonomic reference list to use for fullnames.}
\item{...}{additional arguments}
}

\author{Florian Jansen \email{florian.jansen@uni-rostock.de} }

\examples{
\dontrun{
elbaue <- tv.veg('elbaue')
elbaue.env <- tv.site('elbaue')
clust <- vector('integer', nrow(elbaue.env))
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1
clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3
clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4
levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')
traits <- tv.traits()
m <- match(rownames(st$syntab), traits$LETTERCODE, nomatch = 0)
trait <- traits[m, c("OEK_F","OEK_N")]
rownames(trait) <- traits$LETTERCODE[m]
st <- syntab(elbaue, clust, mupa=TRUE)
print(st, limit=30, trait=trait)
# Manipulation of the syntaxonomic table
sttable <- st$syntab
sttable <- sttable[sttable$p.value < 0.05 & !is.na(sttable$p.value), !names(sttable) %in% c('stat')]
taxa <- tax(rownames(sttable))
rownames(sttable) <- taxa[match(rownames(sttable), taxa$LETTERCODE, nomatch = 0),'TaxonName']
write.csv(sttable, 'sttable.csv')
}
}

\keyword{misc}

