\name{tv.veg}
\alias{tv.veg}

\title{Tabulates vegetation tables from Turboveg database}

\description{Tabulates vegetation tables from Turboveg resp. VegetWeb database, including taxonomic emendation and layer combination. Using various default parameters for the included functions.
It is a wrapper for \code{tv.obs}, \code{tv.taxval}, \code{tv.coverperc}.}

\usage{
tv.veg(db, tv_home, tax = TRUE, convcode=TRUE, lc = c("layer", "mean", "max", "sum", "first"), pseudo = list(lc.1, "LAYER"), values='COVER_PERC', concept, names=c('short','long'), dec = 0, obs, refl, spc, site, RelScale, sysPath = FALSE, ...)
}

\arguments{
\item{db}{Name of your Turboveg database. Directory name containing tvabund.dbf, tvhabita.dbf and tvwin.set. Please specify pathnames below (if you sorted your databases in subfolders) but not above Turbowin/Data.}
\item{tv_home}{Turbowin installation path.}
\item{tax}{Should taxonomic valuation (see \code{\link{tv.taxval}}) be performed?}
\item{convcode}{Should cover code be converted to percentage values?}
\item{lc}{Layer combination type. Possible values: layer (default), sum, mean or max, see details}
\item{pseudo}{List for layer combinations, see details}
\item{values}{Name of the variable which should be used for the vegetations matrix.}
\item{concept}{Name of alternative taxon concept list, see \code{vignette(vegdata)}}
\item{names}{Should species numbers be replaced by shortletters or real names?}
\item{dec}{Number of decimals for cover values in the resulting vegetation matrix.}
\item{obs}{Observations, optional}
\item{refl}{Taxonomic reference list, optional}
\item{spc}{If you want to pick a subset of species.}
\item{site}{Dataframe with site informations.}
\item{RelScale}{Vector with Cover Scale code per Releve.}
\item{sysPath}{Load system files instead of Turboveg files.}
\item{...}{additional arguments for included functions}
}

\details{
\code{layer} means, the different layers are combined assuming there independence (a species occuring in two layers with a cover of 50\% will result in a overall cover of 75\%. \code{sum} will sum up cover values of all layers

With option \code{pseudo} you can decide, which layers should be combined. Give a list with a combination data.frame  (see \code{\link{lc}} and second the name of the column for combination. For an example see \code{comb = list(lc.1,c('LAYER'))}. Option pseudo=NULL will prevent any layer aggregation.
For further details see also \code{\link{tv.coverperc}} and \code{\link{tv.taxval}}.
  }

\value{
Function returns an object of class matrix with (combined) cover values.
}

\examples{
\dontrun{vignette("vegdata")}
# If you have a local Turboveg installation try for a beginning tv.veg('your databasename', tax=FALSE).
args(tv.veg)
args(tv.taxval)

veg <- tv.veg('taxatest', sysPath=TRUE)
names(veg)
tv.veg('taxatest', uncertain=list('DET_CERT', data.frame(0:2,c('pres','agg','agg'))), pseudo=list(lc.0,'LAYER'), genus = 'delete', sysPath=TRUE)
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }


\keyword{misc,manip,survey}
