\name{tv.traits}
\alias{tv.traits}
\alias{meanTraits}
\alias{tv.eco}
\title{Load species traits from Turboveg reference list}

\usage{
tv.traits(db, trait.db = 'ecodbase.dbf', refl, quiet = FALSE, ...)
meanTraits(trait, veg, trait.db = 'ecodbase.dbf', join = 'LETTERCODE', refl, zero.is.NA = TRUE, ...)
}

\arguments{
\item{db}{Path name to the Turboveg database directory}
\item{trait.db}{Name of species trait DBase file, default is 'ecodbase'}
\item{refl}{Name of the taxonomic reference list, if veg is not loaded with tv.veg}
\item{quiet}{Hide messages}
\item{trait}{Name of the column to use from the trait table}
\item{veg}{vegetation matrix}
\item{join}{Column name of the trait table corresponding to the colnames of the vegetation matrix}
\item{zero.is.NA}{Should zero trait values handled as NA values}
\item{\dots}{additional arguments for tv.traits}
}

\description{
Loading Turboveg ecodbase or any other specified dBase file in this directory and do basic data evaluation. Empty columns are eliminated.
meanTraits will calculate mean trait values like mean Ellenberg indicator values (see vignette).
}

\details{
You can use the final output line to make a summary statistic for attributes with potentially misleading '0' values. Just delete the \" at beginning and end.
  }

\value{
data.frame of ecological traits, see \code{metainfo(refl, eco=TRUE)}.
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }
        
\examples{
\dontrun{
veg <- tv.veg('elbaue', cover.transform='pa')
# mEIV <- meanTraits('OEK_F', veg, 'ecodbase.dbf')
site <- tv.site('elbaue')
# plot(site$MGL, mEIV)
}}
\keyword{misc}