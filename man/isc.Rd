\name{isc}
\alias{isc}
\alias{showindiplot}

\title{Indicate site conditions with community weighted mean values of traits or with mode of gradient classes (sum of species amplitudes).}

\usage{
isc(veg, trait.db, ivnames, keyname = 'LETTERCODE', method = c('mean', 'mode'), 
weight, db, ...)
showindiplot(veg, trait.db, plotid, weight, keyname = 'LETTERCODE')
}
                
\arguments{
\item{veg}{Vegetation matrix with plots in rows and species in columns}
\item{trait.db}{data frame with species trait values}
\item{ivnames}{Name of the trait in trait.db to be used}
\item{keyname}{Name of the column in trait dataframe to join with colnames of veg table}
\item{method}{mean (weighted value of single traits, or mode (maximum) of trait classes)}
\item{weight}{additional weight, e.g niche breath of species}
\item{db}{name of Turboveg database}
\item{plotid}{number or id of the plot to show}
\item{\dots}{additional arguments}
}


\description{
Calculates community weightes mean trait values, like mean Ellenberg indicator values. Alternatively (method = 'mode') environmental conditions can be calculated according to th econcept of sums of amplitudes of species along ecological gradients.
}

\details{
Zero trait values will be handled as NA values.
}

\value{
Vector with the ecological classification of sites. Either mean trait values or mode of gradient classes.
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }
        
\examples{
db <- 'elbaue'
veg <- tv.veg(db, cover.transform='sqrt', check.critical = FALSE)
site <- tv.site(db, verbose = FALSE)
# Exclude plots with very high water level fluctuation
veg <- veg[site$SDGL < 60,]
veg <- veg[,colSums(veg) > 0]
site <- site[site$SDGL < 60,]
# Load species trait value database
traits <- tv.traits(db)

# Mean indicator values of Ellenberg F values
mEIV_F <- isc(veg, traits, 'OEK_F', method = 'mean')
plot(site$MGL, mEIV_F, xlab = 'Mean groundwater level')
\dontrun{
# Mode (most frequent level) of Ellenberg F values
library(reshape)
traitmat <- cast(traits, LETTERCODE ~ OEK_F)
traitmat <- traitmat[,-14]
ilevel <- isc(veg, traitmat, ivnames= as.character(1:12), method = 'mode')
boxplot(site$MGL ~ ilevel)
}
}
\keyword{misc}