\name{elbaue}
\alias{elbaue}
\alias{elbaue.env}
\non_function{}

\title{Species Data and Altitude from floodplains of the river Elbe, Germany.}

\description{
The \code{elbaue} data frame has 33 sites (rows) and 53 species (columns).
}

\details{
Data frame \code{elbaue.env} contains the following variables:
  \describe{
  \item{RELEVE_NR}{a unique number}
  \item{DATE}{a fictitious date of the plot survey}
  \item{SURF_AREA}{the plot area}
 \item{FLOOD}{logical, inundated floodplain ("Altaue"); (1=recently inundated, 0= no inundation}
 \item{OLD}{logical; former floodplain (Altaue; Durch Deiche von der rezenten Aue getrennter Auenbereich); 1= old floodplain]}
 \item{BORDER}{border of floodplain (Auenrand; Grenze der Aue zu anderen Naturraeumen, haeufig vermoort); 1= Auengrenzbereich}
 \item{INTENS}{intensity of land use; 1= sporadic use, 2= yearly with low intensity, 3= yearly}
 \item{MGL}{mean groundwater level in cm (ueber zwei Jahre aus Tageswerten gemittelter Wasserstand)}
 \item{SDGL}{standard deviation of groundwater level (Standardabweichung der Wassergang-Zeitreihe [cm], Mass fuer die Groesse der ueber zwei Jahre gemittelten Wasserstaenden)}
 \item{InUnD}{duration of inundation period (Ueberflutungsdauer Log[Tage/Jahr]; Logarithmus der ueber zwei Jahre gemittelten Ueberflutungsdauer)}
 \item{InUnD_50}{duration of inundation period above 50cm (Dauer von Wasserstaenden hoeher 50cm ueber Flur Log[Tage/Jahr]; Logarithmus der ueber zwei Jahre gemittelten Werte)}
  }
}

\references{
 Leyer, Ilona and Wesche, Carsten 2007: Multivariate Statistik in der Oekologie, p. 221, Springer, Berlin.
}


\examples{
\dontrun{
  elbaue <- tv.veg('elbaue')
  elbaue.env <- tv.site('elbaue')
}
}
