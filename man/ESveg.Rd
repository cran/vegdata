\name{ESveg.veg}
\alias{ESveg.veg}
\alias{ESveg.site}

\title{Load vegetation data from ESveg formatted data files}

\description{Tabulates vegetation tables resp. header data from ESveg formatted file. 
Until now only very basic support, without any taxonomic emendation and layer combination.}

\usage{
ESveg.veg(db, refl, ...)
ESveg.site(db, ...)
}

\arguments{
\item{db}{Name of your ESveg formatted file}
\item{refl}{Taxonomic reference list. No yet implemented.}
\item{...}{additional arguments for included functions}
}

\value{
Function returns an matrix object of class 'veg'
}

\examples{
path <- system.file(package = "vegdata")
veg <- ESveg.veg(file.path(path,'tvdata', 'Data', 'tvexport.xml'))
names(veg)
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }


\keyword{misc,manip,survey}
