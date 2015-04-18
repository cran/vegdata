\name{ESveg}
\alias{ESveg.obs}
\alias{ESveg.site}

\title{Load vegetation data from ESveg formatted data files}

\description{Tabulates vegetation tables resp. header data from ESveg formatted file. 
Until now only very basic support, without any taxonomic emendation and layer combination.}

\usage{
ESveg.obs(db, ...)
ESveg.site(db, ...)
}

\arguments{
  \item{db}{Name and path to your ESveg formatted file}
  \item{...}{additional arguments}
}

\value{
  The function returns an matrix object of class 'veg'
}

\examples{
\dontrun{
path <- system.file(package = "vegdata")
obs <- ESveg.obs(file.path(path,'tvdata', 'Data', 'elbaue.xml'))
veg <- tv.veg(obs=obs, refl='GermanSL 1.2', taxval=TRUE, convcode=FALSE, )
dimnames(veg)
}
}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }


\keyword{misc,manip,survey}
