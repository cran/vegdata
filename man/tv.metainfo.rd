\name{tv.metainfo}
\alias{tv.metainfo}
\title{Show metainfo of vegetation database or ecodbase}

\description{
Showing "metainfo.txt" when specified and saved in Turboveg database directory.
When db = 'eco' and refl specified, metainfo of species attribute table is displayed.
}

\usage{
tv.metainfo(db, refl='GermanSL 1.2', tv_home, filename = 'metainfo.txt', ...)
}

\arguments{
\item{db}{Turboveg database name}
\item{refl}{Turboveg taxonomic reference list, declaration only necessary for ecodbase info}
\item{tv_home}{Turboveg installation path}
\item{filename}{Name of metainfo file residing in database directory}
\item{...}{additional arguments}
}

\details{Since Turboveg provides no formalised method to store information about database fields, 
I suggest to save a simple text file, named for example "metainfo.txt" into the directory of your Turboveg database.}

\author{Florian Jansen
\email{jansen@uni-greifswald.de}
        }

\keyword{misc}