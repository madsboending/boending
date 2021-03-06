% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nps.R
\name{nps}
\alias{nps}
\title{Hurtig udregning af Net Promotor Score}
\usage{
nps(x)
}
\arguments{
\item{x}{er en integer som udelukkende indeholder nps v�rdier 1-10 .}
}
\description{
Funktionen udregner net promotor score.
}
\examples{
#eksempel
dat <- c(1,3,2,4,1,4,8,9,10,10,9,9,8,10,6,10)

nps(dat)

}
\keyword{net}
\keyword{promotor}
\keyword{score}

