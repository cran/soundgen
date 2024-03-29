% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{rbind_fill}
\alias{rbind_fill}
\title{rbind_fill}
\usage{
rbind_fill(df1, df2)
}
\arguments{
\item{df1, df2}{two dataframes with partly matching columns}
}
\description{
Internal soundgen function
}
\details{
Fills missing columns with NAs, then rbinds - handy in case one has extra
columns. Used in formant_app(), pitch_app()
}
\keyword{internal}
