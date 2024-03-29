% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{splitIntoChunks}
\alias{splitIntoChunks}
\title{Split vector into chunks}
\usage{
splitIntoChunks(x, n)
}
\arguments{
\item{x}{numeric vector to split}

\item{n}{number of chunks}
}
\value{
Returns a list of length \code{n} containing the chunks
}
\description{
Internal soundgen function.
}
\details{
Takes a numeric vector x and splits it into n chunks. This is the fastest
splitting algorithm from
https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
}
\examples{
# prepare chunks of iterator to run in parallel on several cores
chunks = soundgen:::splitIntoChunks(1:21, 4)
chunks
}
\keyword{internal}
