% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{notesToHz}
\alias{notesToHz}
\title{Convert notes to Hz}
\usage{
notesToHz(n, A4 = 440)
}
\arguments{
\item{n}{vector or matrix of notes}

\item{A4}{frequency of note A in the fourth octave (modern standard ISO 16 or
concert pitch = 440 Hz)}
}
\description{
Converts to Hz from musical notation like A4 - note A of the fourth octave
above C0 (16.35 Hz).
}
\examples{
notesToHz(c("A4", "D4", "A#2", "C0", "C-2"))

# Baroque tuning A415, half a semitone flat relative to concert pitch A440
notesToHz(c("A4", "D4", "A#2", "C0", "C-2"), A4 = 415)
}
\seealso{
\code{\link{HzToNotes}} \code{\link{HzToSemitones}}
}
