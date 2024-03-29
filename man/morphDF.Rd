% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/morph_utilities.R
\name{morphDF}
\alias{morphDF}
\title{Morph dataframes}
\usage{
morphDF(
  a,
  b,
  nMorphs = 5,
  method = c("smooth", "perAnchor")[2],
  lenSmooth = 50,
  matchIdx = NULL,
  plot = F,
  ...
)
}
\arguments{
\item{a, b}{dataframes to morph}

\item{nMorphs}{length of morphing sequence}

\item{method}{morphing method. 'smooth' equalizes contour lengths and takes a
weighted mean. 'perAnchor' is a more sophisticated algorithm that attempts
to match individual anchors}

\item{lenSmooth}{the length of curves generated from anchors prior to
averaging (only applicable if method is 'smooth')}

\item{matchIdx}{manual override of anchor matching: if you have a better idea
of which anchors should morph into each other, specify}

\item{plot}{if TRUE, plots the morphing sequence of anchors}

\item{...}{other graphical pars passed on to \code{plot}}
}
\value{
A list of length nMorphs containing anchor dataframes for morphing
}
\description{
Internal soundgen function.
}
\details{
Morphs two dataframes of anchors, with two columns and at least two rows in
each.
}
\examples{
a = data.frame(time=c(0, .2, .9, 1), value=c(100, 110, 180, 110))
b = data.frame(time=c(0, .3, .5, .8, 1), value=c(300, 220, 190, 400, 350))
plot (a, type = 'b', ylim = c(0, 500))
points (b, type = 'b', col = 'blue')
m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'smooth', plot = TRUE)
m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'perAnchor', plot = TRUE)

m = soundgen:::morphDF(a = data.frame(time = c(0, 1), freq = c(700, 700)),
                       b = data.frame(time = c(0, 1), freq = c(400, 600)),
                       nMorphs = 5, method = 'perAnchor', plot = TRUE)
m = soundgen:::morphDF(a = data.frame(time = c(-30, 120, 350), value = c(-120, 10, -120)),
                       b = data.frame(time = c(50, 500), value = c(0, -30)),
                       nMorphs = 10, method = 'perAnchor', plot = TRUE)
m = soundgen:::morphDF(a = data.frame(time = c(-50, 1214), value = c(-50, -70)),
                       b = data.frame(time = c(0, 49, 256), value = c(-120, 10, -120)),
                       nMorphs = 8, method = 'perAnchor', plot = TRUE)
}
\keyword{internal}
