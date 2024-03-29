% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vtl.R
\name{schwa}
\alias{schwa}
\title{Schwa-related formant conversion}
\usage{
schwa(
  formants = NULL,
  vocalTract = NULL,
  formants_relative = NULL,
  nForm = 8,
  interceptZero = TRUE,
  tube = c("closed-open", "open-open")[1],
  speedSound = 35400,
  plot = FALSE
)
}
\arguments{
\item{formants}{a numeric vector of observed (measured) formant frequencies,
Hz}

\item{vocalTract}{the length of vocal tract, cm}

\item{formants_relative}{a numeric vector of target relative formant
frequencies, \% deviation from schwa (see examples)}

\item{nForm}{the number of formants to estimate (integer)}

\item{interceptZero}{if TRUE, forces the regression curve to pass through the
origin. This reduces the influence of highly variable lower formants, but
we have to commit to a particular model of the vocal tract: closed-open or
open-open/closed-closed (method = "regression" only)}

\item{tube}{the vocal tract is assumed to be a cylindrical tube that is
either "closed-open" or "open-open" (same as closed-closed)}

\item{speedSound}{speed of sound in warm air, cm/s. Stevens (2000) "Acoustic
phonetics", p. 138}

\item{plot}{if TRUE, plots vowel quality in speaker-normalized F1-F2 space}
}
\value{
Returns a list with the following components: \describe{
  \item{vtl_measured}{VTL as provided by the user, cm}
  \item{vocalTract_apparent}{VTL estimated based on formants frequencies
  provided by the user, cm}
  \item{formantDispersion}{average distance between formants, Hz}
  \item{ff_measured}{formant frequencies as
  provided by the user, Hz}
  \item{ff_schwa}{formant frequencies corresponding
  to a neutral schwa sound in this vocal tract, Hz}
  \item{ff_theoretical}{formant frequencies corresponding to the
  user-provided relative formant frequencies, Hz}
  \item{ff_relative}{deviation of formant frequencies from those expected for
  a schwa, \% (e.g. if the first ff_relative is -25, it means that F1 is 25\%
  lower than expected for a schwa in this vocal tract)}
  \item{ff_relative_semitones}{deviation of formant frequencies from those
  expected for a schwa, semitones. Like \code{ff_relative}, this metric is
  invariant to vocal tract length, but the variance tends to be greater for
  lower vs. higher formants}
  \item{ff_relative_dF}{deviation of formant frequencies from those expected
  for a schwa, proportion of formant spacing (dF). Unlike \code{ff_relative}
  and \code{ff_relative_semitones}, this metric has similar variance for
  lower and higher formants}
}
}
\description{
This function performs several conceptually related types of conversion of
formant frequencies in relation to the neutral schwa sound based on the
one-tube model of the vocal tract. This is useful for speaker normalization
because absolute formant frequencies measured in Hz depend strongly on
overall vocal tract length (VTL). For example, adult men vs. children or
grizzly bears vs. dog puppies have very different formant spaces in Hz, but
it is possible to define a VTL-normalized formant space that is applicable to
all species and sizes. Case 1: if we know vocal tract length (VTL) but not
formant frequencies, \code{schwa()} estimates formants corresponding to a
neutral schwa sound in this vocal tract, assuming that it is perfectly
cylindrical. Case 2: if we know the frequencies of a few lower formants,
\code{schwa()} estimates the deviation of observed formant frequencies from
the neutral values expected in a perfectly cylindrical vocal tract (based on
the VTL as specified or as estimated from formant dispersion). Case 3: if we
want to generate a sound with particular relative formant frequencies (e.g.
high F1 and low F2 relative to the schwa for this vocal tract),
\code{schwa()} calculates the corresponding formant frequencies in Hz. See
examples below for an illustration of these three suggested uses.
}
\details{
Algorithm: the expected formant dispersion is given by \eqn{(2 *
formant_number - 1) * speedSound / (4 * formant_frequency)} for a closed-open
tube (mouth open) and \eqn{formant_number * speedSound / (2 *
formant_frequency)} for an open-open or closed-closed tube. F1 is schwa is
expected at half the value of formant dispersion. See e.g. Stevens (2000)
"Acoustic phonetics", p. 139. Basically, we estimate vocal tract length and
see if each formant is higher or lower than expected for this vocal tract.
For this to work, we have to know either the frequencies of enough formants
(not just the first two) or the true length of the vocal tract. See also
\code{\link{estimateVTL}} on the algorithm for estimating formant dispersion
if VTL is not known (note that \code{schwa} calls \code{\link{estimateVTL}}
with the option \code{method = 'regression'}).
}
\examples{
## CASE 1: known VTL
# If vocal tract length is known, we calculate expected formant frequencies
schwa(vocalTract = 17.5)
schwa(vocalTract = 13, nForm = 5)
schwa(vocalTract = 13, nForm = 5, tube = 'open-open')

## CASE 2: known (observed) formant frequencies
# Let's take formant frequencies in four vocalizations, namely
# (/a/, /i/, /mmm/, /roar/) by the same male speaker:
formants_a = c(860, 1430, 2900, NA, 5200)  # NAs are OK - here F4 is unknown
s_a = schwa(formants = formants_a, plot = TRUE)
s_a
# We get an estimate of VTL (s_a$vtl_apparent),
#   same as with estimateVTL(formants_a)
# We also get theoretical schwa formants: s_a$ff_schwa
# And we get the difference (\% and semitones) in observed vs expected
#   formant frequencies: s_a[c('ff_relative', 'ff_relative_semitones')]
# [a]: F1 much higher than expected, F2 slightly lower (see plot)

formants_i = c(300, 2700, 3400, 4400, 5300, 6400)
s_i = schwa(formants = formants_i, plot = TRUE)
s_i
# The apparent VTL is slightly smaller (14.5 cm)
# [i]: very low F1, very high F2

formants_mmm = c(1200, 2000, 2800, 3800, 5400, 6400)
schwa(formants_mmm, tube = 'closed-closed', plot = TRUE)
# ~schwa, but with a closed mouth

formants_roar = c(550, 1000, 1460, 2280, 3350,
                  4300, 4900, 5800, 6900, 7900)
s_roar = schwa(formants = formants_roar, plot = TRUE)
s_roar
# Note the enormous apparent VTL (22.5 cm!)
# (lowered larynx and rounded lips exaggerate the apparent size)
# s_roar$ff_relative: high F1 and low F2-F4

schwa(formants = formants_roar[1:4], plot = TRUE)
# based on F1-F4, apparent VTL is almost 28 cm!
# Since the lowest formants are the most salient,
# the apparent size is exaggerated even further

# If you know VTL, a few lower formants are enough to get
#   a good estimate of the relative formant values:
schwa(formants = formants_roar[1:4], vocalTract = 19, plot = TRUE)
# NB: in this case theoretical and relative formants are calculated
#  based on user-provided VTL (vtl_measured) rather than vtl_apparent

## CASE 3: from relative to absolute formant frequencies
# Say we want to generate a vowel sound with F1 20\% below schwa
#    and F2 40\% above schwa, with VTL = 15 cm
s = schwa(formants_relative = c(-20, 40), vocalTract = 15, plot = TRUE)
# s$ff_schwa gives formant frequencies for a schwa, while
#   s$ff_theoretical gives formant frequencies for a sound with
#   target relative formant values (low F1, high F2)
schwa(formants = s$ff_theoretical)
}
\seealso{
\code{\link{estimateVTL}}
}
