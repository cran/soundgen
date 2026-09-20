#' Estimate vocal tract length
#'
#' Estimates the length of vocal tract based on formant frequencies. If
#' \code{method = 'meanFormant'}, vocal tract length (VTL) is calculated
#' separately for each formant, and then the resulting VTLs are averaged. The
#' equation used is \eqn{(2 * n - 1) * speedSound / (4 *
#' Fn)} for a closed-open tube (mouth open) and
#' \eqn{n * speedSound / (2 * Fn)} for an open-open or closed-closed tube (eg
#' closed mouth in mmm or open mouth and open glottis in whispering). If
#' \code{method = 'meanDispersion'}, formant dispersion is calculated as the
#' mean distance between formants, and then VTL is calculated
#' as \eqn{speed of sound / 2 / formant dispersion}. If \code{method =
#' 'regression'}, formant dispersion is estimated using the regression method
#' described in Reby et al. (2005) and Anikin et al. (2024). For a review of
#' VTL-related summary measures of formant frequencies, refer to Pisanski et al.
#' (2014). See also \code{\link{schwa}} for VTL estimation with additional
#' information on formant frequencies.
#'
#' @seealso \code{\link{schwa}}
#'
#' @references \itemize{
#'   \item Reby, D., McComb, K., Cargnelutti, B., Darwin, C., Fitch, W. T., &
#'   Clutton-Brock, T. (2005). Red deer stags use formants as assessment cues
#'   during intrasexual agonistic interactions. Proceedings of the Royal Society
#'   B: Biological Sciences, 272(1566), 941-947.
#'   \item Pisanski et al. (2014). Vocal indicators of body size in men and
#'   women: a meta-analysis. Animal Behaviour, 95, 89-99.
#'   \item Anikin, A., Barreda, S. & Reby, D. (2024) A practical guide to
#'   calculating vocal tract length and scale-invariant formant patterns.
#'   Behavior Research Methods 56, 5588–5604.
#' }
#'
#' @param formants formant frequencies in any format recognized by
#'   \code{\link{soundgen}}: a vector of formant frequencies like \code{c(550,
#'   1600, 3200)}; a list with multiple values per formant like \code{list(f1 =
#'   c(500, 550), f2 = 1200)}; or a character string like \code{aaui} referring
#'   to default presets for speaker "M1" in soundgen presets
#' @param method the method of estimating vocal tract length (see Description):
#'   "regression" (default), "meanDispersion", or "meanFormant"
#' @param interceptZero if TRUE, forces the regression curve to pass through the
#'   origin, reducing the influence of highly variable lower formants (method =
#'   "regression" only)
#' @param tube the vocal tract is assumed to be a cylindrical tube that is
#'   either "closed-open" or "open-open" (same as closed-closed)
#' @param speedSound speed of sound in warm air, by default 35400 cm/s. Stevens
#'   (2000) "Acoustic phonetics", p. 138
#' @param checkFormat if FALSE, only a list of properly formatted formant
#'   frequencies is accepted
#' @param output "simple" (default) = just the VTL; "detailed" = a list of
#'   additional stats (for method = "regression" only)
#' @param plot if TRUE, plots the regression line whose slope gives formant
#'   dispersion dF (method = "regression" only). Label sizes show the influence
#'   of each formant, and the blue line corresponds to each formant being an
#'   integer multiple of F1 (as when harmonics are misidentified as formants);
#'   the second plot shows how VTL varies depending on the number of formants
#'   used
#'
#' @return If \code{output = 'simple'} (default), returns the estimated vocal
#'   tract length in cm. If \code{output = 'detailed'} and \code{method =
#'   'regression'}, returns a list with extra stats used for plotting. Namely,
#'   \code{$regressionInfo$infl} gives the influence of each observation
#'   calculated as the absolute change in VTL with vs without the observation *
#'   10 + 1 (the size of labels on the first plot). \code{$vtlPerFormant$vtl}
#'   gives the VTL as it would be estimated if only the first \code{nFormants}
#'   were used.
#' @export
#' @examples
#' estimateVTL(NA)
#' estimateVTL(500)
#' estimateVTL(c(600, 1850, 2800, 3600, 5000), plot = TRUE)
#' es = estimateVTL(c(600, 1850, 2800, 3600, 5000), plot = TRUE, output = 'detailed')
#' es
#'
#' estimateVTL(c(1200, 2000, 2800, 3800, 5400, 6400),
#'   tube = 'open-open', interceptZero = FALSE, plot = TRUE)
#' estimateVTL(c(1200, 2000, 2800, 3800, 5400, 6400),
#'   tube = 'open-open', interceptZero = TRUE, plot = TRUE)
#'
#' # Multiple measurements are OK
#' estimateVTL(
#'   formants = list(f1 = c(540, 600, 550),
#'   f2 = 1650, f3 = c(2400, 2550)),
#'   plot = TRUE, output = 'detailed')
#' # NB: this is better than averaging formant values. Cf.:
#' estimateVTL(
#'   formants = list(f1 = mean(c(540, 600, 550)),
#'   f2 = 1650, f3 = mean(c(2400, 2550))),
#'   plot = TRUE)
#'
#' # Missing values are OK
#' estimateVTL(c(600, 1850, 3100, NA, 5000), plot = TRUE)
#' estimateVTL(list(f1 = 500, f2 = c(1650, NA, 1400), f3 = 2700), plot = TRUE)
#'
#' # Note that VTL estimates based on the commonly reported 'meanDispersion'
#' # depend only on the first and last formants
#' estimateVTL(c(500, 1400, 2800, 4100), method = 'meanDispersion')
#' estimateVTL(c(500, 1100, 2300, 4100), method = 'meanDispersion') # identical
#' # ...but this is not the case for 'meanFormant' and 'regression' methods
#' estimateVTL(c(500, 1400, 2800, 4100), method = 'meanFormant')
#' estimateVTL(c(500, 1100, 2300, 4100), method = 'meanFormant') # much longer
#'
#' # Compare the results produced by the three methods
#' nIter = 100
#' out = data.frame(meanFormant = rep(NA, nIter), meanDispersion = NA, regression = NA)
#' for (i in 1:nIter) {
#'   # generate a random formant configuration
#'   f = runif(1, 300, 900) + (1:6) * rnorm(6, 1000, 200)
#'   out$meanFormant[i]    = estimateVTL(f, method = 'meanFormant')
#'   out$meanDispersion[i] = estimateVTL(f, method = 'meanDispersion')
#'   out$regression[i]     = estimateVTL(f, method = 'regression')
#' }
#' pairs(out)
#' cor(out)
#' # 'meanDispersion' is pretty different, while 'meanFormant' and 'regression'
#' # give broadly comparable results
estimateVTL = function(
    formants,
    method = c('regression', 'meanDispersion', 'meanFormant'),
    interceptZero = TRUE,
    tube = c('closed-open', 'open-open', 'open-closed', 'closed-closed'),
    speedSound = 35400,
    checkFormat = TRUE,
    output = c('simple', 'detailed'),
    plot = FALSE
) {
  tube = match.arg(tube)
  method = match.arg(method)
  output = match.arg(output)
  if (checkFormat) {
    # convert to a properly formatted list
    formants = reformatFormants(formants, output = 'freqs', keepNonInteger = FALSE)
    if (!is.list(formants)) return(NA)
  }
  # if we don't know vocalTract, but at least one formant is defined,
  # we guess the length of vocal tract
  if (method == 'meanFormant') {
    formant_freqs = unlist(lapply(formants, function(f) mean(f$freq)))
    nf = length(formant_freqs)
    if (nf > 0) {
      if (tube %in% c('closed-open', 'open-closed')) {
        vtls = (2 * (1:nf) - 1) * speedSound / 4 / formant_freqs
      } else if (tube %in% c('open-open', 'closed-closed')) {
        vtls = 1:nf * speedSound / 2 / formant_freqs
      }
      vocalTract = mean(vtls, na.rm = TRUE)
    } else {
      vocalTract = NA
    }
    dF = NA
  } else if (method %in% c('meanDispersion', 'regression')) {
    fd = getdF(formants,
               speedSound = speedSound,
               method = method,
               interceptZero = interceptZero,
               tube = tube,
               plot = plot,
               checkFormat = FALSE,
               output = output)
    if (is.list(fd)) {
      dF = fd$dF
    } else {
      dF = fd
    }
    vocalTract = speedSound / 2 / dF
  }
  if (output == 'detailed' && method == 'regression') {
    return(list(
      vocalTract = vocalTract,
      vtl_95CI = rev(speedSound / 2 / fd$dF_95CI),
      dF = dF,
      dF_95CI = fd$dF_95CI,
      regressionInfo = fd$regressionInfo,
      vtlPerFormant = fd$vtlPerFormant
    ))
  } else {
    return(vocalTract)
  }
}


#' Schwa-related formant conversion
#'
#' This function performs several conceptually related types of conversion of
#' formant frequencies in relation to the neutral schwa sound based on the
#' one-tube model of the vocal tract. This is useful for speaker normalization
#' because absolute formant frequencies measured in Hz depend strongly on
#' overall vocal tract length (VTL). For example, adult men vs. children or
#' grizzly bears vs. dog puppies have very different formant spaces in Hz, but
#' it is possible to define a VTL-normalized formant space that is applicable to
#' all species and sizes. Case 1: if we know vocal tract length (VTL) but not
#' formant frequencies, \code{schwa()} estimates formants corresponding to a
#' neutral schwa sound in this vocal tract, assuming that it is perfectly
#' cylindrical. Case 2: if we know the frequencies of a few lower formants,
#' \code{schwa()} estimates the deviation of observed formant frequencies from
#' the neutral values expected in a perfectly cylindrical vocal tract (based on
#' the VTL as specified or as estimated from formant dispersion). Case 3: if we
#' want to generate a sound with particular relative formant frequencies (e.g.
#' high F1 and low F2 relative to the schwa for this vocal tract),
#' \code{schwa()} calculates the corresponding formant frequencies in Hz. See
#' examples below for an illustration of these three suggested uses and
#' \code{\link{estimateVTL}} for details on the algorithm for estimating formant
#' dispersion if VTL is not known (note that \code{schwa} calls
#' \code{\link{estimateVTL}} with the option \code{method = 'regression'}).
#'
#' @seealso \code{\link{estimateVTL}}
#'
#' @references \itemize{
#'   \item Stevens, K. N. (2000). Acoustic phonetics (Vol. 30). MIT press.
#'   \item Anikin, A., Barreda, S. & Reby, D. (2024) A practical guide to
#'   calculating vocal tract length and scale-invariant formant patterns.
#'   Behavior Research Methods 56, 5588–5604.
#' }
#'
#' @return A list with the following components: \describe{
#'   \item{vtl_measured}{VTL as provided by the user, cm}
#'   \item{vtl_apparent}{VTL estimated based on formants frequencies provided by
#'   the user, cm}
#'   \item{dF}{average distance between formants, Hz}
#'   \item{ff_measured}{formant frequencies as
#'   provided by the user, Hz}
#'   \item{ff_schwa}{formant frequencies corresponding
#'   to a neutral schwa sound in this vocal tract, Hz}
#'   \item{ff_theoretical}{formant frequencies corresponding to the
#'   user-provided relative formant frequencies, Hz}
#'   \item{ff_relative}{deviation of formant frequencies from those expected for
#'   a schwa, \% (e.g. if the first ff_relative is -25, it means that F1 is 25\%
#'   lower than expected for a schwa in this vocal tract)}
#'   \item{ff_relative_semitones}{deviation of formant frequencies from those
#'   expected for a schwa, semitones. Like \code{ff_relative}, this metric is
#'   invariant to vocal tract length, but the variance tends to be greater for
#'   lower vs. higher formants}
#'   \item{ff_relative_dF}{deviation of formant frequencies from those expected
#'   for a schwa, proportion of formant spacing (dF). Unlike \code{ff_relative}
#'   and \code{ff_relative_semitones}, this metric has similar variance for
#'   lower and higher formants}
#' }
#' @param formants a numeric vector of observed (measured) formant frequencies,
#'   Hz
#' @param vocalTract the length of vocal tract, cm
#' @param formants_relative a numeric vector of target relative formant
#'   frequencies, \% deviation from schwa (see examples)
#' @param nFormants the number of formants to estimate (integer)
#' @param plot if TRUE, plots vowel quality in speaker-normalized F1-F2 space
#' @inheritParams getFormantFilter
#' @inheritParams estimateVTL
#' @export
#' @examples
#' ## CASE 1: known VTL
#' # If vocal tract length is known, we calculate expected formant frequencies
#' schwa(vocalTract = 17.5)
#' schwa(vocalTract = 13, nFormants = 5)
#' schwa(vocalTract = 13, nFormants = 5, tube = 'open-open')
#'
#' ## CASE 2: known (observed) formant frequencies
#' # Let's take formant frequencies in four vocalizations, namely
#' # (/a/, /i/, /mmm/, /roar/) by the same male speaker:
#' formants_a = c(860, 1430, 2900, NA, 5200)  # NAs are OK - here F4 is unknown
#' s_a = schwa(formants = formants_a, plot = TRUE)
#' s_a
#' # We get an estimate of VTL (s_a$vtl_apparent),
#' #   same as with estimateVTL(formants_a)
#' # We also get theoretical schwa formants: s_a$ff_schwa
#' # And we get the difference (%, semitones, or dF units) in observed vs expected
#' #   formant frequencies: s_a[c('ff_relative', 'ff_relative_semitones', 'ff_relative_dF')]
#' # [a]: F1 much higher than expected, F2 slightly lower (see plot)
#'
#' formants_i = c(300, 2700, 3400, 4400, 5300, 6400)
#' s_i = schwa(formants = formants_i, plot = TRUE)
#' s_i
#' # The apparent VTL is slightly smaller (14.5 cm)
#' # [i]: very low F1, very high F2
#'
#' formants_mmm = c(1200, 2000, 2800, 3800, 5400, 6400)
#' schwa(formants_mmm, tube = 'closed-closed', plot = TRUE)
#' # ~schwa, but with a closed mouth
#'
#' formants_roar = c(550, 1000, 1460, 2280, 3350,
#'                   4300, 4900, 5800, 6900, 7900)
#' s_roar = schwa(formants = formants_roar, plot = TRUE)
#' s_roar
#' # Note the enormous apparent VTL (22.5 cm!)
#' # (lowered larynx and rounded lips exaggerate the apparent size)
#' # s_roar$ff_relative: high F1 and low F2-F4
#'
#' schwa(formants = formants_roar[1:4], plot = TRUE)
#' # based on F1-F4, apparent VTL is almost 28 cm!
#' # Since the lowest formants are the most salient,
#' # the apparent size is exaggerated even further
#'
#' # If you know VTL, a few lower formants are enough to get
#' #   a good estimate of the relative formant values:
#' schwa(formants = formants_roar[1:4], vocalTract = 19, plot = TRUE)
#' # NB: in this case theoretical and relative formants are calculated
#' #  based on user-provided VTL (vtl_measured) rather than vtl_apparent
#'
#' ## CASE 3: from relative to absolute formant frequencies
#' # Say we want to generate a vowel sound with F1 20% below schwa
#' #    and F2 40% above schwa, with VTL = 15 cm
#' s = schwa(formants_relative = c(-20, 40), vocalTract = 15, plot = TRUE)
#' # s$ff_schwa gives formant frequencies for a schwa, while
#' #   s$ff_theoretical gives formant frequencies for a sound with
#' #   target relative formant values (low F1, high F2)
#' schwa(formants = s$ff_theoretical)
schwa = function(formants = NULL,
                 vocalTract = NULL,
                 formants_relative = NULL,
                 nFormants = 8,
                 interceptZero = TRUE,
                 tube = c('closed-open', 'open-open', 'open-closed', 'closed-closed'),
                 speedSound = 35400,
                 plot = FALSE) {
  # check input
  tube = match.arg(tube)
  if (is.null(formants) && is.null(vocalTract)) {
    stop('Please specify formant frequencies and/or vocal tract length')
  }
  if (!is.null(formants_relative)) {
    if (!is.numeric(formants_relative)) {
      stop('formants_relative must be numeric')
    }
  }
  if (!is.null(vocalTract)) {
    if (!is.numeric(vocalTract) || vocalTract < 0) {
      stop('vocalTract must be a positive number (cm)')
    }
  }
  if (!is.null(formants_relative)) {
    if (is.null(vocalTract)) {
      stop('vocalTract must be specified to convert relative formants to Hz')
    }
  }

  if (is.null(formants_relative)) {
    ## we don't know and want to calculate formants_relative
    # calculate formant dispersion and apparent VTL
    if (is.null(formants)) {
      # we don't know formants, we know VTL
      dF = speedSound / (2 * vocalTract)
      vtl_apparent = NULL
    } else {
      # we know formants
      if (is.null(vocalTract)) {
        # we don't know VTL
        dF = getdF(
          formants,
          speedSound = speedSound,
          method = 'regression',
          interceptZero = interceptZero,
          tube = tube)
        vtl_apparent = speedSound / (2 * dF)
      } else {
        # we know VTL
        dF_apparent = getdF(
          formants,
          speedSound = speedSound,
          method = 'regression',
          interceptZero = interceptZero,
          tube = tube
        )
        dF = speedSound / (2 * vocalTract)
        vtl_apparent = speedSound / (2 * dF_apparent)
      }
    }

    # calculate relative formant frequencies
    if (is.null(formants)) {
      idx = 1:nFormants
    } else {
      idx = seq_along(formants)
    }
    if (tube %in% c('closed-open', 'open-closed')) {
      ff_schwa = (2 * idx - 1) / 2 * dF
    } else if (tube %in% c('open-open', 'closed-closed')) {
      ff_schwa = idx * dF
    }
    ff_relative = (formants / ff_schwa - 1) * 100
    ff_relative_semitones = HzToSemitones(formants) - HzToSemitones(ff_schwa)
    ff_relative_dF = (formants - ff_schwa) / dF
    ff_theoretical = NULL
  } else {
    ## we know formants_relative and vocalTract and want to convert ff to Hz

    # make sure formants_relative is at least nFormants long
    if (length(formants_relative) < nFormants) {
      formants_relative = c(formants_relative,
                            rep(0, nFormants - length(formants_relative)))
    }

    # calculate formants in Hz
    dF = speedSound / (2 * vocalTract)
    idx = seq_along(formants_relative)
    if (tube %in% c('closed-open', 'open-closed')) {
      ff_schwa = (2 * idx - 1) / 2 * dF
    } else if (tube %in% c('open-open', 'closed-closed')) {
      ff_schwa = idx * dF
    }
    ff_theoretical = ff_schwa * (1 + formants_relative / 100)
    vtl_apparent = NULL
    ff_relative = formants_relative
    ff_relative_semitones = 12 * log2(ff_theoretical / ff_schwa)
    ff_relative_dF = (ff_theoretical - ff_schwa) / dF
  }

  # prepare the output
  out = list(vtl_measured = vocalTract,
             vtl_apparent = vtl_apparent,
             dF = dF,
             ff_measured = formants,
             ff_schwa = ff_schwa,
             ff_theoretical = ff_theoretical,
             ff_relative = ff_relative,
             ff_relative_semitones = ff_relative_semitones,
             ff_relative_dF = ff_relative_dF)

  # plotting
  if (plot) {
    hillenbrand = get('hillenbrand')  # otherwise CMD check complains
    fmrs = out$ff_relative_dF[1:2]
    if (!any(!is.finite(fmrs))) {
      if (!is.null(out$vtl_apparent)) {
        main = paste('Apparent VTL =', round(out$vtl_apparent, 1), 'cm')
      } else {
        if (!is.null(out$vtl_measured)) {
          main = paste('Measured VTL =', round(out$vtl_measured, 1), 'cm')
        } else {
          main = 'VTL unknown'
        }
      }
      xlim = range(c(hillenbrand$F1Rel, fmrs[1]))
      ylim = range(c(hillenbrand$F2Rel, fmrs[2]))
      plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n',
           xlab = 'F1', ylab = 'F2',
           xlim = xlim, ylim = ylim, main = main)
      text(hillenbrand$F1Rel, hillenbrand$F2Rel,
           labels = hillenbrand$vowel, cex = 1, col = 'blue')
      points(fmrs[1], fmrs[2], pch = 4, cex = 1.5, col = 'red')
    }
  }

  # do not return empty elements
  out[lapply(out, length) > 0]
}


#' Get formant dispersion
#'
#' Estimates formant dispersion based on one or more formant frequencies.
#' @inheritParams estimateVTL
#' @noRd
#' @examples
#' soundgen:::getdF(list(f1 = c(570, 750), f2 = NA, f3 = c(2400, 2200, NA)))
#' soundgen:::getdF(list(f1 = c(570, 750), f2 = NA, f3 = c(2400, 2200, NA)),
#'   output = 'detailed')
getdF = function(
    formants,
    method = c('regression', 'meanDispersion'),
    tube = c('closed-open', 'open-open', 'open-closed', 'closed-closed'),
    interceptZero = TRUE,
    speedSound = 35400,
    plot = FALSE,
    checkFormat = TRUE,
    output = c('simple', 'detailed')
) {
  tube = match.arg(tube)
  method = match.arg(method)
  output = match.arg(output)
  if (plot) output = 'detailed'
  if (checkFormat) {
    formants = reformatFormants(formants,
                                output = 'freqs',
                                keepNonInteger = FALSE)
  }
  nf = length(formants)
  if (!is.list(formants) ||     # expect a preformatted list...
      nf < 1 ||   # ...with at least one formant...
      # ...and at least one non-NA
      !any(is.finite(unlist(lapply(formants, function(x) x$freq)))) ) return(NA)
  if (method == 'meanDispersion') {
    formant_freqs = as.numeric(unlist(lapply(formants, function(f)
      mean(f$freq, na.rm = TRUE))))
    idx_notNA = which(is.finite(formant_freqs))
    if (length(idx_notNA) < 1) {
      dF = NA
    } else if (length(idx_notNA) == 1) {
      # a single formant
      if (tube %in% c('closed-open', 'open-closed')) {
        # Fn = dF * (2n-1)/2
        dF = 2 * formant_freqs[idx_notNA] / (2 * idx_notNA - 1)
      } else if (tube %in% c('open-open', 'closed-closed')) {
        # Fn = dF * n
        dF = formant_freqs[idx_notNA] / idx_notNA
      }
    } else {
      ran_notNA = range(idx_notNA)
      dF = (formant_freqs[ran_notNA[2]] - formant_freqs[ran_notNA[1]]) /
        (ran_notNA[2] - ran_notNA[1])
    }
  } else if (method == 'regression') {
    # Reby et al. (2005) "Red deer stags use formants..."
    fdf = NULL
    if (tube %in% c('closed-open', 'open-closed')) {
      # have to loop because there may be multiple measuremets per formant
      for (i in seq_along(formants)) {
        temp = data.frame(
          formant_idx = i,
          formant = paste0('F', i),
          formantSpacing = i - 0.5, # same as (2 * i - 1) / 2,
          freq = formants[[i]]$freq,
          infl = NA)
        if (is.null(fdf)) fdf = temp else fdf = rbind(fdf, temp)
      }
    } else if (tube %in% c('open-open', 'closed-closed')) {
      for (i in seq_along(formants)) {
        temp = data.frame(
          formant_idx = i,
          formant = paste0('F', i),
          formantSpacing = i,
          freq = formants[[i]]$freq,
          infl = NA)
        if (is.null(fdf)) fdf = temp else fdf = rbind(fdf, temp)
      }
    }

    if (interceptZero) {
      # no intercept, i.e. forced to pass through 0 (closed-open tube)
      mod = try(suppressWarnings(lm(freq ~ -1 + formantSpacing, fdf)), silent = TRUE)
      if (inherits(mod, 'try-error')) {
        return(NA)
      } else {
        mod_coefs = suppressWarnings(summary(mod)$coef)
        dF = mod_coefs[1, 1]
        SE = mod_coefs[1, 2]
      }
    } else {
      # estimate both intercept and slope
      mod = try(suppressWarnings(lm(freq ~ 1 + formantSpacing, fdf)), silent = TRUE)
      if (inherits(mod, 'try-error')) {
        return(NA)
      } else {
        mod_coefs = suppressWarnings(summary(mod)$coef)
        if (nrow(mod_coefs) == 2) {
          dF = mod_coefs[2, 1]
          SE = mod_coefs[2, 2]
        } else {
          warning(paste('cannot estimate slope and intercept from a single formant;',
                        'use method = meanDispersion or set intercept to zero'))
          return(NA)
        }
      }
    }
    ci = dF + c(-1.96, 1.96) * SE

    if (output == 'detailed') {
      vtl_full = speedSound / 2 / dF
      # calculate VTL for the first 1:f formants
      vf = data.frame(nFormants = seq_along(unique(fdf$formant)), vtl = NA)
      for (i in 1:nrow(vf)) {
        fdf_i = fdf[fdf$formant_idx <= i, ]
        if (any(!is.na(fdf_i$freq))) {
          if (interceptZero) {
            mod_i = try(suppressWarnings(lm(freq ~ -1 + formantSpacing, fdf_i)),
                        silent = TRUE)
            if (!inherits(mod_i, 'try-error')) {
              sum_mod_i = suppressWarnings(summary(mod_i)$coef)
              vf$vtl[i] = speedSound / 2 / sum_mod_i[1, 1]
            }
          } else {
            mod_i = try(suppressWarnings(lm(freq ~ formantSpacing, fdf_i)),
                        silent = TRUE)
            if (!inherits(mod_i, 'try-error')) {
              sum_mod_i = suppressWarnings(summary(mod_i)$coef)
              # only intercept estimated for F1, so slope is NA
              if (nrow(sum_mod_i) == 2) vf$vtl[i] = speedSound / 2 / sum_mod_i[2, 1]
            }
          }
        }
      }

      # calculate the influence of each observation on the VTL estimate based on
      # all formants
      for (i in seq_len(nrow(fdf))) {
        fdf_i = fdf[-i, ]
        vtl_i = NA
        if (any(!is.na(fdf$freq[i])) && any(!is.na(fdf_i$freq))) {
          if (interceptZero) {
            mod_i = try(suppressWarnings(
              lm(freq ~ -1 + formantSpacing, fdf_i)), silent = TRUE)
            if (!inherits(mod_i, 'try-error')) {
              sum_mod_i = suppressWarnings(summary(mod_i)$coef)
              vtl_i = speedSound / 2 / sum_mod_i[1, 1]
            }
          } else {
            mod_i = try(suppressWarnings(
              lm(freq ~ formantSpacing, fdf_i)), silent = TRUE)
            if (!inherits(mod_i, 'try-error')) {
              sum_mod_i = suppressWarnings(summary(mod_i)$coef)
              if (nrow(sum_mod_i) == 2) vtl_i = speedSound / 2 / sum_mod_i[2, 1]
            }
          }
          fdf$infl[i] = abs(vtl_full - vtl_i) / vtl_full * 10 + 1
        }
      }
    }
    if (plot) {
      oldpar = par()$mfrow
      par(mfrow = c(1, 2))
      on.exit(par(mfrow = oldpar))
      plot(
        fdf$formantSpacing, fdf$freq,
        type = 'n',
        xlab = 'Formant spacings',
        ylab = 'Formant frequency, Hz',
        xaxs = 'i', yaxs = 'i',
        xlim = c(0, tail(fdf$formantSpacing, 1) + 1),
        ylim = c(0, max(fdf$freq, na.rm = TRUE) * 1.2),
        main = paste0('Formant dispersion = ',
                      round(dF, 0),
                      ' Hz')
      )
      text(
        fdf$formantSpacing, fdf$freq,
        labels = fdf$formant,
        cex = ifelse(is.finite(fdf$infl), fdf$infl, 1) # cooks.distance(mod) + 1
      )
      # for closed-open tubes we don't expect formant freqs to be integer
      # multiples - could be harmonics instead of formants, show with a blue
      # line
      f1freq = if (!is.null(formants$f1$freq)) formants$f1$freq[1] else NA
      if (length(formants) > 1 && is.finite(f1freq) &&
          tube %in% c('closed-open', 'open-closed'))
        abline(f1freq / 2, f1freq, lty = 3, col = 'blue')

      if (method == 'regression') {
        if (interceptZero) {
          if (is.finite(dF)) abline(a = 0, b = dF, lty = 2, lwd = 2)
        } else {
          co = coef(mod)
          if (length(co) == 2 && all(is.finite(co)))
            abline(co, lty = 2, lwd = 2)
        }
      }
      plot(vf, type = 'b',
           xlab = 'Number of formants used',
           ylab = 'VTL estimate, cm',
           main = paste('VTL =', round(speedSound / 2 / dF, 1), 'cm'))
    }

    if (output == 'detailed') {
      dF = list(dF = dF,
                dF_95CI = ci,
                regressionInfo = fdf,
                vtlPerFormant = vf)
    }
  }
  dF
}
