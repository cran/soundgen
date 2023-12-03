#' Estimate vocal tract length
#'
#' Estimates the length of vocal tract based on formant frequencies. If
#' \code{method = 'meanFormant'}, vocal tract length (VTL) is calculated
#' separately for each formant, and then the resulting VTLs are averaged. The
#' equation used is \eqn{(2 * formant_number - 1) * speedSound / (4 *
#' formant_frequency)} for a closed-open tube (mouth open) and
#' \eqn{formant_number * speedSound / (2 * formant_frequency)} for an open-open
#' or closed-closed tube (eg closed mouth in mmm or open mouth and open glottis
#' in whispering). If \code{method = 'meanDispersion'}, formant dispersion is
#' calculated as the mean distance between formants, and then VTL is calculated
#' as \eqn{speed of sound / 2 / formant dispersion}. If \code{method =
#' 'regression'}, formant dispersion is estimated using the regression method
#' described in Reby et al. (2005) "Red deer stags use formants as assessment
#' cues during intrasexual agonistic interactions". For a review of these and
#' other VTL-related summary measures of formant frequencies, refer to Pisanski
#' et al. (2014) "Vocal indicators of body size in men and women: a
#' meta-analysis". See also \code{\link{schwa}} for VTL estimation with
#' additional information on formant frequencies.
#'
#' @seealso \code{\link{schwa}}
#'
#' @param formants formant frequencies in any format recognized by
#'   \code{\link{soundgen}}: a vector of formant frequencies like \code{c(550,
#'   1600, 3200)}; a list with multiple values per formant like \code{list(f1 =
#'   c(500, 550), f2 = 1200))}; or a character string like \code{aaui} referring
#'   to default presets for speaker "M1" in soundgen presets
#' @param method the method of estimating vocal tract length (see details)
#' @param interceptZero if TRUE, forces the regression curve to pass through the
#'   origin. This reduces the influence of highly variable lower formants, but
#'   we have to commit to a particular model of the vocal tract: closed-open or
#'   open-open/closed-closed (method = "regression" only)
#' @param tube the vocal tract is assumed to be a cylindrical tube that is
#'   either "closed-open" or "open-open" (same as closed-closed)
#' @param speedSound speed of sound in warm air, by default 35400 cm/s. Stevens
#'   (2000) "Acoustic phonetics", p. 138
#' @param checkFormat if FALSE, only a list of properly formatted formant
#'   frequencies is accepted
#' @param output "simple" (default) = just the VTL; "detailed" = a list of
#'   additional stats (see Value below)
#' @param plot if TRUE, plots the regression line whose slope gives formant
#'   dispersion (method = "regression" only). Label sizes show the influence of
#'   each formant, and the blue line corresponds to each formant being an
#'   integer multiple of F1 (as when harmonics are misidentified as formants);
#'   the second plot shows how VTL varies depending on the number of formants
#'   used
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
#' estimateVTL(c(600, 1850, 2800, 3600, 5000), plot = TRUE, output = 'detailed')
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
#' \dontrun{
#' # Compare the results produced by the three methods
#' nIter = 1000
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
#' }
estimateVTL = function(
    formants,
    method = c('regression', 'meanDispersion', 'meanFormant')[1],
    interceptZero = TRUE,
    tube = c('closed-open', 'open-open')[1],
    speedSound = 35400,
    checkFormat = TRUE,
    output = c('simple', 'detailed')[1],
    plot = FALSE
) {
  if (!method %in% c('meanFormant', 'meanDispersion', 'regression')) {
    stop('Invalid method; valid methods are: meanFormant, meanDispersion, regression')
  }
  if (checkFormat) {
    # convert to a properly formatted list
    formants = reformatFormants(formants, output = 'freqs', keepNonInteger = FALSE)
    if (!is.list(formants)) return(NA)
  }
  # if we don't know vocalTract, but at least one formant is defined,
  # we guess the length of vocal tract
  if (method == 'meanFormant') {
    formant_freqs = unlist(sapply(formants, function(f) mean(f$freq)))
    nf = length(formant_freqs)
    if (tube %in% c('closed-open', 'open-closed')) {
      vtls = (2 * (1:nf) - 1) * speedSound / 4 / formant_freqs
    } else if (tube %in% c('open-open', 'closed-closed')) {
      vtls = 1:nf * speedSound / 2 / formant_freqs
    } else {
      stop('the tube can be closed-open or open-open')
    }
    vocalTract = mean(vtls, na.rm = TRUE)
    formantDispersion = NA
  } else if (method %in% c('meanDispersion', 'regression')) {
    fd = getFormantDispersion(formants,
                              speedSound = speedSound,
                              method = method,
                              interceptZero = interceptZero,
                              tube = tube,
                              plot = plot,
                              checkFormat = FALSE,
                              output = output)
    if (is.list(fd)) {
      formantDispersion = fd$formantDispersion
    } else {
      formantDispersion = fd
    }
    vocalTract = speedSound / 2 / formantDispersion
  }
  if (output == 'detailed') {
    return(c(list(vocalTract = vocalTract,
                  vocalTract_95CI = rev(speedSound / 2 / fd$formantDispersion_95CI),
                  formantDispersion = formantDispersion,
                  formantDispersion_95CI = fd$formantDispersion_95CI)))
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
#' examples below for an illustration of these three suggested uses.
#'
#' Algorithm: the expected formant dispersion is given by \eqn{(2 *
#' formant_number - 1) * speedSound / (4 * formant_frequency)} for a closed-open
#' tube (mouth open) and \eqn{formant_number * speedSound / (2 *
#' formant_frequency)} for an open-open or closed-closed tube. F1 is schwa is
#' expected at half the value of formant dispersion. See e.g. Stevens (2000)
#' "Acoustic phonetics", p. 139. Basically, we estimate vocal tract length and
#' see if each formant is higher or lower than expected for this vocal tract.
#' For this to work, we have to know either the frequencies of enough formants
#' (not just the first two) or the true length of the vocal tract. See also
#' \code{\link{estimateVTL}} on the algorithm for estimating formant dispersion
#' if VTL is not known (note that \code{schwa} calls \code{\link{estimateVTL}}
#' with the option \code{method = 'regression'}).
#'
#' @seealso \code{\link{estimateVTL}}
#'
#' @return Returns a list with the following components: \describe{
#'   \item{vtl_measured}{VTL as provided by the user, cm}
#'   \item{vocalTract_apparent}{VTL estimated based on formants frequencies
#'   provided by the user, cm}
#'   \item{formantDispersion}{average distance between formants, Hz}
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
#' @param nForm the number of formants to estimate (integer)
#' @param plot if TRUE, plots vowel quality in speaker-normalized F1-F2 space
#' @inheritParams getSpectralEnvelope
#' @inheritParams estimateVTL
#' @export
#' @examples
#' ## CASE 1: known VTL
#' # If vocal tract length is known, we calculate expected formant frequencies
#' schwa(vocalTract = 17.5)
#' schwa(vocalTract = 13, nForm = 5)
#' schwa(vocalTract = 13, nForm = 5, tube = 'open-open')
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
#' # And we get the difference (% and semitones) in observed vs expected
#' #   formant frequencies: s_a[c('ff_relative', 'ff_relative_semitones')]
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
                 nForm = 8,
                 interceptZero = TRUE,
                 tube = c('closed-open', 'open-open')[1],
                 speedSound = 35400,
                 plot = FALSE) {
  # check input
  if (is.null(formants) & is.null(vocalTract)) {
    stop('Please pecify formant frequencies and/or vocal tract length')
  }
  if (!is.null(formants_relative)) {
    if (!is.numeric(formants_relative)) {
      stop('formants_relative must be positive numbers (Hz)')
    }
  }
  if (!is.null(vocalTract)) {
    if (!is.numeric(vocalTract) | vocalTract < 0) {
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
      formantDispersion = speedSound / (2 * vocalTract)
      vocalTract_apparent = NULL
    } else {
      # we know formants
      if (is.null(vocalTract)) {
        # we don't know VTL
        formantDispersion = getFormantDispersion(
          formants,
          speedSound = speedSound,
          method = 'regression',
          interceptZero = interceptZero,
          tube = tube)
        vocalTract_apparent = speedSound / (2 * formantDispersion)
      } else {
        # we know VTL
        formantDispersion_apparent = getFormantDispersion(
          formants,
          speedSound = speedSound,
          method = 'regression',
          interceptZero = interceptZero,
          tube = tube
        )
        formantDispersion = speedSound / (2 * vocalTract)
        vocalTract_apparent = speedSound / (2 * formantDispersion_apparent)
      }
    }

    # calculate relative formant frequencies
    if (is.null(formants)) {
      idx = 1:nForm
    } else {
      idx = 1:length(formants)
    }
    if (tube %in% c('closed-open', 'open-closed')) {
      ff_schwa = (2 * idx - 1) / 2 * formantDispersion
    } else if (tube %in% c('open-open', 'closed-closed')) {
      ff_schwa = idx * formantDispersion
    } else {
      stop('the tube can be closed-open or open-open')
    }
    ff_relative = (formants / ff_schwa - 1) * 100
    ff_relative_semitones = HzToSemitones(formants) - HzToSemitones(ff_schwa)
    ff_relative_dF = (formants - ff_schwa) / formantDispersion
    ff_theoretical = NULL
  } else {
    ## we know formants_relative and vocalTract and want to convert ff to Hz

    # make sure formants_relative is at least nForm long
    if (length(formants_relative) < nForm) {
      formants_relative = c(formants_relative,
                            rep(0, nForm - length(formants_relative)))
    }

    # calculate formants in Hz
    formantDispersion = speedSound / (2 * vocalTract)
    idx = 1:length(formants_relative)
    if (tube %in% c('closed-open', 'open-closed')) {
      ff_schwa = (2 * idx - 1) / 2 * formantDispersion
    } else if (tube %in% c('open-open', 'closed-closed')) {
      ff_schwa = idx * formantDispersion
    } else {
      stop('the tube can be closed-open or open-open')
    }
    ff_theoretical = ff_schwa * (1 + formants_relative / 100)
    vocalTract_apparent = NULL
    ff_relative = formants_relative
    ff_relative_semitones = 12 * log2(ff_theoretical / ff_schwa)
    ff_relative_dF = (ff_theoretical - ff_schwa) / formantDispersion
  }

  # prepare the output
  out = list(vtl_measured = vocalTract,
             vtl_apparent = vocalTract_apparent,
             formantDispersion = formantDispersion,
             ff_measured = formants,
             ff_schwa = ff_schwa,
             ff_theoretical = ff_theoretical,
             ff_relative = ff_relative,
             ff_relative_semitones = ff_relative_semitones,
             ff_relative_dF = ff_relative_dF)

  # plotting
  if (plot) {
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
      hillenbrand = hillenbrand  # otherwise CMD check complains
      plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n',
           xlab = 'F1', ylab = 'F2',
           xlim = xlim, ylim = ylim, main = main)
      text(hillenbrand$F1Rel, hillenbrand$F2Rel,
           labels = hillenbrand$vowel, cex = 1, col = 'blue')
      points(fmrs[1], fmrs[2], pch = 4, cex = 1.5, col = 'red')
    }
  }

  # do not return empty elements
  out = out[lapply(out, length) > 0]
  return(out)
}


#' Get formant dispersion
#'
#' Internal soundgen function.
#'
#' Estimates formant dispersion based on one or more formant frequencies.
#' @inheritParams estimateVTL
#' @keywords internal
#' @examples
#' soundgen:::getFormantDispersion(
#'   list(f1 = c(570, 750), f2 = NA, f3 = c(2400, 2200, NA)))
getFormantDispersion = function(
    formants,
    method = c('meanDispersion', 'regression')[2],
    tube = c('closed-open', 'open-open')[1],
    interceptZero = TRUE,
    speedSound = 35400,
    plot = FALSE,
    checkFormat = TRUE,
    output = c('simple', 'detailed')[1]
) {
  if (plot) output = 'detailed'
  if (checkFormat) {
    formants = reformatFormants(formants,
                                output = 'freqs',
                                keepNonInteger = FALSE)
  }
  nf = length(formants)
  if (!is.list(formants) |     # expect a preformatted list...
      nf < 1 |   # ...with at least one formant...
      !any(!is.na(formants))) return(NA)  # ...and at least one non-NA
  if (method == 'meanDispersion') {
    formant_freqs = unlist(sapply(formants, function(f) mean(f$freq)))
    if (nf > 1) {
      formantDispersion = mean(diff(formant_freqs), na.rm = TRUE)
    } else {
      # a single formant
      if (!is.na(formants$f1)) {
        formantDispersion = 2 * formant_freqs
      } else {
        formantDispersion = NA
      }
    }
  } else if (method == 'regression') {
    # Reby et al. (2005) "Red deer stags use formants..."
    fdf = NULL
    if (tube %in% c('closed-open', 'open-closed')) {
      # have to loop because there may be multiple measuremets per formant
      for (i in 1:length(formants)) {
        temp = data.frame(
          formant_idx = i,
          formant = paste0('F', i),
          formantSpacing = i - 0.5, # same as (2 * i - 1) / 2,
          freq = formants[[i, drop = FALSE]]$freq,
          infl = NA)
        if (is.null(fdf)) fdf = temp else fdf = rbind(fdf, temp)
      }
    } else if (tube %in% c('open-open', 'closed-closed')) {
      for (i in 1:length(formants)) {
        temp = data.frame(
          formant_idx = i,
          formant = paste0('F', i),
          formantSpacing = i,
          freq = formants[[i, drop = FALSE]]$freq,
          infl = NA)
        if (is.null(fdf)) fdf = temp else fdf = rbind(fdf, temp)
      }
    } else {
      stop('the tube can be closed-open or open-open')
    }
    if (interceptZero) {
      # no intercept, i.e. forced to pass through 0 (closed-open tube)
      mod = suppressWarnings(lm(freq ~ -1 + formantSpacing, fdf))
      formantDispersion = suppressWarnings(summary(mod)$coef[1])
    } else {
      mod = suppressWarnings(lm(freq ~ 1 + formantSpacing, fdf))
      formantDispersion = suppressWarnings(summary(mod)$coef[2])
    }
    ci = formantDispersion + c(-1.96, 1.96) * summary(mod)$coef[2]

    if (output == 'detailed') {
      vtl_full = speedSound / 2 / formantDispersion
      # calculate VTL for the first 1:f formants
      vf = data.frame(nFormants = 1:length(unique(fdf$formant)), vtl = NA)
      for (i in 1:nrow(vf)) {
        fdf_i = fdf[fdf$formant_idx <= i, ]
        if (any(!is.na(fdf_i$freq))) {
          if (interceptZero) {
            mod_i = suppressWarnings(lm(freq ~ -1 + formantSpacing, fdf_i))
            vf$vtl[i] = speedSound / 2 / suppressWarnings(summary(mod_i)$coef[1])
          } else {
            mod_i = suppressWarnings(lm(freq ~ formantSpacing, fdf_i))
            vf$vtl[i] = speedSound / 2 / suppressWarnings(summary(mod_i)$coef[2])
          }
        }
      }

      # calculate the influence of each observation on the VTL estimate based on
      # all formants
      for (i in 1:nrow(fdf)) {
        fdf_i = fdf[-i, ]
        if (any(!is.na(fdf$freq[i])) & any(!is.na(fdf_i$freq))) {
          if (interceptZero) {
            mod_i = suppressWarnings(lm(freq ~ -1 + formantSpacing, fdf_i))
            vtl_i = speedSound / 2 / suppressWarnings(summary(mod_i)$coef[1])
          } else {
            mod_i = suppressWarnings(lm(freq ~ formantSpacing, fdf_i))
            vtl_i = speedSound / 2 / suppressWarnings(summary(mod_i)$coef[2])
          }
          fdf$infl[i] = abs(vtl_full - vtl_i) / vtl_full * 10 + 1
        }
      }
    }
    if (plot) {
      par(mfrow = c(1, 2))
      plot(
        fdf$formantSpacing, fdf$freq,
        type = 'n',
        xlab = 'Formant spacings',
        ylab = 'Formant frequency, Hz',
        xaxs = 'i', yaxs = 'i',
        xlim = c(0, tail(fdf$formantSpacing, 1) + 1),
        ylim = c(0, max(fdf$freq, na.rm = TRUE) * 1.2),
        main = paste0('Formant dispersion = ',
                      round(formantDispersion, 0),
                      ' Hz')
      )
      text(
        fdf$formantSpacing, fdf$freq,
        labels = fdf$formant,
        cex = fdf$infl # cooks.distance(mod) + 1
      )
      # for closed-open tubes we don't expect formant freqs to be integer
      # multiples - could be harmonics instead of formants, show with a blue
      # line
      if (length(formants) > 1 & is.finite(formants$f1$freq[1]) &
          tube %in% c('closed-open', 'open-closed'))
        abline(formants$f1$freq[1] / 2, formants$f1$freq[1],
               lty = 3, col = 'blue')
      if (method == 'regression') {
        if (interceptZero) {
          abline(a = 0, b = formantDispersion, lty = 2, lwd = 2)
        } else {
          abline(summary(mod)$coef[, 1], lty = 2, lwd = 2)
        }
      }
      plot(vf, type = 'b',
           xlab = 'Number of formants used',
           ylab = 'VTL estimate, cm',
           main = paste('VTL =', round(speedSound / 2 / formantDispersion, 1), 'cm'))
      par(mfrow = c(1, 1))
    }
    if (output == 'detailed') {
      formantDispersion = list(formantDispersion = formantDispersion,
                               formantDispersion_95CI = ci,
                               regressionInfo = fdf,
                               vtlPerFormant = vf)
    }
  }
  return(formantDispersion)
}
