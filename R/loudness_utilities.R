#' Phon-sone conversion
#'
#' Conversion between phon and sone loudness scales. Source: Timoney, J.,
#' Lysaght, T., Schoenwiesner, M., & MacManus, L. (2004). Implementing loudness
#' models in matlab.
#' @param phon loudness level, phon (vectorized)
#' @noRd
#' @examples
#' phon = seq(0, 120, 2)
#' sone = soundgen:::phonToSone(phon)
#' plot(phon, sone, type = 'b')
#' plot(phon, log2(sone), type = 'b')
#'
#' phon2 = soundgen:::soneToPhon(sone)
#' plot(phon, phon2); abline(0, 1)
phonToSone = function(phon) {
  sone = phon
  idx1 = which(phon < 40)
  idx2 = which(phon >= 40)
  sone[idx1] = (phon[idx1] / 40) ^ 2.642
  # alternative formula http://www.sengpielaudio.com/calculatorSonephon.htm
  # sone[idx1] = (phon[idx1] / 40) ^ 2.86 - .005
  sone[idx2] = 2 ^ ((phon[idx2] - 40) / 10)
  sone
}

#' @rdname phonToSone
#' @param sone loudness in sones (vectorized)
#' @noRd
soneToPhon = function(sone) {
  phon = sone
  idx1 = which(sone < 1)
  idx2 = which(sone >= 1)
  phon[idx1] = sone[idx1] ^ (1/2.642) * 40
  phon[idx2] = 40 + 10 * log2(sone[idx2])
  phon
}


#' Scale SPL
#'
#' Converts a sound from SPL on any scale to a desired level of dB SPL. See
#' Timoney et al. (2004) "Implementing loudness models in MATLAB", but with Pref
#' removed (effectively set to 1 instead of 2e-5).
#' @param x centered numeric vector
#' @inheritParams getLoudness
#' @noRd
#' @examples
#' sound = rnorm(100) * approx(c(0, 1, 0), n = 100)$y
#' sound = sound / max(abs(sound))
#' range(sound)
#' sound_scaled = soundgen:::scaleSPL(sound)
#' range(sound_scaled)
#' # check that the new RMS is SPL_measured:
#' 20 * log10(sqrt(mean(sound_scaled^2)))
#' # =SPL_measured if SPL_measured when scale = NULL,
#' # or when max(abs(x)) / scale == 1
#'
#' sound2 = sound / max(abs(sound))
#' range(soundgen:::scaleSPL(sound2, scale = NULL))
#' range(soundgen:::scaleSPL(sound2, scale = 5))
scaleSPL = function(x, scale = NULL, SPL_measured = 70) {
  x = x - mean(x)
  RMS = sqrt(mean(x ^ 2))
  if (!is.finite(RMS) || RMS == 0) return(x)
  SPL_internal = 20 * log10(RMS)
  cc = 10 ^ ((SPL_measured - SPL_internal) / 20)
  if (is.numeric(scale)) cc = cc * max(abs(x)) / scale
  cc * x
}


#' iso226
#'
#' Calculates equal loudness curves according to the ISO226 standard. Expected
#' range of input values in phon is 0 to 90 (1 phon is 1 dB at 1 kHz). The range
#' of evaluated frequencies is 20 to 12500 Hz, with a total of 29 values (so
#' upsample if more resolution is needed, but not that beyond 22 barks it's just
#' assumed to be flat). Translated from the matlab implementation by Jeff Tackett
#' (03/01/05) available from
#' "https://www.mathworks.com/matlabcentral/fileexchange/
#' 7028-iso-226-equal-loudness-level-contour-signal"
#' @return A LIST containing evaluated frequencies and SPL values.
#' @param phon the phon value in dB SPL represented by the loudness curve
#' @noRd
#' @examples
#' i = soundgen:::iso226(40)
#' plot(i$curve29, type = 'l')
#' plot(i$curveBark$freq_Hz, i$curveBark$spl, type = 'l')
iso226 = function(phon, nBarks = 22) {
  #  Table from ISO 226
  f = c(20, 25, 31.5, 40, 50, 63, 80, 100, 125, 160,
        200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600,
        2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000, 12500)

  af = c(0.532, 0.506, 0.480, 0.455, 0.432, 0.409, 0.387, 0.367, 0.349, 0.330,
         0.315, 0.301, 0.288, 0.276, 0.267, 0.259, 0.253, 0.250, 0.246, 0.244,
         0.243, 0.243, 0.243, 0.242, 0.242, 0.245, 0.254, 0.271, 0.301)

  Lu = c(-31.6, -27.2, -23.0, -19.1, -15.9, -13.0, -10.3, -8.1, -6.2, -4.5,
         -3.1, -2.0, -1.1, -0.4, 0.0, 0.3, 0.5, 0.0, -2.7, -4.1,
         -1.0, 1.7, 2.5, 1.2, -2.1, -7.1, -11.2, -10.7, -3.1)

  Tf = c(78.5, 68.7, 59.5, 51.1, 44.0, 37.5, 31.5, 26.5, 22.1, 17.9,
         14.4, 11.4, 8.6, 6.2, 4.4, 3.0, 2.2, 2.4, 3.5, 1.7,
         -1.3, -4.2, -6.0, -5.4, -1.5, 6.0, 12.6, 13.9, 12.3)

  # Warn if phon is outside the covered range
  if (any(phon < 0 | phon > 90, na.rm = TRUE)) {
    warning('Valid range 0 to 90; extrapolating beyond may be incorrect')
  }

  # Deriving sound pressure level from loudness level (iso226 sect 4.1)
  Af = 4.47e-3 * (10 ^ (0.025 * phon) - 1.15) +
    (0.4 * 10 ^ (((Tf + Lu) / 10) - 9)) ^ af
  Lp = ((10 / af) * log10(Af)) - Lu + 94

  # Calculate on the bark scale
  barkFreqs_hz = 600 * sinh(seq_len(nBarks) / 6)
  s = spline(y = Lp, x = f, n = 1000)
  ups = data.frame(freq = s$x, spl = s$y)  # upsampled curve
  b = data.frame(
    freq_bark = seq_len(nBarks),
    freq_Hz = barkFreqs_hz
  )
  freq_KHz = b$freq_Hz / 1000
  b$hearingThres_dB = 3.64 / (freq_KHz ^ 0.8) -
    6.5 * exp(-0.6 * (freq_KHz - 3.3) ^ 2) +
    0.001 * freq_KHz ^ 4
  for (i in 1:nrow(b)) {
    b$spl[i] = ups$spl[which.min(abs(ups$freq - barkFreqs_hz[i]))]
  }

  list(
    curve29 = data.frame(freq = f, spl = Lp),
    curveBark = b
  )
}


#' Spread spectrum
#'
#' Spreads spectrum to account for frequency masking across critical bands. See
#' Wonho (1999) "Enhanced modified bark spectral distortion (EMBSD)"
#' @param barkSpec a numeric vector of length equal to the number of critical
#'   bands (eg 1 to 24 barks), giving the power in each band on a linear scale
#'   (not dB)
#' @noRd
#' @examples
#' barkSpec = c(rep(0, 10), 20000, 2e5, 2e4, 1e4, 2000, rep(0, 8))
#' plot(soundgen:::spreadSpec(barkSpec), type = 'b', col = 'red')
#' points(barkSpec, type = 'b', col = 'blue')
spreadSpec = function(barkSpec) {
  n = length(barkSpec)
  barkSpec_spread = vector('numeric', n)
  for (i in 1:n) {
    barkSpec_spread[i] = sum(spreadSpecCoef[i, 1:n] * barkSpec)
  }
  barkSpec_spread
}


#' Get sharpness
#'
#' Takes a sone-spectrogram and calculates the sharpness of each frame as the
#' first moment of specific loudness on the Bark scale:
#' \deqn{S = 0.11 * \frac{\int z * g(z) * N'(z) dz}{\int N'(z) dz}}
#' By default, it is assumed that each row corresponds to 1 Bark. If rows are
#' spaced differently (e.g. an auditory filter bank), supply \code{bark} and
#' \code{barkWidth}.
#'
#' @param specSone a matrix of specific loudness per bin and STFT frame created
#'   by .getLoudness: time in columns and specific loudness (sones per bark) in
#'   rows
#' @param method the method of calculating sharpness (mostly differ in
#'   weighting functions; only "aures" uses level-dependent weights)
#' @param loudness optional total loudness per frame, usually
#'   \code{colSums(specSone * barkWidth)}. If NULL, it is calculated internally.
#' @param bark optional numeric vector of critical-band centers in Bark. If
#'   NULL, defaults to 1:nrow(specSone).
#' @param barkWidth optional numeric vector of critical-band widths in Bark. If
#'   NULL, defaults to 1 for each row.
#'
#' @return A numeric vector of sharpness values in acum, one per STFT frame.
#' @keywords internal
#' @examples
#' s1 = cos(2*pi*1000*(1:8000)/16000)
#' l1 = getLoudness(s1, 16000, SPL_measured = 60, sharpnessMethod = 'aures')
#' # image(t(l1$detailed$specSone))
#' l1$detailed$sharpness
#' soundgen:::getSharpness(l1$detailed$specSone, method = 'aures') # same
#'
#' s2 = soundgen()
#' l2 = getLoudness(s2, 16000, SPL_measured = 60, sharpnessMethod = 'aures')
#' # image(t(l2$detailed$specSone))
#' plot(l2$detailed$sharpness, type = 'b', ylim = c(0, 5))
#' points(soundgen:::getSharpness(l2$detailed$specSone, method = 'DIN45692'),
#'   type = 'l', col = 'blue')
#' points(soundgen:::getSharpness(l2$detailed$specSone, method = 'bismarck'),
#'   type = 'l', col = 'green')
#' points(soundgen:::getSharpness(l2$detailed$specSone, method = 'aures'),
#'   type = 'l', col = 'orange')
getSharpness = function(
    specSone,
    method = c('aures', 'DIN45692', 'bismarck'),
    loudness = NULL,
    bark = NULL,
    barkWidth = NULL) {
  method = match.arg(method)

  # allow vector input
  if (is.null(dim(specSone))) {
    specSone = matrix(specSone, ncol = 1)
  }
  nr = nrow(specSone)
  nc = ncol(specSone)
  if (nr == 0 || nc == 0) {
    return(numeric(0))
  }

  # non-finite values would contaminate the sums
  specSone[!is.finite(specSone)] = 0

  # default: one row = one critical band of width 1 Bark
  if (is.null(bark)) {
    bark = seq_len(nr)
  }
  if (is.null(barkWidth)) {
    barkWidth = rep(1, nr)
  }
  bark = rep_len(bark, nr)
  barkWidth = rep_len(barkWidth, nr)
  bark[!is.finite(bark)] = 0
  barkWidth[!is.finite(barkWidth) | barkWidth < 0] = 0

  # calculate the weights
  if (method == 'aures') {
    # Aure's weights depend on the specific loudness in each bin and frame
    weights = matrix(1, nr, nc)
    z = matrix(bark, nr, nc)

    idx = specSone > 0 & z > 0
    if (any(idx)) {
      weights[idx] = 0.078 * (exp(0.171 * z[idx]) / z[idx]) *
        (specSone[idx] / log(0.05 * specSone[idx] + 1))
    }

    # all-zero frames get zero weights, matching sharpnessWeights()
    zeroFrames = which(colSums(specSone > 0) == 0)
    if (length(zeroFrames) > 0) {
      weights[, zeroFrames] = 0
    }
  } else {
    # re-use the same weights for all frames
    weights = rep(1, nr)

    if (method == 'DIN45692') {
      # Widmann model according to DIN 45692 (2009)
      idx = bark >= 15.8
      weights[idx] = 0.15 * exp(0.42 * (bark[idx] - 15.8)) + 0.85
    } else if (method == 'bismarck') {
      # von Bismarck's model according to DIN 45692 (2009)
      idx = bark >= 15
      weights[idx] = 0.2 * exp(0.308 * (bark[idx] - 15)) + 0.8
    }

    weights = matrix(weights, nr, nc)
  }

  weights[!is.finite(weights)] = 1

  # total loudness is the integral of specific loudness across Bark
  z = matrix(bark, nr, nc)
  dz = matrix(barkWidth, nr, nc)

  if (is.null(loudness)) {
    loudness = colSums(specSone * dz)
  } else {
    loudness = rep_len(loudness, nc)
  }

  # calculate sharpness as weighted first moment of specific loudness
  numerator = colSums(specSone * weights * z * dz)

  sharpness = rep(NA_real_, nc)
  idx_ok = loudness > 0 & is.finite(loudness)
  sharpness[idx_ok] = 0.11 * numerator[idx_ok] / loudness[idx_ok]

  sharpness[!is.finite(sharpness)] = NA
  sharpness
}
