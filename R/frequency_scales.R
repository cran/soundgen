### UTILITIES FOR CONVERTING BETWEEN DIFFERENT FREQUENCY SCALES ###

#' Convert semitones to Hz and back
#'
#' Converts between Hz and semitones above C-5 (~0.5109875 Hz) or another
#' reference frequency. This may not seem very useful, but note that this gives
#' us a nice logarithmic scale for generating natural pitch transitions. Accepts
#' vectors and missing values.
#'
#' @seealso \code{\link{HzToOther}}
#'
#' @param x A vector or matrix of frequencies
#' @param ref frequency of the reference value (defaults to C-5, 0.51 Hz)
#' @export
#' @examples
#' s = HzToSemitones(c(440, 293, 115))
#' # to convert to musical notation
#' notesDict$note[1 + round(s)]
#' # note the "1 +": semitones ABOVE C-5, i.e. notesDict[1, ] is C-5
#'
#' # Any reference tone can be specified. For ex., for semitones above C0, use:
#' HzToSemitones(440, ref = 16.35)
#' # TIP: see notesDict for a table of Hz frequencies to musical notation
#'
#' semitonesToHz(c(117, 105, 60))
HzToSemitones = function(x, ref = 0.5109875) {
  log2(x / ref) * 12
}


#' @rdname HzToSemitones
#' @export
semitonesToHz = function(x, ref = 0.5109875) {
  ref * 2 ^ (x / 12)
}


#' Convert between Hz and notes
#'
#' Converts from Hz to musical notation like A4 - note A of the fourth octave
#' above C0 (16.35 Hz).
#'
#' @seealso \code{\link{HzToOther}}
#'
#' @param x vector or matrix of frequencies or notes
#' @param showCents if TRUE, show cents to the nearest notes (cent = 1/100 of a
#'   semitone)
#' @param A4 frequency of note A in the fourth octave (modern standard ISO 16 or
#'   concert pitch = 440 Hz)
#' @export
#' @examples
#' HzToNotes(c(440, 293, 115, 16.35, 4))
#' notesToHz(c("A4", "D4", "A#2", "C0", "C-2"))
#'
#' HzToNotes(c(440, 415, 80, 81), showCents = TRUE)
#' # 80 Hz is almost exactly midway (+49 cents) between D#2 and E2
#'
#' # Baroque tuning A415, half a semitone flat relative to concert pitch A440
#' HzToNotes(c(440, 415, 16.35), A4 = 415)
#' notesToHz(c("A4", "D4", "A#2", "C0", "C-2"), A4 = 415)
HzToNotes = function(x, showCents = FALSE, A4 = 440) {
  ref = A4 / 861.0779
  # C4 is 9 semitones below A4 (2^(9/12) * 512 = 861.0779), then another 9
  # octaves down to C-5 in notesDict
  sem = log2(x / ref) * 12
  round_sem = round(sem)
  nearest_note = soundgen::notesDict$note[1 + round_sem]
  if (showCents) {
    cents = paste0(' ', as.character(round((sem - round_sem) * 100)))
    cents[cents == ' 0'] = ''
    nearest_note = paste0(nearest_note, cents)
  }
  nearest_note
}


#' @describeIn HzToNotes Convert notes to Hz
#' @export
notesToHz = function(x, A4 = 440) {
  soundgen::notesDict$freq[match(x, soundgen::notesDict$note)] * A4 / 440
}


#' Convert Hz to ERB rate
#'
#' Converts from Hz to the number of Equivalent Rectangular Bandwidths (ERBs)
#' below input frequency. See https://www2.ling.su.se/staff/hartmut/bark.htm and
#' https://en.wikipedia.org/wiki/Equivalent_rectangular_bandwidth
#'
#' @references \itemize{
#'   \item Moore, B. C., & Glasberg, B. R. (1983). Suggested formulae for
#'   calculating auditory-filter bandwidths and excitation patterns. The journal
#'   of the acoustical society of America, 74(3), 750-753.
#'   \item Glasberg, B. R., & Moore, B. C. (1990). Derivation of auditory filter
#'   shapes from notched-noise data. Hearing research, 47(1-2), 103-138.
#' }
#'
#' @seealso \code{\link{HzToOther}}
#'
#' @param x vector or matrix of frequencies
#' @param method approximation to use: "linear" = Glasberg & Moore (1990),
#'   "quadratic" = Moore & Glasberg (1983)
#' @export
#' @examples
#' HzToERB(c(-20, 20, 100, 440, 1000, NA))
#'
#' f = 20:20000
#' erb_lin = HzToERB(f, 'linear')
#' erb_quadratic = HzToERB(f, 'quadratic')
#' plot(f, erb_lin, log = 'x', type = 'l')
#' points(f, erb_quadratic, col = 'blue', type = 'l')
#'
#' freqs_Hz = c(-20, 20, 100, 440, 1000, 20000, NA)
#' e_lin = HzToERB(freqs_Hz, 'linear')
#' ERBToHz(e_lin, 'linear')
#'
#' e_quad = HzToERB(freqs_Hz, 'quadratic')
#' ERBToHz(e_quad, 'quadratic')
#'
#' # compare with the bark scale:
#' barks = HzToOther(f, 'bark')
#' points(f, barks / max(barks) * max(erb_lin),
#'   col = 'red', type = 'l', lty = 2)
HzToERB = function(x,
                   method = c('linear', 'quadratic')[1]) {
  if (method == 'linear') {
    21.4 * log10(1 + .00437 * x)
  } else if (method == 'quadratic') {
    # 11.17 * log( (x + 312) / (x + 14675)) + 43
    11.17268 * log(1 + (46.06538 * x) / (x + 14678.49))
  }
}


#' @describeIn HzToERB Convert ERB rate to Hz
#' @export
ERBToHz = function(x,
                   method = c('linear', 'quadratic')[1]) {
  if (method == 'linear') {
    (10 ^ (x / 21.4) - 1) / .00437
  } else if (method == 'quadratic') {
    676170.4 / (47.06538 - exp(x * 0.08950404)) - 14678.49
  }
}


#' Convert Hz to mel
#'
#' Internal soundgen function: a temporary fix needed because tuneR::hz2mel
#' doesn't accept NAs or vectors.
#' @param f frequency, Hz
#' @param htk if TRUE, uses an alternative formula 2595 * log10(1 + f/700)
#' @keywords internal
#' @examples
#' soundgen:::hz2mel(c(440, 220, NA))
#' freq = 1:10000
#' plot(freq, soundgen:::hz2mel(freq), type = 'l')
hz2mel = function(f, htk = TRUE) {
  idx_nonFinite = which(!is.finite(f))
  f[idx_nonFinite] = 0
  if (htk) {
    z = 2595 * log10(1 + f/700)
  } else {
    f_0 = 0
    f_sp = 200/3
    brkfrq = 1000
    brkpt = (brkfrq - f_0) / f_sp
    logstep = exp(log(6.4) / 27)
    linpts = (f < brkfrq)
    z = 0 * f
    z[linpts] = (f[linpts] - f_0) / f_sp
    z[!linpts] = brkpt + (log(f[!linpts] / brkfrq)) / log(logstep)
  }
  z[idx_nonFinite] = NA
  z
}


#' Convert between Hz and other frequency scales
#'
#' Converts betwen Hz and ERB, bark, mel, log, semitones relative to a reference
#' frequency, or musical notes. Accepts vectors and missing values.
#'
#' @seealso \code{\link{HzToSemitones}} \code{\link{HzToNotes}}
#'   \code{\link{HzToERB}}
#'
#' @param x vector or matrix of frequencies
#' @param scale target scale: "bark" = Zwicker's critical bandwidth scale
#'   calculated as in Wang et al. 1992 (see
#'   https://en.wikipedia.org/wiki/Bark_scale), "mel" = O'Shaughnessy's original
#'   formula (https://en.wikipedia.org/wiki/Mel_scale), "ERB" = Equivalent
#'   Rectangular Bandwidth rate (see \code{\link{HzToERB}}), "log" = log2,
#'   "semitones" = semitones relative to a reference value (see
#'   \code{\link{HzToSemitones}}), "notes" = musical notation (see
#'   \code{\link{HzToNotes}}), "linear" = no change
#' @param ... other arguments passed on to the scale-specific function
#' @export
#' @examples
#' x = c(-20, 20, 100, 440, 1000, NA)
#' HzToOther(x, 'ERB')
#' HzToOther(x, 'ERB', 'quadratic')
#' HzToOther(x, 'bark')
#' HzToOther(x, 'mel')
#' HzToOther(x, 'log')
#' HzToOther(x, 'semitones', ref = 16)
#' HzToOther(x, 'notes', showCents = TRUE)
#'
#' # ...and back to Hz
#' x = c(0:10, NA)
#'
#' otherToHz(x, 'ERB')
#' otherToHz(x, 'ERB', method = 'quadratic')
#' otherToHz(HzToOther(c(100, 440, 2000), 'ERB'), 'ERB')
#'
#' otherToHz(x, 'bark')
#' otherToHz(HzToOther(c(100, 440, 2000), 'bark'), 'bark')
#'
#' otherToHz(x, 'mel')
#' otherToHz(HzToOther(c(100, 440, 2000), 'mel'), 'mel')
#'
#' otherToHz(x, 'log')
#'
#' otherToHz(x, 'semitones')
#' HzToOther(c(440, 210, 880), 'semitones', ref = 440)
#' otherToHz(HzToOther(c(440, 210, 880), 'semitones'), 'semitones')
#'
#' otherToHz(c('A4', 'C#6', 'blabla', NA), 'notes')
HzToOther = function(
    x,
    scale = c('ERB', 'bark', 'mel', 'log', 'semitones', 'notes')[1],
    ...) {
  if (scale == 'ERB') {
    # 21.4 * log10(1 + .00437 * x)
    do.call(HzToERB, list(x, ...))
  } else if (scale == 'bark') {
    # tuneR::hz2bark doesn't accept NAs
    6 * asinh(x / 600)
  } else if (scale == 'mel') {
    2595 * log10(1 + x / 700)
    # do.call(hz2mel, list(x, ...))
  } else if (scale == 'log') {
    log2(x)
  } else if (scale == 'semitones') {
    do.call(HzToSemitones, list(x, ...))
  } else if (scale == 'notes') {
    do.call(HzToNotes, list(x, ...))
  } else if (scale == 'linear') {
    # convenient to avoid having to check for this special case when calling
    # HzToOther()
    x
  } else {
    stop('scale not recognized')
  }
}


#' @rdname HzToOther
#' @export
otherToHz = function(
    x,
    scale = c('ERB', 'bark', 'mel', 'log', 'semitones', 'notes')[1],
    ...) {
  if (scale == 'ERB') {
    do.call(ERBToHz, list(x, ...))
  } else if (scale == 'bark') {
    600 * sinh(x / 6)
  } else if (scale == 'mel') {
    700 * (10 ^ (x / 2595) - 1)
  } else if (scale == 'log') {
    2^x
  } else if (scale == 'semitones') {
    do.call(semitonesToHz, list(x, ...))
  } else if (scale == 'notes') {
    do.call(notesToHz, list(x, ...))
  } else if (scale == 'linear') {
    # convenient to avoid having to check for this special case when calling
    # HzToOther()
    x
  } else {
    stop('scale not recognized')
  }
}
