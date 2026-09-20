### UTILITIES FOR CONVERTING BETWEEN DIFFERENT FREQUENCY SCALES ###

#' Convert between Hz and other frequency scales
#'
#' Converts between Hz and ERB, bark, mel, log, semitones relative to a reference
#' frequency, or musical notes. Accepts vectors and missing values.
#'
#' @param x vector or matrix of frequencies
#' @param scale target scale: "bark" = Zwicker's critical bandwidth scale
#'   calculated as in Wang et al. 1992 (see
#'   https://en.wikipedia.org/wiki/Bark_scale), "mel" = O'Shaughnessy's original
#'   formula (https://en.wikipedia.org/wiki/Mel_scale), "ERB" = Equivalent
#'   Rectangular Bandwidth rate (see Moore & Glasberg 1983, 1990), "log" = log2,
#'   "semitones" = semitones relative to a reference value, "notes" = musical
#'   notation, "linear" or "orig" = no change
#' @param ... other arguments passed on to the scale-specific function
#'
#' @section Scale‑specific arguments:
#' The following arguments can be passed via \code{...} . They are only used when
#' the corresponding \code{scale} is selected; otherwise, they are silently ignored.
#' \describe{
#'   \item{method}{Character, used when \code{scale = "ERB"}. Choose between
#'     "linear" (Glasberg & Moore, 1990) and "quadratic" (Moore &
#'     Glasberg, 1983). Default is "linear".}
#'   \item{ref}{Numeric, used when \code{scale = "semitones"}. Reference
#'     frequency (Hz) for the semitone scale. Default is C-5
#'     (0.5109875 Hz).}
#'   \item{A4}{Numeric, used when \code{scale = "notes"}. Frequency of the
#'     note A4. Default is 440 Hz (modern standard ISO 16, concert pitch).}
#'   \item{showCents}{Logical, used only with HzToOther() when
#'     \code{scale = "notes"}. If \code{TRUE}, appends the cent deviation
#'     (rounded to the nearest integer) to the note name. Default is
#'     \code{FALSE}. Not applicable to otherToHz().}
#' }
#' No additional arguments are required or usedor scale = "bark", "mel", "log",
#' or "linear".
#'
#' @references \itemize{
#'   \item Moore, B. C., & Glasberg, B. R. (1983). Suggested formulae for
#'   calculating auditory-filter bandwidths and excitation patterns. The journal
#'   of the acoustical society of America, 74(3), 750-753.
#'   \item Glasberg, B. R., & Moore, B. C. (1990). Derivation of auditory filter
#'   shapes from notched-noise data. Hearing research, 47(1-2), 103-138.
#' }
#'
#' @return Numeric vector of converted frequencies.
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
#' otherToHz(c('A4', 'C#6', 'blabla', 'C0', 'C-2', NA), 'notes')
#' HzToOther(c(440, 293, 115, 16.35, 4), 'notes')
#'
#' HzToOther(c(440, 415, 80, 81), 'notes', showCents = TRUE)
#' # 80 Hz is almost exactly midway (+49 cents) between D#2 and E2
#'
#' # Baroque tuning A415, half a semitone flat relative to concert pitch A440
#' HzToOther(c(440, 415, 16.35), 'notes', A4 = 415)
#' otherToHz(c("A4", "D4", "A#2", "C0", "C-2"), 'notes', A4 = 415)
HzToOther = function(
    x,
    scale = c('ERB', 'bark', 'mel', 'log', 'semitones', 'notes', 'linear', 'orig'),
    ...) {
  if (is.null(scale)) return(x)
  scale = match.arg(scale)

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
  } else if (scale %in% c('linear', 'orig')) {
    # convenient to avoid having to check for this special case when calling
    # HzToOther()
    x
  }
}


#' @rdname HzToOther
#' @export
otherToHz = function(
    x,
    scale = c('ERB', 'bark', 'mel', 'log', 'semitones', 'notes', 'linear', 'orig'),
    ...) {
  if (is.null(scale)) return(x)
  scale = match.arg(scale)
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
  } else if (scale %in% c('linear', 'orig')) {
    # convenient to avoid having to check for this special case when calling
    # HzToOther()
    x
  }
}


#' Convert between Hz and notes
#'
#' Converts from Hz to musical notation like A4 - note A of the fourth octave
#' above C0 (16.35 Hz).
#' @noRd
HzToNotes = function(x, showCents = FALSE, A4 = 440) {
  ref = A4 / 861.0779
  # C4 is 9 semitones below A4 (2^(9/12) * 512 = 861.0779), then another 9
  # octaves down to C-5 in notesDict
  sem = log2(x / ref) * 12
  round_sem = round(sem)
  idx = 1 + round_sem
  if (any(idx < 1 | idx > nrow(notesDict), na.rm = TRUE)) {
    out_of_range = idx < 1 | idx > nrow(notesDict)
    idx[out_of_range] = NA
  }
  nearest_note = notesDict$note[idx]
  if (showCents) {
    cents = paste0(' ', as.character(round((sem - round_sem) * 100)))
    cents[cents == ' 0'] = ''
    nearest_note = paste0(nearest_note, cents)
  }
  nearest_note
}


#' Convert notes to Hz
#' @noRd
notesToHz = function(x, A4 = 440) {
  notesDict$freq[match(x, notesDict$note)] * A4 / 440
}


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
#' @return Numeric vector of converted frequencies.
#' @noRd
#' @examples
#' s = soundgen:::HzToSemitones(c(440, 293, 115))
#' # to convert to musical notation
#' notesDict$note[1 + round(s)]
#' # note the "1 +": semitones ABOVE C-5, i.e. notesDict[1, ] is C-5
#'
#' # Any reference tone can be specified. For ex., for semitones above C0, use:
#' soundgen:::HzToSemitones(440, ref = 16.35)
#' # TIP: see notesDict for a table of Hz frequencies to musical notation
#'
#' soundgen:::semitonesToHz(c(117, 105, 60))
HzToSemitones = function(x, ref = 0.5109875) {
  log2(x / ref) * 12
}


#' HzToSemitones
#' @noRd
semitonesToHz = function(x, ref = 0.5109875) {
  ref * 2 ^ (x / 12)
}


#' Convert Hz to ERB rate
#' @noRd
HzToERB = function(x,
                   method = c('linear', 'quadratic')) {
  method = match.arg(method)
  if (method == 'linear') {
    21.4 * log10(1 + .00437 * x)
  } else if (method == 'quadratic') {
    # 11.17 * log( (x + 312) / (x + 14675)) + 43
    11.17268 * log(1 + (46.06538 * x) / (x + 14678.49))
  }
}


#' Convert ERB rate to Hz
#' @noRd
ERBToHz = function(x,
                   method = c('linear', 'quadratic')) {
  method = match.arg(method)
  if (method == 'linear') {
    (10 ^ (x / 21.4) - 1) / .00437
  } else if (method == 'quadratic') {
    676170.4 / (47.06538 - exp(x * 0.08950404)) - 14678.49
  }
}
