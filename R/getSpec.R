#' Universal spectrogram extractor
#'
#' Routes user-friendly strings to various functions for producing
#' spectrogram-like representations (spectrogram, mel-spectrograms, RMS
#' envelopes, spectra, etc.), executes them, and standardizes the output to a
#' matrix with features in rows and time (in ms) in columns.
#'
#' @return Returns a spectrogram-like matrix with time in columns (ms) and
#'   features in rows (names as passed by \code{specFun}).
#'
#' @param audio a list returned by \code{readAudio}
#' @param specFun the function used to extract a spectrogram-like feature matrix.
#'   Can be a string or a custom function that takes audio (numeric vector) as the
#'   first argument and returns a spectrogram-like matrix with time in columns
#'   and features in rows (see examples). Supported strings:
#'   \describe{
#'     \item{\code{\link{stft_simple}}}{
#'       \code{'stft'} / \code{STFT} / \code{'stft_simple'} (amplitude spectrogram)
#'       Parameters in \code{specFun_pars}: \code{samplingRate} (optional if
#'       frequency and time labels are not needed), \code{wl} (samples),
#'       \code{step} (samples), \code{wn}, \code{zp}, \code{padWithSilence}.
#'     }
#'     \item{\code{\link{spectrogram}}}{
#'       \code{'spectrogram'} (amplitude spectrogram - a wrapper around
#'       stft_simple with more options).
#'       Parameters in \code{specFun_pars}: see \code{\link{spectrogram}}.
#'     }
#'     \item{\code{\link[tuneR:powspec]{powspec}}}{
#'       \code{'powerspec'} (power spectrogram with tuneR).
#'       Parameters in \code{specFun_pars}: \code{wintime} (s),
#'        \code{steptime} (s), \code{dither}.
#'     }
#'     \item{\code{\link[tuneR:melfcc]{melfcc}}}{
#'       \code{'melspec'} (mel-spectrogram with tuneR), \code{'mfcc'} /
#'       \code{'melfcc'} (MFCCs).
#'       Parameters in \code{specFun_pars}: \code{windowLength} (ms), \code{step} (ms),
#'       \code{nbands}, \code{maxfreq} (Hz), \code{MFCC} (integer vector).
#'     }
#'     \item{\code{\link{audSpectrogram}}}{
#'       \code{'audSpectrogram'} / \code{'audSpec'} (auditory spectrogram).
#'       Parameters in \code{specFun_pars}: see \code{\link{audSpectrogram}}.
#'     }
#'     \item{\code{\link{getRMS}}}{
#'       \code{'getRMS'} / \code{'rms'} (RMS amplitude envelope).
#'       Parameters in \code{specFun_pars}: see \code{\link{getRMS}}.
#'     }
#'     \item{\code{\link{getEnv}}}{
#'       \code{'getEnv'} / \code{'env'} (upsampled envelopes: RMS, analytical, peak, etc.).
#'       Parameters in \code{specFun_pars}: see \code{\link{getEnv}}.
#'     }
#'     \item{\code{\link{spectrum}}}{
#'       \code{'spectrum'} (short-term spectrum).
#'       Parameters in \code{specFun_pars}: see \code{\link{spectrum}}.
#'     }
#'     \item{\code{\link{meanSpectrum}}}{
#'       \code{'meanSpectrum'} / \code{'meanspec'} / \code{'meanSpec'}
#'       (long-term average spectrum).
#'       Parameters in \code{specFun_pars}: see \code{\link{meanSpectrum}}.
#'     }
#'     \item{\code{\link{ssm}}}{
#'       \code{'ssm'} (self-similarity matrix).
#'       Parameters in \code{specFun_pars}: see \code{\link{ssm}}.
#'     }
#'     \item{\code{\link{modulationSpectrum}}}{
#'       \code{'ms'} / \code{'modulationSpectrum'} (modulation spectrum).
#'       Parameters in \code{specFun_pars}: see \code{\link{modulationSpectrum}}.
#'     }
#'   }
#' @param specFun_pars a list of parameters passed to \code{specFun}
#' @keywords internal
#' @examples
#' a = rnorm(100)
#' # call function by name as a string (must be among the recognized strings)
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'powspec',  # adds freq labels
#'         specFun_pars = list(wintime = .02, steptime = .01))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'spectrogram',
#'         specFun_pars = list(windowLength = 25))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'stft',
#'         specFun_pars = list(wl = 20))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'melspec',
#'         specFun_pars = list(nbands = 5))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'env',
#'         specFun_pars = list(wl = 20))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'ssm',  # ssm itself calls getSpec()
#'         specFun_pars = list(specFun = 'stft', specFun_pars = list(wl = 20)))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'env',
#'         specFun_pars = list(wl = 20))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'env',
#'         specFun_pars = list(wl = 20))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'env',
#'         specFun_pars = list(wl = 20))
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = 'spectrum',
#'         specFun_pars = list(wl = 20))
#'
#' # pass a function object directly (must accept audio as a numeric vector)
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = tuneR::powspec,
#'         specFun_pars = list(sr = 1600, wintime = .02, steptime = .01))
#' # (NB: frequency labels not added - must be handled by the specFun itself)
#' soundgen:::getSpec(audio = soundgen:::readAudio(a, samplingRate = 1600),
#'         specFun = spectrogram,
#'         specFun_pars = list(samplingRate = 1600, windowLength = 2, plot = FALSE))
#'
#' # construct a custom function
#' soundgen:::getSpec(audio = soundgen:::readAudio(1:10, samplingRate = 20),
#'         specFun = function(x) matrix(cumsum(x), nrow = 1),
#'         specFun_pars = list())
getSpec = function(audio,
                   specFun = 'melfcc',
                   specFun_pars = list()) {

  # Helper to validate output
  validate_spec = function(mat) {
    if (!is.matrix(mat)) return(FALSE)
    if (!is.numeric(mat)) return(FALSE)
    if (nrow(mat) == 0 || ncol(mat) == 0) return(FALSE)
    return(TRUE)
  }

  # Make sure we don't save any plots when we call specFun
  audio$savePlots = FALSE
  # could also set plot = FALSE, but not all functions have that argument (eg tuneR::powspec)
  # instead, let's ensure that all soundgen internals default to plot = FALSE
  timeShift = if (is.null(audio$timeShift)) 0 else audio$timeShift

  # List of known internal functions
  known_funs = c('powerspec', 'powspec',
                 'stft', 'STFT', 'stft_simple',
                 'melspec', 'mfcc', 'melfcc',
                 'audSpectrogram', 'audSpec',
                 'ssm',
                 'ms', 'modulationSpectrum',
                 'spectrogram',
                 'getRMS', 'rms', 'env',
                 'spectrum', 'meanSpectrum', 'meanspec', 'meanSpec')

  # Resolve and call the function
  if (is.function(specFun)) {
    # User provided a function object directly
    fun = 'custom'
    res = try(do.call(specFun, c(list(audio$sound), specFun_pars)), silent = TRUE)
    if (inherits(res, 'try-error')) {
      stop('Failed to extract spectrogram using custom function: ',
           attr(res, 'condition')$message)
    }
  } else if (is.character(specFun) && specFun %in% known_funs) {
    # Route to internal dot-functions
    if (specFun %in% c('powerspec', 'powspec')) {
      fun = 'powspec'
    } else if (specFun %in% c('stft', 'STFT', 'stft_simple')) {
      fun = 'stft_simple'
    } else if (specFun %in% c('melspec', 'mfcc', 'melfcc')) {
      fun = '.getMelfcc'
      type = switch(specFun, 'melspec' = 'melspec', 'mfcc' = 'mfcc',
                    'melfcc' = 'mfcc')
      specFun_pars$type = type
    } else if (specFun %in% c('audSpectrogram', 'audSpec')) {
      fun = '.audSpectrogram'
      specFun_pars$plot = FALSE
    } else if (specFun == 'spectrogram') {
      fun = '.spectrogram'
      specFun_pars$plot = FALSE
      if (is.null(specFun_pars$output)) specFun_pars$output = 'original'
    } else if (specFun %in% c('getRMS', 'rms')) {
      fun = '.getRMS'
      specFun_pars$plot = FALSE
    } else if (specFun %in% c('env', 'getEnv')) {
      fun = 'getEnv'
    } else if (specFun == 'spectrum') {
      fun = '.spectrum'
      specFun_pars$plot = FALSE
    } else if (specFun %in% c('meanSpectrum', 'meanspec', 'meanSpec')) {
      fun = '.meanSpectrum'
      specFun_pars$plot = FALSE
    } else if (specFun == 'ssm') {
      fun = '.ssm'
      specFun_pars$plot = FALSE
    } else if (specFun %in% c('ms', 'modulationSpectrum')) {
      fun = '.modulationSpectrum'
      specFun_pars$plot = FALSE
    }

    # special cases: functions that don't accept "audio"
    if (fun == 'stft_simple') {
      specFun_pars$samplingRate = NULL
      res = try(do.call(stft_simple, c(
        list(x = audio$sound, samplingRate = audio$samplingRate), specFun_pars)),
        silent = TRUE)
    } else if (fun == 'getEnv') {
      res = try(do.call(getEnv, c(
        list(audio$sound), specFun_pars)),
        silent = TRUE)
    } else if (fun == 'powspec') {
      specFun_pars$sr = NULL
      res = try(do.call(tuneR::powspec, c(list(
        x = audio$sound, sr = audio$samplingRate),
        specFun_pars)), silent = TRUE)
      if (!inherits(res, 'try-error')) {
        colnames(res) = seq(audio$timeShift, audio$duration, length.out = ncol(res))
        # wl always even in tuneR::powspec, and it returns 1:(wl %/% 2) rows (w/o nyquist bin)
        nr = nrow(res)
        bin_width = audio$samplingRate / (nr * 2)
        rownames(res) = (0:(nr - 1)) * bin_width / 1000
      }
    } else {
      res = try(do.call(fun, c(list(audio = audio), specFun_pars)),
                silent = TRUE)
    }

    if (inherits(res, 'try-error'))
      stop('Failed to extract spectrogram using internal function "', specFun,
           '": ', attr(res, 'condition')$message)
  } else {
    # Try to match a user-defined function by name
    fun = try(match.fun(specFun), silent = TRUE)
    if (inherits(fun, 'try-error')) {
      stop('specFun "', specFun, '" is not a recognized internal function ',
           'and could not be matched to a user-defined function.')
    }

    res = try(do.call(fun, c(list(audio$sound), specFun_pars)), silent = TRUE)
    if (inherits(res, 'try-error')) {
      stop('Failed to extract spectrogram using custom function "',
           specFun, '": ', attr(res, 'condition')$message)
    }
    fun = 'custom'
  }

  # Standardize output to matrix (features x time in ms)
  mat = NULL
  if (fun == 'stft_simple') {
    mat = Mod(res[1:(nrow(res) %/% 2 + 1), , drop = FALSE])
  } else if (fun == '.audSpectrogram') {
    mat = res$audSpec
  } else if (fun %in% c('.getRMS', 'getEnv')) {
    mat = matrix(res, nrow = 1)
  } else if (fun %in% c('.spectrum', '.meanSpectrum')) {
    mat = matrix(res$ampl, ncol = 1)
    rownames(mat) = res$freq
  } else if (fun == '.ssm') {
    mat = res$ssm
  } else if (fun == '.modulationSpectrum') {
    mat = res$original
  } else {
    mat = res
  }

  # Validate final matrix
  if (!validate_spec(mat))
    stop("specFun did not return a recognized spectrogram-like representation")

  if (is.null(colnames(mat))) {
    colnames(mat) = seq(audio$timeShift * 1000, audio$duration * 1000,
                        length.out = ncol(mat))
  } else {
    cols_num = as.numeric(colnames(mat))
    ran = tail(cols_num, 1)
    if (!is.finite(ran) ||  # weird non-numeric time labels
        ran < (audio$duration * 2))  # probably in s, not ms
      colnames(mat) = cols_num * 1000
  }

  mat
}


#' Extract MFCC / Melspec / Spec via tuneR
#'
#' Internal wrapper for tuneR::melfcc that handles Wave conversion, validation,
#' and formatting the output matrix to have features in rows and time in seconds.
#' @param audio a list returned by \code{readAudio}
#' @param type one of 'melspec', 'mfcc', or 'spec'
#' @param MFCC which cepstral coefficients to keep (if type == 'mfcc')
#' @param windowLength,step,nbands,maxfreq parameters passed to tuneR::melfcc
#' @param ... additional arguments passed to tuneR::melfcc
#' @noRd
.getMelfcc = function(audio,
                      type = c('melspec', 'mfcc', 'melfcc'),
                      MFCC = 2:13,
                      windowLength = 25,
                      step = 25 / 4,
                      nbands = 50,
                      maxfreq = NULL,
                      ...) {
  type = match.arg(type)
  nyquist = audio$samplingRate / 2
  if (is.null(maxfreq)) maxfreq = nyquist
  if (is.null(windowLength)) windowLength = 25
  if (is.null(step)) step = windowLength / 4

  if (!is.numeric(windowLength) || windowLength <= 0 ||
      windowLength > (audio$duration / 2 * 1000)) {
    windowLength = min(50, max(1, round(audio$duration / 2 * 1000)))
    warning(sprintf(
      '"windowLength" must be between 0 and half the sound duration (in ms); resetting to %s ms',
      windowLength))
  }
  if (is.null(nbands)) nbands = round(100 * windowLength / 20)

  sound = tuneR::Wave(left = audio$sound, samp.rate = audio$samplingRate, bit = 16)
  mel = do.call(tuneR::melfcc, c(list(
    samples = sound,
    wintime = windowLength / 1000,
    hoptime = step / 1000,
    maxfreq = maxfreq,
    spec_out = TRUE,
    numcep = max(MFCC),
    nbands = nbands
  ), list(...)))

  if (type %in% c('mfcc', 'melfcc')) {
    # the first cepstrum overestimates the similarity of different frames
    mat = t(mel$cepstra)[MFCC, , drop = FALSE]
    mat[is.na(mat)] = 0  # MFCC are NaN for silent frames
  } else {
    mat = t(mel$aspectrum)
  }

  colnames(mat) = seq(audio$timeShift, audio$duration, length.out = ncol(mat))
  if (type == 'mfcc') {
    rownames(mat) = seq_len(nrow(mat))
  } else {
    rownames(mat) = otherToHz(
      seq(0, HzToOther(nyquist, "mel"), length.out = nrow(mat)), "mel") / 1000
  }
  mat
}
