#' Add amplitude modulation
#'
#' Adds sinusoidal or logistic amplitude modulation to a sound. Sinusoidal AM
#' creates a single pair of sidebands at ±amFreq around each original harmonic,
#' whereas non-sinusoidal AM creates broader sidebands with more extra harmonics
#' (see examples).
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams soundgen
#'
#' @return Returns the modified audio as a numeric vector with the original
#'   sampling rate.
#' @export
#' @examples
#' sound1 = soundgen(pitch = c(200, 300), addSilence = 0)
#' s1 = addAM(sound1, 16000, amDep = c(0, 50, 0), amFreq = 75, plot = TRUE)
#' # playme(s1)
#' \dontrun{
#' # Parameters can be specified as in the soundgen() function, eg:
#' s2 = addAM(sound1, 16000,
#'          amDep = list(time = c(0, 50, 52, 200, 201, 300),
#'                       value = c(0, 0, 35, 25, 0, 0)),
#'          plot = TRUE, play = TRUE)
#'
#' # Sinusoidal AM produces exactly 2 extra harmonics at ±amFreq
#' # around each f0 harmonic (amFreq, and thus the width of sidebands,
#' # may vary over time):
#' s3 = addAM(sound1, 16000, amDep = 30, amFreq = c(50, 80),
#'            amType = 'sine', plot = TRUE, play = TRUE)
#' spectrogram(s3, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # Non-sinusoidal AM produces multiple new harmonics,
#' # which can resemble subharmonics...
#' s4 = addAM(sound1, 16000, amDep = 70, amFreq = 50, amShape = -1,
#'            plot = TRUE, play = TRUE)
#' spectrogram(s4, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # ...but more often look like sidebands
#' sound3 = soundgen(sylLen = 600, pitch = c(800, 1300, 1100), addSilence = 0)
#' s5 = addAM(sound3, 16000, amDep = c(0, 30, 100, 40, 0),
#'            amFreq = 105, amShape = -.3,
#'            plot = TRUE, play = TRUE)
#' spectrogram(s5, 16000, ylim = c(0, 5))
#'
#' # Feel free to add AM stochastically:
#' s6 = addAM(sound1, 16000,
#'            amDep = rnorm(10, 40, 20), amFreq = rnorm(20, 70, 20),
#'            plot = TRUE, play = TRUE)
#' spectrogram(s6, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # If amFreq is locked to an integer ratio of f0, we can get subharmonics
#' # For ex., here is with pitch 400-600-400 Hz (soundgen interpolates pitch
#' # on a log scale and amFreq on a linear scale, so we align them by extracting
#' # a long contour on a log scale for both)
#' con = soundgen:::getSmoothContour(anchors = c(400, 600, 400),
#'                        len = 20, thisIsPitch = TRUE)
#' s = soundgen(sylLen = 1500, pitch = con, amFreq = con/3, amDep = 30,
#'              plot = TRUE, play = TRUE, ylim = c(0, 3))
#'
#' # Process all files in a folder and save the modified audio
#' addAM('~/Downloads/temp', saveAudio = TRUE, amFreq = 70, amDep = c(0, 50))
#' }
addAM = function(x,
                 samplingRate = NULL,
                 amDep = 25,
                 amFreq = 30,
                 amType = c('logistic', 'sine'),
                 amShape = 0,
                 invalidArgAction = c('adjust', 'abort', 'ignore'),
                 play = FALSE,
                 saveAudio = FALSE,
                 plot = FALSE,
                 savePlots = FALSE,
                 embed = FALSE,
                 reportEvery = NULL,
                 cores = 1,
                 width = 900,
                 height = 500,
                 units = 'px',
                 res = NA) {
  # check the format of AM pars
  amType = match.arg(amType)
  invalidArgAction = match.arg(invalidArgAction)
  amDep = reformatAnchors(amDep)
  amFreq = reformatAnchors(amFreq)
  amShape = reformatAnchors(amShape)

  # match args
  myPars = c(as.list(environment()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio', 'savePlots', 'embed')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.addAM',
                    suffix = 'addAM',
                    savePlots = savePlots,
                    saveAudio = saveAudio,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Add AM to a sound
#' @param audio a list returned by \code{readAudio}
#' @param amDep,amFreq contour lists
#' @noRd
.addAM = function(
    audio,
    amDep,
    amFreq,
    amType = 'logistic',
    amShape = reformatAnchors(0),
    invalidArgAction = 'adjust',
    plot = FALSE,
    play = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA
) {

  # vectorize
  if (amType == 'sine') {
    amPar_vect = c('amDep', 'amFreq')
  } else {
    amPar_vect = c('amDep', 'amFreq', 'amShape')
  }

  # just to get rid of of NOTE on CRAN:
  amDep_vector = amFreq_vector = amShape_vector = vector()
  for (p in amPar_vect) {
    p_unique_value = unique(get(p)$value)
    if (length(p_unique_value) > 1) {
      if (invalidArgAction == 'ignore') {
        valueFloor_p = valueCeiling_p = NULL
      } else {
        valueFloor_p = permittedValues[p, 'low']
        valueCeiling_p = permittedValues[p, 'high']
      }
      p_vectorized = getSmoothContour(
        anchors = get(p),
        len = audio$ls,
        interpol = 'linear',
        valueFloor = valueFloor_p,
        valueCeiling = valueCeiling_p
      )
      # plot(p_vectorized, type = 'l')
      assign(paste0(p, '_vector'), p_vectorized)
    } else {
      assign(paste0(p, '_vector'), p_unique_value)
    }
  }

  # prepare am vector
  if (amType == 'sine') {
    if (length(amFreq_vector) == 1) {
      int = amFreq_vector * (0:(audio$ls - 1))
    } else {
      int = cumsum(amFreq_vector)
    }
    sig = .5 + .5 * cos(2 * pi * int / audio$samplingRate)
  } else {
    sig = getSigmoid(len = audio$ls,
                     samplingRate = audio$samplingRate,
                     freq = amFreq_vector,
                     shape = amShape_vector)
  }
  # plot(sig, type = 'l')
  # sig is on a scale [0, 1]
  # if (length(sig) != length(amDep_vector)) browser()
  amDep_vector[amDep_vector < 0] = 0
  amDep_vector[amDep_vector > 100] = 100
  am = 1 - sig * amDep_vector / 100
  sound_am = audio$sound * am

  # plotting
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    .osc(audio = list(sound = am,
                      samplingRate = audio$samplingRate,
                      ls = length(am)),
         main = 'Amplitude modulation',
         xlab = 'Time, ms',
         ylab = '',
         ylim = c(0, 1),
         midline = FALSE)
  }
  if (isTRUE(play)) {
    playme(sound_am, audio$samplingRate)
  } else if (is.character(play)) {
    playme(sound_am, audio$samplingRate, player = play)
  }
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(sound_am, audio = audio, filename = filename)
  }
  sound_am
}


#' Get amplitude modulation
#'
#' \code{getAM_env} extracts the amplitude envelope via the Hilbert transform
#' and identifies the dominant modulation frequency within \code{amRange}, its
#' spectral purity, and the modulation depth. The depth is derived from the
#' ratio of the AC component (at the peak modulation frequency) to the DC
#' component (mean envelope level). Time resolution of the analysis is
#' determined by the slowest AM of interest - the lower bound of \code{amRange}.
#' \code{getAM_ms} extracts the dominant AM frequency and its strength from a
#' modulation spectrum, normally returned by \code{\link{modulationSpectrum}}.
#' The modulation spectrum is folded around 0 Hz temporal modulation and
#' averaged across frequency bands. The function then searches for local maxima
#' within \code{amRange} and returns the highest peak. This averages AM over the
#' entire sound; to measure its change over time, call
#' \code{\link{modulationSpectrum}}, which automatically chunks the audio.
#'
#' @param audio a list returned by \code{readAudio}
#' @param amRange target range of AM frequencies, Hz (vector of length 2)
#' @param overlap STFT overlap, \% (passed to \code{getPeakFreq})
#' @param parab if TRUE, use parabolic interpolation to refine the peak
#' @param plot if TRUE, produces a plot
#' @param m numeric matrix of non-negative values with column names giving
#'   temporal modulation frequencies in Hz. The column names may run from
#'   negative to positive temporal modulation frequencies or contain positive
#'   frequencies only
#' @param amRes target frequency resolution of the AM peak search, Hz. This
#'   controls the width of the rolling window used to search for peaks.
#'   If \code{NULL}, a minimal window of three bins is used
#'
#' @return \code{getAM_env} returns a dataframe with columns \code{time} (ms), \code{freq} (Hz),
#'   \code{purity} (0 to 1), and \code{dep} (0 to 100). \code{getAM_ms} returns a list with two components:
#' \itemize{
#'   \item \code{amMsFreq}: frequency of the highest AM peak within
#'   \code{amRange}, Hz, or \code{NA} if no suitable peak is found.
#'   \item \code{amMsPurity}: peak amplitude relative to the mean amplitude of
#'   the other folded AM values within \code{amRange}, in dB. Positive values
#'   indicate that the peak is stronger than the surrounding AM; \code{NA} is
#'   returned if this ratio cannot be calculated.
#' }
#' @noRd
#' @examples
#' s = soundgen(sylLen = 1000, pitch = 600,
#'              amFreq = c(50, 50, 120, 120, 120, 100, 100, 100),
#'              amDep = c(20, 20, 60, 60, 60, 40, 40, 40),
#'              temperature = 0.001, plot = TRUE)
#'
#' # Measure AM from the envelope (tracks AM over time)
#' am = soundgen:::getAM_env(
#'   audio = soundgen:::readAudio(s, samplingRate = 16000),
#'   amRange = c(20, 200))
#' plot(am$time, am$freq, type = 'b',
#'      ylab = 'AM frequency, Hz', xlab = 'Time, ms')
#' plot(am$time, am$dep, type = 'b',
#'      ylab = 'AM depth, %', xlab = 'Time, ms')
#' plot(am$time, am$purity, type = 'b',
#'      ylab = 'AM purity', xlab = 'Time, ms')
#'
#' # Measure AM from the modulation spectrum (one value per sound)
#' ms = modulationSpectrum(s, samplingRate = 16000, plot = FALSE)
#' soundgen:::getAM_ms(ms$detailed$original, amRes = 15)
getAM_env = function(audio,
                     amRange = c(20, 60),
                     overlap = 75,
                     parab = TRUE,
                     plot = FALSE) {
  out = data.frame(time = NA, freq = NA,
                   purity = NA, dep = NA)

  # Amplitude envelope via Hilbert transform
  env = try(hilbert_exact(audio$sound)$envelope, silent = TRUE)
  if (inherits(env, 'try-error') || !any(is.finite(env))) {
    warning('Failed to extract the amplitude envelope')
    return(out)
  }

  # Peak frequency, purity, and depth from the UNFILTERED envelope.
  # getPeakFreq() reads the DC component from the full spectrum (bin 1)
  # and restricts the peak search to amRange internally, so neither
  # .flatEnv() nor .bandpass() is needed.
  am = getPeakFreq(env,
                   samplingRate = audio$samplingRate,
                   freqRange = amRange,
                   overlap = overlap,
                   parab = parab,
                   plot = plot)

  # Clamp to valid physical ranges for AM (getPeakFreq leaves them
  # unclamped because it is also used for FM, where values may be uncalibrated)
  am$purity = pmin(pmax(am$purity, 0), 1)
  am$dep = pmin(pmax(am$dep, 0), 100)
  am
}


#' @noRd
#' @examples
#' s = soundgen(sylLen = 500, pitch = 400, amFreq = 75, amDep = 50, plot = FALSE)
#' ms = modulationSpectrum(s, samplingRate = 16000, plot = FALSE)
#' soundgen:::getAM_ms(ms$original)
getAM_ms = function(m,
                    amRange = c(10, 60),
                    amRes = NULL) {
  out = list(amMsFreq = NA, amMsPurity = NA)

  # minimal checks for exported use
  if (is.null(m) || !is.matrix(m) || ncol(m) < 2 || is.null(colnames(m))) return(out)
  if (is.null(amRange) || length(amRange) != 2 || any(!is.finite(amRange))) return(out)
  if (amRange[1] > amRange[2]) amRange = sort(amRange)
  if (is.null(amRes) || !is.numeric(amRes) || length(amRes) != 1 ||
      !is.finite(amRes) || amRes < 0) amRes = 0

  colNames = abs(as.numeric(colnames(m)))
  keep = is.finite(colNames)
  if (sum(keep) < 3) return(out)

  colNames = colNames[keep]
  freq_signed = as.numeric(colnames(m))[keep]
  amp = colSums(m[, keep, drop = FALSE], na.rm = TRUE)

  # estimate bin width from the original signed frequency axis when possible
  d = abs(diff(freq_signed))
  d = d[is.finite(d) & d > 0]
  if (length(d) > 0) {
    bin = median(d)
  } else {
    bin = NA
  }

  # fallback: use sorted unique absolute frequencies
  if (!is.finite(bin) || bin <= 0) {
    d = abs(diff(sort(unique(colNames))))
    d = d[is.finite(d) & d > 0]
    if (length(d) > 0) bin = median(d) else bin = NA
  }

  # fold around 0 and average AM across all FM bins
  am = data.frame(freq = colNames, amp = amp)
  am_sm = foldMS(am, tol = bin / 4)
  # plot(am, type = 'l')
  # lines(am_sm, type = 'l', col = 'blue')

  # find all local maxima (quick, and a peak at the boundary of amRange can then be included)
  if (is.finite(bin) && bin > 0) {
    wl = max(3, round(amRes / bin))
  } else {
    wl = 3
  }
  wl = min(wl, nrow(am_sm))
  if (wl %% 2 == 0) wl = wl - 1
  if (wl < 3) return(out)

  peaks = findPeaks(am_sm$amp, wl = wl)
  idx_am = which(am_sm$freq >= amRange[1] & am_sm$freq <= amRange[2])

  if (length(peaks) > 0 && length(idx_am) > 0) {
    # focus on local maxima within amRange
    peaks_amRan = peaks[peaks >= min(idx_am) & peaks <= max(idx_am)]
    if (length(peaks_amRan) > 0) {
      highest_peak = peaks_amRan[which.max(am_sm$amp[peaks_amRan])]
      peak_amp = am_sm$amp[highest_peak]

      if (is.finite(peak_amp) && peak_amp > 0) {
        # amMsFreq = freq of highest peak
        out$amMsFreq = am_sm$freq[highest_peak]

        # amMsPurity = ratio of peak to mean of other values within amRange, in dB
        pos_in_range = match(highest_peak, idx_am)
        if (is.na(pos_in_range)) {
          denom = am_sm$amp[idx_am]
          denom = denom[idx_am != highest_peak]
        } else {
          denom = am_sm$amp[idx_am][-pos_in_range]
        }
        denom = denom[is.finite(denom)]

        if (length(denom) > 0) {
          mean_denom = mean(denom)
          if (is.finite(mean_denom) && mean_denom > 0) {
            out$amMsPurity = log10(peak_amp / mean_denom) * 20
          }
        }
      }
    }
  }
  out
}


#' Get peak frequency
#'
#' Performs STFT and finds the dominant frequency within a target range for each
#' frame, together with its spectral purity (proportion of normalized spectral
#' magnitude at the peak) and, when a meaningful DC component is present, the
#' modulation depth derived from the AC/DC ratio. Used by "getAM_env" for
#' measuring amplitude modulation and by \code{\link{analyze}} for measuring
#' frequency modulation.
#'
#' For a sinusoidal modulator with depth \code{m = amDep / 100}, the
#' one-sided (un-doubled) envelope spectrum satisfies
#' \code{|X(f_am)| / |X(0)| = m / (4 - 2m)}, giving
#' \code{amDep = 400 * purity / (dc + 2 * purity)}.
#' The sum-to-one normalization cancels in this ratio, so the formula
#' is unaffected by spectral leakage.
#'
#' @param x numeric vector (amplitude envelope, pitch contour, etc.)
#' @param samplingRate sampling rate of \code{x}, Hz
#' @param freqRange a vector of length 2: the frequency range (Hz) in which
#'   to search for the peak. The DC component (0 Hz) is always extracted
#'   from the full spectrum, regardless of \code{freqRange}.
#' @param overlap overlap between consecutive STFT frames, \%
#'   (default 75, i.e. step = window length / 4)
#' @param parab if TRUE, refines the peak location by parabolic interpolation
#'   on a log10 scale. Purity is based on the raw peak bin, not on the
#'   interpolated amplitude.
#' @param plot if TRUE, produces a simple plot
#' @return A dataframe with one row per STFT frame: \describe{
#'   \item{time}{time stamp, ms}
#'   \item{freq}{peak frequency, Hz (NA for frames with no usable energy
#'     in the target frequency range)}
#'   \item{purity}{peak magnitude as a proportion of the frame's total
#'     spectral magnitude. Normally approximately 0 to 1, but not clamped;
#'     values outside this range are preserved as diagnostic information.}
#'   \item{dep}{approximate modulation depth, on a 0 to 100 scale for an
#'     ideal sinusoidal envelope. Not clamped; values outside 0-100 can occur
#'     for non-sinusoidal or otherwise uncalibrated inputs. NA if the DC
#'     component is zero or non-finite.}
#' }
#' @keywords internal
#' @examples{
#' # White noise with sinusoidal AM
#' amFreq = 10; amDep = 60
#' am = .5 + .5 * cospi(pi * amFreq * (1:1000) / 4000)
#' env = rnorm(4000) * (1 - am * amDep / 100)
#' plot(env, type = 'l')
#'
#' soundgen:::getPeakFreq(env, samplingRate = 4000, freqRange = c(5, 50))
#' }
getPeakFreq = function(x,
                       samplingRate,
                       freqRange = NULL,
                       overlap = 75,
                       parab = TRUE,
                       plot = FALSE) {
  out = data.frame(time = NA, freq = NA,
                   purity = NA, dep = NA)

  ## input validation
  if (length(samplingRate) != 1 || !is.finite(samplingRate) || samplingRate <= 0)
    return(out)
  if (is.null(freqRange) || length(freqRange) < 2)
    return(out)
  freqRange = freqRange[1:2]
  if (!all(is.finite(freqRange)) || freqRange[1] <= 0 || freqRange[1] >= freqRange[2])
    return(out)
  if (length(overlap) != 1 || !is.finite(overlap))
    overlap = 75

  ## STFT: window spans 4 periods of the lowest target frequency
  wl = max(3, round(samplingRate / freqRange[1] * 4))
  if (!is.finite(wl)) return(out)
  step = max(1, round(wl * (1 - overlap / 100)))
  sp = suppressWarnings(try(
    stft_simple(x, samplingRate = samplingRate, wl = wl, step = step, zp = 0),
    silent = TRUE))
  if (inherits(sp, 'try-error')) return(out)

  ## One-sided magnitude spectrum (positive frequencies, un-doubled)
  sp_raw = Mod(sp[1:(nrow(sp) %/% 2 + 1), , drop = FALSE])
  nc = ncol(sp_raw)
  if (nc < 1 || nrow(sp_raw) < 2) return(out)

  times = as.numeric(colnames(sp_raw))
  if (length(times) != nc) times = rep(NA, nc)
  freqs = as.numeric(rownames(sp_raw)) * 1000   # kHz -> Hz
  if (length(freqs) != nrow(sp_raw)) return(out)
  bin = freqs[2] - freqs[1]
  if (!is.finite(bin) || bin <= 0) return(out)

  ## replace non-finite magnitudes by zero
  sp_raw[!is.finite(sp_raw)] = 0

  ## DC component (bin 1 = 0 Hz), extracted from the FULL spectrum
  ## before any normalization or frequency-range subsetting
  dc_raw = sp_raw[1, ]

  ## total spectral energy per frame, used to detect empty frames
  energy_raw = colSums(sp_raw, na.rm = TRUE)
  valid_frame = is.finite(energy_raw) & energy_raw > 0

  ## Normalize each frame to sum = 1 (makes purity scale-invariant)
  cs = energy_raw
  cs[!is.finite(cs) | cs <= 0] = 1
  sp_norm = sweep(sp_raw, 2, cs, '/')
  dc_norm = sp_norm[1, ]   # normalized DC

  ## Restrict the peak search to freqRange
  idx_keep = which(freqs >= freqRange[1] & freqs <= freqRange[2])
  if (length(idx_keep) == 0) return(out)
  sp_search    = sp_norm[idx_keep, , drop = FALSE]
  freqs_search = freqs[idx_keep]
  nr = nrow(sp_search)

  ## Special case: a single frequency bin in range
  if (nr == 1) {
    freq = rep(freqs_search[1], nc)
    purity = as.numeric(sp_search)

    ## no usable energy in the target range -> leave freq/purity as NA
    no_peak = !valid_frame | !is.finite(purity) | purity <= 0
    freq[no_peak] = NA
    purity[no_peak] = NA

    dep = rep(NA_real_, nc)
    dep_valid = valid_frame & is.finite(dc_raw) & dc_raw > 0 & !is.na(purity)
    dep[dep_valid] = 400 * purity[dep_valid] /
      (dc_norm[dep_valid] + 2 * purity[dep_valid])
    dep[!is.finite(dep)] = NA

    return(data.frame(time = times,
                      freq = freq,
                      purity = purity,
                      dep = dep))
  }

  ## Find the peak in each frame
  peakFreq = data.frame(time = times,
                        freq = rep(NA_real_, nc),
                        purity = rep(NA_real_, nc),
                        dep = rep(NA_real_, nc))

  for (i in seq_len(nc)) {
    if (!valid_frame[i]) next
    sp_i = as.numeric(sp_search[, i])
    if (sum(sp_i, na.rm = TRUE) <= 0) next
    idx_peak = which.max(sp_i)
    if (length(idx_peak) == 0 || is.na(idx_peak)) next

    if (parab && idx_peak > 1 && idx_peak < nr) {
      # parabolic interpolation on log10 scale (more accurate for
      # spectral peaks); 1e-10 floor avoids log10(0). Only the frequency
      # is interpolated; purity is taken from the raw peak bin.
      threePoints = log10(sp_i[(idx_peak - 1):(idx_peak + 1)] + 1e-10)
      if (all(is.finite(threePoints))) {
        parabCor = try(parabPeakInterpol(threePoints), silent = TRUE)
        if (!inherits(parabCor, 'try-error') && is.finite(parabCor$p)) {
          p_corr = max(-0.5, min(0.5, parabCor$p))
          peakFreq$freq[i] = freqs_search[idx_peak] + bin * p_corr
        } else {
          peakFreq$freq[i] = freqs_search[idx_peak]
        }
      } else {
        peakFreq$freq[i] = freqs_search[idx_peak]
      }
    } else {
      peakFreq$freq[i] = freqs_search[idx_peak]
    }

    peakFreq$purity[i] = sp_i[idx_peak]
  }

  ## Modulation depth from the AC/DC ratio
  ## (meaningful when the input has a genuine DC component, e.g. an
  ## amplitude envelope; for pitch contours the values are uncalibrated
  ## but harmless, since updateAnalyze() does not use dep for FM)
  dep_valid = valid_frame & is.finite(dc_raw) & dc_raw > 0 &
    !is.na(peakFreq$purity)
  peakFreq$dep[dep_valid] = 400 * peakFreq$purity[dep_valid] /
    (dc_norm[dep_valid] + 2 * peakFreq$purity[dep_valid])
  peakFreq$dep[!is.finite(peakFreq$dep)] = NA

  if (plot) {
    plot(peakFreq$time, peakFreq$freq, type = 'b',
         cex = pmax(peakFreq$purity * 5, 0.3, na.rm = TRUE),
         xlab = 'Time, ms', ylab = 'Frequency, Hz',
         main = 'Peak frequency')
  }

  peakFreq
}
