## UTILITIES FOR FINDING SYLLABLES AND VOCAL BURSTS ##

#' Extract annotated syllables
#'
#' Takes a table of syllable annotations, formatted as the \code{syllables}
#' dataframe returned by \code{\link{segment}}, extracts the corresponding
#' audio segments, and saves each segment as a separate .wav file in a
#' subdirectory named "segment".
#'
#' If \code{x} is a path to an audio file, the default output directory is
#' \code{segment} inside the folder containing that file. If \code{x} is a
#' \code{Wave} object or a numeric vector, the default output directory is
#' \code{segment} inside the current working directory. Use \code{path} to
#' override the output location.
#'
#' @seealso \code{\link{segment}}
#'
#' @param syllables data frame with at least columns \code{start} and \code{end}
#'   (ms), e.g. the \code{syllables} data frame returned by
#'   \code{\link{segment}}
#' @param x path to a .wav or .mp3 file, a \code{Wave} object, or a numeric
#'   vector
#' @param samplingRate sampling rate of \code{x}; only needed if \code{x} is a
#'   numeric vector
#' @param scale maximum possible amplitude of \code{x}; only needed if \code{x}
#'   is a numeric vector. If NULL, the scale is guessed from the maximum
#'   observed amplitude
#' @param addSilence silence added before and after each extracted segment, ms
#' @param path optional full path to the directory in which to save the
#'   extracted segments. If NULL, a subdirectory named "segment" is created
#'   either in the folder containing the audio file (if \code{x} is a file) or
#'   in the working directory (if \code{x} is a \code{Wave} object or numeric
#'   vector)
#'
#' @return Does not return anything, just saves the files.
#' @export
#' @examples
#' sound = soundgen(nSyl = 3, sylLen = 100, pauseLen = 70,
#'                  pitch = 250, temperature = 0.001)
#' s = segment(sound, samplingRate = 16000, plot = FALSE)
#' s$syllables
#' # the annotation table should look like this, with start/end times in ms!
#'
#' \dontrun{
#' # extract and save all syllables
#' segment_ann(s$syllables, sound, samplingRate = 16000)
#'
#' # save to a custom folder
#' segment_ann(s$syllables, sound, samplingRate = 16000,
#'             path = '~/Downloads/temp_segment')
#' }
segment_ann = function(syllables,
                       x,
                       samplingRate = NULL,
                       scale = NULL,
                       addSilence = 50,
                       path = NULL) {
  if (!is.data.frame(syllables)) {
    stop('syllables must be a data frame, e.g. segment()$syllables')
  }
  if (is.null(syllables$start) || is.null(syllables$end)) {
    stop('syllables must contain columns start and end')
  }

  # keep only valid rows
  syllables = syllables[!is.na(syllables$start) &
                          !is.na(syllables$end), , drop = FALSE]
  if (nrow(syllables) == 0) {
    stop('No valid syllables to extract')
  }
  syllables$start = as.numeric(syllables$start)
  syllables$end = as.numeric(syllables$end)
  if (any(syllables$start < 0)) {
    stop('start times cannot be negative')
  }
  if (any(syllables$end < syllables$start)) {
    stop('some syllables have end < start')
  }

  # read the audio
  input = checkInputType(x)
  if (input$n != 1) {
    stop(paste('x must be a single sound: one file, one Wave object,',
               'or one numeric vector'))
  }
  audio = readAudio(x,
                    input = input,
                    i = 1,
                    samplingRate = samplingRate,
                    scale = scale)
  if (audio$failed) {
    stop('failed to read the audio input')
  }
  if (!is.finite(audio$duration) || audio$duration <= 0) {
    stop('invalid audio duration')
  }

  # verify that the annotations fit the audio
  dur_ms = audio$duration * 1000
  max_end = max(syllables$end)
  tolerance_ms = 10
  if (max_end > dur_ms + tolerance_ms) {
    stop(paste0(
      'annotations extend to ', round(max_end, 1),
      ' ms, but the audio is only ', round(dur_ms, 1),
      ' ms long; check that syllables and audio match'))
  }

  # output directory
  if (is.null(path)) {
    if (input$type[1] == 'file') {
      path = file.path(dirname(input$filenames[1]), 'segment')
    } else {
      path = file.path(getwd(), 'segment')
    }
  }
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  if (!is.finite(addSilence) || addSilence < 0) addSilence = 0
  addSil = rep(0, round(addSilence * audio$samplingRate / 1000))

  # loop through the annotations
  for (i in 1:nrow(syllables)) {
    from = as.integer(round(audio$samplingRate * syllables$start[i] / 1000))
    to = as.integer(round(audio$samplingRate * syllables$end[i] / 1000))
    from = max(1L, from)
    to = min(audio$ls, to)
    if (to < from) {
      warning(paste('Skipping syllable', i, 'because end is before start'))
      next
    }
    temp = c(addSil, audio$sound[from:to], addSil)
    times = round(c(syllables$start[i], syllables$end[i]))
    filename_i = file.path(
      path,
      paste0(audio$filename_base, '_', times[1], '-', times[2], '.wav')
    )
    writeAudio(temp,
               audio = audio,
               filename = filename_i,
               scale_used = max(abs(temp)))
  }
}


#' Find syllables
#'
#' Called by \code{\link{segment}}. Syllables are detected as contiguous regions
#' of the envelope above threshold. Short sub-threshold gaps are merged if the
#' pause between syllable edges is shorter than \code{shortestPause}.
#'
#' @param ampl downsampled amplitude envelope
#' @param step time difference between two points in the envelope (ms)
#' @param threshold all continuous segments above this value are considered to
#'   be syllables
#' @return A dataframe with timing of syllables. Times correspond to edges of
#'   envelope bins; \code{pauseLen} is the time between the end of the previous
#'   syllable and the start of the current syllable.
#' @noRd
findSyllables = function(ampl,
                         step,
                         windowLength,
                         threshold,
                         shortestSyl,
                         shortestPause) {
  empty = data.frame(
    syllable = NA_real_,
    start_idx = NA_real_, end_idx = NA_real_,
    start = NA_real_, end = NA_real_,
    sylLen = NA_real_, pauseLen = NA_real_
  )
  n = length(ampl)
  if (n == 0 || length(threshold) == 0) return(empty)
  if (!is.finite(step) || step <= 0) return(empty)
  if (length(threshold) == 1) threshold = rep(threshold, n)
  if (length(threshold) != n) {
    stop('threshold must be a scalar or have the same length as ampl')
  }
  if (!is.finite(shortestSyl) || shortestSyl < 0) shortestSyl = 0
  if (!is.finite(shortestPause) || shortestPause < 0) shortestPause = 0
  if (!is.finite(windowLength) || windowLength < 0) windowLength = 0

  aboveThres = ampl > threshold
  aboveThres[is.na(aboveThres)] = FALSE
  if (!any(aboveThres)) return(empty)

  r = rle(aboveThres)
  run_end = cumsum(r$lengths)
  run_start = run_end - r$lengths + 1
  idx_true = which(r$values)
  if (length(idx_true) == 0) return(empty)
  start_idx_runs = run_start[idx_true]
  end_idx_runs = run_end[idx_true]

  # merge TRUE runs separated by pauses shorter than shortestPause;
  # pause is measured between edges of envelope bins
  if (length(start_idx_runs) > 1) {
    gap_frames = start_idx_runs[-1] - end_idx_runs[-length(end_idx_runs)] - 1
    split_here = gap_frames * step >= shortestPause
  } else {
    split_here = logical(0)
  }
  start_idx = start_idx_runs[c(TRUE, split_here)]
  end_idx = end_idx_runs[c(which(split_here), length(start_idx_runs))]

  # each envelope point is treated as a bin of width step, centered at
  # (idx - 1) * step + windowLength / 2
  start = (start_idx - 1) * step + windowLength / 2 - step / 2
  end = (end_idx - 1) * step + windowLength / 2 + step / 2
  start = pmax(0, start)
  sylLen = end - start

  keep = sylLen >= shortestSyl
  if (!any(keep)) return(empty)

  start_idx = start_idx[keep]
  end_idx = end_idx[keep]
  start = start[keep]
  end = end[keep]
  sylLen = sylLen[keep]

  nr = length(start)
  pauseLen = numeric(nr)
  pauseLen[1] = start[1]
  if (nr > 1) {
    pauseLen[2:nr] = start[2:nr] - end[1:(nr - 1)]
  }

  data.frame(
    syllable = 1:nr,
    start_idx = start_idx,
    end_idx = end_idx,
    start = start,
    end = end,
    sylLen = sylLen,
    pauseLen = pauseLen
  )
}


#' Get Proportion of Time Vocalizing (PTV)
#'
#' @param syllables dataframe returned by findSyllables
#' @param duration audio duration, s
#' @param ptvStep,ptvTime,ptvFreq the instantaneous proportion of time
#'   vocalizing (PTV) is calculated by producing a binary (sound on/off) contour
#'   with a step of ptvStep ms and convolving it with a half-Gaussian filter
#'   with SD = ptvTime ($ptv_conv) and by low-pass filtering it over ptvFreq
#'   ($ptv_lowpass)
#' @noRd
#' @examples
#' ampl = c(rep(0, 5), rep(1, 4), rep(0, 3), rep(1, 8),
#'   rep(0, 12), rep(1, 20), rep(0, 5), rep(1, 3), rep(0, 10))
#' syllables = soundgen:::findSyllables(ampl, step = 10, windowLength = 25,
#'   threshold = .5, shortestSyl = 5, shortestPause = 5)
#' ptv = soundgen:::getPTV(syllables = syllables, duration = length(ampl) * 10 / 1000)
#' ptv$syllables
#' plot(ptv$ptv_cont$time, ptv$ptv_cont$on, type = 'b')
#' points(ptv$ptv_cont$time, ptv$ptv_cont$ptv_lowpass, type = 'l', col = 'green')
#' points(ptv$ptv_cont$time, ptv$ptv_cont$ptv_conv, type = 'l', col = 'blue')
getPTV = function(syllables,
                  duration,
                  ptvStep = 25,
                  ptvTime = 0.5,
                  ptvFreq = 1) {
  if (is.null(syllables$start) || is.null(syllables$end))
    stop('need at least start and end columns')
  nr = nrow(syllables)
  if (nr < 2) {
    syllables[, c('sylRate', 'ptv')] = NA
    return(list(syllables = syllables, ptv_cont = NA))
  }

  # calculate sylRate and ptv for each syllable
  syllables$sylRate = 1000 / (syllables$sylLen + syllables$pauseLen)
  syllables$ptv = syllables$sylLen / (syllables$sylLen + syllables$pauseLen)
  syllables$ptv[1] = syllables$sylRate[1] = NA

  ## calculate instantaneous ptv and sylRate
  # split 0/1 contour into bins
  ptv_cont = data.frame(time = seq(0, duration * 1000, ptvStep), on = 0)
  for (s in 1:nr) {
    ptv_cont$on[ptv_cont$time >= syllables$start[s] &
                  ptv_cont$time <= syllables$end[s]] = 1
  }
  # plot(ptv_cont, type = 'l')
  len_env = nrow(ptv_cont)
  sr = len_env / duration

  # low-pass
  ptv_cont$ptv_lowpass = pitchSmoothPraat(ptv_cont$on, bandwidth = ptvFreq,
                                          samplingRate = sr, preprocess = FALSE)
  ptv_cont$ptv_lowpass[ptv_cont$ptv_lowpass < 0] = 0
  ptv_cont$ptv_lowpass[ptv_cont$ptv_lowpass > 1] = 1

  # convolve with a half-Gaussian filter
  len_win = max(1, min(len_env, round(ptvTime * sr) * 3))  # -3 SD to 0
  # 3 SD gives the same result as 4 SD; 2 SD is also very similar
  win = dnorm(seq(-3, 0, length.out = len_win))
  win = win / sum(win)  # so all weights sum to 1
  if (len_env > len_win) {
    win = c(rep(0, len_env - len_win), win)
  }
  # plot(win)
  ptv_cont$ptv_conv = convolve(ptv_cont$on, win)

  return(list(syllables = syllables, ptv_cont = ptv_cont))
}


#' Spectral noise contrast
#' Calculates the noise-contrast contour used by \code{\link{segment}} when
#' \code{input = 'spec' or 'mel'}. Each frame of the log-spectrogram is compared
#' to the estimated noise spectrum using the selected spectral measure, and the
#' resulting spectral contrast can be combined with the amplitude contour in
#' several ways.
#'
#' @param sp log-spectrogram with frequency bins in rows and frames in columns
#' @param noise estimated noise spectrum (one value per frequency bin)
#' @param cs amplitude contour (typically column means of \code{sp}, shifted to
#'   be non-negative)
#' @param thres_noise estimated noise level on the amplitude contour
#' @param col_noise indices of frames estimated to represent background noise
#' @param propNoise proportion of frames assumed to represent background noise
#' @param specDiffMeasure similarity/distance measure between each spectrum and
#'   \code{noise}: 'cosine' = cosine distance without centering;
#'   'logSpecDist' = root-mean-square log-spectral distance;
#'   'specDiff' = mean absolute log-spectral difference
#' @param amplWeight amplitude weighting applied to the spectral contrast:
#'   'add' = amplitude-adjusted contrast, the current soundgen default when
#'   \code{specDiffMeasure = 'cosine'};
#'   'gate' = amplitude-gated contrast;
#'   'scale' = spectral contrast scaled to the dynamic range of \code{cs};
#'   'none' = spectral contrast only
#' @return Numeric vector with one noise-contrast value per frame
#' @noRd
.spectralNoiseContrast = function(sp,
                                  noise,
                                  cs,
                                  thres_noise,
                                  col_noise = NULL,
                                  propNoise = NULL,
                                  specDiffMeasure = c('cosine', 'logSpecDist', 'specDiff'),
                                  amplWeight = c('add', 'gate', 'scale', 'none')) {
  specDiffMeasure = match.arg(specDiffMeasure)
  amplWeight = match.arg(amplWeight)
  nc = ncol(sp)
  nFreq = nrow(sp)
  if (nc == 0 || nFreq == 0) return(numeric(0))
  if (length(noise) != nFreq) {
    warning('Length of noise spectrum does not match nrow(sp); returning zeros')
    return(rep(0, nc))
  }
  noise[!is.finite(noise)] = 0
  if (length(cs) == 0) {
    cs = rep(0, nc)
  } else if (length(cs) != nc) {
    cs = rep_len(cs, nc)
  }
  cs_finite = cs[is.finite(cs)]
  max_cs = if (length(cs_finite) > 0) max(cs_finite) else 0
  if (!is.finite(max_cs) || max_cs < 0) max_cs = 0

  # amplitude stats
  amplAboveNoise = cs - thres_noise
  amplAboveNoise[!is.finite(amplAboveNoise)] = 0
  amplGate = pmax(0, amplAboveNoise)
  max_amplGate = if (length(amplGate) > 0) max(amplGate) else 0
  if (!is.finite(max_amplGate) || max_amplGate < 0) max_amplGate = 0

  # safe noise proportion for noise-floor estimation
  propNoise_i = if (is.null(propNoise) || length(propNoise) == 0) NA else propNoise[1]
  if (is.na(propNoise_i)) propNoise_i = 0.5
  propNoise_i = min(1, max(0, propNoise_i))

  # clean col_noise
  if (!is.null(col_noise)) {
    col_noise = col_noise[is.finite(col_noise) & col_noise >= 1 & col_noise <= nc]
  } else {
    col_noise = integer(0)
  }

  ## spectral contrast
  if (specDiffMeasure == 'cosine') {
    # cosine similarity without centering (i.e. not Pearson correlation)
    noise_sq = sum(noise ^ 2)
    sp_sq = colSums(sp ^ 2)
    sp_noise = as.numeric(crossprod(noise, sp))
    if (noise_sq > 0) {
      cosSim = sp_noise / sqrt(sp_sq * noise_sq)
      cosSim[!is.finite(cosSim)] = 1
      cosSim = pmin(pmax(cosSim, -1), 1)
    } else {
      cosSim = rep(1, nc)
    }
    contrast = 1 - cosSim
  } else {
    if (specDiffMeasure == 'logSpecDist') {
      # RMS log-spectral distance, computed without forming the full sp - noise matrix
      noise_sq = sum(noise ^ 2)
      sp_sq = colSums(sp ^ 2)
      sp_noise = as.numeric(crossprod(noise, sp))
      sqDist = sp_sq / nFreq - 2 * sp_noise / nFreq + noise_sq / nFreq
      sqDist[sqDist < 0 | !is.finite(sqDist)] = 0
      specDist = sqrt(sqDist)
    } else { # 'specDiff'
      # mean absolute log-spectral difference
      specDist = colMeans(abs(sp - noise))
    }
    specDist[!is.finite(specDist)] = 0

    # approximately center the distance on the estimated noise floor
    if (length(col_noise) > 0) {
      noiseFloor = median(specDist[col_noise])
    } else {
      noiseFloor = as.numeric(quantile(specDist, probs = propNoise_i))
    }
    if (!is.finite(noiseFloor)) noiseFloor = 0

    contrast = specDist - noiseFloor
    contrast[contrast < 0 | !is.finite(contrast)] = 0
  }

  contrast[!is.finite(contrast)] = 0
  max_contrast = if (length(contrast) > 0) max(contrast) else 0
  if (!is.finite(max_contrast) || max_contrast < 0) max_contrast = 0

  if (max_contrast > 0) {
    contrast_norm = contrast / max_contrast
  } else {
    contrast_norm = rep(0, nc)
  }

  ## amplitude weighting / combination
  if (amplWeight == 'add') {
    # amplitude-adjusted contrast: current default for cosine
    if (max_cs > 0) {
      difNoise = contrast_norm * max_cs + amplAboveNoise
    } else {
      difNoise = amplAboveNoise
    }

  } else if (amplWeight == 'gate') {
    # amplitude-gated contrast: spectral contrast only where amplitude is above noise
    if (max_contrast > 0 && max_amplGate > 0) {
      difNoise = contrast_norm * amplGate
    } else {
      # if spectral contrast is uninformative, fall back to amplitude gating
      difNoise = amplGate
    }

  } else if (amplWeight == 'scale') {
    # spectral contrast only, rescaled to the dynamic range of the amplitude contour
    if (max_cs > 0) {
      difNoise = contrast_norm * max_cs
    } else {
      difNoise = rep(0, nc)
    }

  } else { # 'none'
    # raw spectral contrast in the units of the selected spectral measure
    difNoise = contrast
  }

  difNoise[!is.finite(difNoise)] = 0
  difNoise
}
