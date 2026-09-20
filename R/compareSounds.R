#' Compare sounds
#'
#' Computes distances between sounds based on comparing their spectrogram-like
#' representations. \code{compareSounds} takes two sounds or feature matrices as
#' input, whereas \code{compareFolder} takes a path to a folder with audio files
#' or a list of feature matrices and returns a matrix of pairwise distances
#' between them. Feature matrices are normalized and compared with
#' Dynamic Time Warp (DTW), correlation, cosine distance, or pixel by pixel.
#'
#' If the input is audio, several methods of producing spectrograms are
#' available ("specFun"). For more customized options, just prepare your
#' spectrograms or feature matrices first (time in columns, features like pitch,
#' peak frequency, etc. in rows), and then pass them to \code{compareSounds}
#' (see examples). All methods except for DTW require that the compared matrices
#' should be of the same size. Compared sounds should ideally have the same
#' sampling rate. If they differ, row (frequency bin) truncation is performed by
#' position, keeping only the first min(nrow) rows, which approximates keeping
#' frequencies up to the lower Nyquist frequency when both spectrograms use the
#' same frequency resolution. In case of differences in duration, the shorter
#' sound is padded with 0 (silence) or NA, as controlled by arguments
#' \code{padWith, padDir}. If passing custom feature matrices, ensure that they
#' have the same dimensions or that padding with 0 (silence) makes sense.
#'
#' @inheritParams getSpec
#' @param x,y either two matrices (spectrograms or feature matrices) or two
#'   sounds to be compared (numeric vectors, Wave objects, or paths to wav/mp3
#'   files)
#' @param samplingRate if one or both inputs are numeric vectors, specify
#'   sampling rate, Hz. This does not resample audio. For meaningful
#'   comparisons, audio inputs should already have the same sampling rate
#' @param logSpec if TRUE, applies a log transform to the spectrograms before
#'   normalization
#' @param method method(s) of comparing spectrograms of two sounds:
#'   "cor" = Pearson's correlation distance; "cosine" = cosine distance;
#'   "diff" = normalized absolute difference; "dtw" = multivariate Dynamic Time
#'   Warp with \code{\link[dtw]{dtw}} (NB: the "dtw" package must be installed
#'   for this method to work)
#' @param padWith if the durations of x and y are not identical, the compared
#'   spectrograms are either padded with silence (\code{padWith = 0}) or
#'   truncated (\code{padWith = NA}) to have the same number of columns. Padding
#'   with NA is like truncating the longer sound to the short one's duration,
#'   whereas padding with 0 means that the shorter sound is padded with zero to
#'   the long one's duration
#' @param padDir if padding, specify where to add zeros or NAs: before the sound
#'   ('left'), after the sound ('right'), or on both sides ('central')
#' @param dtw_pars a list of parameters passed to \code{\link[dtw]{dtw}}
#' @param myfolder path to folder containing audio files to compare
#' @param spectrograms a list of spectrogram-like feature matrices to use
#'   instead of analyzing the audio - extracted acoustic features,
#'   modulation spectra, similarity matrices, ... (overrides \code{myfolder})
#' @param matchAllLengths if TRUE, all spectrograms are length-matched - e.g.,
#'   if 100 sounds are compared and \code{padWith = 0}, 99 of them are padded
#'   with silence to match the longest sound (faster); if FALSE, length matching
#'   is performed separately for each compared pair. Note that this also affects
#'   DTW matching if other distance metrics are used at the same time; thus, you
#'   may want to disable \code{matchAllLengths} if using DTW
#' @inheritParams spectrogram
#'
#' @return \code{compareSounds} returns a dataframe with two columns: "method"
#'   for the method(s) used, and "distance" for the distance between the two
#'   sounds calculated with that method. The range of distances is \code{[0, 1]}
#'   for "cor", "cosine", and "diff", and \code{[0, Inf)} for "dtw".
#'   \code{compareFolder} returns a list of distance matrices (\code{dist}
#'   objects), one for each method.
#' @export
#' @examples
#' s1 = soundgen(sylLen = 100, pitch = c(80, 180), formants = 'a')
#' s2 = soundgen(sylLen = 120, pitch = c(150, 350), formants = 'u')
#' compareSounds(s1, s2, samplingRate = 16000, method = c('cor', 'cosine', 'diff'))
#' # spectrogram(s1); playme(s1)
#' # spectrogram(s2); playme(s2)
#'
#' \dontrun{
#' # NB: install the "dtw" library to run the examples
#'
#' # compare all sounds in a folder, e.g.:
#' target = '~/Documents/Research/zz_test_audio/temp_long'
#' cf = compareFolder(target, logSpec = TRUE)
#' mds = as.data.frame(cmdscale(cf$cor))
#' plot(mds, type = 'n'); text(mds, labels = abbreviate(rownames(mds)))
#'
#' # or use manually produced spectrograms
#' sp = spectrogram(target, windowLength = c(10, 40), overlap = 75,
#'   yScale = 'ERB', output = 'processed', plot = FALSE, cores = 4)
#' image(sp[[1]])
#' cf1 = compareFolder(spectrograms = sp)
#' mds1 = as.data.frame(cmdscale(cf1$cor))
#' plot(mds1, type = 'n'); text(mds1, labels = abbreviate(rownames(mds1)))
#'
#' # extract a spectrogram-like representation using a custom function
#' # (e.g., full-resolution analytic envelopes instead of downsampled RMS)
#' compareSounds(s1, s2, samplingRate = 16000,
#'   specFun = function(x) matrix(hilbert_approx(x)$envelope, nrow = 1))
#'
#' # some more examples
#' s1 = soundgen(formants = 'a', play = TRUE)
#' s2 = soundgen(formants = 'ae', play = TRUE)
#' s3 = soundgen(formants = 'eae', sylLen = 700, play = TRUE)
#' s4 = runif(8000, -1, 1)  # white noise
#' compareSounds(s1, s2, samplingRate = 16000)
#' compareSounds(s1, s4, samplingRate = 16000)
#'
#' # the central section of s3 is more similar to s1 than is the beg/end of s3
#' compareSounds(s1, s3, samplingRate = 16000, padDir = 'left')
#' compareSounds(s1, s3, samplingRate = 16000, padDir = 'central')
#'
#' # padding with 0 penalizes differences in duration, whereas padding with NA
#' # is like saying we only care about the overlapping part
#' compareSounds(s1, s4, samplingRate = 16000, padWith = 0)
#' compareSounds(s1, s4, samplingRate = 16000, padWith = NA)
#'
#' # different types of spectrograms produce quite different results
#' compareSounds(s1, s3, samplingRate = 16000, specFun = 'stft')
#' compareSounds(s1, s3, samplingRate = 16000, specFun = 'melspec')
#' compareSounds(s1, s3, samplingRate = 16000, specFun = 'mfcc')
#' compareSounds(s1, s3, samplingRate = 16000, specFun = 'audSpec')
#'
#' # pass additional control parameters to specFun and DTW
#' compareSounds(s1, s3, samplingRate = 16000,
#'               specFun = 'melspec',
#'               specFun_pars = list(nbands = 128),
#'               dtw_pars = list(dist.method = "Manhattan"))
#'
#' # use feature matrices instead of spectrograms
#' # (time in columns, features in rows)
#' a1 = t(as.matrix(analyze(s1, samplingRate = 16000)$detailed))
#' a1 = a1[4:nrow(a1), ]; a1[is.na(a1)] = 0  # don't use dur and time stamps
#' a2 = t(as.matrix(analyze(s2, samplingRate = 16000)$detailed))
#' a2 = a2[4:nrow(a2), ]; a2[is.na(a2)] = 0
#' a4 = t(as.matrix(analyze(s4, samplingRate = 16000)$detailed))
#' a4 = a4[4:nrow(a4), ]; a4[is.na(a4)] = 0
#' compareSounds(a1, a2, method = c('cosine', 'dtw'))
#' compareSounds(a1, a4, method = c('cosine', 'dtw'))
#' }
compareSounds = function(
    x, y,
    samplingRate = NULL,
    specFun = 'melspec',
    specFun_pars = list(),
    logSpec = FALSE,
    method = c('cor', 'cosine', 'diff', 'dtw'),
    padWith = NA,
    padDir = c('central', 'left', 'right'),
    dtw_pars = list()) {
  method = unique(match.arg(method, choices = c('cor', 'cosine', 'diff', 'dtw'),
                            several.ok = TRUE))
  padDir = match.arg(padDir)

  if ('dtw' %in% method) {
    # check if package "dtw" is available
    if (!requireNamespace("dtw", quietly = TRUE)) {
      warning('Please install the "dtw" package to use this method: ',
              '`install.packages("dtw")`')
      method = method[which(method != 'dtw')]
    }
  }

  # check sampling rates
  if (!is.null(samplingRate)) {
    if (length(samplingRate) == 1) {
      samplingRate = rep(samplingRate, 2)
    } else if (length(samplingRate) != 2) {
      stop('samplingRate must be of length 1 or 2')
    }
  }

  # extract spectrograms
  if (is.matrix(x) != is.matrix(y))
    warning('Input should normally be either two audio files or two matrices; mix at your risk')
  if (is.matrix(x)) {
    spec1 = x
  } else {
    audio1 = readAudio(x, samplingRate = if (!is.null(samplingRate)) samplingRate[1] else NULL)
    if (audio1$failed) stop('Failed to read audio input x')
    spec1 = getSpec(audio1, specFun = specFun, specFun_pars = specFun_pars)
  }
  if (logSpec) spec1 = floor_log(spec1, dynamicRange = 120)
  spec1 = spec1 / max(spec1, na.rm = TRUE)

  if (is.matrix(y)) {
    spec2 = y
  } else {
    audio2 = readAudio(y, samplingRate = if (!is.null(samplingRate)) samplingRate[2] else NULL)
    if (audio2$failed) stop('Failed to read audio input y')
    spec2 = getSpec(audio2, specFun = specFun, specFun_pars = specFun_pars)
  }
  if (logSpec) spec2 = floor_log(spec2, dynamicRange = 120)
  spec2 = spec2 / max(spec2, na.rm = TRUE)

  # calculate distances
  dists = .calcDistance(spec1, spec2, method = method, padWith = padWith,
                        padDir = padDir, dtw_pars = dtw_pars)

  return(data.frame(method = method, distance = dists, row.names = NULL))
}


#' @rdname compareSounds
#' @export
compareFolder = function(
    myfolder = NULL,
    spectrograms = NULL,
    matchAllLengths = FALSE,
    specFun = 'melspec',
    specFun_pars = list(),
    logSpec = FALSE,
    method = c('cor', 'cosine', 'diff', 'dtw'),
    padWith = NA,
    padDir = c('central', 'left', 'right'),
    dtw_pars = list(),
    cores = 1,
    reportEvery = NULL) {

  method = match.arg(method, choices = c('cor', 'cosine', 'diff', 'dtw'),
                     several.ok = TRUE)
  padDir = match.arg(padDir)
  if (!is.null(myfolder) && !is.null(spectrograms)) {
    warning('Both myfolder and spectrograms provided; using spectrograms.')
  }

  if ('dtw' %in% method) {
    if (!requireNamespace("dtw", quietly = TRUE))
      stop('Please install the "dtw" package to use this method: ',
           '`install.packages("dtw")`')
  }

  if (is.null(myfolder) && is.null(spectrograms)) {
    stop(paste('provide either path to audio files (myfolder)',
               'or a named list of spectrograms (spectrograms)'))
  }

  # extract the spectrograms of all files to avoid re-doing it during pairwise comparisons
  if (is.null(spectrograms)) {
    message('Extracting spectrograms...')

    # Use processAudio for parallel extraction
    extracted = processAudio(
      x = myfolder,
      funToCall = getSpec,
      myPars = list(specFun = specFun, specFun_pars = specFun_pars),
      reportEvery = reportEvery,
      cores = cores
    )

    spectrograms = extracted$result
    # Remove failed extractions
    failed = extracted$input$failed
    if (any(failed)) {
      warning(sprintf('%d file(s) failed to process and will be skipped.', sum(failed)))
      spectrograms = spectrograms[!failed]
    }

    filenames = names(spectrograms)
    n = length(spectrograms)
    if (n < 2) stop('<2 audio files successfully processed - nothing to do')
  } else {
    filenames = names(spectrograms)
    n = length(spectrograms)
    if (n < 2) stop('<2 spectrograms provided - nothing to do')
    if (is.null(filenames))
      filenames = paste0('sound_', seq_len(n))
    names(spectrograms) = make.unique(filenames)
  }

  # Preprocess all spectrograms once
  message('Preprocessing spectrograms...')
  spectrograms = lapply(spectrograms, function(sp) {
    if (!is.matrix(sp)) {
      sp = as.matrix(sp)
    }
    if (logSpec) sp = floor_log(sp, dynamicRange = 120)
    sp / max(sp, na.rm = TRUE)
  })

  # standardize the number of columns (STFT frames) in all spectrograms
  if (matchAllLengths && any(method != 'dtw')) {
    message('Standardizing spectrogram lengths...')
    ncols = vapply(spectrograms, ncol, integer(1))
    if (is.na(padWith)) {
      # simply discard non-overlapping sections (shorten everything to the shortest sound)
      min_ncols = min(ncols)
      for (i in seq_len(n)) {
        if (ncols[i] != min_ncols) {
          col_long = matchLengths(seq_len(ncols[i]), min_ncols, padDir = padDir)
          spectrograms[[i]] = spectrograms[[i]][, col_long, drop = FALSE]
        }
      }
    } else {
      # pad everything to the longest spectrogram
      max_ncols = max(ncols)
      for (i in seq_len(n)) {
        if (ncols[i] != max_ncols) {
          spectrograms[[i]] = matchColumns(matrix_short = spectrograms[[i]],
                                           nCol = max_ncols,
                                           padWith = padWith,
                                           padDir = padDir)
        }
      }
    }
  }

  # prepare a dataframe of sound pairs
  len = rev(seq_len(n - 1))
  i = rep(seq_len(n - 1), times = len)
  j = sequence(len, from = seq_len(n - 1) + 1)
  pairs = cbind(i = i, j = j)

  # calculate pairwise distances
  message('Comparing the sounds...')
  nIter = nrow(pairs)
  time_start = proc.time()

  # Initialize distance matrices for each method
  dist_matrices = vector('list', length(method))
  names(dist_matrices) = method
  for (m in method) {
    dist_matrices[[m]] = matrix(0, nrow = n, ncol = n,
                                dimnames = list(filenames, filenames))
  }

  for (i in seq_len(nIter)) {
    s1 = pairs[i, 1]
    s2 = pairs[i, 2]
    dists = .calcDistance(spectrograms[[s1]], spectrograms[[s2]],
                          method = method, padWith = padWith, padDir = padDir,
                          dtw_pars = dtw_pars)
    for (m in method) {
      dist_matrices[[m]][s1, s2] = dists[m]
      dist_matrices[[m]][s2, s1] = dists[m]
    }

    reportTime(i, time_start, nIter, reportEvery = reportEvery)
  }

  # Set diagonal to 0 and convert to dist
  out = vector('list', length(method))
  names(out) = method
  for (m in method) {
    diag(dist_matrices[[m]]) = 0
    out[[m]] = as.dist(dist_matrices[[m]])
  }

  return(out)
}


#' Calculate Pairwise Distances
#'
#' Internal worker that assumes inputs are already preprocessed matrices.
#' Calculates distances for the requested methods.
#' @param spec1,spec2 matrices with features in rows, time in cols
#' @param method vector of methods to calculate
#' @param padWith,padDir how to match columns if lengths differ
#' @param dtw_pars list of arguments for dtw::dtw
#' @noRd
.calcDistance = function(spec1,
                         spec2,
                         method = c('cor', 'cosine', 'diff', 'dtw'),
                         padWith = NA,
                         padDir = 'central',
                         dtw_pars = list()) {

  dists = numeric(length(method))
  names(dists) = method

  # Match rows (frequency bins / features)
  nr1 = nrow(spec1); nr2 = nrow(spec2)
  if (nr1 != nr2) {
    if (nr1 < nr2) spec2 = spec2[seq_len(nr1), , drop = FALSE]
    else spec1 = spec1[seq_len(nr2), , drop = FALSE]
  }
  s1_res = spec1
  s2_res = spec2

  # DTW handles unequal lengths natively, but requires at least 2 time frames
  if ('dtw' %in% method) {
    if (ncol(spec1) < 2 || ncol(spec2) < 2) {
      dists['dtw'] = NA  # no time dimension to warp
    } else {
      # sanitize dtw_pars to prevent conflicts
      dtw_pars[c('x', 'y', 'distance.only')] = NULL
      d = try(do.call(dtw::dtw, c(list(
        x = t(spec1), y = t(spec2),
        distance.only = TRUE),
        dtw_pars)), silent = TRUE)
      dists['dtw'] = if (inherits(d, 'try-error')) NA else d$normalizedDistance
    }
  }

  # Other methods require same dimensions
  non_dtw_methods = setdiff(method, 'dtw')
  if (length(non_dtw_methods) > 0) {

    # Match columns (time frames)
    if (ncol(spec1) != ncol(spec2)) {
      if (is.na(padWith)) {
        # truncate the longer sound
        if (ncol(spec1) < ncol(spec2)) {
          col_long = matchLengths(seq_len(ncol(spec2)), ncol(spec1), padDir = padDir)
          s2_res = spec2[, col_long, drop = FALSE]
          s1_res = spec1
        } else {
          col_long = matchLengths(seq_len(ncol(spec1)), ncol(spec2), padDir = padDir)
          s1_res = spec1[, col_long, drop = FALSE]
          s2_res = spec2
        }
      } else {
        # pad the shorter sound
        if (ncol(spec1) < ncol(spec2)) {
          s1_res = matchColumns(matrix_short = spec1, nCol = ncol(spec2),
                                padWith = padWith, padDir = padDir)
          s2_res = spec2
        } else {
          s2_res = matchColumns(matrix_short = spec2, nCol = ncol(spec1),
                                padWith = padWith, padDir = padDir)
          s1_res = spec1
        }
      }
    }

    tgt = as.numeric(s1_res)
    cnd = as.numeric(s2_res)

    if ('cor' %in% non_dtw_methods) {
      r = cor(tgt, cnd, use = 'na.or.complete')
      dists['cor'] = if (is.na(r)) NA else (1 - r) / 2
    }

    if ('cosine' %in% non_dtw_methods) {
      norm1 = sum(tgt * tgt, na.rm = TRUE)
      norm2 = sum(cnd * cnd, na.rm = TRUE)
      if (norm1 == 0 && norm2 == 0) {
        dists['cosine'] = 0  # both silent
      } else if (norm1 == 0 || norm2 == 0) {
        dists['cosine'] = 1  # one silent
      } else {
        sim = sum(tgt * cnd, na.rm = TRUE) / sqrt(norm1 * norm2)
        dists['cosine'] = 1 - sim
      }
    }

    if ('diff' %in% non_dtw_methods) {
      sum1 = sum(tgt, na.rm = TRUE)
      sum2 = sum(cnd, na.rm = TRUE)
      if (sum1 == 0 && sum2 == 0) {
        dists['diff'] = 0  # both silent
      } else if (sum1 == 0 || sum2 == 0) {
        dists['diff'] = 1  # one silent
      } else {
        dists['diff'] = sum(abs(tgt - cnd), na.rm = TRUE) / (sum1 + sum2)
      }
    }
  }

  dists
}
