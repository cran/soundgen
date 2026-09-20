### UTILITIES FOR POSTPROCESSING OF PITCH CONTOURS ###

#' Pathfinder
#'
#' Finds the optimal pitch contour through frame-by-frame pitch candidates
#' using the Viterbi algorithm. The cost function has two additive terms:
#' (1) an emission cost equal to the negative log-certainty of each candidate,
#' and (2) a transition cost that penalizes pitch jumps between consecutive
#' frames via a sigmoid function of pitch velocity (semitones per unit time),
#' normalized so that the "nearly free" zone is approximately 3 semitones
#' per 25 ms regardless of \code{step}. The two terms are weighted by
#' \code{certWeight} and \code{(1 - certWeight) * jumpPenalty}, respectively.
#'
#' @param pitchCands a matrix of pitch candidates (Hz): rows = candidates,
#'   columns = FFT frames
#' @param pitchCert a matrix of certainties (0 to 1), same dimensions as
#'   \code{pitchCands}
#' @param pitchSource a matrix of source labels, same dimensions
#' @param step time step between frames, ms
#' @param manual a list with \code{$frame} and \code{$freq} specifying
#'   inviolable manual pitch values (or NULL)
#' @param certWeight 0 to 1; weight of the emission (certainty) cost relative
#'   to the transition (smoothness) cost
#' @param jumpPenalty positive number; multiplier of the transition cost.
#'   Increase to enforce smoother contours, decrease to follow candidates
#'   more faithfully
#' @param interpolWin_bin,interpolTol,interpolCert parameters passed to
#'   \code{\link{interpolatePitchCands}}
#' @param manualCert certainty assigned to manual candidates
#' @return A numeric vector of pitch values (Hz) representing the optimal
#'   path through pitch candidates.
#' @noRd
pathfinder = function(pitchCands,
                      pitchCert,
                      pitchSource,
                      step,
                      manual = NULL,
                      certWeight = 0.5,
                      jumpPenalty = 1,
                      interpolWin_bin = 3,
                      interpolTol = 0.05,
                      interpolCert = 0.3,
                      manualCert = 1) {
  nr = nrow(pitchCands)
  nc = ncol(pitchCands)

  ## 1. Add inviolable manual candidates as an extra row
  if (length(manual$frame) > 0) {
    freqMan = certMan = rep(NA, nc)
    freqMan[manual$frame] = manual$freq
    certMan[manual$frame] = manualCert
    pitchCands[, manual$frame] = NA  # suppress other candidates in manual frames
    pitchCands  = rbind(pitchCands,  freqMan)
    pitchCert   = rbind(pitchCert,   certMan)
    pitchSource = rbind(pitchSource, rep('manual', nc))
    nr = nr + 1
  }

  ## 2. Work in log2 space (approximates perceptual pitch scale)
  pitchCands[pitchCands <= 0] = NA  # avoid log(negative)
  pitchCands[!is.na(pitchCands)] = log2(pitchCands[!is.na(pitchCands)])
  if (!is.null(manual)) manual$freq = log2(manual$freq)

  ## 3. Certainty-weighted mean of candidates per frame (used for
  ##    interpolation and as a fallback)
  pitchCenterGravity = vapply(1:nc, function(i) {
    idxMan = which(pitchSource[, i] == 'manual' & pitchCert[, i] == manualCert)
    if (length(idxMan) == 1) {
      pitchCands[idxMan, i]
    } else {
      weighted.mean(pitchCands[, i], pitchCert[, i], na.rm = TRUE)
    }
  }, numeric(1))

  ## 4. Interpolate: add low-certainty candidates in frames that lack
  ##    any candidate near the expected pitch
  if (!is.null(interpolWin_bin) &&
      is.finite(interpolWin_bin) &&
      interpolWin_bin > 0 &&
      !is.null(interpolTol) &&
      is.finite(interpolTol) &&
      !is.null(interpolCert) &&
      is.finite(interpolCert)) {
    intplt = interpolatePitchCands(
      pitchCands = pitchCands, pitchCert = pitchCert,
      pitchSource = pitchSource, pitchCenterGravity = pitchCenterGravity,
      interpolWin_bin = interpolWin_bin, interpolTol = interpolTol,
      interpolCert = interpolCert)
    pitchCands  = intplt$pitchCands
    pitchCert   = intplt$pitchCert
    pitchSource = intplt$pitchSource
    pitchCenterGravity = intplt$pitchCenterGravity
  }

  # in manual frames, keep only the manual candidate
  if (length(manual$frame) > 0) {
    for (f in manual$frame) {
      pitchCands[pitchSource[, f] != 'manual', f] = NA
    }
  }

  # drop rows that are entirely NA
  keep = rowSums(!is.na(pitchCands)) > 0
  if (any(keep)) {
    pitchCands  = pitchCands[keep, , drop = FALSE]
    pitchCert   = pitchCert[keep, , drop = FALSE]
    pitchSource = pitchSource[keep, , drop = FALSE]
  }
  nr = nrow(pitchCands)

  ## 5. Trivial cases
  if (nr == 0) return(rep(NA_real_, nc))
  if (nr == 1 || nc < 2) {
    bestPath = rep(NA_real_, nc)
    for (i in seq_len(nc)) {
      idx = which.min(abs(pitchCands[, i] - pitchCenterGravity[i]))
      if (length(idx) > 0 && !is.na(idx)) bestPath[i] = pitchCands[idx, i]
    }
    if (length(manual$frame) > 0) bestPath[manual$frame] = manual$freq
    return(2 ^ bestPath)
  }

  ## 6. Viterbi
  bestPath = pathfinding_viterbi(
    pitchCands = pitchCands,
    pitchCert = pitchCert,
    pitchSource = pitchSource,
    certWeight = certWeight,
    jumpPenalty = jumpPenalty,
    step = step,
    manual = manual,
    manualCert = manualCert
  )

  ## 7. Safety net
  if (!is.numeric(bestPath) || length(bestPath) != nc) {
    bestPath = pitchCenterGravity
  }

  2 ^ bestPath
}


#' Viterbi algorithm for pitch tracking
#'
#' Finds the globally optimal path through a matrix of pitch candidates by
#' dynamic programming. The total cost of a path is the sum over frames of
#' \code{certWeight * emission(t)} plus the sum over transitions of
#' \code{(1 - certWeight) * jumpPenalty * transition(t-1, t)}, where
#' emission = \code{-log2(cert)} and transition is a sigmoid of pitch
#' velocity that saturates at 1 for large jumps.
#'
#' @inheritParams pathfinder
#' @return A numeric vector of pitch values in log2(Hz), one per frame.
#' @noRd
pathfinding_viterbi = function(pitchCands,
                               pitchCert,
                               pitchSource,
                               certWeight,
                               jumpPenalty,
                               step,
                               manual,
                               manualCert) {
  nr = nrow(pitchCands)
  nc = ncol(pitchCands)

  ## Emission cost: negative log-certainty
  ## Floor at 1e-6 avoids Inf; NAs (absent candidates) become Inf
  emission = -log2(pmax(pitchCert, 1e-6))
  emission[is.na(emission)] = Inf

  ## At manual frames, only the manual candidate is admissible
  if (length(manual$frame) > 0) {
    for (f in manual$frame) {
      emission[pitchSource[, f] != 'manual', f] = Inf
    }
  }

  ## Transition cost scaling:
  ## Convert log2 difference to semitones, then normalize by step so that
  ## the sigmoid's transition zone (~3 semitones) corresponds to roughly
  ## 3 semitones per 25 ms regardless of the actual step.
  ## d = Δlog2 * 12 * (25 / step) = Δsemitones normalized to a 25-ms frame
  step_factor = 12 * 25 / step

  ## Sigmoid shape: 1 / (1 + 10 * exp(3 - 0.6 * |d|))
  ##   |d| = 0   -> ~0.005  (essentially free)
  ##   |d| = 5   -> ~0.09   (5 semitones per 25 ms: still cheap)
  ##   |d| = 10  -> ~0.66   (10 semitones per 25 ms: expensive)
  ##   |d| -> Inf -> 1       (saturates)
  ## Scaled by 20 to match the range of emission costs (~0 to ~20).
  trans_scale = jumpPenalty * 20

  ## Forward pass
  V  = matrix(Inf, nrow = nr, ncol = nc)          # cumulative cost
  bp = matrix(NA_integer_, nrow = nr, ncol = nc)  # backpointers

  V[, 1] = certWeight * emission[, 1]

  for (t in 2:nc) {
    curr   = pitchCands[, t]
    prev   = pitchCands[, t - 1]
    V_prev = V[, t - 1]

    # only consider candidates that actually exist at frame t
    valid_j = which(!is.na(curr) & is.finite(emission[, t]))

    for (j in valid_j) {
      # normalized pitch jump from every predecessor to candidate j
      d = (prev - curr[j]) * step_factor

      # sigmoid transition cost (NA predecessors produce NA -> ignored)
      trans = trans_scale / (1 + 10 * exp(3 - 0.6 * abs(d)))

      # total cost via each predecessor
      total = V_prev + (1 - certWeight) * trans
      best  = which.min(total)

      if (length(best) > 0 && is.finite(total[best])) {
        V[j, t]  = certWeight * emission[j, t] + total[best]
        bp[j, t] = best
      }
    }
  }

  ## Backtrack
  path = rep(NA_integer_, nc)
  path[nc] = which.min(V[, nc])

  if (is.na(path[nc]) || !is.finite(V[path[nc], nc])) {
    # no valid path found: fall back to best candidate per frame
    for (t in 1:nc) {
      idx = which.min(emission[, t])
      if (length(idx) > 0 && is.finite(emission[idx, t])) path[t] = idx
    }
  } else {
    for (t in (nc - 1):1) {
      path[t] = bp[path[t + 1], t + 1]
    }
  }

  ## Extract pitch values
  out = rep(NA_real_, nc)
  for (t in 1:nc) {
    if (!is.na(path[t])) out[t] = pitchCands[path[t], t]
  }
  out
}


#' Interpolate pitch candidates
#'
#' If a frame has no pitch candidate, or no candidate within
#' \code{±log2(1 + interpolTol)} of the value predicted from neighbouring
#' frames, a new candidate is inserted at the predicted value with certainty
#' \code{interpolCert * mean(neighbouring certainties)}. The prediction is
#' obtained by fitting a natural spline through the pitch center of gravity
#' (or manual candidates) over a window of \code{±interpolWin_bin} frames;
#' linear interpolation is used as a fallback near the edges or when fewer
#' than three reference points are available.
#'
#' @inheritParams pathfinder
#' @param pitchCenterGravity numeric vector: certainty-weighted mean of
#'   pitch candidates per frame (in log2 Hz)
#' @return A list with updated \code{pitchCands}, \code{pitchCert},
#'   \code{pitchSource}, and \code{pitchCenterGravity}.
#' @noRd
interpolatePitchCands = function(pitchCands,
                                 pitchCert,
                                 pitchSource,
                                 pitchCenterGravity,
                                 interpolWin_bin = 3,
                                 interpolTol = 0.3,
                                 interpolCert = 0.3) {
  nc = ncol(pitchCands)
  nr_orig = nrow(pitchCands)  # before adding the interpolation row

  # add an empty row at the top to hold interpolated candidates
  pitchCands = rbind(rep(NA_real_, nc), pitchCands)
  pitchCert = rbind(rep(NA_real_, nc), pitchCert)
  pitchSource = rbind(rep('intpl', nc), pitchSource)

  # convert the multiplicative tolerance (a proportion, e.g. 0.3 = ±30%)
  # to an additive tolerance in log2 space
  tol_log2 = log2(1 + max(interpolTol, 1e-6))

  for (f in 1:nc) {
    left = max(1, f - interpolWin_bin)
    right = min(nc, f + interpolWin_bin)
    win = left:right
    if (length(win) < 2) next  # nothing to interpolate from
    pos_f = f - left + 1  # position of frame f within the window

    ## build a reference curve over the window
    idx_man = which(pitchSource[, win] == 'manual' &
                      !is.na(pitchCert[, win]), arr.ind = TRUE)

    if (nrow(idx_man) > 0) {
      # manual candidates take precedence: use only them
      curve = rep(NA_real_, length(win))
      curve[idx_man[, 'col']] = pitchCands[, win][idx_man]
    } else {
      # use the pre-computed center of gravity, masking the current frame
      # so that we predict it from its neighbors
      curve = pitchCenterGravity[win]
      curve[pos_f] = NA
    }

    ## predict pitch at frame f from the reference curve
    known = which(!is.na(curve) & is.finite(curve))
    if (length(known) == 0) next  # no reference points at all
    if (length(known) == 1) {
      # single reference point: constant extrapolation
      predicted = curve[known]
    } else if (length(known) >= 3 &&
               pos_f > min(known) && pos_f < max(known)) {
      # interior prediction with >= 3 points: natural spline
      sp = try(
        spline(x = known, y = curve[known],
               xout = pos_f, method = 'natural'),
        silent = TRUE)
      if (!inherits(sp, 'try-error') &&
          length(sp$y) == 1 && is.finite(sp$y)) {
        predicted = sp$y
      } else {
        # spline failed (e.g. duplicate x): fall back to linear
        predicted = approx(known, curve[known],
                           xout = pos_f, rule = 2)$y
      }

    } else {
      # two points, or target outside the convex hull:
      # linear interpolation / constant extrapolation
      predicted = approx(known, curve[known],
                         xout = pos_f, rule = 2)$y
    }

    if (!is.finite(predicted)) next

    ## decide whether to insert an interpolated candidate
    hasLocalCand = any(abs(pitchCands[, f] - predicted) < tol_log2,
                       na.rm = TRUE)

    if (!hasLocalCand) {
      pitchCands[1, f] = predicted

      # scale certainty by the mean certainty of real (non-interpolated)
      # candidates in the neighbourhood, so that interpolation in a
      # high-certainty region is trusted more than in a noisy one
      if (nr_orig > 0) {
        realCerts = pitchCert[2:(nr_orig + 1), win]
        realCerts = realCerts[!is.na(realCerts)]
        meanCert  = if (length(realCerts) > 0) mean(realCerts) else 0
      } else {
        meanCert = 0
      }
      pitchCert[1, f] = interpolCert * meanCert

      pitchCenterGravity[f] = predicted
    }
    # if a suitable candidate already exists, leave everything unchanged
  }

  list(pitchCands  = pitchCands,
       pitchCert   = pitchCert,
       pitchSource = pitchSource,
       pitchCenterGravity = pitchCenterGravity)
}


#' Median smoothing
#'
#' Replaces outliers in each column of a dataframe with the running median
#' over a window of \code{smoothing_ww} points. Only values that deviate
#' from the running median by more than \code{smoothingThres} semitones
#' (i.e. \code{12 * log2(x / median)}) are replaced; set
#' \code{smoothingThres = 0} to replace every value with the running median.
#' Uses \code{\link[stats]{runmed}} for columns without NAs and an
#' \code{\link{embed}}-based fallback for columns with NAs.
#'
#' @param df dataframe (each column is smoothed independently)
#' @param smoothing_ww width of the smoothing window (points); the effective
#'   window is \code{2 * (smoothing_ww \%/\% 2) + 1} (forced odd)
#' @param smoothingThres tolerated deviation from the running median, in
#'   semitones (0 = replace everything, not just outliers)
#' @param inviolable integer vector of row indices that must not be modified
#'   (e.g. manual pitch values); they still contribute to the running median
#'   of neighbouring rows
#' @return A dataframe of the same dimensions as \code{df}.
#' @noRd
medianSmoother = function(df,
                          smoothing_ww,
                          smoothingThres,
                          inviolable = NULL) {
  hw = smoothing_ww %/% 2
  if (hw < 1) return(df)

  nr = nrow(df)
  nc = ncol(df)

  # effective window size (odd, and no larger than the data)
  k = 2 * hw + 1
  if (k > nr) {
    k = nr
    if (k %% 2 == 0) k = max(1, k - 1)
    hw = k %/% 2
  }
  if (k <= 1) return(df)

  out = df
  checkThres = (smoothingThres > 0)

  for (j in seq_len(nc)) {
    x = df[[j]]

    ## Running median
    if (anyNA(x)) {
      x_pad = c(rep(NA, hw), x, rep(NA, hw))
      med = apply(embed(x_pad, k), 1, median, na.rm = TRUE)
    } else {
      # fast C implementation; endrule = "median" uses a shrinking
      # window at the edges, matching the NA-padding behaviour above
      med = runmed(x, k = k, endrule = "median")
    }

    ## Replace outliers (or everything, if smoothingThres == 0)
    if (checkThres) {
      # deviation from running median, in semitones
      # (appropriate for pitch and other Hz-valued contours)
      deviance = 12 * log2(x / pmax(med, 1e-6))
      idx_out = which(abs(deviance) > smoothingThres)
      if (length(idx_out) > 0) out[idx_out, j] = med[idx_out]
    } else {
      out[[j]] = med
    }
  }

  ## Restore inviolable rows
  if (!is.null(inviolable) && length(inviolable) > 0) {
    out[inviolable, ] = df[inviolable, ]
  }

  out
}


#' Find voiced segments
#'
#' Internal helper function for postprocessing of pitch contours. Merges voiced
#' segments at least \code{shortestSyl} ms long and separated by less than
#' \code{shortestPause} ms. Called by \code{\link{analyze}}
#' @param pitchCands matrix of possible pitch values per column. One column is
#'   one fft frame, one row is one pitch candidate
#' @inheritParams analyze
#' @inheritParams spectrogram
#' @param minVoicedCands a frame is considered to be voiced if at least this
#'   many pitch candidates are not NA. Defaults to 2: since dom is usually
#'   defined, in practice this means that we also want at least one other pitch
#'   candidate (autocor, cep or BaNa)
#' @param pitchMethods methods of pitch tracking in analyze()
#' @param manualV index of frames that should definitely be voiced (manual
#'   candidates)
#' @param manualTryToV index of frames that should be treated as voiced as long
#'   as they have any candidates at all (even <minVoicedCands)
#' @param manualUnv index of frames forced to be unvoiced
#' @return A dataframe specifying where each voiced segment starts and ends (in
#'   fft frames, not ms!)
#' @noRd
#' @examples
#' # 1. Basic separate segments
#' pitch = c(rep(NA, 5), rnorm(15, 300, 15), rep(NA, 7),
#'           rnorm(10, 400, 10), rep(NA, 6), 250, 250)
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 20, shortestPause = 60, step = 10, minVoicedCands = 1, pitchMethods = 'autocor'
#' )
#' # Expected: 3 segments (6-20, 28-37, 44-45). The last one is length 2 (>= nRequired=2).
#'
#' # 2. Merging across short gaps
#' # 10 voiced, 1 gap, 10 voiced
#' pitch = c(rep(300, 10), NA, rep(300, 10))
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 50, shortestPause = 20, step = 10, minVoicedCands = 1, pitchMethods = 'autocor'
#' )
#' # Expected: 1 segment (1-21) because the gap (1 frame = 10ms) is < toleratedGap (20ms).
#'
#' # 3. Preventing merge across manualUnv
#' # Same as above, but frame 11 is forced unvoiced
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 50, shortestPause = 20, step = 10, minVoicedCands = 1, pitchMethods = 'autocor',
#'   manualUnv = 11
#' )
#' # Expected: 2 segments (1-10, 12-21) because manualUnv acts as a hard barrier.
#'
#' # 4. Segment at the very end of the file (Bug fix check)
#' pitch = c(rep(NA, 20), rep(300, 5))
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 40, shortestPause = 10, step = 10, minVoicedCands = 1, pitchMethods = 'autocor'
#' )
#' # Expected: 1 segment (21-25)
#'
#' # 5. manualV forcing a start on a short segment
#' pitch = c(rep(NA, 5), 300, rep(NA, 5)) # only 1 voiced frame
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 50, shortestPause = 10, step = 10, minVoicedCands = 1, pitchMethods = 'autocor',
#'   manualV = 6
#' )
#' # Expected: 1 segment (6-6). Normally too short, but manualV forces it to be a seed.
#'
#' # 6. Out of bounds manual indices (Safety check)
#' pitch = c(rep(300, 10), NA, rep(300, 10))
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 50, shortestPause = 20, step = 10, minVoicedCands = 1, pitchMethods = 'autocor',
#'   manualUnv = 100, manualV = 50
#' )
#' # Expected: safely ignores out-of-bounds indices, returns merged 1-21.
#'
#' # 7. Alternating T/F (should not start a segment if nRequired > 1)
#' pitch = c(300, NA, 300, NA, 300, NA, 300)
#' soundgen:::findVoicedSegments(
#'   pitchCands = matrix(pitch, nrow = 1),
#'   shortestSyl = 30, shortestPause = 20, step = 10, minVoicedCands = 1, pitchMethods = 'autocor'
#' )
#' # Expected: empty dataframe. Max consecutive TRUEs is 1, but nRequired is 3.
findVoicedSegments = function(pitchCands,
                              shortestSyl,
                              shortestPause,
                              step,
                              minVoicedCands,
                              pitchMethods,
                              manualV = NULL,
                              manualTryToV = NULL,
                              manualUnv = NULL) {
  resetMVC = FALSE
  messageMVC = FALSE
  if (is.null(minVoicedCands) || (is.numeric(minVoicedCands) && is.na(minVoicedCands))) {
    resetMVC = TRUE
  } else if (is.numeric(minVoicedCands)) {
    if (minVoicedCands < 1 || minVoicedCands > length(pitchMethods)) {
      resetMVC = TRUE
      messageMVC = TRUE
    }
  }
  if (resetMVC) {
    if ('dom' %in% pitchMethods && length(pitchMethods) > 1) {
      # since dom is usually defined, we want at least one more pitch candidate
      # (unless dom is the ONLY method that the user wants for pitch tracking)
      minVoicedCands = 2
    } else {
      minVoicedCands = 1
    }
    if (messageMVC) {
      message(paste0('minVoicedCands must be between 1 and length(pitchMethods);',
                     ' resetting to ', minVoicedCands))
    }
  }

  lenV = ncol(pitchCands)
  if (lenV == 0) return(data.frame(segmentStart = integer(0), segmentEnd = integer(0)))

  # Safeguard indices against out-of-bounds
  if (length(manualV) > 0) manualV = manualV[manualV >= 1 & manualV <= lenV]
  if (length(manualUnv) > 0) manualUnv = manualUnv[manualUnv >= 1 & manualUnv <= lenV]
  if (length(manualTryToV) > 0) manualTryToV = manualTryToV[manualTryToV >= 1 & manualTryToV <= lenV]

  # Count non-NA candidates per frame
  n_cands = colSums(!is.na(pitchCands))
  putativelyVoiced = n_cands >= minVoicedCands

  if (length(manualTryToV) > 0) {
    # voice as forced by manualTryToV, as long as those frames have any cand-s
    putativelyVoiced[manualTryToV] = putativelyVoiced[manualTryToV] | (n_cands[manualTryToV] > 0)
  }
  if (length(manualV) > 0) putativelyVoiced[manualV] = TRUE
  if (length(manualUnv) > 0) putativelyVoiced[manualUnv] = FALSE

  if (!any(putativelyVoiced)) {
    return(data.frame(segmentStart = integer(0), segmentEnd = integer(0)))
  }

  # the smallest number of consecutive non-NA pitch values that constitute a
  # voiced segment; but at least 1
  nRequired = max(1, ceiling(shortestSyl / step))

  # Run-length encoding to find contiguous voiced/unvoiced runs
  r = rle(putativelyVoiced)
  ends = cumsum(r$lengths)
  starts = ends - r$lengths + 1
  runs = data.frame(
    start = starts,
    end = ends,
    val = r$values,
    is_seed = FALSE,
    stringsAsFactors = FALSE
  )

  # Identify seed runs (must have nRequired consecutive TRUEs, or contain a manualV)
  true_runs = which(runs$val == TRUE)
  for (i in true_runs) {
    if ((runs$end[i] - runs$start[i] + 1) >= nRequired) {
      runs$is_seed[i] = TRUE
    } else if (length(manualV) > 0) {
      if (any(manualV >= runs$start[i] & manualV <= runs$end[i])) {
        runs$is_seed[i] = TRUE
      }
    }
  }

  segmentStart = integer()
  segmentEnd = integer()

  seed_idx = which(runs$is_seed)
  if (length(seed_idx) > 0) {
    i = 1
    while (i <= length(seed_idx)) {
      cur_seed_run = seed_idx[i]
      seg_start = runs$start[cur_seed_run]
      seg_end = runs$end[cur_seed_run]

      # Extend forward through subsequent runs if gap is tolerable and contains no manualUnv
      j = cur_seed_run + 1
      while (j <= nrow(runs)) {
        if (runs$val[j] == FALSE) {
          gap_run = j
          gap_len = runs$end[gap_run] - runs$start[gap_run] + 1
          gap_has_manualUnv = FALSE

          if (length(manualUnv) > 0) {
            if (any(manualUnv >= runs$start[gap_run] & manualUnv <= runs$end[gap_run])) {
              gap_has_manualUnv = TRUE
            }
          }

          # Gap duration must be strictly less than shortestPause to be merged
          if (gap_len * step < shortestPause && !gap_has_manualUnv && (j + 1) <= nrow(runs)) {
            # Gap is tolerable, absorb the next TRUE run
            seg_end = runs$end[j + 1]
            j = j + 2  # advance past the absorbed TRUE run
          } else {
            break
          }
        } else {
          break
        }
      }

      segmentStart = c(segmentStart, seg_start)
      segmentEnd = c(segmentEnd, seg_end)

      # Advance i to the next seed that ends after the current segment
      while (i <= length(seed_idx) && runs$end[seed_idx[i]] <= seg_end) {
        i = i + 1
      }
    }
  }

  data.frame(segmentStart = segmentStart, segmentEnd = segmentEnd)
}



#' Plot pitch candidates
#'
#' Plots pitch candidates or adds them to a spectrogram.
#' @param pitchCands,pitchCert,pitchSource matrices of pitch candidates, their
#'   certainty, and pitch tracking method used as generated internally by
#'   analyze(); columns = STFT frames, rows = candidates
#' @param pitch best guess at pitch contour; length = ncol(pitchCands)
#' @param candPlot,pitchPlot lists of graphical settings for plotting candidates
#'   and pitch contour, respectively
#' @param extraContour another contour to add to the plot, such as harmHeight, Hz
#' @param addToExistingPlot if TRUE, assumes that a spectrogram is already
#'   plotted; if FALSE, sets up a new plot
#' @param showLegend if TRUE, shows a legend
#' @param y_Hz if TRUE, plot in Hz, otherwise in kHz
#' @param ... other graphical parameters used for creating a new plot if
#'   addToExistingPlot = FALSE
#' @noRd
#' @examples
#' s = soundgen()
#' a = soundgen:::.analyze(soundgen:::readAudio(s, samplingRate = 16000),
#'  windowLength = 25, step = 25,
#'  returnPitchCands = TRUE, plot = FALSE)
#' spectrogram(s, 16000, windowLength = 25, step = 5, ylim = c(0, 1), osc = 'none')
#' soundgen:::addPitchCands(
#'   pitchCands = a$pitchCands$freq,
#'   pitchCert = a$pitchCands$cert,
#'   pitchSource = a$pitchCands$source,
#'   pitch = a$result$pitch,
#'   timestamps = a$result$time / 1000)
addPitchCands = function(
    pitchCands,
    pitchCert,
    pitchSource,
    pitch,
    timestamps = NULL,
    candPlot = list(),
    pitchPlot = list(),
    extraContour = NULL,
    extraContour_pars = list(col = 'blue', lwd = 3, lty = 1),
    extraContour_warp = TRUE,
    priorMean = NULL,
    priorSD = NULL,
    pitchFloor = NULL,
    pitchCeiling = NULL,
    addToExistingPlot = TRUE,
    showLegend = TRUE,
    y_Hz = FALSE,
    yScale = c('linear', 'bark', 'mel')[1],
    ...) {
  if (is.null(pitchCands) && is.null(pitch)) return()
  if (length(pitchCands) < 1 && length(pitch) < 1) return()
  if (is.null(timestamps) && !is.null(pitchCands)) {
    timestamps = as.numeric(colnames(pitchCands))
  }
  if (is.null(pitchPlot$showPrior)) {
    showPrior = TRUE
  } else {
    showPrior = pitchPlot$showPrior
  }
  if (showPrior && is.numeric(priorMean) && is.numeric(priorSD)) {
    prior = getPrior(
      priorMean = priorMean,
      priorSD = priorSD,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling,
      len = 100,
      plot = FALSE
    )
  } else {
    prior = NULL
  }
  pitchPlot = pitchPlot[names(pitchPlot) != 'showPrior']
  yScaleCoef = ifelse(y_Hz | yScale %in% c('bark', 'mel', 'ERB', 'log'), 1, 1/1000)
  if (yScale != 'linear') {
    pitchCands = HzToOther(pitchCands, yScale)
    pitch = HzToOther(pitch, yScale)
    if (!is.null(prior)) prior$freq = HzToOther(prior$freq, yScale)
    if (!is.null(extraContour) && extraContour_warp)
      extraContour = HzToOther(extraContour, yScale)
    if (!is.null(pitchFloor)) pitchFloor = HzToOther(pitchFloor, yScale)
    if (!is.null(pitchCeiling)) pitchCeiling = HzToOther(pitchCeiling, yScale)
    if (is.numeric(priorMean)) priorMean = HzToOther(priorMean, yScale)
  }

  # If addToExistingPlot is FALSE, we first have to set up an empty plot
  if (!addToExistingPlot) {
    arguments = list(...)  # save ... arguments as a list
    m = max(pitchCands, na.rm = TRUE) * yScaleCoef  # for ylim on the empty plot
    if (!is.finite(m)) m = 5  # samplingRate / 2 * yScaleCoef
    arguments$ylim = c(0, m)
    do.call(plot,   # need do.call, otherwise can't pass the modified ylim
            args = c(list(x = timestamps,
                          y = rep(0, ncol(pitchCands)),
                          type = 'n'),
                     arguments))
  }

  # Prepare plot pars - combine user-defined with defaults
  plotPars = defaults_analyze_pitchCand  # see presets.R
  candPlot$final = pitchPlot

  for (i in seq_along(candPlot)) {
    temp = candPlot[[i]]
    if (length(temp) > 0) {
      method = names(candPlot)[i]
      idx = which(plotPars$method == method)
      if (length(idx) > 0) {
        for (j in seq_along(temp)) {
          property = names(temp[j])[1]
          if (!property %in% colnames(plotPars)) {
            # set to default, otherwise other rows become NA
            if (property == 'cex') {
              plotPars[, property] = 2
            } else if (property == 'lty') {
              plotPars[, property] = 1  # par(lty) gives "solid" - a character
            } else {
              plotPars[, property] = par(property)
            }
          }
          plotPars[idx, property] = temp[j]
        }
      }
    }
  }

  # Add pitch candidates to the plot
  pitchMethods = unique(na.omit(as.character(pitchSource)))
  if (length(pitchMethods) > 0) {
    for (r in 1:nrow(pitchCands)) {
      method_r = as.character(pitchSource[r, ])
      method_r[is.na(method_r)] = 'def'
      method_pars = as.list(plotPars[match(method_r, plotPars$method), 2:ncol(plotPars)])
      method_pars$cex = method_pars$cex *
        plotPars$cex[plotPars$method == 'final'] *
        pitchCert[r, ]
      # for manual, certainty could be Inf, so we reset to 2
      method_pars$cex[which(!is.na(method_pars$cex) &
                              method_pars$cex == Inf)] = 2
      # method_pars$cex[is.na(method_pars$cex)] = 0
      do.call(points, c(method_pars,
                        list(x = timestamps,
                             y = pitchCands[r, ] * yScaleCoef)))
    }
  } else {
    showLegend = FALSE
  }

  # Add the final pitch contour to the plot
  if (any(is.finite(pitch))) {
    pitchContour_pars = as.list(plotPars[plotPars$method == 'final',
                                         2:ncol(plotPars)])
    # first points, in case there are isolated, unconnected points surrounded by
    # NAs (not plotted with type = 'l')
    do.call('points', c(list(
      x = timestamps,
      y = pitch * yScaleCoef
    ), pitchContour_pars))
    # now join pitch contour with a line
    do.call('points', c(list(
      x = timestamps,
      y = pitch * yScaleCoef,
      type = 'l'
    ), pitchContour_pars))
  }

  # Add an extra contour such as harmHeight
  if (!is.null(extraContour)) {
    if (any(!is.na(extraContour)) && length(timestamps) > 0) {
      if (is.null(extraContour_pars$type)) extraContour_pars$type = 'l'
      if (extraContour_pars$type != 'n') {
        if (is.null(extraContour_pars$lty)) extraContour_pars$lty = 1
        if (is.null(extraContour_pars$lwd)) extraContour_pars$lwd = 3
        if (is.null(extraContour_pars$col)) extraContour_pars$col = 'blue'
        do.call(points, c(list(x = timestamps, y = extraContour * yScaleCoef),
                          extraContour_pars))
      }
    }
  }

  # Show prior
  if (!is.null(prior)) {
    ran_x_5 = (tail(timestamps, 1) - timestamps[1]) * .075   # 7.5% of plot width
    points(x = prior$prob * ran_x_5,
           y = prior$freq * yScaleCoef, type = 'l', lty = 2)
    text(x = ran_x_5,
         y = priorMean * yScaleCoef,
         pos = 2, labels = 'Prior', cex = 0.65, offset = 0.25)
  }
  if (!is.null(pitchFloor))
    text(x = 0, y = pitchFloor * yScaleCoef,
         pos = 4, labels = 'floor', cex = 0.65, offset = 0)
  if (!is.null(pitchCeiling))
    text(x = 0, y = pitchCeiling * yScaleCoef,
         pos = 4, labels = 'ceiling', cex = 0.65, offset = 0)

  # Add a legend
  if (showLegend) {
    pl = plotPars[plotPars$method %in% c(pitchMethods, 'final'), ]
    legend("topright",
           legend = pl$method,
           pch = pl$pch,
           lty = ifelse(pl$method == 'final', pl$lty, NA),
           lwd = pl$lwd,
           col = pl$col,
           bg = "white")
  }
  return()
}
