# Functions for controlling the spectrum of generated sounds (rolloff and formants).

#' Rolloff of harmonics in source spectrum
#'
#' A periodic sound often contains many harmonics of the fundamental frequency
#' (f0). For instance, \code{\link{soundgen}} generates voice source as a
#' combination of pure tones - one for each partial in the source spectrum.
#' Harmonics typically become weaker at higher frequencies, and the rolloff
#' (roll-off) function describes the loss of energy in upper harmonics relative
#' to f0. \code{\link{getRolloff}} provides flexible control over this rolloff
#' function: simple exponential (\code{rolloff}), \code{rolloffOct} that varies
#' per octave, and \code{rolloffKHz} for rolloff correction depending on f0. It
#' calculates the rolloff function based on these parameters and returns the
#' amplitude of each harmonic.
#'
#' @seealso \code{\link{soundgen}}
#'
#' @param pitch_per_gc a vector of f0 per glottal cycle, Hz
#' @param nHarmonics maximum number of harmonics or partials to generate,
#'   including f0 (all harmonics above Nyquist frequency or with amplitude <
#'   \code{-dynamicRange} are discarded)
#' @inheritParams soundgen
#' @param rolloff the rate at which f0 harmonics in the spectrum become weaker,
#'   dB/oct (anchor format for all rolloff-related parameters). More negative
#'   rolloff = weaker upper harmonics
#' @param baseline The "neutral" f0, at which no adjustment of rolloff
#'   takes place regardless of \code{rolloffKHz}
#' @param samplingRate sampling rate (needed to stop at Nyquist frequency and
#'   for plotting purposes)
#' @param plot if TRUE, produces a plot
#' @return A matrix of amplitude multiplication factors for adjusting the
#'   amplitude of harmonics relative to f0 (1 = no adjustment, 0 = silent). Each
#'   row of output contains one harmonic, and each column contains one glottal
#'   cycle.
#' @export
#' @examples
#' # steady exponential rolloff of -12 dB per octave
#' rolloff1 = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffOct = 0, rolloffKHz = 0, plot = TRUE)
#' # the rate of rolloff slows down by 1 dB each octave
#' r = getRolloff(pitch_per_gc = 100, rolloff = -6, rolloffOct = 0)
#' r1 = getRolloff(pitch_per_gc = 100, rolloff = -6, rolloffOct = -1)
#' round(20*log10(r[c(1, 2, 4, 8, 16)]), 1)
#' round(20*log10(r1[c(1, 2, 4, 8, 16)]), 1)
#'
#' # rolloff can be made to depend on f0 using rolloffKHz
#' rolloff = getRolloff(pitch_per_gc = c(150, 400, 800),
#'   rolloffOct = 0, rolloffKHz = -3, plot = TRUE)
#' # without the correction for f0 (rolloffKHz),
#'   # high-pitched sounds have the same rolloff as low-pitched sounds,
#'   # producing unnaturally strong high-frequency harmonics
#' rolloff = getRolloff(pitch_per_gc = c(150, 400, 800),
#'   rolloffOct = 0, rolloffKHz = 0, plot = TRUE)
#'
#' # dynamic rolloff (varies over time)
#' rolloff = getRolloff(pitch_per_gc = c(150, 250),
#'                      rolloff = c(-12, -18, -24), plot = TRUE)
getRolloff = function(pitch_per_gc = c(440),
                      nHarmonics = 1000,
                      rolloff = -6,
                      rolloffOct = 0,
                      rolloffKHz = 0,
                      baseline = 200,
                      dynamicRange = 80,
                      samplingRate = 16000,
                      plot = FALSE) {
  if (any(pitch_per_gc <= 0)) stop('pitch_per_gc must be positive')

  # Don't need extra harmonics above Nyquist
  nyquist = samplingRate / 2
  nHarmonics = min(nHarmonics, floor(nyquist / min(pitch_per_gc)))

  ## Convert rolloff pars from df to a single number (if static)
  #  or to a vector of length nGC (if dynamic)
  nGC = length(pitch_per_gc)
  update_pars = c('rolloff', 'rolloffOct', 'rolloffKHz')

  # if any of the par-s have more values than gc (unlikely), upsample pitch
  # NB: for numeric par values only, since soundgen() uses dataframes,
  # and we do NOT want to mess with pitch values when getRolloff()
  # is called internally by soundgen()
  max_length = max(vapply(update_pars, function(x) {
    if (is.numeric(get(x))) length(get(x)) else 1
  }, numeric(1)))
  if (max_length > nGC) {
    pitch_per_gc = approx(pitch_per_gc, n = max_length)$y
    nGC = length(pitch_per_gc)
  }

  # make all par-s vectors of length 1 or nGC
  for (p in update_pars) {
    old = get(p)
    if (is.list(old)) {  # par is data.frame(time = ..., value = ...)
      if (length(unique(old$value)) > 1 && nrow(old) != nGC) {
        # if not all values are the same and the length is off, upsample them
        values_upsampled = getSmoothContour(
          anchors = old, len = nGC,
          valueFloor = permittedValues[p, 'low'],
          valueCeiling = permittedValues[p, 'high'])
        assign(p, values_upsampled)
      } else {
        assign(p, unique(old$value))  # just one number
      }
    } else {             # par is numeric
      if (length(old) != nGC) {
        new = getSmoothContour(old, len = nGC)
        assign(p, new)
      }
    }
  }

  # create rolloff matrix
  # adjust rolloff by rolloffKHz for every kHz that f0 is above "baseline"
  deltas_kHz = rolloffKHz * (pitch_per_gc - baseline) / 1000
  h = seq_len(nHarmonics)
  k = log2(h)
  r = tcrossprod(k, rolloff + deltas_kHz)

  # rolloffOct correction:
  # cumulative dB correction at octave k is rolloffOct * k * (k - 1) / 2
  # e.g. k = 1, 2, 3 gives correction factors 0, 1, 3
  if (any(rolloffOct != 0)) {
    oct_corr = 0.5 * k * (k - 1)
    r = r + tcrossprod(oct_corr, rolloffOct)
  }

  # to avoid aliasing, we discard all harmonics above Nyquist frequency
  r[outer(h, pitch_per_gc) >= nyquist] = -Inf
  # plot(r[, 1], type = 'b')

  # set values under -dynamicRange to zero
  if (is.numeric(dynamicRange)) {
    # if not null and not NA
    r[r < -dynamicRange] = -Inf
  } else {
    dynamicRange = 80  # for plotting
  }

  # normalize so the amplitude of F0 is always 0
  r = sweep(r, 2, r[1, ], `-`)

  # plotting
  if (plot) {
    x_max = nyquist / 1000
    if (nGC == 1 || var(pitch_per_gc) == 0) {
      idx = which(r[, 1] > -Inf)
      plot ( idx * pitch_per_gc[1] / 1000, r[idx, 1],
             type = 'b', xlim = c(0, x_max), xlab = 'Frequency, KHz',
             ylab = 'Amplitude, dB', main = 'Glottal source rolloff')
    } else {
      pitch_min = min(pitch_per_gc)
      pitch_max = max(pitch_per_gc)
      idx_min = which.min(pitch_per_gc)
      idx_max = which.max(pitch_per_gc)
      rows_min = 1:tail(which(r[, idx_min] > -Inf), 1)
      rows_max = 1:tail(which(r[, idx_max] > -Inf), 1)
      freqs_min = rows_min * pitch_min / 1000
      freqs_max = rows_max * pitch_max / 1000
      rolloff_min = r[rows_min, idx_min]
      rolloff_max = r[rows_max, idx_max]
      ymin = min(rolloff_min, rolloff_max)
      if (ymin < -dynamicRange) ymin = -dynamicRange
      ymax = max(rolloff_min, rolloff_max)
      plot(freqs_min, rolloff_min, type = 'b', col = 'blue',
           xlim = c(0, x_max), xlab = 'Frequency, kHz',
           ylim = c(ymin, ymax), ylab = 'Amplitude, dB',
           main = 'Glottal source rolloff')
      text(x = x_max, y = -10, labels = 'Lowest pitch',
           col = 'blue', pos = 2)
      points(freqs_max, rolloff_max, type = 'b', col = 'red')
      text(x = x_max, y = 0, labels = 'Highest pitch',
           col = 'red', pos = 2)
    }
  }

  # convert from dB to linear amplitude multipliers
  r = 10 ^ (r / 20)
  r[is.nan(r)] = 0  # in case of -Inf problems

  # shorten by discarding harmonics that are 0 throughout the sound
  r = r[which(rowSums(r) > 0), , drop = FALSE]
  rownames(r) = 1:nrow(r) # helpful for adding vocal fry
  r
}


#' Add subharmonics to rolloff matrix
#' Adds subharmonics to the main (f0) harmonic stack, forming sidebands.
#' @param subDep_min if subDep is smaller than this, it is reset to 0 to avoid
#'   wasting computational effort on inaudible subharmonics
#' @return A list consisting of a list of rolloff matrices (one matrix per
#'   epoch) and a dataframe of epochs.
#' @noRd
#' @examples
#' pitch_per_gc = c(400, 500, 600, 700)
#' rolloff = getRolloff(pitch_per_gc, rolloff = -30)
#' # one epoch, two subharmonics
#' rolloff_subh = soundgen:::addSubh(rolloff, pitch_per_gc,
#'   subDep = rep(150, 4), shortestEpoch = 100)
#' # three epochs with 2/3/4 subharmonics
#' rolloff_subh = soundgen:::addSubh(rolloff, pitch_per_gc,
#'   subDep = rep(50, 4), shortestEpoch = 0)
addSubh = function(rolloff,
                   pitch_per_gc,
                   subRatio = 2,
                   subDep = 10,
                   subDep_min = 1,
                   dynamicRange = 80,
                   shortestEpoch = 300) {
  subRatio = round(subRatio)
  nSubharm = subRatio - 1
  nSubharm[nSubharm < 0] = 0
  subDep[subDep < subDep_min] = 0
  if (max(nSubharm) < 1 || !any(subDep > 0)) {
    # nothing to add
    return(list(
      'rolloff' = list(rolloff),
      'epochs' = data.frame('start' = 1, 'end' = length(pitch_per_gc))
    ))
  }

  throwaway01 = 10 ^ (-dynamicRange / 20)
  period_ms = 1000 / pitch_per_gc
  min_epoch_length_points = round(shortestEpoch / period_ms)

  # to ensure each epoch is long enough, we keep the same number of harmonics
  # for at least min_epoch_length_points glottal cycles
  if (length(nSubharm) > 1) {
    nSubharm = clumper(nSubharm, minLength = min_epoch_length_points)
  }

  # divide the task into epochs based on the required number of subharmonics
  nSubharm_change_idx = which(diff(nSubharm) != 0)  # last idx before change
  epoch_start_idx = c(1, nSubharm_change_idx + 1)
  epoch_end_idx = c(nSubharm_change_idx, length(pitch_per_gc))
  nEpochs = length(nSubharm_change_idx) + 1
  # check that nEpochs == length(unique(nSubharm))
  nSubharm_per_epoch = nSubharm[c(nSubharm_change_idx, length(nSubharm))]

  # add subharmonics to rolloff for each epoch separately
  rolloff_new = vector("list", nEpochs)
  for (e in 1:nEpochs) {
    idx = epoch_start_idx[e]:epoch_end_idx[e]
    rolloff_new[[e]] = addSubh_per_epoch(
      rolloff = rolloff[, idx, drop = FALSE],
      nSubharm = nSubharm_per_epoch[e],
      subDep_vector = subDep[idx],
      throwaway01 = throwaway01
    )
  }

  list(
    'rolloff' = rolloff_new,
    'epochs' = data.frame('start' = epoch_start_idx, 'end' = epoch_end_idx)
  )
}


#' Add constant subharmonics to rolloff matrix
#' Helper function for adding subharmonics to a single epoch, with unchanging
#' subharmonic regime (fixed number of subharmonics and sideband width). See
#' master function addSubh().
#' @param rolloff matrix of original amplitudes of each harmonic in f0 stack
#'   returned by \code{\link{getRolloff}} (columns=time, rows=frequency bins)
#' @param nSubharm the number of subharmonics to generate (a positive integer).
#'   If nSubharm==1, g0 = f0 / 2. If nSubharm==2, g0 = f0 / 3, etc.
#' @param subDep_vector (either numeric or vector of the same length as
#'   pitch_per_gc): the amplitude of subharmonics relative to f0 partials
#' @param throwaway01 discard harmonics that are weaker than this number (between
#'   0 and 1) to save computational resources
#' @return A modified rolloff matrix with added subharmonics.
#' @noRd
addSubh_per_epoch = function(rolloff,
                             nSubharm,
                             subDep_vector,
                             throwaway01) {
  if (nSubharm < 1) return(rolloff)

  nHarm = nrow(rolloff)
  nGC = ncol(rolloff)
  if (nHarm == 0 || nGC == 0) return(rolloff)
  nSubharm = as.integer(nSubharm)
  m = nSubharm + 1L
  nNew = nHarm * m

  # Fractional harmonic numbers:
  # 1/m, 2/m, ..., 1, 1+1/m, ..., nHarm
  newPos = seq_len(nNew) / m

  # For every new row, the nearest f0 partial above it is:
  # harmonic 1 for the first nSubharm subharmonic rows,
  # then harmonic 1 itself,
  # then harmonic 2 for the next nSubharm subharmonic rows,
  # then harmonic 2 itself, etc.
  upper_idx = rep(seq_len(nHarm), each = m)

  # Start by repeating each f0 partial m times
  rolloff_new = rolloff[upper_idx, , drop = FALSE]

  # Every m-th row is an original f0 harmonic; the others are subharmonics
  is_f = (seq_len(nNew) %% m) == 0L
  sub_rows = !is_f
  nSub = sum(sub_rows)

  if (nSub > 0) {
    # If subDep is on the old 0-100 scale, use:
    # rep(subDep_vector / 100, each = nSub)
    rolloff_new[sub_rows, ] =
      rolloff_new[sub_rows, , drop = FALSE] *
      rep(subDep_vector / 100, each = nSub)
  }

  # discard very quiet partials
  rolloff_new[rolloff_new < throwaway01] = 0
  rownames(rolloff_new) = newPos
  rolloff_new[rowSums(rolloff_new) > 0, , drop = FALSE]
}
