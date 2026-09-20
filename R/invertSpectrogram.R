#' Invert spectrogram
#'
#' Transforms a spectrogram into a time series with inverse STFT. The problem is
#' that an ordinary spectrogram preserves only the magnitude (modulus) of the
#' complex STFT, while the phase is lost, and without phase it is impossible to
#' reconstruct the original audio accurately. So there are a number of
#' algorithms for "guessing" the phase that would produce an audio whose
#' magnitude spectrogram is very similar to the target spectrogram. Useful for
#' certain filtering operations that modify the magnitude spectrogram followed
#' by inverse STFT, such as filtering in the spectro-temporal modulation domain.
#'
#' Algorithm: takes the spectrogram, makes an initial guess at the phase (zero,
#' noise, or a more intelligent estimate by the SPSI algorithm), fine-tunes over
#' \code{nIter} iterations with the GL algorithm, reconstructs the complex
#' spectrogram using the best phase estimate, and performs inverse STFT. The
#' single-pass spectrogram inversion (SPSI) algorithm is implemented as
#' described in Beauregard et al. (2015) following the python code at
#' https://github.com/lonce/SPSI_Python. The Griffin-Lim (GL) algorithm is based
#' on Griffin & Lim (1984).
#'
#' @seealso \code{\link{spectrogram}} \code{\link{filterSoundByMS}}
#'
#' @references \itemize{
#'   \item Griffin, D., & Lim, J. (1984). Signal estimation from modified
#'   short-time Fourier transform. IEEE Transactions on Acoustics, Speech, and
#'   Signal Processing, 32(2), 236-243.
#'   \item Beauregard, G. T., Harish, M., & Wyse, L. (2015, July). Single pass
#'   spectrogram inversion. In 2015 IEEE International Conference on Digital
#'   Signal Processing (DSP) (pp. 427-431). IEEE.
#' }
#'
#' @param spec the spectrogram that is to be transform to a time series: numeric
#'   matrix of real value (no phase) with frequency bins in rows (kHz) and time
#'   frames in columns (ms)
#' @param samplingRate sampling rate (not needed if the spectrogram has rownames
#'   corresponding to frequency bins with the last one at Nyquist = samplingRate
#'   / 2)
#' @param windowLength,step,overlap,wn STFT parameters used to create the original
#'   spectrogram; make sure \code{zp = 0}
#' @param specScale the scale of target spectrogram: 'spec' = untransformed
#'   amplitude spectrum, 'power' = power spectrum, 'log' = log-transformed, 'dB'
#'   = in decibels
#' @param initialPhase initial phase estimate: "spsi" (default) = single-pass
#'   spectrogram inversion (Beauregard et al., 2015); "zero" = set all phases to
#'   zero; "random" = Gaussian noise
#' @param nIter the number of iterations of the GL algorithm (Griffin & Lim,
#'   1984), 0 = don't run
#' @param normalize if TRUE, normalizes the output to range from -1 to +1
#' @param play if TRUE, plays back the reconstructed audio
#' @param verbose if TRUE, prints estimated time left every 10\% of GL
#'   iterations
#' @param plotError if TRUE, plots the error during GL iterations and prints the
#'   final reconstruction error (useful for choosing nIter)
#'
#' @return Reconstructed audio as a numeric vector.
#' @export
#' @examples
#' # Create a spectrogram. NB: do NOT use zero-padding
#' samplingRate = 16000
#' windowLength = 40
#' step = 5
#' wn = 'gaussian'
#' # NB: the more detailed a spectrogram, the more precisely it can be inverted,
#' # so a relatively long window AND a short step make for best results
#'
#' s = soundgen(samplingRate = samplingRate, addSilence = 50)
#' spec = spectrogram(s, samplingRate = samplingRate,
#'   wn = wn, windowLength = windowLength, step = step,
#'   zp = 0,  # otherwise it changes the window length and messes up istft
#'   padWithSilence = FALSE, output = 'original')
#'
#' # Invert the spectrogram, attempting to guess the phase
#' # Note that we need to know the original windowLength, step, and wn
#' # (i.e., you have to know how the spectrogram was created)
#' s_new = invertSpectrogram(spec, samplingRate = samplingRate,
#'   windowLength = windowLength, step = step, wn = wn,
#'   initialPhase = 'spsi', nIter = 50, play = FALSE)
#'
#' # Verify the quality of audio reconstruction
#' # playme(s, samplingRate); playme(s_new, samplingRate)
#'
#' \dontrun{
#' # to improve the quality of reconstruction, increase the number of iterations
#' s_new = invertSpectrogram(spec, samplingRate = samplingRate,
#'   windowLength = windowLength, step = step, wn = wn,
#'   initialPhase = 'spsi', nIter = 500, play = FALSE)
#' playme(s, samplingRate); playme(s_new, samplingRate)
#' spectrogram(s, samplingRate)
#' spectrogram(s_new, samplingRate)
#' }
invertSpectrogram = function(
    spec,
    samplingRate = NULL,
    windowLength,
    step,
    overlap = NULL,
    wn,
    specScale = c('spec', 'power', 'log', 'dB'),
    initialPhase = c('spsi', 'random', 'zero'),
    nIter = 50,
    normalize = TRUE,
    play = FALSE,
    verbose = FALSE,
    plotError = TRUE) {
  specScale = match.arg(specScale)
  initialPhase = match.arg(initialPhase)
  nr = nrow(spec)  # frequency bins
  nc = ncol(spec)  # time frames
  max_freq = as.numeric(rownames(spec)[nr])
  if (is.null(samplingRate)) {
    if (is.na(max_freq)) {
      stop('samplingRate not specified, and no rownames with frequency labels')
    }
    # Heuristic: if max freq > 100, assume labels are in Hz, else kHz
    samplingRate = if (max_freq > 100) max_freq * 2 else max_freq * 2000
  }
  if (is.null(step)) {
    if (is.null(overlap)) {
      stop('Need to specify either step or overlap')
    } else {
      step = windowLength * (1 - overlap / 100)
    }
  }
  step_points = round(step / 1000 * samplingRate)
  wl = round(windowLength / 1000 * samplingRate)


  # Rescale the spectrogram
  if (specScale == 'log') {
    spec = exp(spec)
  } else if (specScale == 'dB') {
    spec = 10 ^ (spec / 20)
  } else if (specScale == 'power') {
    spec = sqrt(spec)
  } else if (specScale == 'spec') {
  } else {
    stop('specScale not recognized')
  }

  # Set the phase to zero / Gaussian noise / an intelligent guess
  if (initialPhase == 'zero') {
    phase_init = matrix(0, nrow = nr, ncol = nc)
  } else if (initialPhase == 'random') {
    phase_init = matrix(runif(nr * nc, -pi, pi), nrow = nr, ncol = nc)
  } else if (initialPhase == 'spsi') {
    phase_init = guessPhase_spsi(spec = spec,
                                 wl = wl,
                                 step_points = step_points)
  }

  # Set the phase at DC and Nyquist to 0 to get all-real output
  phase_init[1, ] = 0
  if (wl %% 2 == 0) phase_init[nr, ] = 0

  # To speed up phase-guessing, reconstruct the full spectrogram with both
  # positive and negative frequencies (see istft_simple)
  if (wl %% 2 == 0) {  # even wl
    spec_full = rbind(spec, spec[(nr - 1):2, ])
    phase_init = rbind(phase_init, -phase_init[(nr - 1):2, , drop = FALSE])
  } else {  # odd wl
    spec_full = rbind(spec, spec[nr:2, ])
    phase_init = rbind(phase_init, -phase_init[nr:2, , drop = FALSE])
  }

  # Fine-tune the phase with an iterative algorithm
  if (nIter > 0) {
    gl = guessPhase_GL(spec = spec_full,
                       phase = phase_init,
                       nIter = nIter,
                       samplingRate = samplingRate,
                       wl = wl,
                       step_points = step_points,
                       wn = wn,
                       verbose = verbose,
                       plotError = plotError)
    phase = gl$phase
  } else {
    phase = phase_init
  }

  # Euler's formula - get complex spectrum from magnitude and phase
  spec_complex = spec_full * exp(1i * phase)

  # Inverse STFT
  out = istft_simple(spec_complex, wl = wl,
                     step = step_points, wn = wn, type = "full")
  if (normalize) out = out / max(abs(out))

  # Play the reconstructed audio
  if (isTRUE(play)) {
    playme(out, samplingRate = samplingRate)
    # spectrogram(out, samplingRate, yScale = 'bark')
  } else if (is.character(play)) {
    playme(out, samplingRate = samplingRate, player = play)
  }

  # Show the error
  if (plotError) {
    spec_new = stft_simple(out, wl = wl,
                           step = step_points, wn = wn)
    spec_new = Mod(spec_new[1:nr, ])
    m = min(ncol(spec), ncol(spec_new))
    m1 = spec[, 1:m] / max(spec[, 1:m])
    m2 = spec_new[, 1:m] / max(spec_new[, 1:m])
    err = sum((m1 - m2)^2) / sum(m1 ^ 2) * 100
    print(paste0('Square error for input vs reconstructed spectrogram = ',
                 round(err, 1), '%'))
  }

  invisible(out)
}


#' Guess phase SPSI
#'
#' Single-pass spectrogram inversion as described in Beauregard, G. T., Harish,
#' M., & Wyse, L. (2015, July). Single pass spectrogram inversion. In 2015 IEEE
#' International Conference on Digital Signal Processing (DSP) (pp. 427-431).
#' IEEE. See \code{\link{invertSpectrogram}} for details.
#' @inheritParams invertSpectrogram
#' @return A matrix of the same dimensions as spec containing the guessed
#'   phase.
#' @param wl STFT window length in points
#' @param step_points STFT step in points
#' @noRd
guessPhase_spsi = function(spec,
                           wl,
                           step_points) {
  nr = nrow(spec)  # frequency bins
  nc = ncol(spec)  # time frames
  phaseAccumulator = rep(0, nr)
  phase = matrix(0, nrow = nr, ncol = nc)

  # Not enough bins for peak detection
  if (nr < 3) return(phase)

  for (i in 1:nc) {
    spec_i = spec[, i]  # magnitude spectrum of frame i
    # plot(spec_i, type = 'b')
    for (j in 2:(nr - 1)) {  # for all freq bins except first and last
      is_peak = spec_i[j] > spec_i[j - 1] &
        spec_i[j] > spec_i[j + 1]
      if (is_peak) {
        # find the true freq of the peak (may be b/w bins)
        alpha = spec_i[j - 1]
        beta = spec_i[j]
        gamma = spec_i[j + 1]
        denom = alpha - 2 * beta + gamma
        p = ifelse(denom == 0, 0, 0.5 * (alpha - gamma) / denom)
        # p is the estimated true peak frequency
        adjustedPhaseRate = 2 * pi * (j - 1 + p) / wl
        phaseAccumulator[j] = phaseAccumulator[j] + step_points * adjustedPhaseRate
        peakPhase = phaseAccumulator[j]

        if (p > 0) {  # If actual peak is to the right of the bin freq
          # First bin to the right has pi shift
          phaseAccumulator[j + 1] = peakPhase + pi

          # Bins to the left have shift of pi
          bin = j - 1
          while ((bin > 1) && (spec_i[bin] < spec_i[bin + 1])) {
            # until the first trough
            phaseAccumulator[bin] = peakPhase + pi
            bin = bin - 1
          }

          # Bins to the right (beyond the first) have 0 shift
          bin = j + 2
          while ((bin < nr) && (spec_i[bin] < spec_i[bin - 1])) {
            phaseAccumulator[bin] = peakPhase
            bin = bin + 1
          }
        } else if (p < 0) {
          # If actual peak is to the left of the bin frequency
          # First bin to left has pi shift
          phaseAccumulator[j - 1] = peakPhase + pi

          # same for bins to the right
          bin = j + 1
          while ((bin < nr) && (spec_i[bin] < spec_i[bin - 1])) {
            phaseAccumulator[bin] = peakPhase + pi
            bin = bin + 1
          }

          # Bins further to the left have zero shift
          bin = j - 2
          while ((bin > 1) && (spec_i[bin] < spec_i[bin + 1])) {  # until trough
            phaseAccumulator[bin] = peakPhase
            bin = bin - 1
          }
        }
      }
    }
    #  remove dc and nyquist
    phaseAccumulator[1] = 0
    if (wl %% 2 == 0) phaseAccumulator[nr] = 0
    spec_complex = spec_i * exp(1i * phaseAccumulator)  # Euler's formula
    spec_complex[1] = 0
    if (wl %% 2 == 0) spec_complex[nr] = 0
    phase[, i] = Arg(spec_complex)
  }
  phase
}


#' Guess phase GL
#'
#' Uses the iterative method proposed by Griffin & Lim (1984) to guess the phase
#' of a magnitude spectrogram.
#' @inheritParams invertSpectrogram
#' @inheritParams guessPhase_spsi
#' @param phase an initial guess at the phase: numeric matrix of the same dimensions as spec
#' @return A matrix of the same dimensions as spec containing the guessed
#'   phase.
#' @noRd
guessPhase_GL = function(spec,
                         phase,
                         nIter,
                         samplingRate,
                         wl,
                         step_points,
                         wn,
                         verbose = TRUE,
                         plotError = TRUE) {
  # wl = nrow(spec) * 2  # otherwise vulnerable to rounding error
  correl = rep(NA, nIter)
  time_start = proc.time()

  for (i in 1:nIter) {
    # Euler's formula - get complex spectrum from magnitude and phase
    spec_complex = spec * exp(1i * phase)
    # Inverse STFT to a candidate time series
    x = istft_simple(spec_complex, wl = wl,
                     step = step_points, wn = wn, type = "full")

    # STFT of the candidate time series
    spec_new = stft_simple(x, samplingRate = samplingRate,
                           wl = wl, step = step_points, wn = wn)

    # Set phase to that of the new spectrogram
    phase = Arg(spec_new)

    # Calculate error
    if (plotError) {
      # squareError[i] = sum((spec - Mod(spec_new))^2) / sum(spec^2) * 100 / 2
      # RMS error requires normalizing the spectrograms - expensive
      correl[i] = cor(as.numeric(spec), as.numeric(Mod(spec_new)))
    }

    # Report progress
    if (verbose) {
      reportTime(i = i, nIter = nIter,
                 time_start = time_start, reportEvery = ceiling(i / 10))
    }
  }
  # Plot square errors per iteration
  if (plotError) {
    plot(1:nIter, correl, type = 'l',
         xlab = 'GL iteration', ylab = 'Cor. with orig. spectrogram',
         main = 'Improvement in fit',
         ylim = c(min(correl), 1),
         yaxs = 'i')
  }
  list(phase = phase, correl = correl)
}
