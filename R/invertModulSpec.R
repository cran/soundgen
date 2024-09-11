#' Filter sound by modulation spectrum
#'
#' Manipulates the modulation spectrum (MS) of a sound so as to remove certain
#' frequencies of amplitude modulation (AM) and frequency modulation (FM).
#' Algorithm: produces a modulation spectrum with
#' \code{\link{modulationSpectrum}}, modifies it with \code{\link{filterMS}},
#' converts the modified MS to a spectrogram with \code{\link{msToSpec}}, and
#' finally inverts the spectrogram with \code{\link{invertSpectrogram}}, thus
#' producing a sound with (approximately) the desired characteristics of the MS.
#' Note that the last step of inverting the spectrogram introduces some noise,
#' so the resulting MS is not precisely the same as the intermediate filtered
#' version. In practice this means that some residual energy will still be
#' present in the filtered-out frequency range (see examples).
#'
#' @seealso \code{\link{invertSpectrogram}} \code{\link{filterMS}}
#'
#' @return Returns the filtered audio as a numeric vector normalized to [-1, 1]
#'   with the same sampling rate as input.
#' @inheritParams modulationSpectrum
#' @inheritParams filterMS
#' @inheritParams invertSpectrogram
#' @inheritParams addAM
#' @param plot if TRUE, produces a triple plot: original MS, filtered MS, and
#'   the MS of the output sound
#' @export
#' @examples
#' # Create a sound to be filtered
#' s = soundgen(pitch = rnorm(n = 20, mean = 200, sd = 25),
#'   amFreq = 25, amDep = 50, samplingRate = 16000,
#'   addSilence = 50, plot = TRUE, osc = TRUE)
#' # playme(s, 16000)
#'
#' # Filter
#' s_filt = filterSoundByMS(s, samplingRate = 16000,
#'   amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5',
#'   nIter = 10,  # increase nIter for best results!
#'   action = 'remove', plot = TRUE)
#' # playme(s_filt, samplingRate = 16000)
#'
#' \dontrun{
#' # Process all files in a folder, save filtered audio and plots
#' s_filt = filterSoundByMS('~/Downloads/temp2',
#'   saveAudio = '~/Downloads/temp2/ms', savePlots = '',
#'   amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5',
#'   action = 'remove', nIter = 10)
#'
#' # Download an example - a bit of speech (sampled at 16000 Hz)
#' download.file('http://cogsci.se/soundgen/audio/speechEx.wav',
#'               destfile = '~/Downloads/speechEx.wav')  # modify as needed
#' target = '~/Downloads/speechEx.wav'
#' samplingRate = tuneR::readWave(target)@samp.rate
#' playme(target)
#' spectrogram(target, osc = TRUE)
#'
#' # Remove AM above 3 Hz from a bit of speech (remove most temporal details)
#' s_filt1 = filterSoundByMS(target, amCond = 'abs(am) > 3',
#'                           action = 'remove', nIter = 15)
#' playme(s_filt1, samplingRate)
#' spectrogram(s_filt1, samplingRate = samplingRate, osc = TRUE)
#'
#' # Intelligigble when AM in 5-25 Hz is preserved:
#' s_filt2 = filterSoundByMS(target, amCond = 'abs(am) > 5 & abs(am) < 25',
#'                           action = 'preserve', nIter = 15)
#' playme(s_filt2, samplingRate)
#' spectrogram(s_filt2, samplingRate = samplingRate, osc = TRUE)
#'
#' # Remove slow AM/FM (prosody) to achieve a "robotic" voice
#' s_filt3 = filterSoundByMS(target, jointCond = 'am^2 + (fm*3)^2 < 300',
#'                           nIter = 15)
#' playme(s_filt3, samplingRate)
#' spectrogram(s_filt3, samplingRate = samplingRate, osc = TRUE)
#'
#'
#' ## An alternative manual workflow w/o calling filterSoundByMS()
#' # This way you can modify the MS directly and more flexibly
#' # than with the filterMS() function called by filterSoundByMS()
#'
#' # (optional) Check that the target spectrogram can be successfully inverted
#' spec = spectrogram(s, 16000, windowLength = 50, step = NULL, overlap = 80,
#'   wn = 'hanning', osc = TRUE, padWithSilence = FALSE)
#' s_rev = invertSpectrogram(spec, samplingRate = 16000,
#'   windowLength = 50, overlap = 80, wn = 'hamming', play = FALSE)
#' # playme(s_rev, 16000)  # should be close to the original
#' spectrogram(s_rev, 16000, osc = TRUE)
#'
#' # Get modulation spectrum starting from the sound...
#' ms = modulationSpectrum(s, samplingRate = 16000, windowLength = 25,
#'   overlap = 80, wn = 'hanning', amRes = NULL, maxDur = Inf, logSpec = FALSE,
#'   power = NA, returnComplex = TRUE, plot = FALSE)$complex
#' # ... or starting from the spectrogram:
#' # ms = specToMS(spec)
#' plotMS(abs(ms))  # this is the original MS
#'
#' # Filter as needed - for ex., remove AM > 10 Hz and FM > 3 cycles/kHz
#' # (removes f0, preserves formants)
#' am = as.numeric(colnames(ms))
#' fm = as.numeric(rownames(ms))
#' idx_row = which(abs(fm) > 3)
#' idx_col = which(abs(am) > 10)
#' ms_filt = ms
#' ms_filt[idx_row, ] = 0
#' ms_filt[, idx_col] = 0
#' plotMS(abs(ms_filt))  # this is the filtered MS
#'
#' # Convert back to a spectrogram
#' spec_filt = msToSpec(ms_filt)
#' image(t(log(abs(spec_filt))))
#'
#' # Invert the spectrogram
#' s_filt = invertSpectrogram(abs(spec_filt), samplingRate = 16000,
#'   windowLength = 25, overlap = 80, wn = 'hanning')
#' # NB: use the same settings as in "spec = spectrogram(s, ...)" above
#'
#' # Compare with the original
#' playme(s, 16000)
#' spectrogram(s, 16000, osc = TRUE)
#' playme(s_filt, 16000)
#' spectrogram(s_filt, 16000, osc = TRUE)
#'
# Check that the modulation spectrum is as desired
#' ms_new = modulationSpectrum(s_filt, samplingRate = 16000,
#'   windowLength = 25, overlap = 80, wn = 'hanning', maxDur = Inf,
#'   plot = TRUE, returnComplex = TRUE)$complex
#' image(x = as.numeric(colnames(ms_new)), y = as.numeric(rownames(ms_new)),
#'   z = t(log(abs(ms_new))))
#' plot(as.numeric(colnames(ms)), log(abs(ms[nrow(ms) / 2, ])), type = 'l')
#' points(as.numeric(colnames(ms_new)), log(ms_new[nrow(ms_new) / 2, ]), type = 'l',
#'   col = 'red', lty = 3)
#' # AM peaks at 25 Hz are removed, but inverting the spectrogram adds a lot of noise
#' }
filterSoundByMS = function(
    x,
    samplingRate = NULL,
    from = NULL,
    to = NULL,
    logSpec = FALSE,
    windowLength = 25,
    step = NULL,
    overlap = 80,
    wn = 'hamming',
    zp = 0,
    amCond = NULL,
    fmCond = NULL,
    jointCond = NULL,
    action = c('remove', 'preserve')[1],
    initialPhase = c('zero', 'random', 'spsi')[3],
    nIter = 50,
    reportEvery = NULL,
    cores = 1,
    play = FALSE,
    saveAudio = NULL,
    plot = TRUE,
    savePlots = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  ## Prepare a list of arguments to pass to .filterSoundByMS()
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'savePlots',
    'saveAudio', 'reportEvery', 'cores')]
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = '.filterSoundByMS',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots,
    saveAudio = saveAudio
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "filterByMS", width = paste0(width, units)))
  }

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Filter a single sound by MS
#'
#' Internal soundgen function.
#' @inheritParams filterSoundByMS
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.filterSoundByMS = function(
    audio,
    logSpec = FALSE,
    windowLength = 25,
    step = NULL,
    overlap = 80,
    wn = 'hamming',
    zp = 0,
    amCond = NULL,
    fmCond = NULL,
    jointCond = NULL,
    action = c('remove', 'preserve')[1],
    initialPhase = c('zero', 'random', 'spsi')[3],
    nIter = 50,
    play = FALSE,
    plot = TRUE,
    savePlots = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  # make sure windowLength_points and step_points are not fractions
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  step_points = round(step / 1000 * audio$samplingRate)
  step = step_points / audio$samplingRate * 1000
  windowLength_points = round(windowLength / 1000 * audio$samplingRate)
  windowLength = windowLength_points / audio$samplingRate * 1000
  overlap = 100 * (1 - step_points / windowLength_points)

  # Get a modulation spectrum
  ms = .modulationSpectrum(
    audio[c('sound', 'samplingRate', 'ls', 'duration')],  # avoid passing savePlots etc
    windowLength = windowLength,
    step = step, overlap = overlap, wn = wn,
    amRes = NULL,  # no roughness contour, the whole sound at once
    maxDur = Inf, logSpec = logSpec,
    power = NA, returnComplex = TRUE,
    plot = FALSE
  )$complex
  # image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)), z = t(log(abs(ms))))

  # Filter as needed
  ms_filt = filterMS(ms, amCond = amCond, fmCond = fmCond,
                     jointCond = jointCond,
                     action = action, plot = FALSE)

  # Convert back to a spectrogram
  spec_filt = abs(msToSpec(ms_filt, windowLength = windowLength, step = step))
  # image(x = as.numeric(colnames(spec_filt)), y = as.numeric(rownames(spec_filt)), z = t(log(abs(spec_filt))))

  # Invert the spectrogram
  s_new = invertSpectrogram(
    spec_filt, samplingRate = audio$samplingRate,
    windowLength = windowLength, wn = wn,
    overlap = overlap, step = step,
    specType = ifelse(logSpec, 'log', 'abs'),
    initialPhase = initialPhase,
    nIter = nIter,
    normalize = TRUE,
    play = FALSE,
    verbose = FALSE,
    plotError = FALSE
  )

  if (play) playme(s_new, audio$samplingRate)

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_filterByMS.png"),
        width = width, height = height, units = units, res = res)
  }

  # save audio
  if (is.character(audio$saveAudio)) {
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(s_new * audio$scale, audio = audio, filename = filename)
  }

  if (plot) {
    # Get an MS of the new sound
    audio_new = audio
    audio_new$sound = s_new
    ms_actual = .modulationSpectrum(
      audio_new[c('sound', 'samplingRate', 'ls', 'duration')],  # avoid passing savePlots etc
      windowLength = windowLength,
      step = step, overlap = overlap, wn = wn,
      amRes = NULL,  # no roughness contour, the whole sound at once
      maxDur = Inf, logSpec = logSpec,
      power = NA, returnComplex = TRUE,
      plot = FALSE
    )$complex

    # Calculate the error based on target vs reconstructed MS
    ms_nr = min(nrow(ms_filt), nrow(ms_actual))
    ms_nc = min(ncol(ms_filt), ncol(ms_actual))
    m1 = abs(ms_filt[1:ms_nr, 1:ms_nc])
    m2 = abs(ms_actual[1:ms_nr, 1:ms_nc])
    err = sum((m1 - m2)^2) / sum(m1^2) * 100

    par(mfrow = c(1, 3))
    image(x = as.numeric(colnames(ms)),
          y = as.numeric(rownames(ms)),
          z = t(log(abs(ms))),
          main = 'Original MS',
          xlab = '', ylab = 'FM, cycle/kHz')
    image(x = as.numeric(colnames(ms_filt)),
          y = as.numeric(rownames(ms_filt)),
          z = t(log(abs(ms_filt))),
          main = 'Filtered MS',
          xlab = 'AM, Hz', ylab = '')
    image(x = as.numeric(colnames(ms_actual)),
          y = as.numeric(rownames(ms_actual)),
          z = t(log(abs(ms_actual))),
          main = paste0('Achieved MS \nSquared error = ', round(err, 1), '%'),
          xlab = '', ylab = '')
    par(mfrow = c(1, 1))
    if (is.character(audio$saveAudio)) dev.off()
  }
  return(s_new)
}


#' Filter modulation spectrum
#'
#' Filters a modulation spectrum by removing a certain range of amplitude
#' modulation (AM) and frequency modulation (FM) frequencies. Conditions can be
#' specified either separately for AM and FM with \code{amCond = ..., fmCond =
#' ...}, implying an OR combination of conditions, or jointly on AM and FM with
#' \code{jointCond}. \code{jointCond} is more general, but using
#' \code{amCond/fmCond} is ~100 times faster.
#' @param ms a modulation spectrum as returned by
#'   \code{\link{modulationSpectrum}} - a matrix of real or complex values, AM
#'   in columns, FM in rows
#' @param amCond,fmCond character strings with valid conditions on amplitude and
#'   frequency modulation (see examples)
#' @param jointCond character string with a valid joint condition amplitude and
#'   frequency modulation
#' @param action should the defined AM-FM region be removed ('remove') or
#'   preserved, while everything else is removed ('preserve')?
#' @param plot if TRUE, plots the filtered modulation spectrum
#' @return Returns the filtered modulation spectrum - a matrix of the original
#'   dimensions, real or complex.
#' @export
#' @examples
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'                         returnComplex = TRUE)$complex
#' # Remove all AM over 25 Hz
#' ms_filt = filterMS(ms, amCond = 'abs(am) > 25')
#'
#' # amCond and fmCond are OR-conditions
#' filterMS(ms, amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5', action = 'remove')
#' filterMS(ms, amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5', action = 'preserve')
#' filterMS(ms, amCond = 'abs(am) > 10 & abs(am) < 25', action = 'remove')
#'
#' # jointCond is an AND-condition
#' filterMS(ms, jointCond = 'am * fm < 5', action = 'remove')
#' filterMS(ms, jointCond = 'am^2 + (fm*3)^2 < 200', action = 'preserve')
#'
#' # So:
#' filterMS(ms, jointCond = 'abs(am) > 5 | abs(fm) < 5')  # slow but general
#' # ...is the same as:
#' filterMS(ms, amCond = 'abs(am) > 5', fmCond = 'abs(fm) < 5')  # fast
filterMS = function(ms,
                    amCond = NULL,
                    fmCond = NULL,
                    jointCond = NULL,
                    action = c('remove', 'preserve')[1],
                    plot = TRUE) {
  nr = nrow(ms)
  nc = ncol(ms)
  am = as.numeric(colnames(ms))
  fm = as.numeric(rownames(ms))
  myf = function() {}  # otherwise R CMD check complains

  # Set up an empty filter matrix
  if (action == 'remove') {
    filter = matrix(1, nrow = nr, ncol = nc)
  } else if (action == 'preserve') {
    filter = matrix(0, nrow = nr, ncol = nc)
  }

  # Calculate the affected region
  if (is.character(jointCond)) {  # use only jointCond
    eval(parse(text = paste0('myf = function(am = NULL, fm = NULL) return(',
                             jointCond, ')')))
    affectedRegion = matrix(FALSE, nrow = nr, ncol = nc)
    for (i in 1:nr) {
      for (j in 1:nc) {
        affectedRegion[i, j] = myf(am = am[j], fm = fm[i])
      }
    }
    if (action == 'remove') {
      filter[which(affectedRegion == TRUE)] = 0
    } else if (action == 'preserve') {
      filter[which(affectedRegion == TRUE)] = 1
    }
  } else {  # use only separate conditions for am & fm
    if (is.character(amCond)) {
      am_cond = paste0('which(', amCond, ')')
      idx_col = try(eval(parse(text = am_cond)), silent = TRUE)
      if (inherits(idx_col, 'try-error')) {
        stop('amCond must be a valid expression to pass to which() - see examples')
      }
    } else {
      idx_col = logical(0)
    }

    if (is.character(fmCond)) {
      fm_cond = paste0('which(', fmCond, ')')
      idx_row = try(eval(parse(text = fm_cond)), silent = TRUE)
      if (inherits(idx_row, 'try-error')) {
        stop('fmCond must be a valid expression to pass to which() - see examples')
      }
    } else {
      idx_row = logical(0)
    }

    if (action == 'remove') {
      filter[idx_row, ] = 0
      filter[, idx_col] = 0
    } else if (action == 'preserve') {
      filter[idx_row, ] = 1
      filter[, idx_col] = 1
    }
  }

  # Multiply the original ms by the prepared filter
  out = ms * filter

  if (plot) {
    if(is.complex(out[1, 1])) {
      out_plot = abs(out)
    } else (
      out_plot = out
    )
    image(x = as.numeric(colnames(out_plot)),
          y = as.numeric(rownames(out_plot)),
          z = t(log(out_plot)))
  }
  invisible(out)
}

