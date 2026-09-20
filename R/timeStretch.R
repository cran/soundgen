#' Time stretch
#'
#' Dynamically time-stretches a sound without preserving its pitch or formants,
#' as if gradually changing playback speed. Algorithm: the audio is resampled at
#' time-varying steps. This is about 100 times faster than time-stretching with
#' a phase vocoder in \code{\link{shiftPitch}}, but pitch and formants cannot be
#' preserved, and compressing the sound (stretch < 1) may cause artifacts due to
#' aliasing, since no anti-aliasing filter is applied (low-pass filter the input
#' and/or output if needed). The \code{stretch} contour is interpreted over the
#' output (stretched) duration, which is why the trajectory differs from
#' \code{\link{shiftPitch}} with the same anchors.
#'
#' @seealso \code{\link{shiftPitch}}
#'
#' @inheritParams .roxygen_defaults
#' @param stretch 1 = no change, >1 = longer, <1 = shorter. Single value, vector,
#'   or anchor format (see \code{\link{soundgen}}). Must be positive
#' @param interpol interpolation method supported by \code{\link{interpolate}}
#' @param precision the number of points used for estimating the duration of
#'   output (more = better, but slower)
#'
#' @return The processed waveform as a numeric vector for a single input or a
#'   list for multiple outputs. The duration is approximate: the output may end
#'   up a few samples shorter than intended if the resampling trajectory reaches
#'   the end of the input.
#' @export
#' @examples
#' data(speechEx, package = 'soundgen')  # import a recording
#' # playme(speechEx)
#' # spectrogram(speechEx)
#' s1 = timeStretch(speechEx, stretch = c(1, 3))
#' # playme(s1, speechEx@samp.rate)
#' # spectrogram(s1, speechEx@samp.rate)
#'
#' # compare to a similar effect achieved with a phase vocoder in shiftPitch():
#' s2 = shiftPitch(
#'   speechEx,
#'   timeStretch = c(1, 3),  # from 1 (original) to mult
#'   multPitch = c(1, 1/3),  # also drop pitch
#'   multFormants = c(1, 1/3)  # also drop formants (by the same proportion)
#' )
#' # playme(s2, speechEx@samp.rate)
#' # spectrogram(s2, speechEx@samp.rate)
#' # NB: because the two algorithms calculate transitions between stretch
#' # factors in different ways, the duration is not identical, even though the
#' # range of pitch change is the same
timeStretch = function(
    x,
    stretch = 1,
    samplingRate = NULL,
    interpol = 'splineFC',
    precision = 1000,
    play = FALSE,
    saveAudio = FALSE,
    reportEvery = NULL,
    cores = 1) {
  stretch = reformatAnchors(stretch)
  if (any(!is.finite(stretch$value)) || any(stretch$value <= 0))
    stop('stretch must be positive')
  if (length(precision) != 1 || !is.finite(precision) || precision < 2)
    stop('precision must be a single finite number >= 2')

  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.timeStretch',
                    suffix = 'timeStretch',
                    saveAudio = saveAudio,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Time stretch per sound
#' @noRd
.timeStretch = function(
    audio,
    stretch,
    precision,
    interpol = 'splineFC',
    play = FALSE) {
  if (!any(stretch$value != 1)) {
    message('Nothing to do')
    out = audio$sound
  } else {
    hop = getSmoothContour(stretch,
                           len = precision,
                           interpol = 'approx')
    new_len = max(1L, round(audio$ls * precision / sum(1 / hop)))  # harmonic mean
    hop_long = getSmoothContour(stretch,
                                len = new_len,
                                interpol = 'approx')
    # resampling trajectory in input coordinates: start at the first sample,
    # increment by 1 / stretch (input samples per output sample)
    time = c(0, cumsum(1 / hop_long)[-new_len]) + 1
    # might be a bit too long if precision is low - remove extra points
    time = time[time <= audio$ls]
    out = interpolate(1:audio$ls, audio$sound, xout = time, method = interpol)
  }

  # playme(out, audio$samplingRate)
  # spectrogram(out, audio$samplingRate)

  # play
  if (isTRUE(play)) {
    playme(out, samplingRate = audio$samplingRate)
  } else if (is.character(play)) {
    playme(out, samplingRate = audio$samplingRate, player = play)
  }

  # save audio
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(out, audio = audio, filename = filename)
  }

  out
}
