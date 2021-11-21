#' Time stretch
#'
#' Dynamically time-stretches a sound without preserving its pitch or formants,
#' as if gradually changing playback speed. Algorithm: the audio is resampled at
#' time-varying steps. This is about 100 times faster than time-stretching with a
#' phase vocoder in \code{\link{shiftPitch}}, but pitch and formants cannot be
#' preserved, and large stretch factors may cause artifacts due to aliasing.
#'
#' @seealso \code{\link{shiftPitch}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @inheritParams soundgen
#' @param stretch 1 = no change, >1 = longer, <1 = shorter. Single value, vector,
#'   or anchor format (see \code{\link{soundgen}})
#' @param precision the number of points used for estimating the duration of
#'   output (more = better, but slower)
#' @export
#' @examples
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' # playme(sheep)
#' # spectrogram(sheep)
#' s1 = timeStretch(sheep, stretch = c(1, 3))
#' # playme(s1, sheep@samp.rate)
#' # spectrogram(s1, sheep@samp.rate)
#'
#' # compare to a similar effect achieved with a phase vocoder in pitchShift():
#' s2 = shiftPitch(
#'   sheep,
#'   timeStretch = c(1, 3),  # from 1 (original) to mult
#'   multPitch = c(1, 1/3),  # also drop pitch
#'   multFormants = c(1, 1/3)  # also drop formants (by the same proportion)
#' )
#' # playme(s2, sheep@samp.rate)
#' # spectrogram(s2, sheep@samp.rate)
#' # NB: because the two algorithms calculate transitions between stretch
#' # factors in different ways, the duration is not identical, even though the
#' # range of pitch change is the same
timeStretch = function(
  x,
  stretch = 1,
  samplingRate = NULL,
  precision = 1000,
  play = FALSE,
  saveAudio = NULL,
  reportEvery = NULL,
  cores = 1) {
  stretch = reformatAnchors(stretch)

  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    funToCall = '.timeStretch',
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
#'
#' Internal soundgen function called by \code{\link{timeStretch}}
#' @inheritParams timeStretch
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.timeStretch = function(
  audio,
  stretch,
  precision,
  normalize = TRUE,
  play = FALSE) {
  if (!any(stretch$value != 1)) {
    message('Nothing to do')
    return(audio$sound)
  }

  hop = getSmoothContour(stretch,
                         len = precision,
                         interpol = 'approx')
  new_len = round(sum(hop) / precision * audio$ls)
  hop_long = getSmoothContour(stretch,
                              len = new_len,
                              interpol = 'approx')
  time = cumsum(1 / hop_long) + 1
  if (FALSE) {
    # another method - a different trajectory
    stretch$value = 1 / stretch$value
    speed = getSmoothContour(stretch,
                             len = precision,
                             interpol = 'approx')
    new_len = round(precision / sum(speed) * audio$ls)
    speed_long = getSmoothContour(stretch,
                                  len = new_len,
                                  interpol = 'approx')
    time = cumsum(speed_long) + 1  # w/o +1 the first indices can be rounded to 0
  }

  # might be a bit too long if precision is low - remove extra points
  time = time[time < audio$ls]
  out = spline(x = audio$sound,
               xout = time,
               n = new_len)$y
  # playme(out, audio$samplingRate)
  # spectrogram(out, audio$samplingRate)
  return(out)
}
