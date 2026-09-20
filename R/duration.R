#' Get duration
#'
#' Returns the duration of one or more audio files (mostly useful for running on
#' an entire folder). If \code{threshold} is set, it also removes the leading
#' and trailing silences or near-silences, thus returning the duration of
#' relatively loud central fragments of each sound. Silences are located based
#' on the amplitude of root mean square (RMS) amplitude with
#' \code{\link{getRMS}}. Note that the threshold is set relative to the observed
#' maximum RMS, just as in \code{\link{analyze}}. This means that even very
#' quiet sounds are not treated as pure silence.
#'
#' @seealso \code{\link{analyze}} \code{\link{getLoudness}}
#'
#' @inheritParams .roxygen_defaults
#' @param silence leading and trailing sections quieter than this proportion of
#'   maximum RMS amplitude are removed when calculating
#'   \code{duration_noSilence} (NULL = don't calculate \code{duration_noSilence}
#'   to save time)
#' @param rms_pars a list of control parameters passed to \code{\link{getRMS}}
#'
#' @return For a single file, a one-row data frame with columns $duration and
#'   $duration_noSilence. For multiple files, a data frame with one row per
#'   file, including a $file column. If \code{silence = NULL},
#'   \code{duration_noSilence} (duration without leading and trailing silence)
#'   is NA. If no RMS frame exceeds the threshold, \code{duration_noSilence} is
#'   0.
#'
#' @export
#' @examples
#' s = c(rep(0, 550), runif(400, -1, 1), rep(0, 50))
#' osc(s, samplingRate = 1000)
#' # true duration_noSilence is 400 ms
#' getDuration(s, samplingRate = 1000, silence = .01)
#' getDuration(s, samplingRate = 1000, silence = .1,
#'             rms_pars = list(windowLength = 5, step = 1))
#'
#' \dontrun{
#' d = getDuration('~/Downloads/temp')
#' d
#' hist(d$duration - d$duration_noSilence)
#' }
getDuration = function(
    x,
    samplingRate = NULL,
    silence = .01,
    rms_pars = list(windowLength = 20, step = 5),
    reportEvery = NULL,
    cores = 1
) {
  # match args
  myPars = list(silence = silence)
  myPars$rms_pars = rms_pars
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.getDuration',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # prepare output
  if (pa$input$n > 1) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
      if (!pa$input$failed[i]) {
        temp[[i]] = pa$result[[i]]
      }
    }
    idx_failed = which(pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]] [1, ]
        filler[1, ] = NA
      } else {
        stop('Failed to analyze any input')
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    out = cbind(data.frame(file = pa$input$filenames_base),
                do.call('rbind', temp))
  } else {
    out = pa$result[[1]]
  }
  return(out)
}


#' Get duration per sound
#'
#' Internal soundgen function called by \code{\link{getDuration}}.
#' @param audio a list returned by \code{readAudio}
#' @noRd
.getDuration = function(
    audio,
    silence = .01,
    rms_pars = list(windowLength = 20, step = 5)
) {
  if (!is.null(silence) && is.finite(silence)) {
    ampl = do.call(.getRMS, c(list(audio = audio, plot = FALSE), rms_pars))
    ampl = ampl / max(ampl)
    idx_loud = which(ampl > silence)
    time_span = names(ampl)[idx_loud[c(1, length(idx_loud))]]
    duration_noSilence = diff(as.numeric(time_span)) / 1000  # ms to s
    if (length(duration_noSilence) < 1) duration_noSilence = 0
  } else {
    duration_noSilence = NA
  }
  data.frame(duration = audio$duration,
             duration_noSilence = duration_noSilence)
}
