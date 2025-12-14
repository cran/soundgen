#' Prosody
#'
#' Exaggerates or flattens the intonation by performing a dynamic pitch shift,
#' changing pitch excursion from its original median value without changing the
#' formants. This is a particular case of pitch shifting, which is performed
#' with \code{\link{shiftPitch}}. The result is likely to be improved if
#' manually corrected pitch contours are provided. Depending on the nature of
#' audio, the settings that control pitch shifting may also need to be
#' fine-tuned with the \code{shiftPitch_pars} argument.
#'
#' @seealso \code{\link{shiftPitch}}
#'
#' @inheritParams spectrogram
#' @inheritParams addAM
#' @inheritParams analyze
#' @param multProsody multiplier of pitch excursion from median (on a
#'   logarithmic or musical scale): >1 = exaggerate intonation, 1 = no change, <1
#'   = flatten, 0 = completely flat at the original median pitch
#' @param analyze_pars a list of parameters to pass to \code{\link{analyze}}
#'   (only needed if \code{pitchManual} is NULL - that is, if we attempt to
#'   track pitch automatically)
#' @param shiftPitch_pars a list of parameters to pass to
#'   \code{\link{shiftPitch}} to fine-tune the pitch-shifting algorithm
#'
#' @return If the input is a single audio (file, Wave, or numeric vector),
#'   returns the processed waveform as a numeric vector with the original
#'   sampling rate and scale. If the input is a folder with several audio files,
#'   returns a list of processed waveforms, one for each file.
#' @export
#' @examples
#' s = soundgen(sylLen = 200, pitch = c(150, 220), addSilence = 50,
#'              plot = TRUE, yScale = 'log')
#' # playme(s)
#' s1 = prosody(s, 16000, multProsody = 2,
#'   analyze_pars = list(windowLength = 30, step = 15),
#'   shiftPitch_pars = list(windowLength = 20, step = 5, freqWindow = 300),
#'   plot = TRUE)
#' # playme(s1)
#' # spectrogram(s1, 16000, yScale = 'log')
#'
#' \dontrun{
#' # Flat intonation - remove all frequency modulation
#' s2 = prosody(s, 16000, multProsody = 0,
#'   analyze_pars = list(windowLength = 30, step = 15),
#'   shiftPitch_pars = list(windowLength = 20, step = 1, freqWindow = 500),
#'   plot = TRUE)
#' playme(s2)
#' spectrogram(s2, 16000, yScale = 'log')
#'
#' # Download an example - a bit of speech (sampled at 16000 Hz)
#' download.file('http://cogsci.se/soundgen/audio/speechEx.wav',
#'               destfile = '~/Downloads/temp1/speechEx.wav')
#' target = '~/Downloads/temp1/speechEx.wav'
#' samplingRate = tuneR::readWave(target)@samp.rate
#' spectrogram(target, yScale = 'log')
#' playme(target)
#'
#' s3 = prosody(target, multProsody = 1.5,
#'   analyze_pars = list(windowLength = 30, step = 15),
#'   shiftPitch_pars = list(freqWindow = 400, propagation = 'adaptive'))
#' spectrogram(s3, tuneR::readWave(target)@samp.rate, yScale = 'log')
#' playme(s3)
#'
#' # process all audio files in a folder
#' s4 = prosody('~/Downloads/temp', multProsody = 2, savePlots = '',
#'              saveAudio = '~/Downloads/temp/prosody')
#' str(s4)  # returns a list with audio (+ saves it to disk)
#' }
prosody = function(
  x,
  samplingRate = NULL,
  multProsody,
  analyze_pars = list(),
  shiftPitch_pars = list(),
  pitchManual = NULL,
  play = FALSE,
  saveAudio = NULL,
  reportEvery = NULL,
  cores = 1,
  plot = FALSE,
  savePlots = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio', 'pitchManual',
    'analyze_pars', 'shiftPitch_pars')]  # otherwise flattens lists
  myPars$analyze_pars = analyze_pars
  myPars$shiftPitch_pars = shiftPitch_pars

  # reformat pitchManual, if any
  if (!is.null(pitchManual))
    myPars$pitchManual_list = formatPitchManual(pitchManual)

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    savePlots = savePlots,
                    saveAudio = saveAudio,
                    funToCall = '.prosody',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "prosody", width = paste0(width, units)))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Prosody per sound
#'
#' Internal soundgen function called by \code{\link{prosody}}
#' @inheritParams prosody
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.prosody = function(audio,
                    multProsody,
                    analyze_pars = list(),
                    shiftPitch_pars = list(),
                    pitchManual_list = NULL,
                    play = FALSE,
                    plot = FALSE,
                    savePlots = NULL,
                    width = 900,
                    height = 500,
                    units = 'px',
                    res = NA,
                    ...) {
  # first, we need the original pitch contour
  if (is.null(pitchManual_list)) {
    an = do.call(analyze, c(analyze_pars, list(
      x = audio$sound,
      samplingRate = audio$samplingRate,
      nFormants = 0,
      loudness = NULL,
      roughness = NULL,
      novelty = NULL)))
    pitch = an$detailed$pitch
  } else {
    if (length(pitchManual_list) == 1) {
      pitch = pitchManual_list[[1]]
    } else {
      pitch = pitchManual_list[[audio$filename_base]]
    }
  }
  if(!any(!is.na(pitch)) | !any(multProsody != 1))
    return(audio$sound)
  if (length(multProsody) > 1)
    multProsody = approx(multProsody, n = length(pitch))$y

  # calculate the new pitch contour based on distance from median pitch
  pitch_sem = HzToSemitones(pitch)
  median_pitch = median(pitch_sem, na.rm = TRUE)
  pitch_new_sem = (pitch_sem - median_pitch) * multProsody + median_pitch
  pitch_new = semitonesToHz(pitch_new_sem)

  # convert the difference between old and new pitch contour to mult
  multPitch = pitch_new / pitch
  # NA means mult = 1 (don't change voiceless frames)
  multPitch[is.na(multPitch)] = 1

  # perform pitch shifting
  out = do.call(.shiftPitch, c(shiftPitch_pars, list(
    audio = audio[!names(audio) %in% c('plot', 'savePlot', 'saveAudio')],
    multPitch = reformatAnchors(multPitch),
    multFormants = reformatAnchors(1),
    timeStretch = reformatAnchors(1))))

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_prosody.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    plot(pitch, ylim = range(c(pitch, pitch_new), na.rm = TRUE), type = 'l')
    points(pitch_new, col = 'blue', type = 'l')

    # spectrogram(out, audio$samplingRate,
    #   extraContour = list(x = pitch, col = 'blue', type = 'l', lty = 1), ...)

    if (is.character(audio$savePlots)) dev.off()
  }

  # Play, save, return
  if (play == TRUE) {
    playme(out, samplingRate = audio$samplingRate)
  }
  if (is.character(play)) {
    playme(out, samplingRate = audio$samplingRate, player = play)
  }
  if (is.character(audio$saveAudio)) {
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(out, audio = audio, filename = filename)
  }
  out
}
