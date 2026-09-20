#' Prosody
#'
#' Exaggerates or flattens the intonation by performing a dynamic pitch shift,
#' changing pitch excursion from its original median value without changing the
#' formants. This is a particular case of pitch shifting, which is performed
#' with \code{\link{shiftPitch}}. The result is likely to be improved if
#' manually corrected pitch contours are provided. Depending on the nature of
#' audio, the settings that control pitch shifting may also need to be
#' fine-tuned with the \code{shiftPitch_pars} argument. Any NAs in pitch contour
#' are treated as voiceless fragments, and their pitch is not modified. If the
#' NAs are not actual silences / voiceless frames, they should be interpolated
#' before passing the pitch contour to prosody().
#'
#' @seealso \code{\link{shiftPitch}}
#'
#' @inheritParams .roxygen_defaults
#' @param multProsody multiplier of pitch excursion from median (on a
#'   logarithmic or musical scale): >1 = exaggerate intonation, 1 = no change, <1
#'   = flatten, 0 = completely flat at the original median pitch; numeric vector
#'   or anchor format \code{list(time = ..., value = ...)}
#' @param analyze_pars a list of parameters to pass to \code{\link{analyze}}
#'   (only needed if \code{pitchManual} is NULL - that is, if we attempt to
#'   track pitch automatically)
#' @param shiftPitch_pars a list of parameters to pass to
#'   \code{\link{shiftPitch}} to fine-tune the pitch-shifting algorithm
#' @param pitchManual manually corrected pitch contour. For a single sound,
#'   provide a numeric vector of any length. For multiple sounds, provide a
#'   dataframe with columns "file" and "pitch" (or path to a csv file) as
#'   returned by \code{\link{pitch_app}}, ideally with the same windowLength and
#'   step as in current call to analyze. A named list with pitch vectors per
#'   file is also accepted - e.g., as returned by \code{\link{pitch_app}}
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
#' data('speechEx', package = 'soundgen')
#' samplingRate = speechEx@samp.rate
#' spectrogram(speechEx, yScale = 'log', ylim = c(.05, 4))
#' # playme(speechEx)
#'
#' # start with exaggerated prosody, then flat towards the end
#' speech1 = prosody(speechEx,
#'   multProsody = list(time = c(0, 1), value = c(1.5, 0.1)),
#'   analyze_pars = list(windowLength = 40, step = 10,
#'   pitchMethods = c('dom', 'autocor', 'cep', 'spec')),
#'   shiftPitch_pars = list(freqWindow = 400))
#' spectrogram(speech1, samplingRate, yScale = 'log')
#' # playme(speech1, samplingRate)
#'
#' # process all audio files in a folder
#' s4 = prosody('~/Downloads/temp', multProsody = 2,
#'   savePlots = TRUE, saveAudio = TRUE)
#' str(s4)  # returns a list with audio (+ saves it to disk)
#' }
prosody = function(
    x,
    samplingRate = NULL,
    multProsody,
    analyze_pars = list(),
    shiftPitch_pars = list(),
    pitchManual = NULL,
    reportEvery = NULL,
    cores = 1,
    play = FALSE,
    saveAudio = FALSE,
    plot = FALSE,
    savePlots = FALSE,
    embed = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA
) {
  # match args
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio',
    'savePlots', 'embed', 'pitchManual')]

  # reformat pitchManual, if any
  if (!is.null(pitchManual))
    myPars$pitchManual_list = formatPitchManual(pitchManual)

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.prosody',
                    suffix = 'prosody',
                    savePlots = savePlots,
                    saveAudio = saveAudio,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units),
                  changesAudio = saveAudio, embed = embed))

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Prosody per sound
#' @noRd
.prosody = function(audio,
                    multProsody,
                    analyze_pars = list(),
                    shiftPitch_pars = list(),
                    pitchManual_list = NULL,
                    play = FALSE,
                    plot = FALSE,
                    width = 900,
                    height = 500,
                    units = 'px',
                    res = NA) {
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
  pitch_new = pitch  # for plotting with prosody = 1
  multProsody = reformatAnchors(multProsody)
  if(all(is.na(pitch)) || all(multProsody$value == 1)) {
    out = audio$sound
  } else {
    if (any(multProsody$value != 1))
      multProsody = approx(multProsody$time, multProsody$value, n = length(pitch))$y

    # calculate the new pitch contour based on distance from median pitch
    pitch_sem = HzToSemitones(pitch)
    median_pitch = median(pitch_sem, na.rm = TRUE)
    pitch_new_sem = (pitch_sem - median_pitch) * multProsody + median_pitch
    pitch_new = semitonesToHz(pitch_new_sem)
    multPitch = pitch_new / pitch
    # NA means mult = 1 (don't change voiceless frames)
    multPitch[!is.finite(multPitch) | multPitch <= 0] = 1

    # perform pitch shifting
    out = do.call(.shiftPitch, c(shiftPitch_pars, list(
      audio = audio[!names(audio) %in% c('plot', 'savePlots', 'saveAudio')],
      multPitch = reformatAnchors(multPitch),
      multFormants = reformatAnchors(1),
      timeStretch = reformatAnchors(1))))
  }

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    plot(pitch, ylim = range(c(pitch, pitch_new), na.rm = TRUE), type = 'l')
    points(pitch_new, col = 'blue', type = 'l')
  }

  # Play, save, return
  if (isTRUE(play)) {
    playme(out, samplingRate = audio$samplingRate)
  } else if (is.character(play)) {
    playme(out, samplingRate = audio$samplingRate, player = play)
  }
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(out, audio = audio, filename = filename)
  }
  out
}
