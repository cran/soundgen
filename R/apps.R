# Note: might wrap shiny::runApp in suppress.warnings()

#' Interactive sound synthesizer
#'
#' Starts a shiny app that provides an interactive wrapper to
#' \code{\link{soundgen}}. Note that the browser has to be able to playback WAV
#' audio files; otherwise, there will be no sound.
#'
#' @seealso \code{\link{soundgen}}
#'
#' @return Does not return anything.
#' @export
#' @examples
#' \dontrun{
#' soundgen_app()  # opens the app in your default browser
#' }
soundgen_app = function() {
  # check if shiny-related packages are available
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("shinyjs", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("base64enc", quietly = TRUE))
    stop('To run apps, please install missing dependencies: ',
         '`install.packages(c("shiny", "shinyjs", "bslib", "base64enc"))`')
  appDir = system.file("shiny", "soundgen_main", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' Interactive pitch tracker
#'
#' Starts a shiny app for manually editing pitch contours. The settings in the
#' panels on the left correspond to arguments to \code{\link{analyze}} - see
#' \code{\link{analyze}} and https://cogsci.se/soundgen/acoustic_analysis.html
#' for help and examples. You can verify the pitch contours first, and then feed
#' them back into \code{analyze} (see examples). Use hotkeys (eg spacebar to
#' play/stop) and avoid working with very large files.
#'
#' @param ... presets like \code{windowLength = 25, pitchMethods = c('autocor',
#'   'cep')}. Full list: dynamicRange, zp, nCands, minVoicedCands,
#'   domThres, domSmooth, autocorThres, autocorSmooth, autocorUpsample,
#'   autocorBestPeak, cepThres, cepZp, specThres, specPeak, specRatios,
#'   specHNRslope, specSmooth, specMerge, specSinglePeakCert, hpsThres, hpsNum,
#'   hpsNorm, hpsPenalty, zcThres, zcWin, certWeight, smooth, interpolCert,
#'   spec_maxPoints, specContrast, specBrightness, blur_freq, blur_time,
#'   reass_cex, osc_maxPoints, windowLength, step, silence, pitchFloor,
#'   pitchCeiling, priorMean, priorSD, shortestSyl, shortestPause, interpolWin,
#'   interpolTol, spec_cex, nColors, reass_windowLength, reass_step,
#'   pitchMethods, summaryFun, summaryFun_text, spec_ylim,
#'   spec_colorTheme, osc, wn
#'
#' @return A list with the last used settings ($settings) plus the output of
#'   analyze() for each file from the last file queue with two additional
#'   columns: "time" and "pitch". NB: only the results of the most recent file
#'   queue are returned, so don't press "Load audio" repeatedly if you need the
#'   output returned to R (the csv file with results should still be saved
#'   correctly). When proceeding to the next file in the queue, the app saves to
#'   disk a backup .csv file with one row per audio file. When the orange
#'   "Download results" button is clicked, a context menu pops up offering to
#'   terminate the app - if that happens, the results are also returned directly
#'   into R. To process pitch contours further in R, work directly with
#'   \code{my_pitch[[myfile]]$detailed$time} and
#'   \code{my_pitch[[myfile]]$detailed$pitch} or, if loading the csv file, do
#'   something like:
#'
#' \preformatted{
#' a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
#' pitch = as.numeric(unlist(strsplit(a$pitch, ',')))
#' mean(pitch, na.rm = TRUE); sd(pitch, na.rm = TRUE)
#' }
#'
#' \bold{Suggested workflow}
#'
#' Start by setting the basic analysis settings such as pitchFloor,
#' pitchCeiling, silence, etc. Then click "Load audio" to upload one or several
#' audio files (wav/mp3). Long files will be very slow, so please cut your audio
#' into manageable chunks (ideally <10 s). If Shiny complains that maximum
#' upload size is exceeded, you can increase it, say to 30 MB, with
#' \code{options(shiny.maxRequestSize = 30 * 1024^2)}. Once the audio has been
#' uploaded to the browser, fine-tune the analysis settings as needed, edit the
#' pitch contour in the first file to your satisfaction, then click "Next" to
#' proceed to the next file, etc. Remember that setting a reasonable prior is
#' often faster than adjusting the contour one anchor at a time. When done,
#' click "Save results". If working with many files, you might want to save the
#' results occasionally in case the app crashes (although you should still be
#' able to recover your data if it does - see below).
#'
#' \bold{How to edit pitch contours}
#'
#' Left-click to add a new anchor, double-click to remove it or unvoice the
#' frame. Each time you make a change, the entire pitch contour is re-fit, so
#' making a change in one frame can affect the path through candidates in
#' adjacent frames. You can control this behavior by changing the settings in
#' Out/Path and Out/Smoothing. If correctly configured, the app corrects the
#' contour with only a few manual values - you shouldn't need to manually edit
#' every single frame. For longer files, you can zoom in/out and navigate within
#' the file. You can also select a region to voice/unvoice or shift it as a
#' whole or to set a prior based on selected frequency range.
#'
#' \bold{Recovering lost data}
#'
#' Every time you click "next" or "last" to move in between files in the queue,
#' the output you've got so far is saved in a temporary backup file. If the app
#' crashes or is closed without saving the results, this backup file preserves
#' your data. To recover it, restart pitch_app() - a dialog box will pop up and
#' ask whether you want to append the old data to the new one. Even so, save
#' your data regularly to be on the safe side!
#'
#' @seealso \code{\link{formant_app}} \code{\link{annotation_app}}
#'
#' @export
#' @examples
#' \dontrun{
#' # Recommended workflow for analyzing a lot of short audio files
#' path_to_audio = '~/Downloads/temp'  # our audio lives here
#'
#' # STEP 1: extract manually corrected pitch contours
#' my_pitch = pitch_app()  # runs in default browser such as Firefox or Chrome
#' # To change system default browser, run something like:
#' options('browser' = '/usr/bin/firefox')  # path to the executable on Linux
#'
#' # You can pass presets with your preferred parameter values:
#' my_pitch = pitch_app(windowLength = 20, step = 10,
#'   pitchMethods = c('dom', 'autocor', 'cep'), spec_ylim = c(0, 6))
#'
#' # Object "my_pitch" contains the output, notably the time-pitch matrix
#' plot(my_pitch[[1]]$detailed$time, my_pitch[[1]]$detailed$pitch, type = 'b',
#'   xlab = 'Time, ms', ylab = 'Pitch, Hz')
#'
#' # Run the app with previously used settings
#' my_pitch2 = do.call(pitch_app, my_pitch$settings)
#'
#' # save the complete output, including the settings used
#' saveRDS(my_pitch2, 'my_pitch_analysis.rds')
#'
#' # STEP 2: run analyze() with manually corrected pitch contours to obtain
#' # accurate descriptives like the proportion of energy in harmonics above f0,
#' # etc. This also gives you formants and loudness estimates (disabled in
#' # pitch_app to speed things up)
#' df2 = analyze(
#'   path_to_audio,
#'   pitchMethods = 'autocor',  # needed for calculating HNR
#'   nFormants = 5,        # now we can measure formants as well
#'   pitchManual = my_pitch
#'   # or, if loading the output of pitch_app() from the disk:
#'   # pitchManual = '~/Downloads/output.csv'
#'   # pitchManual = '~/path_to_some_folder/my_pitch_contours.rds
#' )
#'
#' # STEP 3: add other acoustic descriptors, for ex.
#' df3 = segment(path_to_audio)
#'
#' # STEP 4: merge df2, df3, df4, ... in R or a spreadsheet editor to have all
#' # acoustic descriptives together
#'
#' # To verify your pitch contours and/or edit them later, copy output.csv to
#' # the folder with your audio, run pitch_app(), and load the audio + csv
#' # together. The saved pitch contours are treated as manual anchors
#' }
pitch_app = function(...) {
  # check if shiny-related packages are available
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("shinyjs", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("base64enc", quietly = TRUE))
    stop('To run apps, please install missing dependencies: ',
         '`install.packages(c("shiny", "shinyjs", "bslib", "base64enc"))`')

  # load defaults
  pitch_app_defaults = as.list(defaults_analyze[, 'default'])
  names(pitch_app_defaults) = rownames(defaults_analyze)
  pitch_app_defaults$spec_ylim = c(0, pitch_app_defaults$spec_ylim)
  pitch_app_defaults = c(pitch_app_defaults, list(
    'pitchMethods' = c('dom', 'autocor'),
    'summaryFun' = c('mean', 'sd'),
    'summaryFun_text' = '',
    'pathfinding' = 'fast',
    'spec_colorTheme' = 'bw',
    'osc' = 'linear',
    'wn' = 'gaussian'
  ))

  # use user-supplied presets, if any, to update the inputs
  pitch_app_defaults = modifyList(pitch_app_defaults, list(...))

  # Store defaults in the shared environment under the 'pitch' list
  .soundgen_env$pitch$def = pitch_app_defaults
  .soundgen_env$pitch$def_pitch = defaults_analyze

  appDir = system.file("shiny", "pitch_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' Interactive formant tracker
#'
#' Starts a shiny app for manually correcting formant measurements. For more
#' tips, see \code{\link{pitch_app}} and http://cogsci.se/soundgen.html.
#'
#' Suggested workflow: load one or several audio files (wav/mp3), preferably not
#' longer than a minute or so. Select a region of interest in the spectrogram -
#' for example, a sustained vowel with clear and relatively steady formants.
#' Double-click within the selection to create a new annotation (you may add a
#' text label if needed). If you are satisfied with the automatically calculated
#' formant frequencies, proceed to the next region of interest. If not, there
#' are three ways to adjust them: (1) click the spectrogram within selection
#' (pick the formant number to adjust by clicking the formant boxes); (2)
#' single-click the spectrum to use the cursor's position, or (3) double-click
#' the spectrum to use the nearest spectral peak. When done with a file, move on
#' to the next one in the queue. Use the orange button to download the results.
#' To continue work, upload the output file from the previous session together
#' with the audio files (you can rename it, but keep the .csv extension). Use
#' hotkeys (eg spacebar to play/stop) and avoid working with very large files.
#'
#' \bold{Recovering lost data}
#'
#' Every time you add an annotation or move in between files in the queue, the
#' output you've got so far is saved in a temporary backup file. If the app
#' crashes or is closed without saving the results, this backup file preserves
#' your data. To recover it, restart formant_app() - a dialog box will pop up
#' and ask whether you want to append the old data to the new one. Even so, save
#' your data regularly to be on the safe side!
#'
#' @seealso \code{\link{pitch_app}} \code{\link{annotation_app}}
#'
#' @param ... presets like \code{windowLength = 25}. Full list:
#'   samplingRate_mult, nFormants, minformant, maxbw, dynamicRange_lpc, zp_lpc,
#'   spec_ylim, dynamicRange, specContrast, specBrightness, blur_freq,
#'   blur_time, reass_cex, zp, spec_maxPoints, osc_maxPoints, spectrum_smooth,
#'   spectrum_xlim, spectrum_len, silence, windowLength_lpc, step_lpc,
#'   windowLength, step, spec_colorTheme, osc, wn, wn_lpc, vtl_method,
#'   speedSound, coeffs, interceptZero, tube, nColors, spec_cex, pitch,
#'   summaryFun, audioMethod, specType, fmtSpacePlot, spec_col, normalizeInput,
#'   adaptivePitch, spectrum_plotSynth
#'
#' @return A list of the last used settings ($settings) plus a data.frame with
#'   the formant measurements. Every time a new annotation is added, the app
#'   creates a backup csv file in the session's temporary directory, and it
#'   returns the final payload upon closing the app.
#'
#' @export
#' @examples
#' \dontrun{
#' f = formant_app()  # runs in default browser such as Firefox or Chrome
#'
#' f1 = formant_app(specType = 'reassigned', windowLength = 5, step = 1)
#'
#' # run the app with previously used settings
#' f2 = do.call(formant_app, f1$settings)
#'
#' # save the complete output, including the settings used
#' saveRDS(f2, 'my_formant_analysis.rds')
#'
#' # To change system default browser, run something like:
#' options('browser' = '/usr/bin/firefox')  # path to the executable on Linux
#' }
formant_app = function(...) {
  # check if shiny-related packages are available
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("shinyjs", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("base64enc", quietly = TRUE))
    stop('To run apps, please install missing dependencies: ',
         '`install.packages(c("shiny", "shinyjs", "bslib", "base64enc"))`')

  # load numeric defaults from matrix
  formant_app_defaults = as.list(def_form[, 'default'])
  names(formant_app_defaults) = rownames(def_form)
  formant_app_defaults$spectrum_xlim = c(0, formant_app_defaults$spectrum_xlim)
  formant_app_defaults$spec_ylim = c(0, formant_app_defaults$spec_ylim)

  # add non-numeric and logical defaults
  formant_app_defaults = c(formant_app_defaults, list(
    'wn' = 'gaussian',
    'wn_lpc' = 'gaussian',
    'spec_colorTheme' = 'bw',
    'osc' = 'linear',
    'vtl_method' = 'regression',
    'speedSound' = 35400,
    'coeffs' = '',
    'interceptZero' = TRUE,
    'tube' = 'closed-open',
    'summaryFun' = 'median',
    'audioMethod' = 'Browser',
    'specType' = 'spectrum',
    'fmtSpacePlot' = 'vowelSpace',
    'spec_col' = '#FF0000FF',
    'normalizeInput' = TRUE,
    'adaptivePitch' = TRUE,
    'spectrum_plotSynth' = TRUE
  ))

  # use user-supplied presets, if any, to update the inputs
  formant_app_defaults = modifyList(formant_app_defaults, list(...))

  # Store defaults in the shared environment under the 'formant' list
  .soundgen_env$formant$def = formant_app_defaults
  .soundgen_env$formant$def_form = def_form

  appDir = system.file("shiny", "formant_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }

  # runApp implicitly returns whatever stopApp() passes to it
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' Annotation app
#'
#' Starts a shiny app for annotating audio. This is a simplified and faster
#' version of \code{\link{formant_app}} intended only for making annotations.
#' Use hotkeys (eg spacebar to play/stop) and avoid working with very large
#' files.
#'
#' \bold{Recovering lost data}
#'
#' Every time you add an annotation or move in between files in the queue, the
#' output you've got so far is saved in a temporary backup file. If the app
#' crashes or is closed without saving the results, this backup file preserves
#' your data. To recover it, restart annotation_app() - a dialog box will pop up
#' and ask whether you want to append the old data to the new one. Even so, save
#' your data regularly to be on the safe side!
#'
#' @param ... presets like \code{windowLength = 25}. Full list: dynamicRange,
#'   specContrast, specBrightness, blur_freq, blur_time, reass_cex, nColors, zp,
#'   spec_maxPoints, osc_maxPoints, windowLength, step, spec_xlim, spec_ylim,
#'   specType, spec_colorTheme, spec_yScale, osc, wn
#'
#' @return A list with two elements: \code{$settings} (a list of the last used
#'   settings) and \code{$annotations} (a data.frame with the annotations).
#'   Every time a new annotation is added, the app creates a backup csv file in
#'   the session's temporary directory, and it returns the final payload upon
#'   closing the app.
#'
#' @seealso \code{\link{formant_app}}
#'
#' @export
#' @examples
#' \dontrun{
#' ann = annotation_app()  # runs in default browser such as Firefox or Chrome
#'
#' ann = annotation_app(specType = 'reassigned', windowLength = 5, step = 1)
#'
#' # full list of parameters that can be passed to annotation_app():
#' paste0(c(rownames(soundgen:::def_ann),
#'  'specType', 'spec_colorTheme', 'spec_yScale', 'osc', 'wn', 'audioMethod'),
#'  collapse = ', ')
#'
#' # save the complete output, including the settings used
#' saveRDS(ann, 'my_annotations.rds')
#'
#' # re-use the same settings in a future session
#' ann2 = do.call(annotation_app, ann$settings)
#'
#' # To change system default browser, run something like:
#' options('browser' = '/usr/bin/firefox')  # path to the executable on Linux
#' }
annotation_app = function(...) {
  # check if shiny-related packages are available
  if (!requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("shinyjs", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE) ||
      !requireNamespace("base64enc", quietly = TRUE))
    stop('To run apps, please install missing dependencies: ',
         '`install.packages(c("shiny", "shinyjs", "bslib", "base64enc"))`')

  # load defaults
  annotation_app_defaults = as.list(def_ann[, 'default'])
  names(annotation_app_defaults) = rownames(def_ann)
  annotation_app_defaults$spec_ylim = c(0, annotation_app_defaults$spec_ylim)
  annotation_app_defaults$spec_xlim = c(0, annotation_app_defaults$spec_xlim)
  annotation_app_defaults = c(annotation_app_defaults, list(
    'specType' = 'spectrum',
    'spec_colorTheme' = 'bw',
    'spec_yScale' = 'linear',
    'wn' = 'gaussian',
    'osc' = 'linear'
  ))

  # use user-supplied presets, if any, to update the inputs
  annotation_app_defaults = modifyList(annotation_app_defaults, list(...))

  # Store defaults in the shared environment under the 'ann' list
  .soundgen_env$ann$def = annotation_app_defaults
  .soundgen_env$ann$def_ann = def_ann

  appDir = system.file("shiny", "annotation_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
