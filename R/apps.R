# Note: might wrap shiny::runApp in suppress.warnings()

#' Interactive sound synthesizer
#'
#' Starts a shiny app that provides an interactive wrapper to
#' \code{\link{soundgen}}. Supported browsers: Firefox / Chrome. Note that the
#' browser has to be able to playback WAV audio files, otherwise there will be
#' no sound.
#' @export
soundgen_app = function() {
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
#' `?analyze` and the vignette on acoustic analysis for help and examples. You
#' can verify the pitch contours first, and then feed them back into
#' \code{analyze} (see examples).
#'
#' @param ... presets like \code{windowLength = 25, pitchMethods = c('auocor',
#'   'cep')}. Full list: dynamicRange, zp, entropyThres, nCands, minVoicedCands,
#'   domThres, domSmooth, autocorThres, autocorSmooth, autocorUpsample,
#'   autocorBestPeak, cepThres, cepZp, specThres, specPeak, specRatios,
#'   specHNRslope, specSmooth, specMerge, specSinglePeakCert, hpsThres, hpsNum,
#'   hpsNorm, hpsPenalty, zcThres, zcWin, certWeight, smooth, interpolCert,
#'   spec_maxPoints, specContrast, specBrightness, blur_freq, blur_time,
#'   reass_cex, osc_maxPoints, windowLength, step, silence, pitchFloor,
#'   pitchCeiling, priorMean, priorSD, shortestSyl, shortestPause, interpolWin,
#'   interpolTol, spec_cex, nColors, reass_windowLength, reass_step,
#'   pitchMethods, summaryFun, summaryFun_text, pathfinding, spec_ylim,
#'   spec_colorTheme, osc, wn
#'
#' @return A list with the last used settings ($settings) plus the output of
#'   analyze() for each file with two additional columns: "time" and "pitch".
#'   When proceeding to the next file in the cue, two types of backups are
#'   created. (1) A global object called "my_pitch" is created or updated. This
#'   list becomes visible when the app is terminated, and it contains the usual
#'   outputs of analyze() ($detailed and $summary) plus lists of manually
#'   corrected voiced and voiceless frames. (2) The app saves to disk a .csv
#'   file with one row per audio file. Apart from the usual descriptives from
#'   analyze(), there are two additional columns: "time" with time stamps (the
#'   midpoint of each STFT frame, ms) and "pitch" with the manually corrected
#'   pitch values for each frame (Hz). When the orange "Download results" button
#'   is clicked, a context menu pops up offering to terminate the app - if that
#'   happens, the results are also returned directly into R. To process pitch
#'   contours further in R, work directly with my_pitch[[myfile]]$time and
#'   my_pitch[[myfile]]$pitch or, if loading the csv file, do something like:
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
#' `options(shiny.maxRequestSize = 30 * 1024^2)`. Once the audio has been
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
#' the output you've got so far is saved in a backup file called "temp.csv", and
#' the "my_pitch" global object is updated. If the app crashes or is closed
#' without saving the results, this backup file preserves your data. To recover
#' it, access this file manually on disk or simply restart pitch_app() - a
#' dialog box will pop up and ask whether you wank to append the old data to the
#' new one. Path to backup file:
#' "[R_installation_folder]/soundgen/shiny/pitch_app/www/temp.csv", for example,
#' "/home/allgoodguys/R/x86_64-pc-linux-gnu-library/3.6/soundgen/shiny/pitch_app/www/temp.csv"
#'
#' @seealso \code{\link{formant_app}}
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
#'   pitchMethods = c('dom', 'autocor', 'cep'), ylim = c(0.1, 3))
#' # full list of parameters that can be passed to pitch_app():
#' # paste0(c(names(input)[which(names(input) %in% rownames(defaults_analyze))],
#' #  'pitchMethods', 'summaryFun', 'summaryFun_text', 'pathfinding', 'spec_ylim',
#' #  'spec_colorTheme', 'osc', 'wn'), collapse = ', ')
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
#' df2 = analyze(path_to_audio,
#'   pitchMethods = 'autocor',  # only needed for HNR
#'   nFormants = 5,        # now we can measure formants as well
#'   pitchManual = my_pitch
#'   # or, if loading the output of pitch_app() from the disk:
#'   # pitchManual = '~/Downloads/output.csv'
#'   # pitchManual = '~/path_to_some_folder/my_pitch_contours.rds
#'   # etc
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
  pitch_app_defaults = as.list(defaults_analyze[, 'default'])
  names(pitch_app_defaults) = rownames(defaults_analyze)
  pitch_app_defaults$spec_ylim = c(0, pitch_app_defaults$spec_ylim)
  pitch_app_defaults = c(pitch_app_defaults, list(
    'pitchMethods' = c('dom', 'autocor'),
    'summaryFun', selected = c('mean', 'sd'),
    'summaryFun_text' = '',
    'pathfinding' = 'fast',
    'spec_colorTheme' = 'bw',
    'osc' = 'linear',
    'wn' = 'gaussian'
  ))
  # use user-input presets, if any, to update the inputs
  pitch_app_defaults = modifyList(pitch_app_defaults, list(...))
  pitch_app_defaults <<- pitch_app_defaults

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
#' are 4 ways to adjust them: (1) type in the correct number in one of the
#' formant boxes in the top right corner; (2) click a spectrogram within
#' selection (pick the formant number to adjust by clicking the formant boxes);
#' (3) single-click the spectrum to use the cursor's position, or (4)
#' double-click the spectrum to use the nearest spectral peak. When done with a
#' file, move on to the next one in the queue. Use the orange button to download
#' the results. To continue work, upload the output file from the previous
#' session together with the audio files (you can rename it, but keep the .csv
#' extension). Use hotkeys (eg spacebar to play/stop) and avoid working with
#' very large files.
#'
#' @seealso \code{\link{pitch_app}}
#'
#' @param ... presets like \code{windowLength = 25}. Full list:
#'   samplingRate_mult, nFormants, minformant, maxbw, dynamicRange_lpc, zp_lpc,
#'   spec_ylim, dynamicRange, specContrast, specBrightness, blur_freq,
#'   blur_time, reass_cex, zp, spec_maxPoints, osc_maxPoints, spectrum_smooth,
#'   spectrum_xlim, spectrum_len, silence, windowLength_lpc, step_lpc,
#'   windowLength, step, spectrum_xlim, spec_ylim, spec_colorTheme, osc, wn,
#'   wn_lpc, vtl_method, speedSound, coeffs, interceptZero, tube
#'
#' @return A list of the last used settings ($settings) plus a data.frame with
#'   the formant measurements. Every time a new annotation is added, the app
#'   creates a backup csv file and creates or updates a global object called
#'   "my_formants", which contains all the annotations.
#'
#' @export
#' @examples
#' \dontrun{
#' f = formant_app()  # runs in default browser such as Firefox or Chrome
#'
#' f1 = formant_app(specType = 'reassigned', windowLength = 5, step = 1)
#' # full list of parameters that can be passed to formant_app():
#' # paste0(c(names(input)[which(names(input) %in% rownames(def_form))],
#' # 'spectrum_xlim', 'spec_ylim', 'spec_colorTheme', 'osc', 'wn', 'wn_lpc',
#' # 'vtl_method', 'speedSound', 'coeffs', 'interceptZero', 'tube'), collapse = ', ')
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
  formant_app_defaults = as.list(def_form[, 'default'])
  names(formant_app_defaults) = rownames(def_form)
  formant_app_defaults$spectrum_xlim = c(0, formant_app_defaults$spectrum_xlim)
  formant_app_defaults$spec_ylim = c(0, formant_app_defaults$spec_ylim)
  formant_app_defaults = c(formant_app_defaults, list(
    'wn' = 'gaussian',
    'wn_lpc' = 'gaussian',
    'spec_colorTheme' = 'bw',
    'osc' = 'linear',
    'vtl_method' = 'regression',
    'speedSound' = 35400,
    'coeffs' = '',
    'interceptZero' = TRUE,
    'tube' = 'closed-open'
  ))
  # use user-input presets, if any, to update the inputs
  formant_app_defaults = modifyList(formant_app_defaults, list(...))
  formant_app_defaults <<- formant_app_defaults

  appDir = system.file("shiny", "formant_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' Annotation app
#'
#' Starts a shiny app for annotating audio. This is a simplified and faster
#' version of \code{\link{formant_app}} intended only for making annotations.
#'
#' @param ... presets like \code{windowLength = 25}. Full list: dynamicRange,
#'   specContrast, specBrightness, blur_freq, blur_time, reass_cex, zp,
#'   spec_maxPoints, osc_maxPoints, windowLength, step, spec_xlim, spec_ylim,
#'   spec_colorTheme, osc, wn
#'
#' @return A list of the last used settings ($settings) plus a data.frame with
#'   the annotations. Every time a new annotation is added, the app creates a
#'   backup csv file and creates or updates a global object called "my_annot",
#'   which contains all the annotations.
#'
#' @export
#' @examples
#' \dontrun{
#' ann = annotation_app()  # runs in default browser such as Firefox or Chrome
#'
#' ann = annotation_app(specType = 'reassigned', windowLength = 5, step = 1)
#' # full list of parameters that can be passed to annotation_app():
#' # paste0(c(names(input)[which(names(input) %in% rownames(def_form))],
#' #  'spec_xlim', 'spec_ylim', 'spec_colorTheme', 'osc', 'wn'), collapse = ', ')
#'
#' # save the complete output, including the settings used
#' saveRDS(ann, 'my_annotations.rds')
#'
#' # To change system default browser, run something like:
#' options('browser' = '/usr/bin/firefox')  # path to the executable on Linux
#' }
annotation_app = function(...) {
  annotation_app_defaults = as.list(def_form[, 'default'])
  names(annotation_app_defaults) = rownames(def_form)
  annotation_app_defaults$spec_ylim = c(0, annotation_app_defaults$spec_ylim)
  annotation_app_defaults = c(annotation_app_defaults, list(
    'wn' = 'gaussian',
    'spec_xlim' = c(0, def_form['spec_xlim','default']),
    'spec_colorTheme' = 'bw',
    'osc' = 'linear'
  ))
  # use user-input presets, if any, to update the inputs
  annotation_app_defaults = modifyList(annotation_app_defaults, list(...))
  annotation_app_defaults <<-annotation_app_defaults

  appDir = system.file("shiny", "annotation_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
