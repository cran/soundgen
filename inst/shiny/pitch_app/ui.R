# pitch_app()
#

library(bslib)

# Retrieve defaults from package environment.
# Fallback is provided for direct shinyapps.io deployment where the wrapper isn't executed.
if (exists(".soundgen_env", envir = asNamespace("soundgen")) &&
    !is.null(soundgen:::.soundgen_env$pitch$def)) {
  df = soundgen:::.soundgen_env$pitch$def_pitch
  def = soundgen:::.soundgen_env$pitch$def
} else {
  # Fallback for shinyapps.io deployment
  df = soundgen:::defaults_analyze
  def = as.list(df[, 'default'])
  names(def) = rownames(df)
  def$spec_ylim = c(0, def$spec_ylim)
  def = c(def, list(
    'pitchMethods' = c('dom', 'autocor'),
    'summaryFun' = c('mean', 'sd'),
    'summaryFun_text' = '',
    'spec_colorTheme' = 'bw',
    'osc' = 'linear',
    'wn' = 'gaussian'
  ))
}

tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))

ui = page_fluid(
  # css
  tags$head(
    shiny::includeCSS(file.path("www", "pitch_app.css"))
  ),

  # js
  includeScript(file.path("www", "pitch_app.js")),
  includeScript(file.path("www", "pitch_app_shinyjs.js")),
  shinyjs::useShinyjs(),
  # handy for calling js functions from R, eg for a collapsible side panel - see
  # https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  # import some js functions to be invoked from R with shinyjs
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'pitch_app_shinyjs.js',
    functions = c('playme_js', 'stopAudio_js', 'play_synth_uri',
                  'clearBrush', 'inheritSize', 'scrollBar')
  ),

  # html
  fluidRow(
    layout_sidebar(
      sidebar = sidebar(
        id = "Sidebar",
        width = "300px",
        bg = "Azure",

        tabsetPanel(
          id='parGroup',
          navbarMenu(
            "In",
            tabPanel(
              "General",
              actionButton(
                'reset_to_def',
                label = 'Reset ALL to defaults') |>
                tooltip("Reset all settings to default values", options = tooltip_opt),

              radioButtons(
                'audioMethod',
                label = "Play audio with",
                choices = list('Browser' = 'Browser', 'R' = 'R'),
                selected = 'Browser', inline = TRUE, width = NULL) |>
                tooltip("Play audio with javascript (recommended in Firefox,
                      doesn't work in Chrome) or with R (browser-independent,
                      but then the cursor doesn't move, and you can't stop playback",
                        options = tooltip_opt),

              checkboxInput(
                'normalizeInput',
                'Normalize for peak amplitude',
                value = TRUE),
            ),

            tabPanel(
              "STFT",
              numericInput(
                'windowLength',
                'Window length, ms ("windowLength")',
                value = def$windowLength,
                min = df['windowLength', 'low'],
                max = df['windowLength', 'high'],
                step = df['windowLength', 'step']) |>
                tooltip("Length of STFT window, ms. Larger values improve frequency
                      resolution at the expense of time resolution", options = tooltip_opt),

              numericInput(
                'step',
                'Step, ms',
                value = def$step,
                min = df['step', 'low'],
                max = df['step', 'high'],
                step = df['step', 'step']) |>
                tooltip("Step between analysis frames, ms (alternative to 'overlap')",
                        options = tooltip_opt),

              sliderInput(
                'dynamicRange',
                'Dynamic range, dB ("dynamicRange")',
                value = def$dynamicRange,
                min = df['dynamicRange', 'low'],
                max = df['dynamicRange', 'high'],
                step = df['dynamicRange', 'step']),

              sliderInput(
                'zp',
                'Zero padding, points 2 ^ n ("zp")',
                value = def$zp,
                min = df['zp', 'low'],
                max = df['zp', 'high'],
                step = df['zp', 'step']) |>
                tooltip("Zero padding of STFT window (improves frequency resolution):
                      8 means 2^8 = 256, etc.", options = tooltip_opt),

              selectInput(
                'wn',
                'Type of STFT window ("wn")',
                choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                            'hamming', 'hanning', 'rectangle'),
                selected = def$wn, multiple = FALSE)
            ),

            tabPanel(
              "Voicing",
              numericInput(
                'silence',
                'Silence threshold (0 to 1) ("silence")',
                value = def$silence,
                min = df['silence', 'low'],
                max = df['silence', 'high'],
                step = df['silence', 'step']) |>
                tooltip("Frames with RMS below silence threshold are not analyzed",
                        options = tooltip_opt),

              sliderInput(
                'nCands',
                'Candidates per method ("nCands")',
                value = def$nCands,
                min = df['nCands', 'low'],
                max = df['nCands', 'high'],
                step = df['nCands', 'step']) |>
                tooltip("Maximum number of pitch candidates to use per method",
                        options = tooltip_opt),

              sliderInput(
                'minVoicedCands',
                'Min candidates for voiced ("minVoicedCands")',
                value = def$minVoicedCands,
                min = df['minVoicedCands', 'low'],
                max = df['minVoicedCands', 'high'],
                step = df['minVoicedCands', 'step']) |>
                tooltip("Minimum number of pitch candidates that have to be defined
                      to consider a frame voiced", options = tooltip_opt)
            ),

            tabPanel(
              "Priors",
              numericInput(
                'pitchFloor',
                'Pitch floor, Hz ("pitchFloor")',
                value = def$pitchFloor,
                min = df['pitchFloor', 'low'],
                max = df['pitchFloor', 'high'],
                step = df['pitchFloor', 'step']) |>
                tooltip("No candidates below this absolute threshold", options = tooltip_opt),

              numericInput(
                'pitchCeiling',
                'Pitch ceiling, Hz ("pitchCeiling")',
                value = def$pitchCeiling,
                min = df['pitchCeiling', 'low'],
                max = df['pitchCeiling', 'high'],
                step = df['pitchCeiling', 'step']) |>
                tooltip("No candidates above this absolute threshold", options = tooltip_opt),

              numericInput(
                'priorMean',
                'Expected pitch (priorMean), Hz ("priorMean")',
                value = def$priorMean,
                min = df['priorMean', 'low'],
                max = df['priorMean', 'high'],
                step = df['priorMean', 'step']) |>
                tooltip("Candidates close to this value are prioritized
                      (how close is determined by priorSD)", options = tooltip_opt),

              numericInput(
                'priorSD',
                'Expected range (priorSD), semitones ("priorSD")',
                value = def$priorSD,
                min = df['priorSD', 'low'],
                max = df['priorSD', 'high'],
                step = df['priorSD', 'step']) |>
                tooltip("Determines the width of expected pitch range
                      (standard deviation of gamma distribution around priorMean)",
                        options = tooltip_opt),

              checkboxInput(
                'priorAdapt',
                'Adaptive prior',
                value = TRUE) |>
                tooltip("Adds a second pass for finding the optimal pitch contour,
                      with prior determined by the initial pitch estimates",
                        options = tooltip_opt)
            )
          ),

          navbarMenu(
            "Trackers",
            tabPanel(
              "Which to use",
              checkboxGroupInput(
                'pitchMethods',
                label = 'Pitch tracking methods ("pitchMethods")',
                choiceValues = c('dom', 'autocor', 'cep', 'spec', 'hps', 'zc'),
                choiceNames = c('Lowest dominant frequency ("dom")',
                                'Autocorrelation ("autocor")',
                                'Cepstrum ("cep")',
                                'Spectral harmonics ("spec")',
                                'Harmonic product spectrum ("hps")',
                                'Zero-crossing rate ("zc")'
                ),
                selected = def$pitchMethods)
            ),

            tabPanel(
              'Lowest dominant frequency ("dom")',
              sliderInput(
                'domThres',
                'Dominant frequency threshold ("domThres")',
                value = def$domThres,
                min = df['domThres', 'low'],
                max = df['domThres', 'high'],
                step = df['domThres', 'step']) |>
                tooltip("Dominant frequency is defined as the lowest bin in a
                      spectrum smoothed and normalized to range from 0 to 1
                      that it at least 'domThres' high (1 = absolute maximum,
                      ie peak frequency)", options = tooltip_opt),

              sliderInput(
                'domSmooth',
                'Width of smoothing interval, Hz ("domSmooth")',
                value = def$domSmooth,
                min = df['domSmooth', 'low'],
                max = df['domSmooth', 'high'],
                step = df['domSmooth', 'step']) |>
                tooltip("Width of smoothing interval for finding the lowest
                      dominant frequency band (low values = no smoothing)",
                        options = tooltip_opt)
            ),

            tabPanel(
              'Autocorrelation ("autocor")',
              sliderInput(
                'autocorThres',
                'Voicing threshold ("autocorThres")',
                value = def$autocorThres,
                min = df['autocorThres', 'low'],
                max = df['autocorThres', 'high'],
                step = df['autocorThres', 'step']) |>
                tooltip("Voicing threshold for autocorrelation algorithm", options = tooltip_opt),

              sliderInput(
                'autocorSmooth',
                'Width of smoothing interval, bins ("autocorSmooth")',
                value = def$autocorSmooth,
                min = df['autocorSmooth', 'low'],
                max = df['autocorSmooth', 'high'],
                step = df['autocorSmooth', 'step']) |>
                tooltip("Width of smoothing interval (in bins) for finding
                      peaks in the autocorrelation function", options = tooltip_opt),

              sliderInput(
                'autocorUpsample',
                'Upsample resolution, Hz ("autocorUpsample")',
                value = def$autocorUpsample,
                min = df['autocorUpsample', 'low'],
                max = df['autocorUpsample', 'high'],
                step = df['autocorUpsample', 'step']) |>
                tooltip("Upsamples acf to this resolution (Hz) to improve
                      accuracy in high frequencies", options = tooltip_opt),

              sliderInput(
                'autocorBestPeak',
                'Best candidate ("autocorBestPeak")',
                value = def$autocorBestPeak,
                min = df['autocorBestPeak', 'low'],
                max = df['autocorBestPeak', 'high'],
                step = df['autocorBestPeak', 'step']) |>
                tooltip("Amplitude of the lowest best candidate relative to
                      the absolute max of the acf", options = tooltip_opt)
            ),

            tabPanel(
              'Cepstrum ("cep")',
              sliderInput(
                'cepThres',
                'Voicing threshold ("cepThres")',
                value = def$cepThres,
                min = df['cepThres', 'low'],
                max = df['cepThres', 'high'],
                step = df['cepThres', 'step']) |>
                tooltip("Voicing threshold for cepstral algorithm", options = tooltip_opt),

              sliderInput(
                'cepZp',
                'Cepstral zero padding, 2 ^ n ("cepZp")',
                value = def$cepZp,
                min = df['cepZp', 'low'],
                max = df['cepZp', 'high'],
                step = df['cepZp', 'step']) |>
                tooltip("Length of cepstral window after zero padding:
                      8 means 2^8 = 256, etc.", options = tooltip_opt)
            ),

            tabPanel(
              'Spectral harmonics ("spec")',
              radioButtons(
                'specMethod',
                tooltip('Method ("specMethod")',
                        "'commonFactor' = greatest common factor of putative harmonics,
                      'BaNa' = ratio of putative harmonics"),
                choices = c("Highest common factor" = "commonFactor",
                            "Ratio of harmonics (BaNa)" = "BaNa"),
                selected = 'commonFactor', inline = TRUE, width = NULL),

              sliderInput(
                'specThres',
                'Voicing threshold ("specThres")',
                value = def$specThres,
                min = df['specThres', 'low'],
                max = df['specThres', 'high'],
                step = df['specThres', 'step']) |>
                tooltip("Voicing threshold for Ba-Na algorithm", options = tooltip_opt),

              sliderInput(
                'specPeak',
                'Spectral peak height ("specPeak")',
                value = def$specPeak,
                min = df['specPeak', 'low'],
                max = df['specPeak', 'high'],
                step = df['specPeak', 'step']) |>
                tooltip("Minimum amplitude of harmonics considered pitch candidates",
                        options = tooltip_opt),

              sliderInput(
                'specRatios',
                'Number of ratios ("specRatios")',
                value = def$specRatios,
                min = df['specRatios', 'low'],
                max = df['specRatios', 'high'],
                step = df['specRatios', 'step']) |>
                tooltip("For method = 'commonFactor', the number of integer fractions
                      to consider", options = tooltip_opt),

              sliderInput(
                'specHNRslope',
                'Slope of HNR discount ("specHNRslope")',
                value = def$specHNRslope,
                min = df['specHNRslope', 'low'],
                max = df['specHNRslope', 'high'],
                step = df['specHNRslope', 'step']) |>
                tooltip("0 = same threshold regardless of HNR; positive = lower
                      threshold in noisy sounds", options = tooltip_opt),

              sliderInput(
                'specSmooth',
                'Width of window for finding harmonics, Hz ("specSmooth")',
                value = def$specSmooth,
                min = df['specSmooth', 'low'],
                max = df['specSmooth', 'high'],
                step = df['specSmooth', 'step']) |>
                tooltip("Width of window for detecting harmonics in the spectrum, Hz",
                        options = tooltip_opt),

              sliderInput(
                'specMerge',
                'Margin for merging candidates, semitones ("specMerge")',
                value = def$specMerge,
                min = df['specMerge', 'low'],
                max = df['specMerge', 'high'],
                step = df['specMerge', 'step']) |>
                tooltip("Pitch candidates within specMerge semitones are merged
                      with boosted certainty", options = tooltip_opt),

              sliderInput(
                'specSinglePeakCert',
                'Certainty of single-ratio candidates ("specSinglePeakCert")',
                value = def$specSinglePeakCert,
                min = df['specSinglePeakCert', 'low'],
                max = df['specSinglePeakCert', 'high'],
                step = df['specSinglePeakCert', 'step']) |>
                tooltip("If pitch is calculated based on a single harmonic ratio
                      (as opposed to several ratios converging on the same candidate),
                      its certainty is taken to be specSinglePeakCert", options = tooltip_opt)
            ),

            tabPanel(
              'Harmonic product spectrum ("hps")',
              sliderInput(
                'hpsThres',
                'Voicing threshold ("hpsThres")',
                value = def$hpsThres,
                min = df['hpsThres', 'low'],
                max = df['hpsThres', 'high'],
                step = df['hpsThres', 'step']) |>
                tooltip("How high a spectral peak has to be to be considered a pitch
                      candidate, ~0 to 1", options = tooltip_opt),

              sliderInput(
                'hpsNum',
                'The number of folds ("hpsNum")',
                value = def$hpsNum,
                min = df['hpsNum', 'low'],
                max = df['hpsNum', 'high'],
                step = df['hpsNum', 'step']) |>
                tooltip("How many times to downsample and then multiply the spectra",
                        options = tooltip_opt),

              sliderInput(
                'hpsNorm',
                'Inflation of hps pitch certainty ("hpsNorm")',
                value = def$hpsNorm,
                min = df['hpsNorm', 'low'],
                max = df['hpsNorm', 'high'],
                step = df['hpsNorm', 'step']) |>
                tooltip("Rather arbitrary normalization of certainty in hps candidates
                      intended to make them more comparable to other pitch tracking
                      methods (0 = no boost in certainty, 2 = default quadratic)",
                        options = tooltip_opt),

              sliderInput(
                'hpsPenalty',
                'Penalty for low-frequency candidates ("hpsPenalty")',
                value = def$hpsPenalty,
                min = df['hpsPenalty', 'low'],
                max = df['hpsPenalty', 'high'],
                step = df['hpsPenalty', 'step']) |>
                tooltip("HPS performs worse at low frequencies (relative to windowLength),
                      so low-frequency pitch candidates are penalized (0 = no penalization,
                      ~10-20 = a lot)", options = tooltip_opt)
            ),

            tabPanel(
              'Zero-crossing rate ("zc")',
              sliderInput(
                'zcThres',
                'Voicing threshold ("zcThres")',
                value = def$zcThres,
                min = df['zcThres', 'low'],
                max = df['zcThres', 'high'],
                step = df['zcThres', 'step']) |>
                tooltip("Minimum certainty in zero-crossing pitch candidates,
                      ~0 to 1", options = tooltip_opt),

              sliderInput(
                'zcWin',
                'Window for estimating stability, glottal cycles ("zcWin")',
                value = def$zcWin,
                min = df['zcWin', 'low'],
                max = df['zcWin', 'high'],
                step = df['zcWin', 'step']) |>
                tooltip("Confidence in zero-crossing candidates depends on how
                      stable the estimates are across zcWin glottal cycles",
                        options = tooltip_opt)
            )
          ),

          navbarMenu(
            "Out",
            tabPanel(
              "Output",
              checkboxGroupInput(
                'summaryFun',
                label = 'Summary function ("summaryFun")',
                choiceValues = c('mean', 'sd', 'median', 'min', 'max'),
                choiceNames = c('Mean', 'SD', 'Median', 'Min', 'Max'),
                selected = def$summaryFun) |>
                tooltip("The function(s) used to summarize output", options = tooltip_opt),

              textInput(
                'summaryFun_text',
                label = 'Manual summary function',
                value = def$summaryFun_text,
                placeholder = 'function(x) diff(range(x, na.rm = TRUE))') |>
                tooltip("If specified, overrides the options above. For short column names,
                      define and name your function in R prior to starting pitch_app",
                        options = tooltip_opt)
            ),

            tabPanel(
              "Path",
              checkboxInput(
                'automPathUpdate',
                'Update path automatically',
                value = TRUE) |>
                tooltip("Update the optimal pitch contour automatically every time
                      an anchor changes? Turn off to avoid delays when editing a long
                      audio", options = tooltip_opt),

              sliderInput(
                'certWeight',
                'Certainty weight ("certWeight")',
                value = def$certWeight,
                min = df['certWeight', 'low'],
                max = df['certWeight', 'high'],
                step = df['certWeight', 'step']) |>
                tooltip("Specifies how much we prioritize the certainty of pitch
                      candidates vs. pitch jumps", options = tooltip_opt),

              numericInput(
                'shortestSyl',
                'Shortest syllable, ms ("shortestSyl")',
                value = def$shortestSyl,
                min = df['shortestSyl', 'low'],
                max = df['shortestSyl', 'high'],
                step = df['shortestSyl', 'step']) |>
                tooltip("Shorter voiced segments (ms) will be treated as voiceless
                      or merged with longer segments", options = tooltip_opt),

              numericInput(
                'shortestPause',
                'Shortest pause, ms ("shortestPause")',
                value = def$shortestPause,
                min = df['shortestPause', 'low'],
                max = df['shortestPause', 'high'],
                step = df['shortestPause', 'step']) |>
                tooltip("The smallest gap between voiced syllables (ms) that means
                      they shouldn't be merged", options = tooltip_opt)
            ),

            tabPanel(
              "Smoothing",
              sliderInput (
                'smooth',
                'Median smoothing (0 = none) ("smooth")',
                value = def$smooth,
                min = df['smooth', 'low'],
                max = df['smooth', 'high'],
                step = df['smooth', 'step']) |>
                tooltip("", options = tooltip_opt),

              numericInput(
                'interpolWin',
                'Interpolation window, ms (0 = none) ("interpolWin")',
                value = def$interpolWin,
                min = df['interpolWin', 'low'],
                max = df['interpolWin', 'high'],
                step = df['interpolWin', 'step']) |>
                tooltip("If no pitch candidates are found within ±interpolTol of the
                      median 'best guess' over ±interpolWin, this median is added
                      as an interpolated candidate", options = tooltip_opt),

              numericInput(
                'interpolTol',
                'Interpolation tolerance ("interpolTol")',
                value = def$interpolTol,
                min = df['interpolTol', 'low'],
                max = df['interpolTol', 'high'],
                step = df['interpolTol', 'step']) |>
                tooltip("Tolerated deviance from 'best guess' before adding an
                      interpolated candidate: proportion of best guess frequency",
                        options = tooltip_opt),

              sliderInput(
                'interpolCert',
                'Interpolation certainty ("interpolCert")',
                value = def$interpolCert,
                min = df['interpolCert', 'low'],
                max = df['interpolCert', 'high'],
                step = df['interpolCert', 'step']) |>
                tooltip("Certainty assigned to interpolated pitch candidates",
                        options = tooltip_opt)
            ),

            tabPanel(
              "Spectrogram",
              accordion(
                sliderInput(
                  'spec_ylim',
                  'Frequency range, kHz ("ylim")',
                  value = def$spec_ylim,
                  min = df['spec_ylim', 'low'],
                  max = df['spec_ylim', 'high'],
                  step = df['spec_ylim', 'step']) |>
                  tooltip("Range of displayed frequencies, kHz", options = tooltip_opt),

                sliderInput(
                  'spec_maxPoints',
                  'Max number of pixels, 10^',
                  value = def$spec_maxPoints,
                  min = df['spec_maxPoints', 'low'],
                  max = df['spec_maxPoints', 'high'],
                  step = df['spec_maxPoints', 'step']) |>
                  tooltip("The number of points to plot in the spectrogram
                      (smaller = faster, but low resolution)", options = tooltip_opt),

                numericInput(
                  'spec_cex',
                  'Point size ("pitchPlot=list(cex=...")',
                  value = def$spec_cex,
                  min = df['spec_cex', 'low'],
                  max = df['spec_cex', 'high'],
                  step = df['spec_cex', 'step']) |>
                  tooltip("Magnification coefficient controlling the size of points
                      showing pitch candidates", options = tooltip_opt),

                radioButtons(
                  inputId = 'spec_colorTheme',
                  label = 'Color scheme ("colorTheme")',
                  choices = c("Seewave" = "seewave",
                              "Heat" = "heat.colors",
                              "Black & white" = "bw"),
                  selected = def$spec_colorTheme,
                  inline = TRUE, width = NULL),

                numericInput(
                  'nColors',
                  'Number of colors in the palette',
                  value = def$nColors,
                  min = df['nColors', 'low'],
                  max = df['nColors', 'high'],
                  step = df['nColors', 'step']),

                sliderInput(
                  'specContrast',
                  'Contrast ("contrast")',
                  value = def$specContrast,
                  min = df['specContrast', 'low'],
                  max = df['specContrast', 'high'],
                  step = df['specContrast', 'step']),

                sliderInput(
                  'specBrightness',
                  'Brightness ("brightness")',
                  value = def$specBrightness,
                  min = df['specBrightness', 'low'],
                  max = df['specBrightness', 'high'],
                  step = df['specBrightness', 'step']),

                sliderInput(
                  'blur_freq',
                  'Blur: frequency (Hz)',
                  value = def$blur_freq,
                  min = df['blur_freq', 'low'],
                  max = df['blur_freq', 'high'],
                  step = df['blur_freq', 'step']) |>
                  tooltip("Gaussian filter of frequency: >0 = blur, <0 = unblur (sharpen)",
                          options = tooltip_opt),

                sliderInput(
                  'blur_time',
                  'Blur: time (ms)',
                  value = def$blur_time,
                  min = df['blur_time', 'low'],
                  max = df['blur_time', 'high'],
                  step = df['blur_time', 'step']) |>
                  tooltip("Gaussian filter of time: >0 = blur, <0 = unblur (sharpen)",
                          options = tooltip_opt),

                accordion_panel(
                  "Advanced",
                  radioButtons(
                    inputId = 'specType',
                    label = tooltip('Spectrogram method',
                                    "Spectrogram type, argument 'specType' in
                                spectrogram(). Affects pitch tracking"),
                    choices = c("Ordinary FFT" = "spectrum",
                                "Reassigned" = "reassigned"),
                    selected = 'spectrum', inline = TRUE, width = NULL),

                  sliderInput(
                    'reass_cex',
                    'Point size (reassigned spectrogram only)',
                    value = def$reass_cex,
                    min = df['reass_cex', 'low'],
                    max = df['reass_cex', 'high'],
                    step = df['reass_cex', 'step']),

                  numericInput(
                    'reass_windowLength',
                    'Window length, ms (reassigned only)',
                    value = def$reass_windowLength,
                    min = df['reass_windowLength', 'low'],
                    max = df['reass_windowLength', 'high'],
                    step = df['reass_windowLength', 'step']),

                  numericInput(
                    'reass_step',
                    'Step, ms (reassigned only)',
                    value = def$reass_step,
                    min = df['reass_step', 'low'],
                    max = df['reass_step', 'high'],
                    step = df['reass_step', 'step'])
                )
              )
            ),

            tabPanel(
              "Oscillogram",
              selectInput(
                'osc',
                'Oscillogram type ("osc")',
                choices = c('linear', 'dB'),
                selected = def$osc, multiple = FALSE),

              sliderInput(
                'osc_maxPoints',
                'Max number of pixels, 10^',
                value = def$osc_maxPoints,
                min = df['osc_maxPoints', 'low'],
                max = df['osc_maxPoints', 'high'],
                step = df['osc_maxPoints', 'step']) |>
                tooltip("The number of points to plot in the oscillogram
                      (smaller = faster, but low resolution)", options = tooltip_opt)
            )
          )
        )
      ),  # end of column "Sidebar"

      fluidRow(
        style="margin-top: -15px; margin-bottom: -15px;",
        column(
          width = 3,
          fileInput(
            inputId = "loadAudio",
            label = NULL, multiple = TRUE,
            buttonLabel = 'Load audio',
            placeholder = '...', width = "175px")
        ),

        column(
          width = 6,
          tags$div(
            actionButton(
              inputId = "lastFile", label = "<<",
              class = "buttonFile") |>
              tooltip("Save and return to the previous file (PageUp)", options = tooltip_opt),
            tags$strong(uiOutput("fileN", inline = TRUE)),
            actionButton(
              inputId = "nextFile", label = ">>",
              class = "buttonFile") |>
              tooltip("Save and proceed to the next file (PageDown)", options = tooltip_opt)
          ),
          selectInput('fileList', label = NULL, choices = list())
        ),

        column(
          width = 3,
          uiOutput("htmlAudio"),  # not actually shown
          downloadButton(
            outputId = "saveRes",
            label = "",
            style="color: blue; background-color: orange;") |>
            tooltip("Download results (see ?pitch_app for recovering unsaved
                    data after a crash)", options = tooltip_opt),
          actionButton('about', label = '?'),
        )
      ),

      fluidRow(
        style="margin-top: -15px; margin-bottom: -15px;",
        column(
          width = 1,
          actionButton(
            inputId = 'zoomIn_freq',
            label = HTML("<img src='icons/zoomIn.png' width = '25px'>"),
            style = "padding: 2px 2px; display: block") |>
            tooltip("Zoom in frequency (+)", options = tooltip_opt),

          actionButton(
            inputId = 'zoomOut_freq',
            label = HTML("<img src='icons/zoomOut.png' width = '25px'>"),
            style = "padding: 2px 2px; display: block") |>
            tooltip("Zoom out frequency (-)", options = tooltip_opt)
        ),

        column(
          width = 3,
          radioButtons(
            'spectro_clickAct',
            label = 'Left click action: ',
            choiceNames = c('Anchor', 'Select'),
            choiceValues = c('addCand', 'select'),
            selected = 'addCand', inline = TRUE) |>
            tooltip("", options = tooltip_opt)
        ),

        column(
          width = 5,
          actionButton(
            inputId = "selection_stop",
            label = HTML("<img src='icons/stop.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Stop playback", options = tooltip_opt),

          actionButton(
            inputId = "selection_play",
            label = HTML("<img src='icons/play.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Play selection (SPACEBAR)", options = tooltip_opt),

          actionButton(
            inputId = "selection_unvoice",
            label = HTML("<img src='icons/unvoice.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Unvoice selection (U)", options = tooltip_opt),

          actionButton(
            inputId = "selection_voice",
            label = HTML("<img src='icons/voice.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Voice selection (V) - obviouslly, pitch estimates may be
                      totally incorrect", options = tooltip_opt),

          actionButton(
            inputId = "selection_octaveUp",
            label = HTML("<img src='icons/octaveUp.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Raise pitch for selection by an octave (R)", options = tooltip_opt),

          actionButton(
            inputId = "selection_octaveDown",
            label = HTML("<img src='icons/octaveDown.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Lower pitch for selection by an octave (L)", options = tooltip_opt),

          actionButton(
            inputId = "selection_setPrior",
            label = HTML("<img src='icons/prior.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Set a prior on expected pitch values corresponding to the
                      selected frequency range (P)", options = tooltip_opt),

          actionButton(
            inputId = "button_pathUpdate",
            label = HTML("<img src='icons/update.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Draw / refresh pitch contour (D) (only needed if
                      'Out/Path/Update path automatically' is turned off)",
                    options = tooltip_opt),

          actionButton(
            inputId = "button_synth",
            label = HTML("<img src='icons/synth.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Synthesize and play pitch contour", options = tooltip_opt)
        ),

        column(
          width = 3,
          actionButton(
            inputId = 'scrollLeft',
            label = HTML("<img src='icons/backward.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Scroll left (arrow LEFT)", options = tooltip_opt),

          actionButton(
            inputId = 'zoomOut',
            label = HTML("<img src='icons/zoomOut.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Zoom out time (arrow DOWN)", options = tooltip_opt),

          actionButton(
            inputId = "zoomToSel",
            label = HTML("<img src='icons/zoomSel.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Zoom to selection (S)", options = tooltip_opt),

          actionButton(
            inputId = 'zoomIn',
            label = HTML("<img src='icons/zoomIn.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Zoom in time (arrow UP)", options = tooltip_opt),

          actionButton(
            inputId = 'scrollRight',
            label = HTML("<img src='icons/forward.png' width = '25px'>"),
            class = "buttonInline") |>
            tooltip("Scroll right (arrow RIGHT)", options = tooltip_opt)
        )
      ),

      fluidRow(
        tags$div(
          id = 'plotRow',
          tags$div(
            id = 'specDiv',
            plotOutput('spectrogram'),
            plotOutput('specSlider'),
            plotOutput(
              'specOver',
              click = "spectrogram_click",
              dblclick = dblclickOpts(id = "spectrogram_dblclick"),
              hover = hoverOpts(id = "spectrogram_hover"),
              brush = brushOpts(id = 'spectrogram_brush', opacity = 0.25, resetOnNew = FALSE))
          ),

          tags$div(
            id = 'scrollBarCont',
            tags$div(
              id = 'scrollBar'
            )
          ),

          tags$div(
            id = 'oscDiv',
            plotOutput('oscillogram'),
            plotOutput(
              'oscOver',
              click = "osc_click"
            )
          ),
          # plotOutput('oscillogram', height = '100px')  # default size
        )
      )

      #fluidRow(
      # htmlOutput('statusBar')  # status bar here
      #),
    )  # end of column "Main"
  )  # end of fluidRow
)
