# pitch_app()
#
library(bslib)
tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))

ui = page_fluid(
  # css
  tags$head(
    shiny::includeCSS("www/pitch_app.css")
  ),

  # js
  includeScript("www/pitch_app.js"),
  includeScript("www/pitch_app_shinyjs.js"),
  shinyjs::useShinyjs(),
  # handy for calling js functions from R, eg for a collapsible side panel - see
  # https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  # import some js functions to be invoked from R with shinyjs
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'www/pitch_app_shinyjs.js',
    functions = c('playme_js', 'stopAudio_js', 'play_file', 'clearBrush', 'inheritSize', 'scrollBar')
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
                value = defaults_analyze['windowLength', 'default'],
                min = defaults_analyze['windowLength', 'low'],
                max = defaults_analyze['windowLength', 'high'],
                step = defaults_analyze['windowLength', 'step']) |>
                tooltip("Length of STFT window, ms. Larger values improve frequency
                      resolution at the expense of time resolution", options = tooltip_opt),

              numericInput(
                'step',
                'Step, ms',
                value = defaults_analyze['step', 'default'],
                min = defaults_analyze['step', 'low'],
                max = defaults_analyze['step', 'high'],
                step = defaults_analyze['step', 'step']) |>
                tooltip("Step between analysis frames, ms (alternative to 'overlap')",
                        options = tooltip_opt),

              sliderInput(
                'dynamicRange',
                'Dynamic range, dB ("dynamicRange")',
                value = defaults_analyze['dynamicRange', 'default'],
                min = defaults_analyze['dynamicRange', 'low'],
                max = defaults_analyze['dynamicRange', 'high'],
                step = defaults_analyze['dynamicRange', 'step']),

              sliderInput(
                'zp',
                'Zero padding, points 2 ^ n ("zp")',
                value = defaults_analyze['zp', 'default'],
                min = defaults_analyze['zp', 'low'],
                max = defaults_analyze['zp', 'high'],
                step = defaults_analyze['zp', 'step']) |>
                tooltip("Zero padding of STFT window (improves frequency resolution):
                      8 means 2^8 = 256, etc.", options = tooltip_opt),

              selectInput(
                'wn',
                'Type of STFT window ("wn")',
                choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                            'hamming', 'hanning', 'rectangle'),
                selected = 'gaussian', multiple = FALSE)
            ),

            tabPanel(
              "Voicing",
              numericInput(
                'silence',
                'Silence threshold (0 to 1) ("silence")',
                value = defaults_analyze['silence', 'default'],
                min = defaults_analyze['silence', 'low'],
                max = defaults_analyze['silence', 'high'],
                step = defaults_analyze['silence', 'step']) |>
                tooltip("Frames with RMS below silence threshold are not analyzed",
                        options = tooltip_opt),

              sliderInput(
                'entropyThres',
                'Entropy threshold (0 to 1) ("entropyThres")',
                value = defaults_analyze['entropyThres', 'default'],
                min = defaults_analyze['entropyThres', 'low'],
                max = defaults_analyze['entropyThres', 'high'],
                step = defaults_analyze['entropyThres', 'step']) |>
                tooltip("Frames with Weiner entropy above entropy threshold are
                      ignored when searching for pitch candidates", options = tooltip_opt),

              sliderInput(
                'nCands',
                'Candidates per method ("nCands")',
                value = defaults_analyze['nCands', 'default'],
                min = defaults_analyze['nCands', 'low'],
                max = defaults_analyze['nCands', 'high'],
                step = defaults_analyze['nCands', 'step']) |>
                tooltip("Maximum number of pitch candidates to use per method",
                        options = tooltip_opt),

              sliderInput(
                'minVoicedCands',
                'Min candidates for voiced ("minVoicedCands")',
                value = defaults_analyze['minVoicedCands', 'default'],
                min = defaults_analyze['minVoicedCands', 'low'],
                max = defaults_analyze['minVoicedCands', 'high'],
                step = defaults_analyze['minVoicedCands', 'step']) |>
                tooltip("Minimum number of pitch candidates that have to be defined
                      to consider a frame voiced", options = tooltip_opt)
            ),

            tabPanel(
              "Priors",
              numericInput(
                'pitchFloor',
                'Pitch floor, Hz ("pitchFloor")',
                value = defaults_analyze['pitchFloor', 'default'],
                min = defaults_analyze['pitchFloor', 'low'],
                max = defaults_analyze['pitchFloor', 'high'],
                step = defaults_analyze['pitchFloor', 'step']) |>
                tooltip("No candidates below this absolute threshold", options = tooltip_opt),

              numericInput(
                'pitchCeiling',
                'Pitch ceiling, Hz ("pitchCeiling")',
                value = defaults_analyze['pitchCeiling', 'default'],
                min = defaults_analyze['pitchCeiling', 'low'],
                max = defaults_analyze['pitchCeiling', 'high'],
                step = defaults_analyze['pitchCeiling', 'step']) |>
                tooltip("No candidates above this absolute threshold", options = tooltip_opt),

              numericInput(
                'priorMean',
                'Expected pitch (priorMean), Hz ("priorMean")',
                value = defaults_analyze['priorMean', 'default'],
                min = defaults_analyze['priorMean', 'low'],
                max = defaults_analyze['priorMean', 'high'],
                step = defaults_analyze['priorMean', 'step']) |>
                tooltip("Candidates close to this value are prioritized
                      (how close is determined by priorSD)", options = tooltip_opt),

              numericInput(
                'priorSD',
                'Expected range (priorSD), semitones ("priorSD")',
                value = defaults_analyze['priorSD', 'default'],
                min = defaults_analyze['priorSD', 'low'],
                max = defaults_analyze['priorSD', 'high'],
                step = defaults_analyze['priorSD', 'step']) |>
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
                selected = c('dom', 'autocor'))
            ),

            tabPanel(
              'Lowest dominant frequency ("dom")',
              sliderInput(
                'domThres',
                'Dominant frequency threshold ("domThres")',
                value = defaults_analyze['domThres', 'default'],
                min = defaults_analyze['domThres', 'low'],
                max = defaults_analyze['domThres', 'high'],
                step = defaults_analyze['domThres', 'step']) |>
                tooltip("Dominant frequency is defined as the lowest bin in a
                      spectrum smoothed and normalized to range from 0 to 1
                      that it at least 'domThres' high (1 = absolute maximum,
                      ie peak frequency)", options = tooltip_opt),

              sliderInput(
                'domSmooth',
                'Width of smoothing interval, Hz ("domSmooth")',
                value = defaults_analyze['domSmooth', 'default'],
                min = defaults_analyze['domSmooth', 'low'],
                max = defaults_analyze['domSmooth', 'high'],
                step = defaults_analyze['domSmooth', 'step']) |>
                tooltip("Width of smoothing interval for finding the lowest
                      dominant frequency band (low values = no smoothing)",
                        options = tooltip_opt)
            ),

            tabPanel(
              'Autocorrelation ("autocor")',
              sliderInput(
                'autocorThres',
                'Voicing threshold ("autocorThres")',
                value = defaults_analyze['autocorThres', 'default'],
                min = defaults_analyze['autocorThres', 'low'],
                max = defaults_analyze['autocorThres', 'high'],
                step = defaults_analyze['autocorThres', 'step']) |>
                tooltip("Voicing threshold for autocorrelation algorithm", options = tooltip_opt),

              sliderInput(
                'autocorSmooth',
                'Width of smoothing interval, bins ("autocorSmooth")',
                value = defaults_analyze['autocorSmooth', 'default'],
                min = defaults_analyze['autocorSmooth', 'low'],
                max = defaults_analyze['autocorSmooth', 'high'],
                step = defaults_analyze['autocorSmooth', 'step']) |>
                tooltip("Width of smoothing interval (in bins) for finding
                      peaks in the autocorrelation function", options = tooltip_opt),

              sliderInput(
                'autocorUpsample',
                'Upsample resolution, Hz ("autocorUpsample")',
                value = defaults_analyze['autocorUpsample', 'default'],
                min = defaults_analyze['autocorUpsample', 'low'],
                max = defaults_analyze['autocorUpsample', 'high'],
                step = defaults_analyze['autocorUpsample', 'step']) |>
                tooltip("Upsamples acf to this resolution (Hz) to improve
                      accuracy in high frequencies", options = tooltip_opt),

              sliderInput(
                'autocorBestPeak',
                'Best candidate ("autocorBestPeak")',
                value = defaults_analyze['autocorBestPeak', 'default'],
                min = defaults_analyze['autocorBestPeak', 'low'],
                max = defaults_analyze['autocorBestPeak', 'high'],
                step = defaults_analyze['autocorBestPeak', 'step']) |>
                tooltip("Amplitude of the lowest best candidate relative to
                      the absolute max of the acf", options = tooltip_opt)
            ),

            tabPanel(
              'Cepstrum ("cep")',
              sliderInput(
                'cepThres',
                'Voicing threshold ("cepThres")',
                value = defaults_analyze['cepThres', 'default'],
                min = defaults_analyze['cepThres', 'low'],
                max = defaults_analyze['cepThres', 'high'],
                step = defaults_analyze['cepThres', 'step']) |>
                tooltip("Voicing threshold for cepstral algorithm", options = tooltip_opt),

              sliderInput(
                'cepZp',
                'Cepstral zero padding, 2 ^ n ("cepZp")',
                value = defaults_analyze['cepZp', 'default'],
                min = defaults_analyze['cepZp', 'low'],
                max = defaults_analyze['cepZp', 'high'],
                step = defaults_analyze['cepZp', 'step']) |>
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
                value = defaults_analyze['specThres', 'default'],
                min = defaults_analyze['specThres', 'low'],
                max = defaults_analyze['specThres', 'high'],
                step = defaults_analyze['specThres', 'step']) |>
                tooltip("Voicing threshold for Ba-Na algorithm", options = tooltip_opt),

              sliderInput(
                'specPeak',
                'Spectral peak height ("specPeak")',
                value = defaults_analyze['specPeak', 'default'],
                min = defaults_analyze['specPeak', 'low'],
                max = defaults_analyze['specPeak', 'high'],
                step = defaults_analyze['specPeak', 'step']) |>
                tooltip("Minimum amplitude of harmonics considered pitch candidates",
                        options = tooltip_opt),

              sliderInput(
                'specRatios',
                'Number of ratios ("specRatios")',
                value = defaults_analyze['specRatios', 'default'],
                min = defaults_analyze['specRatios', 'low'],
                max = defaults_analyze['specRatios', 'high'],
                step = defaults_analyze['specRatios', 'step']) |>
                tooltip("For method = 'commonFactor', the number of integer fractions
                      to consider", options = tooltip_opt),

              sliderInput(
                'specHNRslope',
                'Slope of HNR discount ("specHNRslope")',
                value = defaults_analyze['specHNRslope', 'default'],
                min = defaults_analyze['specHNRslope', 'low'],
                max = defaults_analyze['specHNRslope', 'high'],
                step = defaults_analyze['specHNRslope', 'step']) |>
                tooltip("0 = same threshold regardless of HNR; positive = lower
                      threshold in noisy sounds", options = tooltip_opt),

              sliderInput(
                'specSmooth',
                'Width of window for finding harmonics, Hz ("specSmooth")',
                value = defaults_analyze['specSmooth', 'default'],
                min = defaults_analyze['specSmooth', 'low'],
                max = defaults_analyze['specSmooth', 'high'],
                step = defaults_analyze['specSmooth', 'step']) |>
                tooltip("Width of window for detecting harmonics in the spectrum, Hz",
                        options = tooltip_opt),

              sliderInput(
                'specMerge',
                'Margin for merging candidates, semitones ("specMerge")',
                value = defaults_analyze['specMerge', 'default'],
                min = defaults_analyze['specMerge', 'low'],
                max = defaults_analyze['specMerge', 'high'],
                step = defaults_analyze['specMerge', 'step']) |>
                tooltip("Pitch candidates within specMerge semitones are merged
                      with boosted certainty", options = tooltip_opt),

              sliderInput(
                'specSinglePeakCert',
                'Certainty of single-ratio candidates ("specSinglePeakCert")',
                value = defaults_analyze['specSinglePeakCert', 'default'],
                min = defaults_analyze['specSinglePeakCert', 'low'],
                max = defaults_analyze['specSinglePeakCert', 'high'],
                step = defaults_analyze['specSinglePeakCert', 'step']) |>
                tooltip("If pitch is calculated based on a single harmonic ratio
                      (as opposed to several ratios converging on the same candidate),
                      its certainty is taken to be specSinglePeakCert", options = tooltip_opt)
            ),

            tabPanel(
              'Harmonic product spectrum ("hps")',
              sliderInput(
                'hpsThres',
                'Voicing threshold ("hpsThres")',
                value = defaults_analyze['hpsThres', 'default'],
                min = defaults_analyze['hpsThres', 'low'],
                max = defaults_analyze['hpsThres', 'high'],
                step = defaults_analyze['hpsThres', 'step']) |>
                tooltip("How high a spectral peak has to be to be considered a pitch
                      candidate, ~0 to 1", options = tooltip_opt),

              sliderInput(
                'hpsNum',
                'The number of folds ("hpsNum")',
                value = defaults_analyze['hpsNum', 'default'],
                min = defaults_analyze['hpsNum', 'low'],
                max = defaults_analyze['hpsNum', 'high'],
                step = defaults_analyze['hpsNum', 'step']) |>
                tooltip("How many times to downsample and then multiply the spectra",
                        options = tooltip_opt),

              sliderInput(
                'hpsNorm',
                'Inflation of hps pitch certainty ("hpsNorm")',
                value = defaults_analyze['hpsNorm', 'default'],
                min = defaults_analyze['hpsNorm', 'low'],
                max = defaults_analyze['hpsNorm', 'high'],
                step = defaults_analyze['hpsNorm', 'step']) |>
                tooltip("Rather arbitrary normalization of certainty in hps candidates
                      intended to make them more comparable to other pitch tracking
                      methods (0 = no boost in certainty, 2 = default quadratic)",
                        options = tooltip_opt),

              sliderInput(
                'hpsPenalty',
                'Penalty for low-frequency candidates ("hpsPenalty")',
                value = defaults_analyze['hpsPenalty', 'default'],
                min = defaults_analyze['hpsPenalty', 'low'],
                max = defaults_analyze['hpsPenalty', 'high'],
                step = defaults_analyze['hpsPenalty', 'step']) |>
                tooltip("HPS performs worse at low frequencies (relative to windowLength),
                      so low-frequency pitch candidates are penalized (0 = no penalization,
                      ~10-20 = a lot)", options = tooltip_opt)
            ),

            tabPanel(
              'Zero-crossing rate ("zc")',
              sliderInput(
                'zcThres',
                'Voicing threshold ("zcThres")',
                value = defaults_analyze['zcThres', 'default'],
                min = defaults_analyze['zcThres', 'low'],
                max = defaults_analyze['zcThres', 'high'],
                step = defaults_analyze['zcThres', 'step']) |>
                tooltip("Minimum certainty in zero-crossing pitch candidates,
                      ~0 to 1", options = tooltip_opt),

              sliderInput(
                'zcWin',
                'Window for estimating stability, glottal cycles ("zcWin")',
                value = defaults_analyze['zcWin', 'default'],
                min = defaults_analyze['zcWin', 'low'],
                max = defaults_analyze['zcWin', 'high'],
                step = defaults_analyze['zcWin', 'step']) |>
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
                selected = c('mean', 'sd')) |>
                tooltip("The function(s) used to summarize output", options = tooltip_opt),

              textInput(
                'summaryFun_text',
                label = 'Manual summary function',
                value = '',
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

              selectInput(
                'pathfinding',
                tooltip('Pathfinding method ("pathfinding")',
                        "Method of finding the optimal path through pitch candidates:
                      'none' = best candidate per frame, 'fast' = simple heuristic,
                      'slow' = annealing (initial analysis only)"),
                choices = c('none', 'fast', 'slow'),
                selected = 'fast', multiple = FALSE) |>
                tooltip("", options = tooltip_opt),

              sliderInput(
                'certWeight',
                'Certainty weight ("certWeight")',
                value = defaults_analyze['certWeight', 'default'],
                min = defaults_analyze['certWeight', 'low'],
                max = defaults_analyze['certWeight', 'high'],
                step = defaults_analyze['certWeight', 'step']) |>
                tooltip("Specifies how much we prioritize the certainty of pitch
                      candidates vs. pitch jumps", options = tooltip_opt),

              numericInput(
                'shortestSyl',
                'Shortest syllable, ms ("shortestSyl")',
                value = defaults_analyze['shortestSyl', 'default'],
                min = defaults_analyze['shortestSyl', 'low'],
                max = defaults_analyze['shortestSyl', 'high'],
                step = defaults_analyze['shortestSyl', 'step']) |>
                tooltip("Shorter voiced segments (ms) will be treated as voiceless
                      or merged with longer segments", options = tooltip_opt),

              numericInput(
                'shortestPause',
                'Shortest pause, ms ("shortestPause")',
                value = defaults_analyze['shortestPause', 'default'],
                min = defaults_analyze['shortestPause', 'low'],
                max = defaults_analyze['shortestPause', 'high'],
                step = defaults_analyze['shortestPause', 'step']) |>
                tooltip("The smallest gap between voiced syllables (ms) that means
                      they shouldn't be merged", options = tooltip_opt)
            ),

            tabPanel(
              "Smoothing",
              sliderInput (
                'smooth',
                'Median smoothing (0 = none) ("smooth")',
                value = defaults_analyze['smooth', 'default'],
                min = defaults_analyze['smooth', 'low'],
                max = defaults_analyze['smooth', 'high'],
                step = defaults_analyze['smooth', 'step']) |>
                tooltip("", options = tooltip_opt),

              numericInput(
                'interpolWin',
                'Interpolation window, ms (0 = none) ("interpolWin")',
                value = defaults_analyze['interpolWin', 'default'],
                min = defaults_analyze['interpolWin', 'low'],
                max = defaults_analyze['interpolWin', 'high'],
                step = defaults_analyze['interpolWin', 'step']) |>
                tooltip("If no pitch candidates are found within ±interpolTol of the
                      median 'best guess' over ±interpolWin, this median is added
                      as an interpolated candidate", options = tooltip_opt),

              numericInput(
                'interpolTol',
                'Interpolation tolerance ("interpolTol")',
                value = defaults_analyze['interpolTol', 'default'],
                min = defaults_analyze['interpolTol', 'low'],
                max = defaults_analyze['interpolTol', 'high'],
                step = defaults_analyze['interpolTol', 'step']) |>
                tooltip("Tolerated deviance from 'best guess' before adding an
                      interpolated candidate: proportion of best guess frequency",
                        options = tooltip_opt),

              sliderInput(
                'interpolCert',
                'Interpolation certainty ("interpolCert")',
                value = defaults_analyze['interpolCert', 'default'],
                min = defaults_analyze['interpolCert', 'low'],
                max = defaults_analyze['interpolCert', 'high'],
                step = defaults_analyze['interpolCert', 'step']) |>
                tooltip("Certainty assigned to interpolated pitch candidates",
                        options = tooltip_opt)
            ),

            tabPanel(
              "Spectrogram",
              accordion(
                sliderInput(
                  'spec_ylim',
                  'Frequency range, kHz ("ylim")',
                  value=c(0, defaults_analyze['spec_ylim', 'default']),
                  min = defaults_analyze['spec_ylim', 'low'],
                  max = defaults_analyze['spec_ylim', 'high'],
                  step = defaults_analyze['spec_ylim', 'step']) |>
                  tooltip("Range of displayed frequencies, kHz", options = tooltip_opt),

                sliderInput(
                  'spec_maxPoints',
                  'Max number of pixels, 10^',
                  value = defaults_analyze['spec_maxPoints', 'default'],
                  min = defaults_analyze['spec_maxPoints', 'low'],
                  max = defaults_analyze['spec_maxPoints', 'high'],
                  step = defaults_analyze['spec_maxPoints', 'step']) |>
                  tooltip("The number of points to plot in the spectrogram
                      (smaller = faster, but low resolution)", options = tooltip_opt),

                numericInput(
                  'spec_cex',
                  'Point size ("pitchPlot=list(cex=...")',
                  value = defaults_analyze['spec_cex', 'default'],
                  min = defaults_analyze['spec_cex', 'low'],
                  max = defaults_analyze['spec_cex', 'high'],
                  step = defaults_analyze['spec_cex', 'step']) |>
                  tooltip("Magnification coefficient controlling the size of points
                      showing pitch candidates", options = tooltip_opt),

                radioButtons(
                  inputId = 'spec_colorTheme',
                  label = 'Color scheme ("colorTheme")',
                  choices = c("Seewave" = "seewave",
                              "Heat" = "heat.colors",
                              "Black & white" = "bw"),
                  selected = 'bw', inline = TRUE, width = NULL),

                numericInput(
                  'nColors',
                  'Number of colors in the palette',
                  value = defaults_analyze['nColors', 'default'],
                  min = defaults_analyze['nColors', 'low'],
                  max = defaults_analyze['nColors', 'high'],
                  step = defaults_analyze['nColors', 'step']),

                sliderInput(
                  'specContrast',
                  'Contrast ("contrast")',
                  value = defaults_analyze['specContrast', 'default'],
                  min = defaults_analyze['specContrast', 'low'],
                  max = defaults_analyze['specContrast', 'high'],
                  step = defaults_analyze['specContrast', 'step']),

                sliderInput(
                  'specBrightness',
                  'Brightness ("brightness")',
                  value = defaults_analyze['specBrightness', 'default'],
                  min = defaults_analyze['specBrightness', 'low'],
                  max = defaults_analyze['specBrightness', 'high'],
                  step = defaults_analyze['specBrightness', 'step']),

                sliderInput(
                  'blur_freq',
                  'Blur: frequency (Hz)',
                  value = defaults_analyze['blur_freq', 'default'],
                  min = defaults_analyze['blur_freq', 'low'],
                  max = defaults_analyze['blur_freq', 'high'],
                  step = defaults_analyze['blur_freq', 'step']) |>
                  tooltip("Gaussian filter of frequency: >0 = blur, <0 = unblur (sharpen)",
                          options = tooltip_opt),

                sliderInput(
                  'blur_time',
                  'Blur: time (ms)',
                  value = defaults_analyze['blur_time', 'default'],
                  min = defaults_analyze['blur_time', 'low'],
                  max = defaults_analyze['blur_time', 'high'],
                  step = defaults_analyze['blur_time', 'step']) |>
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
                    value = defaults_analyze['reass_cex', 'default'],
                    min = defaults_analyze['reass_cex', 'low'],
                    max = defaults_analyze['reass_cex', 'high'],
                    step = defaults_analyze['reass_cex', 'step']),

                  numericInput(
                    'reass_windowLength',
                    'Window length, ms (reassigned only)',
                    value = defaults_analyze['reass_windowLength', 'default'],
                    min = defaults_analyze['reass_windowLength', 'low'],
                    max = defaults_analyze['reass_windowLength', 'high'],
                    step = defaults_analyze['reass_windowLength', 'step']),

                  numericInput(
                    'reass_step',
                    'Step, ms (reassigned only)',
                    value = defaults_analyze['reass_step', 'default'],
                    min = defaults_analyze['reass_step', 'low'],
                    max = defaults_analyze['reass_step', 'high'],
                    step = defaults_analyze['reass_step', 'step'])
                )
              )
            ),

            tabPanel(
              "Oscillogram",
              selectInput(
                'osc',
                'Oscillogram type ("osc")',
                choices = c('linear', 'dB'),
                selected = 'linear', multiple = FALSE),

              sliderInput(
                'osc_maxPoints',
                'Max number of pixels, 10^',
                value = defaults_analyze['osc_maxPoints', 'default'],
                min = defaults_analyze['osc_maxPoints', 'low'],
                max = defaults_analyze['osc_maxPoints', 'high'],
                step = defaults_analyze['osc_maxPoints', 'step']) |>
                tooltip("The number of points to plot in the oscillogram
                      (smaller = faster, but low resolution)", options = tooltip_opt)
            )
          )
        )
      ),  # end of column "Sidebar"

      fluidRow(
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
