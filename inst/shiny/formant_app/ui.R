# formant_app() UI

library(bslib)

# Retrieve defaults from package environment.
# Fallback is provided for direct shinyapps.io deployment where the wrapper isn't executed.
if (exists(".soundgen_env", envir = asNamespace("soundgen")) &&
    !is.null(soundgen:::.soundgen_env$formant$def)) {
  df = soundgen:::.soundgen_env$formant$def_form
  def = soundgen:::.soundgen_env$formant$def
} else {
  # Fallback for shinyapps.io deployment
  df = soundgen:::def_form
  def = as.list(df[, 'default'])
  names(def) = rownames(df)
  def$spectrum_xlim = c(0, def$spectrum_xlim)
  def$spec_ylim = c(0, def$spec_ylim)
  def = c(def, list(
    'wn' = 'gaussian', 'wn_lpc' = 'gaussian', 'spec_colorTheme' = 'bw',
    'osc' = 'linear', 'vtl_method' = 'regression', 'speedSound' = 35400,
    'coeffs' = '', 'interceptZero' = TRUE, 'tube' = 'closed-open',
    'summaryFun' = 'median', 'audioMethod' = 'Browser', 'specType' = 'spectrum',
    'fmtSpacePlot' = 'vowelSpace', 'spec_col' = '#FF0000FF',
    'normalizeInput' = TRUE, 'adaptivePitch' = TRUE, 'spectrum_plotSynth' = TRUE
  ))
}

tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))

ui = bslib::page_fluid(
  # css
  tags$head(
    shiny::includeCSS(file.path("www", "formant_app.css"))
  ),

  # js
  shiny::includeScript(file.path("www", "formant_app.js")),
  shiny::includeScript(file.path("www", "formant_app_shinyjs.js")),
  shinyjs::useShinyjs(),

  shinyjs::extendShinyjs(
    script = 'formant_app_shinyjs.js',
    functions = c('playme_js', 'stopAudio_js', 'play_synth_uri',
                  'clearBrush', 'inheritSize', 'scrollBar')
  ),

  # html
  tags$div(
    id = 'gridCont',

    tags$div(
      id = 'left',

      fluidRow(
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "Sidebar",
            width = "300px",
            bg = "Azure",

            tabsetPanel(
              id = 'parGroup',
              navbarMenu(
                "Analysis",
                tabPanel(
                  "General",
                  actionButton('reset_to_def', "Reset ALL to defaults") |>
                    bslib::tooltip("Reset all settings to default values", options = tooltip_opt),

                  radioButtons(
                    'audioMethod',
                    label = bslib::tooltip("Play analyzed audio with",
                                    "Play audio with javascript or with R (browser-independent,
                                  but then the cursor doesn't move, and you can't stop playback)",
                                    options = tooltip_opt),
                    choices = list('Browser' = 'Browser', 'R' = 'R'),
                    selected = def$audioMethod, inline = TRUE, width = NULL),

                  sliderInput(
                    'pitch',
                    'Playback pitch, Hz',
                    value = def$pitch,
                    min = df['pitch', 'low'],
                    max = df['pitch', 'high'],
                    step = df['pitch', 'step']),

                  checkboxInput(
                    'adaptivePitch',
                    'Adapt playback pitch by imflied VTL',
                    value = def$adaptivePitch),

                  sliderInput(
                    'samplingRate_mult',
                    'Playback speed, 2 ^',
                    value = def$samplingRate_mult,
                    min = df['samplingRate_mult', 'low'],
                    max = df['samplingRate_mult', 'high'],
                    step = df['samplingRate_mult', 'step']) |>
                    bslib::tooltip("Speed up or slow down the original and synthesized audio
                          for playback purposes only, without affecting the measurements
                          (eg to make it sound more human-like)", options = tooltip_opt),

                  checkboxInput(
                    'normalizeInput',
                    'Normalize for peak amplitude',
                    value = def$normalizeInput)
                ),

                tabPanel(
                  "LPC",
                  sliderInput(
                    'nFormants',
                    'Number of formants',
                    value = def$nFormants,
                    min = df['nFormants', 'low'],
                    max = df['nFormants', 'high'],
                    step = df['nFormants', 'step']),

                  numericInput(
                    'silence',
                    'Silence threshold (0 to 1)',
                    value = def$silence,
                    min = df['silence', 'low'],
                    max = df['silence', 'high'],
                    step = df['silence', 'step']) |>
                    bslib::tooltip("Frames below this threshold are not analyzed", options = tooltip_opt),

                  selectInput(
                    'summaryFun',
                    'Average measure per annotated region',
                    choices = list('Median' = 'median',
                                   'Mean' = 'mean'),
                    selected = def$summaryFun),

                  textInput(
                    'coeffs',
                    'Number of LPC coefficients',
                    value = def$coeffs,
                    placeholder = 'round(samplingRate/1000) + 3') |>
                    bslib::tooltip("The number of LPC coefficients (see ?phonTools::findformants)",
                            options = tooltip_opt),

                  sliderInput(
                    'minformant',
                    'Minimum formant frequency',
                    value = def$minformant,
                    min = df['minformant', 'low'],
                    max = df['minformant', 'high'],
                    step = df['minformant', 'step']) |>
                    bslib::tooltip("Lowest accepted formant frequency (see ?phonTools::findformants)",
                            options = tooltip_opt),

                  sliderInput(
                    'maxbw',
                    'Maximum formant bandwidth',
                    value = def$maxbw,
                    min = df['maxbw', 'low'],
                    max = df['maxbw', 'high'],
                    step = df['maxbw', 'step']) |>
                    bslib::tooltip("Maximum accepted formant bandwidth (see ?phonTools::findformants)",
                            options = tooltip_opt)
                ),

                tabPanel(
                  "Windowing",
                  numericInput(
                    'windowLength_lpc',
                    'Window length, ms',
                    value = def$windowLength_lpc,
                    min = df['windowLength_lpc', 'low'],
                    max = df['windowLength_lpc', 'high'],
                    step = df['windowLength_lpc', 'step']) |>
                    bslib::tooltip("Length of STFT window for LPC analysis, ms. Independent of
                          the window used for plotting the spectrogram", options = tooltip_opt),

                  numericInput(
                    'step_lpc',
                    'Step, ms',
                    value = def$step_lpc,
                    min = df['step_lpc', 'low'],
                    max = df['step_lpc', 'high'],
                    step = df['step_lpc', 'step']) |>
                    bslib::tooltip("Step between analysis frames, ms", options = tooltip_opt),

                  sliderInput(
                    'dynamicRange_lpc',
                    'Dynamic range, dB',
                    value = def$dynamicRange_lpc,
                    min = df['dynamicRange_lpc', 'low'],
                    max = df['dynamicRange_lpc', 'high'],
                    step = df['dynamicRange_lpc', 'step']),

                  selectInput(
                    'wn_lpc',
                    'Type of STFT window',
                    choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                                'hamming', 'hanning', 'rectangle'),
                    selected = def$wn_lpc, multiple = FALSE)
                ),

                tabPanel(
                  "Vocal tract",
                  selectInput(
                    'vtl_method',
                    bslib::tooltip(trigger = 'Method for estimating VTL',
                            "Method of calculating vocal tract length (VTL). See ?estimateVTL",
                            options = tooltip_opt),
                    choices = list('Regression' = 'regression',
                                   'Mean formant dispersion' = 'meanDispersion',
                                   'Mean formant' = 'meanFormant'),
                    selected = def$vtl_method),

                  numericInput(
                    'speedSound',
                    'Speed of sound, cm/s',
                    value = def$speedSound,
                    min = 1, max = 100000, step = 1) |>
                    bslib::tooltip("VTL estimate depends on the assumed speed of sound
                          inside the vocal tract", options = tooltip_opt),

                  checkboxInput(
                    'interceptZero',
                    'Set intercept to zero',
                    value = def$interceptZero) |>
                    bslib::tooltip("Set the regression intercept to zero to reduce the influence
                          of lower formants (careful with whether you are using
                          closed-open or open-open tube approximation)", options = tooltip_opt),

                  selectInput(
                    'tube',
                    'Tube approximation of vocal tract',
                    choices = list('Closed-open' = 'closed-open',
                                   'Open-open / closed-closed' = 'open-open'),
                    selected = def$tube) |>
                    bslib::tooltip("The vocal tract can be modeled as a uniform tube either
                          closed at one end and open at the other, or else as having both
                          ends either closed or open", options = tooltip_opt)
                )
              ),

              navbarMenu(
                "Plotting",
                tabPanel(
                  "Spectrogram",
                  bslib::accordion(
                    sliderInput(
                      'spec_ylim',
                      'Frequency range, kHz',
                      value = def$spec_ylim,
                      min = df['spec_ylim', 'low'],
                      max = df['spec_ylim', 'high'],
                      step = df['spec_ylim', 'step']),

                    numericInput(
                      'windowLength',
                      'Window length, ms',
                      value = def$windowLength,
                      min = df['windowLength', 'low'],
                      max = df['windowLength', 'high'],
                      step = df['windowLength', 'step']),

                    numericInput(
                      'step',
                      'Step, ms',
                      value = def$step,
                      min = df['step', 'low'],
                      max = df['step', 'high'],
                      step = df['step', 'step']),

                    radioButtons(
                      inputId = 'specType',
                      label = bslib::tooltip('Spectrogram method',
                                      "Spectrogram type, argument 'specType' in spectrogram().
                                    Visualization only, does NOT affect formant tracking
                                    with LPC", options = tooltip_opt),
                      choices = c("Ordinary FFT" = "spectrum",
                                  "Reassigned" = "reassigned"),
                      selected = def$specType, inline = TRUE, width = NULL),

                    sliderInput(
                      'dynamicRange',
                      'Dynamic range, dB',
                      value = def$dynamicRange,
                      min = df['dynamicRange', 'low'],
                      max = df['dynamicRange', 'high'],
                      step = df['dynamicRange', 'step']),

                    radioButtons(
                      inputId = 'spec_colorTheme',
                      label = 'Color scheme',
                      choices = c("Seewave" = "seewave",
                                  "Heat" = "heat.colors",
                                  "Black & white" = "bw"),
                      selected = def$spec_colorTheme, inline = TRUE, width = NULL),

                    numericInput(
                      'nColors',
                      'Number of colors in the palette',
                      value = def$nColors,
                      min = df['nColors', 'low'],
                      max = df['nColors', 'high'],
                      step = df['nColors', 'step']),

                    sliderInput(
                      'specContrast',
                      'Contrast',
                      value = def$specContrast,
                      min = df['specContrast', 'low'],
                      max = df['specContrast', 'high'],
                      step = df['specContrast', 'step']),

                    sliderInput(
                      'specBrightness',
                      'Brightness',
                      value = def$specBrightness,
                      min = df['specBrightness', 'low'],
                      max = df['specBrightness', 'high'],
                      step = df['specBrightness', 'step']),

                    bslib::accordion_panel(
                      "Advanced",
                      sliderInput(
                        'spec_cex',
                        'Point size of formant tracks',
                        value = def$spec_cex,
                        min = df['spec_cex', 'low'],
                        max = df['spec_cex', 'high'],
                        step = df['spec_cex', 'step']) |>
                        bslib::tooltip("Magnification coefficient controlling the size
                              of points showing formant candidates", options = tooltip_opt),

                      textInput(
                        'spec_col',
                        'Color of formant tracks',
                        value = def$spec_col,
                        placeholder = '#FF0000FF'
                      ),

                      sliderInput(
                        'blur_freq',
                        'Blur: frequency (Hz)',
                        value = def$blur_freq,
                        min = df['blur_freq', 'low'],
                        max = df['blur_freq', 'high'],
                        step = df['blur_freq', 'step']) |>
                        bslib::tooltip("Gaussian filter of frequency: >0 = blur, <0 = unblur (sharpen)",
                                options = tooltip_opt),

                      sliderInput(
                        'blur_time',
                        'Blur: time (ms)',
                        value = def$blur_time,
                        min = df['blur_time', 'low'],
                        max = df['blur_time', 'high'],
                        step = df['blur_time', 'step']) |>
                        bslib::tooltip("Gaussian filter of time: >0 = blur, <0 = unblur (sharpen)",
                                options = tooltip_opt),

                      sliderInput(
                        'reass_cex',
                        'Point size (reassigned spectrogram only)',
                        value = def$reass_cex,
                        min = df['reass_cex', 'low'],
                        max = df['reass_cex', 'high'],
                        step = df['reass_cex', 'step']),

                      sliderInput(
                        'zp',
                        'Zero padding, points 2 ^ n',
                        value = def$zp,
                        min = df['zp', 'low'],
                        max = df['zp', 'high'],
                        step = df['zp', 'step']) |>
                        bslib::tooltip("Zero padding: 8 means 2^8 = 256, etc.", options = tooltip_opt),

                      selectInput(
                        'wn',
                        'Window type',
                        choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                                    'hamming', 'hanning', 'rectangle'),
                        selected = def$wn, multiple = FALSE),

                      sliderInput(
                        'spec_maxPoints',
                        'Max number of pixels, 10^',
                        value = def$spec_maxPoints,
                        min = df['spec_maxPoints', 'low'],
                        max = df['spec_maxPoints', 'high'],
                        step = df['spec_maxPoints', 'step']) |>
                        bslib::tooltip("The number of points to plot in the spectrogram
                              (smaller = faster, but low resolution)", options = tooltip_opt))
                  )
                ),

                tabPanel(
                  "Oscillogram",
                  selectInput(
                    'osc',
                    'Oscillogram type',
                    choices = c('linear', 'dB'),
                    selected = def$osc, multiple = FALSE),

                  sliderInput(
                    'osc_maxPoints',
                    'Max number of pixels, 10^',
                    value = def$osc_maxPoints,
                    min = df['osc_maxPoints', 'low'],
                    max = df['osc_maxPoints', 'high'],
                    step = df['osc_maxPoints', 'step']) |>
                    bslib::tooltip("The number of points to plot in the oscillogram
                          (smaller = faster, but low resolution)", options = tooltip_opt)
                ),

                tabPanel(
                  "Spectrum",
                  sliderInput(
                    'spectrum_len',
                    'Spectral resolution, samples / kHz',
                    value = def$spectrum_len,
                    min = df['spectrum_len', 'low'],
                    max = df['spectrum_len', 'high'],
                    step = df['spectrum_len', 'step']) |>
                    bslib::tooltip("The number of independent points per kHz in the spectrum
                          (less = more smoothing)", options = tooltip_opt),

                  sliderInput(
                    'spectrum_xlim',
                    'Frequency range, kHz',
                    value = def$spectrum_xlim,
                    min = df['spectrum_xlim', 'low'],
                    max = df['spectrum_xlim', 'high'],
                    step = df['spectrum_xlim', 'step']) |>
                    bslib::tooltip("Range of displayed frequencies, kHz
                          (normally tied to spectrogram range)", options = tooltip_opt),

                  checkboxInput(
                    'spectrum_plotSynth',
                    'Plot the spectrum of the synthetic vowel',
                    value = def$spectrum_plotSynth)
                )
              )
            )
          ),  # end of column "sidebar"

          fluidRow(
            style="margin-top: -15px; margin-bottom: -15px;",
            column(
              width = 3,
              fileInput(
                inputId = "loadAudio", label = NULL,
                multiple = TRUE, buttonLabel = 'Load audio',
                placeholder = '...')
            ),

            column(
              width = 6,
              tags$div(
                actionButton(
                  inputId = "lastFile", label = "<<",
                  class = "buttonFile") |>
                  bslib::tooltip("Save and return to the previous file (PageUp)", options = tooltip_opt),

                tags$strong(shiny::uiOutput("fileN", inline = TRUE)),

                actionButton(
                  inputId = "nextFile", label = ">>",
                  class = "buttonFile") |>
                  bslib::tooltip("Save and proceed to the next file (PageDown)", options = tooltip_opt)
              ),

              selectInput('fileList', label = NULL, choices = list(),
                          selectize = FALSE  # to enable custom css
              )
            ),

            column(
              width = 3,
              shiny::uiOutput("htmlAudio"),  # not actually shown
              downloadButton(
                outputId = "saveRes", label = "",
                style="color: blue; background-color: orange"),
              actionButton(
                'about', label = '?', width = "10px")
            )
          ),

          fluidRow(
            style="margin-top: -15px; margin-bottom: -15px;",
            column(
              width = 2,
              actionButton(
                inputId = 'zoomOut_freq',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Zoom out frequency (-)", options = tooltip_opt),

              actionButton(
                inputId = 'zoomIn_freq',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Zoom in frequency (+)", options = tooltip_opt)
            ),

            column(
              width = 4,
              actionButton(
                inputId = "selection_stop",
                label = HTML("<img src='icons/stop.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Stop playback", options = tooltip_opt),

              actionButton(
                inputId = "selection_play",
                label = HTML("<img src='icons/play.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Play selection (SPACEBAR)", options = tooltip_opt),

              actionButton(
                inputId = "selection_annotate",
                label = HTML("<img src='icons/annotate.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Create a new annotation (A or DOUBLE-CLICK)", options = tooltip_opt),

              actionButton(
                inputId = "selection_delete",
                label = HTML("<img src='icons/delete.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Remove annotation (DELETE / BACKSPACE)", options = tooltip_opt),

              actionButton(
                inputId = "selection_snap",
                label = HTML("<img src='icons/snap.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Time active annotation to coincide with selection (T)",
                        options = tooltip_opt),
            ),

            column(
              width = 6,
              actionButton(
                inputId = 'scrollLeft',
                label = HTML("<img src='icons/backward.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Scroll left (arrow LEFT)", options = tooltip_opt),

              actionButton(
                inputId = 'zoomOut',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Zoom out time (arrow DOWN)", options = tooltip_opt),

              actionButton(
                inputId = "zoomToSel",
                label = HTML("<img src='icons/zoomSel.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Zoom to selection (S)", options = tooltip_opt),

              actionButton(
                inputId = 'zoomIn',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Zoom in time (arrow UP)", options = tooltip_opt),

              actionButton(
                inputId = 'scrollRight',
                label = HTML("<img src='icons/forward.png' width = '20px'>"),
                class = "buttonInline") |>
                bslib::tooltip("Scroll right (arrow RIGHT)", options = tooltip_opt)
            )
          ),

          fluidRow(
            tags$div(
              id = 'plotRow',

              tags$div(
                id = 'specDiv',
                shiny::plotOutput('spectrogram'),
                shiny::plotOutput('specSlider'),
                shiny::plotOutput(
                  'specOver',
                  click = "spectrogram_click",
                  dblclick = shiny::dblclickOpts(id = "spectrogram_dblclick"),
                  hover = shiny::hoverOpts(id = "spectrogram_hover"),
                  brush = shiny::brushOpts(id = 'spectrogram_brush', opacity = 0.25, resetOnNew = FALSE))
              ),

              tags$div(
                id = 'scrollBarCont',
                tags$div(
                  id = 'scrollBar'
                )
              ),

              shiny::plotOutput(
                'oscillogram', height = '100px'  # auto to inherit from css in the header
              ),

              shiny::plotOutput(
                'ann_plot', height = '60px',
                click = "ann_click",
                dblclick = shiny::dblclickOpts(id = "ann_dblclick")
              )
            )
          )
        )
      )
    ),

    tags$div(
      id = 'right',
      fluidRow(
        column(
          width = 10,
          shiny::uiOutput('fButtons', style = "height: 60px;")
        ),

        column(
          width = 2,
          actionButton(
            inputId = 'synthBtn',
            label = HTML("<img src='icons/synth.png' width = '30px'>"),
            class = "buttonInline") |>
            bslib::tooltip("Synthesize and play a vowel with formants as measured
                    in the current annotation (P). Check ?playme() if no sound",
                    options = tooltip_opt)
        )
      ),

      fluidRow(
        tags$div(
          id = 'spectrumDiv',
          shiny::plotOutput(
            'spectrum',
            height = "300px",
            click = "spectrum_click",
            dblclick = shiny::dblclickOpts(id = "spectrum_dblclick"),
            hover = shiny::hoverOpts(id = "spectrum_hover"))
        )
      ),

      fluidRow(
        tags$div(
          id = 'fmtSpaceDiv',
          radioButtons(
            'fmtSpacePlot',
            label = NULL,
            choices = list('Vowel space' = 'vowelSpace', 'Regression' = 'regression'),
            selected = def$fmtSpacePlot, inline = TRUE, width = NULL
          ),

          shiny::plotOutput(
            'fmtSpace', height = "175px")
        )
      ),

      fluidRow(
        shiny::tableOutput('ann_table')  # dataTableOutput('ann_table')
      )
    )
  )
)
