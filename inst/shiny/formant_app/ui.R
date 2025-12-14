# formant_app()
#
library(bslib)
tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))

ui = page_fluid(
  # css
  tags$head(
    shiny::includeCSS("www/formant_app.css")
  ),
  # theme = bs_theme(version = 5, bootswatch = "simplex"),

  # js
  includeScript("www/formant_app.js"),
  includeScript("www/formant_app_shinyjs.js"),
  shinyjs::useShinyjs(),
  # handy for calling js functions from R, eg for a collapsible side panel - see
  # https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  # import some js functions to be invoked from R with shinyjs
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'www/formant_app_shinyjs.js',
    functions = c('playme_js', 'stopAudio_js', 'play_file',
                  'clearBrush', 'inheritSize', 'scrollBar')
  ),

  # html
  tags$div(
    id = 'gridCont',

    tags$div(
      id = 'left',

      fluidRow(
        layout_sidebar(
          sidebar = sidebar(
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
                    tooltip("Reset all settings to default values", options = tooltip_opt),

                  radioButtons(
                    'audioMethod',
                    label = tooltip("Play analyzed audio with",
                                    "Play audio with javascript (recommended in Firefox,
                                  doesn't work in Chrome) or with R (browser-independent,
                                  but then the cursor doesn't move, and you can't stop playback)",
                                    options = tooltip_opt),
                    choices = list('Browser' = 'Browser', 'R' = 'R'),
                    selected = 'Browser', inline = TRUE, width = NULL),

                  sliderInput(
                    'pitch',
                    'Playback pitch, Hz',
                    value = 150,
                    min = 20,
                    max = 500,
                    step = 5),

                  checkboxInput(
                    'adaptivePitch',
                    'Adapt playback pitch by imflied VTL',
                    value = TRUE),

                  sliderInput(
                    'samplingRate_mult',
                    'Playback speed, 2 ^',
                    value = def_form['samplingRate_mult', 'default'],
                    min = def_form['samplingRate_mult', 'low'],
                    max = def_form['samplingRate_mult', 'high'],
                    step = def_form['samplingRate_mult', 'step']) |>
                    tooltip("Speed up or slow down the original and synthesized audio
                          for playback purposes only, without affecting the measurements
                          (eg to make it sound more human-like)", options = tooltip_opt),

                  checkboxInput(
                    'normalizeInput',
                    'Normalize for peak amplitude',
                    value = TRUE)
                ),

                tabPanel(
                  "LPC",
                  sliderInput(
                    'nFormants',
                    'Number of formants',
                    value = def_form['nFormant', 'default'],
                    min = def_form['nFormant', 'low'],
                    max = def_form['nFormant', 'high'],
                    step = def_form['nFormant', 'step']),

                  numericInput(
                    'silence',
                    'Silence threshold (0 to 1)',
                    value = def_form['silence', 'default'],
                    min = def_form['silence', 'low'],
                    max = def_form['silence', 'high'],
                    step = def_form['silence', 'step']) |>
                    tooltip("Frames below this threshold are not analyzed", options = tooltip_opt),

                  selectInput(
                    'summaryFun',
                    'Average measure per annotated region',
                    choices = list('Median' = 'median',
                                   'Mean' = 'mean'),
                    selected = 'median'),

                  textInput(
                    'coeffs',
                    'Number of LPC coefficients',
                    value = '',
                    placeholder = 'round(samplingRate/1000) + 3') |>
                    tooltip("The number of LPC coefficients (see ?phonTools::findformants)",
                            options = tooltip_opt),

                  sliderInput(
                    'minformant',
                    'Minimum formant frequency',
                    value = def_form['minformant', 'default'],
                    min = def_form['minformant', 'low'],
                    max = def_form['minformant', 'high'],
                    step = def_form['minformant', 'step']) |>
                    tooltip("Lowest accepted formant frequency (see ?phonTools::findformants)",
                            options = tooltip_opt),

                  sliderInput(
                    'maxbw',
                    'Maximum formant bandwidth',
                    value = def_form['maxbw', 'default'],
                    min = def_form['maxbw', 'low'],
                    max = def_form['maxbw', 'high'],
                    step = def_form['maxbw', 'step']) |>
                    tooltip("Maximum accepted formant bandwidth (see ?phonTools::findformants)",
                            options = tooltip_opt)
                ),

                tabPanel(
                  "Windowing",
                  numericInput(
                    'windowLength_lpc',
                    'Window length, ms',
                    value = def_form['windowLength_lpc', 'default'],
                    min = def_form['windowLength_lpc', 'low'],
                    max = def_form['windowLength_lpc', 'high'],
                    step = def_form['windowLength_lpc', 'step']) |>
                    tooltip("Length of STFT window for LPC analysis, ms. Independent of
                          the window used for plotting the spectrogram", options = tooltip_opt),

                  numericInput(
                    'step_lpc',
                    'Step, ms',
                    value = def_form['step_lpc', 'default'],
                    min = def_form['step_lpc', 'low'],
                    max = def_form['step_lpc', 'high'],
                    step = def_form['step_lpc', 'step']) |>
                    tooltip("Step between analysis frames, ms", options = tooltip_opt),

                  sliderInput(
                    'dynamicRange_lpc',
                    'Dynamic range, dB',
                    value = def_form['dynamicRange_lpc', 'default'],
                    min = def_form['dynamicRange_lpc', 'low'],
                    max = def_form['dynamicRange_lpc', 'high'],
                    step = def_form['dynamicRange_lpc', 'step']),

                  sliderInput(
                    'zp_lpc',
                    'Zero padding, points 2 ^ n',
                    value = def_form['zp_lpc', 'default'],
                    min = def_form['zp', 'low'],
                    max = def_form['zp_lpc', 'high'],
                    step = def_form['zp_lpc', 'step']) |>
                    tooltip("Zero padding: 8 means 2^8 = 256, etc.", options = tooltip_opt),

                  selectInput(
                    'wn_lpc',
                    'Type of STFT window',
                    choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                                'hamming', 'hanning', 'rectangle'),
                    selected = 'gaussian', multiple = FALSE)
                ),

                tabPanel(
                  "Vocal tract",
                  selectInput(
                    'vtl_method',
                    tooltip(trigger = 'Method for estimating VTL',
                            "Method of calculating vocal tract length (VTL). See ?estimateVTL",
                            options = tooltip_opt),
                    choices = list('Regression' = 'regression',
                                   'Mean formant dispersion' = 'meanDispersion',
                                   'Mean formant' = 'meanFormant'),
                    selected = 'regression'),

                  numericInput(
                    'speedSound',
                    'Speed of sound, cm/s',
                    value = '35400',
                    min = 1, max = 100000, step = 1) |>
                    tooltip("VTL estimate depends on the assumed speed of sound
                          inside the vocal tract", options = tooltip_opt),

                  checkboxInput(
                    'interceptZero',
                    'Set intercept to zero',
                    value = TRUE) |>
                    tooltip("Set the regression intercept to zero to reduce the influence
                          of lower formants (careful with whether you are using
                          closed-open or open-open tube approximation)", options = tooltip_opt),

                  selectInput(
                    'tube',
                    'Tube approximation of vocal tract',
                    choices = list('Closed-open' = 'closed-open',
                                   'Open-open / closed-closed' = 'open-open'),
                    selected = 'closed-open') |>
                    tooltip("The vocal tract can be modeled as a uniform tube either
                          closed at one end and open at the other, or else as having both
                          ends either closed or open", options = tooltip_opt)
                )
              ),

              navbarMenu(
                "Plotting",
                tabPanel(
                  "Spectrogram",
                  accordion(
                    sliderInput(
                      'spec_ylim',
                      'Frequency range, kHz',
                      value = c(0, def_form['spec_ylim', 'default']),
                      min = def_form['spec_ylim', 'low'],
                      max = def_form['spec_ylim', 'high'],
                      step = def_form['spec_ylim', 'step']),

                    numericInput(
                      'windowLength',
                      'Window length, ms',
                      value = def_form['windowLength', 'default'],
                      min = def_form['windowLength', 'low'],
                      max = def_form['windowLength', 'high'],
                      step = def_form['windowLength', 'step']),

                    numericInput(
                      'step',
                      'Step, ms',
                      value = def_form['step', 'default'],
                      min = def_form['step', 'low'],
                      max = def_form['step', 'high'],
                      step = def_form['step', 'step']),

                    radioButtons(
                      inputId = 'specType',
                      label = tooltip('Spectrogram method',
                                      "Spectrogram type, argument 'specType' in spectrogram().
                                    Visualization only, does NOT affect formant tracking
                                    with LPC", options = tooltip_opt),
                      choices = c("Ordinary FFT" = "spectrum",
                                  "Reassigned" = "reassigned"),
                      selected = 'spectrum', inline = TRUE, width = NULL),

                    sliderInput(
                      'dynamicRange',
                      'Dynamic range, dB',
                      value = def_form['dynamicRange', 'default'],
                      min = def_form['dynamicRange', 'low'],
                      max = def_form['dynamicRange', 'high'],
                      step = def_form['dynamicRange', 'step']),

                    radioButtons(
                      inputId = 'spec_colorTheme',
                      label = 'Color scheme',
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
                      'Contrast',
                      value = def_form['specContrast', 'default'],
                      min = def_form['specContrast', 'low'],
                      max = def_form['specContrast', 'high'],
                      step = def_form['specContrast', 'step']),

                    sliderInput(
                      'specBrightness',
                      'Brightness',
                      value = def_form['specBrightness', 'default'],
                      min = def_form['specBrightness', 'low'],
                      max = def_form['specBrightness', 'high'],
                      step = def_form['specBrightness', 'step']),

                    accordion_panel(
                      "Advanced",
                      sliderInput(
                        'spec_cex',
                        'Point size of formant tracks',
                        value = 1,
                        min = 0,
                        max = 3,
                        step = .1) |>
                        tooltip("Magnification coefficient controlling the size
                              of points showing formant candidates", options = tooltip_opt),

                      textInput(
                        'spec_col',
                        'Color of formant tracks',
                        value = '#FF0000FF', # red
                        placeholder = '#FF0000FF'
                      ),

                      sliderInput(
                        'blur_freq',
                        'Blur: frequency (Hz)',
                        value = def_form['blur_freq', 'default'],
                        min = def_form['blur_freq', 'low'],
                        max = def_form['blur_freq', 'high'],
                        step = def_form['blur_freq', 'step']) |>
                        tooltip("Gaussian filter of frequency: >0 = blur, <0 = unblur (sharpen)",
                                options = tooltip_opt),

                      sliderInput(
                        'blur_time',
                        'Blur: time (ms)',
                        value = def_form['blur_time', 'default'],
                        min = def_form['blur_time', 'low'],
                        max = def_form['blur_time', 'high'],
                        step = def_form['blur_time', 'step']) |>
                        tooltip("Gaussian filter of time: >0 = blur, <0 = unblur (sharpen)",
                                options = tooltip_opt),

                      sliderInput(
                        'reass_cex',
                        'Point size (reassigned spectrogram only)',
                        value = def_form['reass_cex', 'default'],
                        min = def_form['reass_cex', 'low'],
                        max = def_form['reass_cex', 'high'],
                        step = def_form['reass_cex', 'step']),

                      sliderInput(
                        'zp',
                        'Zero padding, points 2 ^ n',
                        value = def_form['zp', 'default'],
                        min = def_form['zp', 'low'],
                        max = def_form['zp', 'high'],
                        step = def_form['zp', 'step']) |>
                        tooltip("Zero padding: 8 means 2^8 = 256, etc.", options = tooltip_opt),

                      selectInput(
                        'wn',
                        'Window type',
                        choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                                    'hamming', 'hanning', 'rectangle'),
                        selected = 'gaussian', multiple = FALSE),

                      sliderInput(
                        'spec_maxPoints',
                        'Max number of pixels, 10^',
                        value = def_form['spec_maxPoints', 'default'],
                        min = def_form['spec_maxPoints', 'low'],
                        max = def_form['spec_maxPoints', 'high'],
                        step = def_form['spec_maxPoints', 'step']) |>
                        tooltip("The number of points to plot in the spectrogram
                              (smaller = faster, but low resolution)", options = tooltip_opt))
                  )
                ),

                tabPanel(
                  "Oscillogram",
                  selectInput(
                    'osc',
                    'Oscillogram type',
                    choices = c('linear', 'dB'),
                    selected = 'linear', multiple = FALSE),

                  sliderInput(
                    'osc_maxPoints',
                    'Max number of pixels, 10^',
                    value = def_form['osc_maxPoints', 'default'],
                    min = def_form['osc_maxPoints', 'low'],
                    max = def_form['osc_maxPoints', 'high'],
                    step = def_form['osc_maxPoints', 'step']) |>
                    tooltip("The number of points to plot in the oscillogram
                          (smaller = faster, but low resolution)", options = tooltip_opt)
                ),

                tabPanel(
                  "Spectrum",
                  sliderInput(
                    'spectrum_smooth',
                    'Smoothing',
                    value = def_form['spectrum_smooth', 'default'],
                    min = def_form['spectrum_smooth', 'low'],
                    max = def_form['spectrum_smooth', 'high'],
                    step = def_form['spectrum_smooth', 'step'],
                    width = '200px') |>
                    tooltip("Range of displayed frequencies, kHz
                          (normally tied to spectrogram range)", options = tooltip_opt),

                  sliderInput(
                    'spectrum_xlim',
                    'Frequency range, kHz',
                    value = c(0, def_form['spectrum_xlim', 'default']),
                    min = def_form['spectrum_xlim', 'low'],
                    max = def_form['spectrum_xlim', 'high'],
                    step = def_form['spectrum_xlim', 'step']),

                  sliderInput(
                    'spectrum_len',
                    'Resolution, points',
                    value = def_form['spectrum_len', 'default'],
                    min = def_form['spectrum_len', 'low'],
                    max = def_form['spectrum_len', 'high'],
                    step = def_form['spectrum_len', 'step']) |>
                    tooltip("The number of points to plot in the spectrum
                          (smaller = faster, but low resolution)", options = tooltip_opt),

                  checkboxInput(
                    'spectrum_plotSynth',
                    'Plot the spectrum of the synthetic vowel',
                    value = TRUE)
                )
              )
            )
          ),  # end of column "sidebar"

          fluidRow(
            column(
              width = 3,
              fileInput(
                inputId = "loadAudio", label = NULL,
                # accept = c('audio/wav', 'audio/wave', 'audio/x-wave', 'audio/vnd.wave',
                #            'audio/mpeg', 'audio/mpeg4-generic', 'audio/mpeg3', 'audio/x-mpeg-3',
                #            'video/mpeg', 'video/x-mpeg'),
                multiple = TRUE, buttonLabel = 'Load audio',
                placeholder = '...')
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
                outputId = "saveRes", label = "",
                style="color: blue; background-color: orange"),
              actionButton(
                'about', label = '?', width = "10px")
            )
          ),

          fluidRow(
            column(
              width = 2,
              actionButton(
                inputId = 'zoomOut_freq',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Zoom out frequency (-)", options = tooltip_opt),

              actionButton(
                inputId = 'zoomIn_freq',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Zoom in frequency (+)", options = tooltip_opt)
            ),

            column(
              width = 4,
              actionButton(
                inputId = "selection_stop",
                label = HTML("<img src='icons/stop.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Stop playback", options = tooltip_opt),

              actionButton(
                inputId = "selection_play",
                label = HTML("<img src='icons/play.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Play selection (SPACEBAR)", options = tooltip_opt),

              actionButton(
                inputId = "selection_annotate",
                label = HTML("<img src='icons/annotate.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Create a new annotation (A or DOUBLE-CLICK)", options = tooltip_opt),

              actionButton(
                inputId = "selection_delete",
                label = HTML("<img src='icons/delete.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Remove annotation (DELETE / BACKSPACE)", options = tooltip_opt)
            ),

            column(
              width = 6,
              actionButton(
                inputId = 'scrollLeft',
                label = HTML("<img src='icons/backward.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Scroll left (arrow LEFT)", options = tooltip_opt),

              actionButton(
                inputId = 'zoomOut',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Zoom out time (arrow DOWN)", options = tooltip_opt),

              actionButton(
                inputId = "zoomToSel",
                label = HTML("<img src='icons/zoomSel.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Zoom to selection (S)", options = tooltip_opt),

              actionButton(
                inputId = 'zoomIn',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Zoom in time (arrow UP)", options = tooltip_opt),

              actionButton(
                inputId = 'scrollRight',
                label = HTML("<img src='icons/forward.png' width = '20px'>"),
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

              plotOutput(
                'oscillogram', height = '100px'  # auto to inherit from css in the header
              ),

              plotOutput(
                'ann_plot', height = '60px',
                click = "ann_click",
                dblclick = dblclickOpts(id = "ann_dblclick")
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
          uiOutput('fButtons', style = "height: 60px;")
        ),

        column(
          width = 2,
          uiOutput("synthAudio"),  # not actually shown
          actionButton(
            inputId = 'synthBtn',
            label = HTML("<img src='icons/synth.png' width = '30px'>"),
            class = "buttonInline") |>
            tooltip("Synthesize and play a vowel with formants as measured
                    in the current annotation (P). Check ?playme() if no sound",
                    options = tooltip_opt)
        )
      ),

      fluidRow(
        tags$div(
          id = 'spectrumDiv',
          plotOutput(
            'spectrum',
            height = "300px",
            click = "spectrum_click",
            dblclick = dblclickOpts(id = "spectrum_dblclick"),
            hover = hoverOpts(id = "spectrum_hover"))
        )
      ),

      fluidRow(
        tags$div(
          id = 'fmtSpaceDiv',
          radioButtons(
            'fmtSpacePlot',
            label = NULL,
            choices = list('Vowel space' = 'vowelSpace', 'Regression' = 'regression'),
            selected = 'vowelSpace', inline = TRUE, width = NULL
          ),

          plotOutput(
            'fmtSpace', height = "175px")
        )
      ),

      fluidRow(
        tableOutput('ann_table')  # dataTableOutput('ann_table')
      )
    )
  )
)
