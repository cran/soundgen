# annotation_app()
#
ui = fluidPage(
  # css
  tags$head(
    shiny::includeCSS("www/annotation_app.css")
  ),

  # js
  includeScript("www/annotation_app.js"),
  includeScript("www/annotation_app_shinyjs.js"),
  shinyjs::useShinyjs(),
  # handy for calling js functions from R, eg for a collapsible side panel - see
  # https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  # import some js functions to be invoked from R with shinyjs
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'www/annotation_app_shinyjs.js',
    functions = c('playme_js', 'stopAudio_js', 'play_file',
                  'clearBrush', 'inheritSize', 'scrollBar')
  ),

  # html
  tags$div(
    id = 'gridCont',

    tags$div(
      id = 'left',
      fluidRow(
        column(
          width = 4,
          id = "Sidebar",
          tabsetPanel(
            id = 'parGroup',
            tabPanel(
              "General",
              actionButton(
                'reset_to_def',
                label = 'Reset ALL to defaults'),
              radioButtons(
                'audioMethod',
                label = "Play analyzed audio with",
                choices = list('Browser' = 'Browser', 'R' = 'R'),
                selected = 'Browser', inline = TRUE, width = NULL
              ),
              checkboxInput(
                'normalizeInput',
                'Normalize for peak amplitude',
                value = TRUE),
            ),

            tabPanel(
              "Spectrogram",
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
                value = 20, # def_form['windowLength', 'default'],
                min = def_form['windowLength', 'low'],
                max = def_form['windowLength', 'high'],
                step = def_form['windowLength', 'step']),
              numericInput(
                'step',
                'Step, ms',
                value = 5, # def_form['step', 'default'],
                min = def_form['step', 'low'],
                max = def_form['step', 'high'],
                step = def_form['step', 'step']),
              # sliderInput(
              #   'overlap',
              #   'Overlap, %',
              #   value = def_form['overlap', 'default'],
              #   min = def_form['overlap', 'low'],
              #   max = def_form['overlap', 'high'],
              #   step = def_form['overlap', 'step']),
              radioButtons(
                inputId = 'specType',
                label = 'Spectrogram type',
                choices = c("Ordinary FFT" = "spectrum",
                            "Reassigned" = "reassigned",
                            "Derivative" = "spectralDerivative"),
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
              radioButtons(
                inputId = 'spec_yScale',
                label = 'Frequency scale',
                choices = c("Linear" = "linear",
                            "Musical (log)" = "log",
                            "Bark" = "bark",
                            "Mel" = "mel",
                            "ERB" = "ERB"),
                selected = 'linear', inline = TRUE, width = NULL),
              sliderInput(
                'specContrast',
                'Contrast',
                value = 0, # def_form['specContrast', 'default'],
                min = def_form['specContrast', 'low'],
                max = def_form['specContrast', 'high'],
                step = def_form['specContrast', 'step']),
              sliderInput(
                'specBrightness',
                'Brightness',
                value =0, # def_form['specBrightness', 'default'],
                min = def_form['specBrightness', 'low'],
                max = def_form['specBrightness', 'high'],
                step = def_form['specBrightness', 'step']),
              shinyBS::bsCollapsePanel(
                "Advanced",
                sliderInput(
                  'blur_freq',
                  'Blur: frequency (Hz)',
                  value = 0, # def_form['blur_freq', 'default'],
                  min = def_form['blur_freq', 'low'],
                  max = def_form['blur_freq', 'high'],
                  step = def_form['blur_freq', 'step']),
                sliderInput(
                  'blur_time',
                  'Blur: time (ms)',
                  value = 0, # def_form['blur_time', 'default'],
                  min = def_form['blur_time', 'low'],
                  max = def_form['blur_time', 'high'],
                  step = def_form['blur_time', 'step']),
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
                  step = def_form['zp', 'step']),
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
                  step = def_form['spec_maxPoints', 'step'])
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
                step = def_form['osc_maxPoints', 'step'])
            )
          )
        ),  # end of column "sidebar"

        column(
          width = 8,
          id ="Main",
          fluidRow(
            column(
              width = 1,
              bsButton(
                "showpanel", label = '', icon = icon("bars"),
                type = "toggle", value = FALSE)
            ),

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
                  class = "buttonFile"),
                tags$strong(uiOutput("fileN", inline = TRUE)),
                actionButton(
                  inputId = "nextFile", label = ">>",
                  class = "buttonFile")
              ),
              selectInput('fileList', label = NULL, choices = list())
            ),

            column(
              width = 2,
              uiOutput("htmlAudio"),  # not actually shown
              downloadButton(
                outputId = "saveRes", label = "",
                style="color: blue; background-color: orange;"),
              actionButton(
                'about', label = '?'),
              shinyBS:::bsPopover
              (id = 'about', title = NULL, content = 'Help',
                placement = "right", trigger = "hover")
              # shinyBS has to be mentioned somewhere in ui,
              # otherwise addTooltip doesn't work in server
            )
          ),

          fluidRow(
            column(
              width = 2,
              actionButton(
                inputId = 'zoomOut_freq',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'zoomIn_freq',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline")
            ),

            column(
              width = 4,
              actionButton(
                inputId = "selection_stop",
                label = HTML("<img src='icons/stop.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "selection_play",
                label = HTML("<img src='icons/play.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "selection_annotate",
                label = HTML("<img src='icons/annotate.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "selection_delete",
                label = HTML("<img src='icons/delete.png' width = '20px'>"),
                class = "buttonInline")
            ),

            column(
              width = 6,
              actionButton(
                inputId = 'scrollLeft',
                label = HTML("<img src='icons/backward.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'zoomOut',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "zoomToSel",
                label = HTML("<img src='icons/zoomSel.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'zoomIn',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'scrollRight',
                label = HTML("<img src='icons/forward.png' width = '20px'>"),
                class = "buttonInline")
            )
          ),

          fluidRow(
            tags$div(
              id = 'plotRow',

              tags$div(
                id = 'specDiv',
                plotOutput(
                  'spectrogram',
                  click = "spectrogram_click",
                  dblclick = dblclickOpts(id = "spectrogram_dblclick"),
                  hover = hoverOpts(id = "spectrogram_hover"),
                  brush = brushOpts(id = 'spectrogram_brush',
                                    opacity = 0.25,
                                    resetOnNew = FALSE))
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
      tableOutput('ann_table')
    )
  )
)
