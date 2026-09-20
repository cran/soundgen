# annotation_app()
#

library(bslib)

# Retrieve defaults from package environment.
# Fallback is provided for direct shinyapps.io deployment where the wrapper isn't executed.
if (exists(".soundgen_env", envir = asNamespace("soundgen")) &&
    !is.null(soundgen:::.soundgen_env$ann$def)) {
  df = soundgen:::.soundgen_env$ann$def_ann
  def = soundgen:::.soundgen_env$ann$def
} else {
  # Fallback for shinyapps.io deployment
  df = soundgen:::def_ann
  def = as.list(df[, 'default'])
  names(def) = rownames(df)
  def$spec_xlim = c(0, def$spec_xlim)
  def$spec_ylim = c(0, def$spec_ylim)
  def = c(def, list(
    'specType' = 'spectrum',
    'spec_colorTheme' = 'bw',
    'spec_yScale' = 'linear',
    'wn' = 'gaussian',
    'osc' = 'linear',
    'audioMethod' = 'Browser'
  ))
}

tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))

ui = page_fluid(
  # theme = bs_theme(preset = "darkly"),

  # css
  tags$head(
    shiny::includeCSS(file.path("www", "annotation_app.css"))
  ),

  # js
  includeScript(file.path("www", "annotation_app_shinyjs.js")),
  includeScript(file.path("www", "annotation_app.js")),
  shinyjs::useShinyjs(),
  # handy for calling js functions from R, eg for a collapsible side panel - see
  # https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard
  # import some js functions to be invoked from R with shinyjs
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'annotation_app_shinyjs.js',
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
              tabPanel(
                "General",
                actionButton(
                  'reset_to_def',
                  label = 'Reset ALL to defaults') |>
                  tooltip("Reset all settings to default values", options = tooltip_opt),
                radioButtons(
                  'audioMethod',
                  label = "Play analyzed audio with",
                  choices = list('Browser' = 'Browser', 'R' = 'R'),
                  selected = 'Browser', inline = TRUE, width = NULL) |>
                  tooltip("Play audio with javascript or with R (browser-independent,
                  but then the cursor doesn't move, and you can't stop playback)",
                          options = tooltip_opt),
                checkboxInput(
                  'normalizeInput',
                  'Normalize for peak amplitude',
                  value = TRUE)
              ),
              tabPanel(
                "Spectrogram",
                accordion(
                  sliderInput(
                    'spec_ylim',
                    'Frequency range, kHz',
                    value = def$spec_ylim,
                    min = df['spec_ylim', 'low'],
                    max = df['spec_ylim', 'high'],
                    step = df['spec_ylim', 'step']) |>
                    tooltip("Range of displayed frequencies, kHz", options = tooltip_opt),
                  numericInput(
                    'windowLength',
                    'Window length, ms',
                    value = def$windowLength,
                    min = df['windowLength', 'low'],
                    max = df['windowLength', 'high'],
                    step = df['windowLength', 'step']) |>
                    tooltip("Length of STFT window, ms", options = tooltip_opt),
                  numericInput(
                    'step',
                    'Step, ms',
                    value = def$step,
                    min = df['step', 'low'],
                    max = df['step', 'high'],
                    step = df['step', 'step']) |>
                    tooltip("Step between analysis frames, ms", options = tooltip_opt),
                  radioButtons(
                    inputId = 'specType',
                    label = tooltip('Spectrogram type', "Spectrogram type,
                    argument 'specType' in spectrogram()"),
                    choices = c("Ordinary FFT" = "spectrum",
                                "Reassigned" = "reassigned",
                                "Derivative" = "derivative"),
                    selected = def$specType,
                    inline = TRUE, width = NULL),
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
                    choices = c("Black & white" = "bw",
                                "Seewave" = "seewave",
                                "Heat" = "heat.colors",
                                "Matlab" = "matlab"),
                    selected = def$spec_colorTheme,
                    inline = TRUE, width = NULL),
                  numericInput(
                    'nColors',
                    'Number of colors in the palette',
                    value = def$nColors,
                    min = df['nColors', 'low'],
                    max = df['nColors', 'high'],
                    step = df['nColors', 'step']),
                  radioButtons(
                    inputId = 'spec_yScale',
                    label = 'Frequency scale',
                    choices = c("Linear" = "linear",
                                "Musical (log)" = "log",
                                "Bark" = "bark",
                                "Mel" = "mel",
                                "ERB" = "ERB"),
                    selected = def$spec_yScale,
                    inline = TRUE, width = NULL),
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
                  accordion_panel(
                    "Advanced",
                    sliderInput(
                      'blur_freq',
                      'Blur: frequency (Hz)',
                      value = def$blur_freq,
                      min = df['blur_freq', 'low'],
                      max = df['blur_freq', 'high'],
                      step = df['blur_freq', 'step']) |>
                      tooltip("Gaussian filter of frequency: >0 = blur, <0 = unblur
                      (sharpen)", options = tooltip_opt),
                    sliderInput(
                      'blur_time',
                      'Blur: time (ms)',
                      value = def$blur_time,
                      min = df['blur_time', 'low'],
                      max = df['blur_time', 'high'],
                      step = df['blur_time', 'step']) |>
                      tooltip("Gaussian filter of time: >0 = blur, <0 = unblur (sharpen)",
                              options = tooltip_opt),
                    sliderInput(
                      'reass_cex',
                      'Point size (reassigned spectrogram only)',
                      value = def$reass_cex,
                      min = df['reass_cex', 'low'],
                      max = df['reass_cex', 'high'],
                      step = df['reass_cex', 'step']) |>
                      tooltip("", options = tooltip_opt),
                    sliderInput(
                      'zp',
                      'Zero padding, points 2 ^ n',
                      value = def$zp,
                      min = df['zp', 'low'],
                      max = df['zp', 'high'],
                      step = df['zp', 'step']) |>
                      tooltip("Zero padding: 8 means 2^8 = 256, etc.", options = tooltip_opt),
                    selectInput(
                      'wn',
                      'STFT window type',
                      choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                                  'hamming', 'hanning', 'rectangle'),
                      selected = def$wn,
                      multiple = FALSE),
                    sliderInput(
                      'spec_maxPoints',
                      'Max number of pixels, 10^',
                      value = def$spec_maxPoints,
                      min = df['spec_maxPoints', 'low'],
                      max = df['spec_maxPoints', 'high'],
                      step = df['spec_maxPoints', 'step']) |>
                      tooltip("The number of points to plot in the spectrogram
                      (smaller = faster, but low resolution)", options = tooltip_opt)
                  )
                )
              ),
              tabPanel(
                "Oscillogram",
                selectInput(
                  'osc',
                  'Oscillogram type',
                  choices = c('linear', 'dB'),
                  selected = def$osc,
                  multiple = FALSE),
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
          ), # end of column "sidebar"
          fluidRow(
            style="margin-top: -15px; margin-bottom: -15px;",
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
              selectInput('fileList', label = NULL, choices = list(),
                          selectize = FALSE  # to enable custom css
              )
            ),
            column(
              width = 2,
              uiOutput("htmlAudio"),  # not actually shown
              downloadButton(
                outputId = "saveRes", label = "",
                style="color: blue; background-color: orange;") |>
                tooltip("Download results (see ?pitch_app for recovering unsaved
                data after a crash)", options = tooltip_opt),
              actionButton(
                'about', label = '?')
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
                tooltip("Remove annotation (DELETE / BACKSPACE)", options = tooltip_opt),
              actionButton(
                inputId = "selection_snap",
                label = HTML("<img src='icons/snap.png' width = '20px'>"),
                class = "buttonInline") |>
                tooltip("Time active annotation to coincide with selection (T)",
                        options = tooltip_opt),
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
