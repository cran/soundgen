library(bslib)
tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))
pv = soundgen:::permittedValues

ui = page_fluid(
  # headerPanel('soundgen v...'),

  fluidRow(
    column(
      width = 6,
      tabsetPanel(
        id = 'parGroup',
        navbarMenu(
          "Main",
          tabPanel(
            "Syllables",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Syllables"),
                sliderInput(
                  'sylLen', 'Syllable length, ms ("sylLen")',
                  value = pv['sylLen','default'],
                  min = pv['sylLen', 'low'],
                  max = pv['sylLen', 'high'],
                  step = pv['sylLen','step']) |>
                  tooltip("Average duration of a continuous VOICED syllable
                          (aperiodic noise is added separately and may fill in
                          the pauses)", options = tooltip_opt),

                sliderInput(
                  'nSyl', 'Number of syllables ("nSyl")',
                  value = pv['nSyl','default'],
                  min = pv['nSyl', 'low'],
                  max = pv['nSyl', 'high'],
                  step = pv['nSyl','step']) |>
                  tooltip("Each sound consists of one or several syllables
                          separated by pauses", options = tooltip_opt),

                sliderInput(
                  'pauseLen', 'Pause, ms ("pauseLen")',
                  value = pv['pauseLen','default'],
                  min = pv['pauseLen', 'low'],
                  max = pv['pauseLen', 'high'],
                  step = pv['pauseLen','step']) |>
                  tooltip("Average pause between syllables", options = tooltip_opt),

                sliderInput(
                  'repeatBout', 'Repeat bout # times ("repeatBout")',
                  value = pv['repeatBout','default'],
                  min = pv['repeatBout', 'low'],
                  max = pv['repeatBout', 'high'],
                  step = pv['repeatBout','step']) |>
                  tooltip("Play the whole bout several times", options = tooltip_opt)
              ),

              mainPanel(
                width = 6,
                plotOutput('plotSyllables')
              )
            )
          ),

          tabPanel(
            "Hypers",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Hypers"),
                sliderInput(
                  'temperature', 'Temperature ("temperature")',
                  value = pv['temperature','default'],
                  min = pv['temperature', 'low'],
                  max = pv['temperature', 'high'],
                  step = pv['temperature','step']) |>
                  tooltip("The degree to which all specified parameters
                          vary stochastically", options = tooltip_opt),

                sliderInput(
                  'maleFemale', 'Male-female *hyper* ("maleFemale")',
                  value = pv['maleFemale','default'],
                  min = pv['maleFemale', 'low'],
                  max = pv['maleFemale', 'high'],
                  step = pv['maleFemale','step']) |>
                  tooltip("Adjusts vocal tract length, pitch contour, and formants
                          to imitate larger/smaller body size", options = tooltip_opt),

                sliderInput(
                  'creakyBreathy',
                  'Creaky-breathy *hyper* ("creakyBreathy")',
                  value = pv['creakyBreathy','default'],
                  min = pv['creakyBreathy', 'low'],
                  max = pv['creakyBreathy', 'high'],
                  step = pv['creakyBreathy','step']) |>
                  tooltip("Changes several parameters to make the VOICED component
                          either constricted (creaky) or breathy", options = tooltip_opt)
              ),

              mainPanel(
                width = 6,
                plotOutput('plotHypers')
              )
            )
          ),

          tabPanel(
            "Settings",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Settings"),
                numericInput(
                  'samplingRate',
                  'Sampling rate, Hz ("samplingRate")',
                  value = pv['samplingRate','default'],
                  min = pv['samplingRate', 'low'],
                  max = pv['samplingRate', 'high'],
                  step = pv['samplingRate','step']) |>
                  tooltip("The number of points per second of audio. Higher = better quality;
                          lower = faster. Can be any integer, not necessarily a power of two.",
                          options = tooltip_opt),

                numericInput(
                  'windowLength',
                  'FFT window length, ms ("windowLength")',
                  value = pv['windowLength','default'],
                  min = pv['windowLength', 'low'],
                  max = pv['windowLength', 'high'],
                  step = pv['windowLength','step']) |>
                  tooltip("The length of window for performing FFT - inverse FFT when
                          filtering the source", options = tooltip_opt),

                numericInput(
                  'pitchSamplingRate',
                  'Pitch sampling rate, Hz ("pitchSamplingRate")',
                  value = pv['pitchSamplingRate', 'default'],
                  min = pv['pitchSamplingRate', 'low'],
                  max = pv['pitchSamplingRate', 'high'],
                  step = pv['pitchSamplingRate', 'step']) |>
                  tooltip("The number of considered F0 values per s of audio. Set up to
                          samplingRate for max precision, but at least >= pitchCeiling",
                          options = tooltip_opt),

                numericInput(
                  'dynamicRange', 'Dynamic range, dB ("dynamicRange")',
                  value = pv['dynamicRange', 'default'],
                  min = pv['dynamicRange', 'low'],
                  max = pv['dynamicRange', 'high'],
                  step = pv['dynamicRange', 'step']) |>
                  tooltip("Discard everything more than dynamicRange dB under
                          maximum amplitude", options = tooltip_opt),

                sliderInput(
                  'pitchFloorCeiling',
                  'Synthesized pitch range, Hz ("pitchFloorCeiling")',
                  value=c(pv['pitch', 'low'],
                          pv['pitch', 'high']),
                  min = pv['pitch', 'low'],
                  max = 8000,
                  step = pv['pitch', 'step']) |>
                  tooltip("Sets the bounds of fundamental frequency for synthesis",
                          options = tooltip_opt)
              ),

              mainPanel(
                width = 6,
                plotOutput('plotSettings')
              )
            )
          )
        ),

        navbarMenu(
          "Intonation",
          tabPanel(
            "Intonation syllable",
            sidebarLayout(
              sidebarPanel(
                tags$h3("Intonation: syllable"),
                checkboxInput(
                  inputId = 'generateVoiced',
                  label = 'Generate voiced component?',
                  value = TRUE),

                actionButton(
                  inputId = "pitch_flatten",
                  label = "Flatten pitch contour") |>
                  tooltip("Revert to a flat intonation contour with pitch equal to
                          the first (left) anchor", options = tooltip_opt),

                tableOutput("pitch_anchors"),
                width = 6),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotIntonation',
                  click = "plotIntonation_click",
                  dblclick = dblclickOpts(id = "plotIntonation_dblclick")),

                sliderInput(
                  'pitchRange', 'Plotted pitch range, Hz',
                  value = c(75, 150),
                  min = pv['pitch', 'low'],
                  max = pv['pitch', 'high'],
                  step = pv['pitch', 'step']) |>
                  tooltip("Set upper / lower limit separately or drag in between the
                          markers to shift both limits simultaneously", options = tooltip_opt)
              )
            )
          ),

          tabPanel(
            "Intonation global",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Intonation: global"),
                actionButton(
                  inputId = "pitch_flattenGlobal",
                  label = "Flatten pitch contour") |>
                  tooltip("No global pitch modulation from syllable to syllable",
                          options = tooltip_opt),

                tableOutput("pitch_anchorsGlobal")
              ),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotIntonationGlobal',
                  click = "plotIntonation_clickGlobal",
                  dblclick = dblclickOpts(id = "plotIntonation_dblclickGlobal"))
              )
            )
          ),

          tabPanel(
            "Vibrato",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Vibrato"),
                sliderInput(
                  'vibratoFreq', 'Vibrato rate, Hz ("vibratoFreq")',
                  value = pv['vibratoFreq','default'],
                  min = pv['vibratoFreq', 'low'],
                  max = pv['vibratoFreq', 'high'],
                  step = pv['vibratoFreq','step']) |>
                  tooltip("Frequency of regular FM", options = tooltip_opt),

                sliderInput(
                  'vibratoDep', 'Vibrato depth, semitones ("vibratoDep")',
                  value = pv['vibratoDep','default'],
                  min = pv['vibratoDep', 'low'],
                  max = pv['vibratoDep', 'high'],
                  step = pv['vibratoDep','step']) |>
                  tooltip("Depth of regular FM", options = tooltip_opt)
              ),

              mainPanel(
                width = 6,
                plotOutput('plotVibrato')
              )
            )
          )
        ),

        navbarMenu(
          "Amplitude",
          tabPanel(
            "Amplitude syllable",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Amplitude: syllable"),
                sliderInput (
                  'attackLen', 'Attack length, ms ("attackLen")',
                  value = pv['attackLen','default'],
                  min = pv['attackLen', 'low'],
                  max = pv['attackLen', 'high'],
                  step = pv['attackLen','step']) |>
                  tooltip("Does the voice start/end abruptly or with a fade-in/out",
                          options = tooltip_opt),

                actionButton(
                  inputId = "ampl_syl_flatten",
                  label = "Flatten amplitude envelope") |>
                  tooltip("Same amplitude over the entire syllable", options = tooltip_opt),
                tableOutput("ampl_syl_anchors")
              ),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotAmplSyl',
                  click = "plotAmplSyl_click",
                  dblclick = dblclickOpts(id = "plotAmplSyl_dblclick"))
              )
            )
          ),

          tabPanel(
            "Amplitude global",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Amplitude: global"),
                actionButton(
                  inputId = "amplGlobal_flatten",
                  label = "Flatten amplitude envelope") |>
                  tooltip("Same amplitude over the entire bout", options = tooltip_opt),

                tableOutput("amplGlobal_anchors")
              ),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotAmplGlobal',
                  click = "plotAmplGlobal_click",
                  dblclick = dblclickOpts(id = "plotAmplGlobal_dblclick"))
              )
            )
          ),

          tabPanel(
            "Amplitude modulation",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Amplitude modulation"),
                sliderInput(
                  'amDep', 'AM depth ("amDep")',
                  value = pv['amDep','default'],
                  min = pv['amDep', 'low'],
                  max = pv['amDep', 'high'],
                  step = pv['amDep','step']) |>
                  tooltip("Depth of amplitude modulation", options = tooltip_opt),

                sliderInput(
                  'amFreq', 'AM frequency, Hz ("amFreq")',
                  value = pv['amFreq','default'],
                  min = pv['amFreq', 'low'],
                  max = pv['amFreq', 'high'],
                  step = pv['amFreq','step']) |>
                  tooltip("Frequency of amplitude modulation", options = tooltip_opt),

                sliderInput(
                  'amShape', 'AM shape ("amShape")',
                  value = pv['amShape','default'],
                  min = pv['amShape', 'low'],
                  max = pv['amShape', 'high'],
                  step = pv['amShape','step']) |>
                  tooltip("Shape of amplitude modulation: 0 = ~sine, -1 = notches,
                          +1 = clicks", options = tooltip_opt)
              ),

              mainPanel(
                width = 6,
                plotOutput('plotAM'),
              )
            )
          )
        ),

        navbarMenu(
          "Source",
          tabPanel(
            "Glottal",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Glottal source"),
                accordion(
                  sliderInput(
                    'rolloff', 'Source rolloff, dB/octave ("rolloff")',
                    value = pv['rolloff','default'],
                    min = pv['rolloff', 'low'],
                    max = pv['rolloff', 'high'],
                    step = pv['rolloff','step']) |>
                    tooltip("Loss of energy in harmonics relative to fundamental frequency (F0);
                            more negative values emphasize F0", options = tooltip_opt),

                  accordion_panel(
                    "Advanced",
                    sliderInput(
                      'rolloffOct',
                      'Change of rolloff with frequency, dB/octave ("rolloffOct")',
                      value = pv['rolloffOct','default'],
                      min = pv['rolloffOct', 'low'],
                      max = pv['rolloffOct', 'high'],
                      step = pv['rolloffOct','step']) |>
                      tooltip("Negative: rolloff is progressively steeper for higher
                              frequencies", options = tooltip_opt),

                    sliderInput(
                      'rolloffKHz',
                      'Adjust rolloff per f0,  dB/kHz ("rolloffKHz")',
                      value = pv['rolloffKHz','default'],
                      min = pv['rolloffKHz', 'low'],
                      max = pv['rolloffKHz', 'high'],
                      step = pv['rolloffKHz','step']) |>
                      tooltip("Steeper/gentler basic rolloff as f0 varies", options = tooltip_opt),

                    sliderInput(
                      'glottis', 'Closed glottis, % ("glottis")',
                      value = pv['glottis', 'default'],
                      min = pv['glottis', 'low'],
                      max = pv['glottis', 'high'],
                      step = pv['glottis','step']) |>
                      tooltip("Proportion of time glottis is closed relative to F0 period;
                              adds silences between glottal pulses", options = tooltip_opt)
                  )
                )
              ),

              mainPanel(
                width = 6,
                plotOutput('plotRolloff')
              )
            )
          ),

          tabPanel(
            "Nonlinear effects",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Nonlinear effects"),
                accordion(
                  sliderInput(
                    'nonlinBalance',
                    'Balance between nonlinear regimes, % ("nonlinBalance")',
                    value = pv['nonlinBalance','default'],
                    min = pv['nonlinBalance', 'low'],
                    max = pv['nonlinBalance', 'high'],
                    step = pv['nonlinBalance','step']) |>
                    tooltip("3 regimes of nonlinear effects: none / subharmonics /
                            subharmonics + jitter", options = tooltip_opt),

                  sliderInput(
                    'shortestEpoch',
                    'Shortest epoch length, ms ("shortestEpoch")',
                    value = pv['shortestEpoch','default'],
                    min = pv['shortestEpoch', 'low'],
                    max = pv['shortestEpoch', 'high'],
                    step = pv['shortestEpoch','step']) |>
                    tooltip("Change nonlinear regime no sooner than after ... ms",
                            options = tooltip_opt),

                  accordion_panel(
                    "Subharmonics",
                    sliderInput(
                      'subRatio',
                      'Subharmonic frequency ratio ("subRatio")',
                      value = pv['subRatio','default'],
                      min = pv['subRatio', 'low'],
                      max = pv['subRatio', 'high'],
                      step = pv['subRatio','step']) |>
                      tooltip("f0/g0 ratio: 1 = no subharmonics, 2 = period doubling, etc.",
                              options = tooltip_opt),

                    sliderInput(
                      'subDep', 'Depth of subharmonics, % ("subDep")',
                      value = pv['subDep','default'],
                      min = pv['subDep', 'low'],
                      max = pv['subDep', 'high'],
                      step = pv['subDep','step']) |>
                      tooltip("The depth of g-harmonics (subharmonics) vs f-harmonics
                              (main frequency component). 0: no subharmonics; 100:
                              as strong as f-harmonics", options = tooltip_opt)
                  ),

                  accordion_panel(
                    "Chaos",
                    sliderInput(
                      'jitterDep',
                      'Jitter depth, semitones ("jitterDep")',
                      value = pv['jitterDep','default'],
                      min = pv['jitterDep', 'low'],
                      max = pv['jitterDep', 'high'],
                      step = pv['jitterDep','step']) |>
                      tooltip("Random variation in F0 per glottal cycle", options = tooltip_opt),

                    sliderInput(
                      'jitterLen', 'Jitter period, ms ("jitterLen")',
                      value = pv['jitterLen','default'],
                      min = pv['jitterLen', 'low'],
                      max = pv['jitterLen', 'high'],
                      step = pv['jitterLen','step']) |>
                      tooltip("f0 jumps every ... ms. Low ~ harsh noise, high ~ shaky voice",
                              options = tooltip_opt),

                    sliderInput(
                      'shimmerDep', 'Shimmer depth, % ("shimmerDep")',
                      value = pv['shimmerDep','default'],
                      min = pv['shimmerDep', 'low'],
                      max = pv['shimmerDep', 'high'],
                      step = pv['shimmerDep','step']) |>
                      tooltip("Random variation in amplitude per glottal cycle",
                              options = tooltip_opt),

                    sliderInput(
                      'shimmerLen', 'Shimmer period, ms ("shimmerLen")',
                      value = pv['shimmerLen','default'],
                      min = pv['shimmerLen', 'low'],
                      max = pv['shimmerLen', 'high'],
                      step = pv['shimmerLen','step']) |>
                      tooltip("The amplitude jumps every ... ms. Low ~ harsh noise,
                            high ~ shaky voice", options = tooltip_opt)
                  )
                )
              ),
              mainPanel(
                width = 6,
                plotOutput('plotNonlin')
              )
            )
          ),

          tabPanel(
            "Noise timing",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Noise: timing"),
                actionButton(
                  inputId = "noise_flatten",
                  label = "Flatten contour") |>
                  tooltip("Revert to a flat contour with amplitude equal to the first
                        (left) anchor", options = tooltip_opt),
                tableOutput("noise_anchors")
              ),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotNoise',
                  click = "plotNoise_click",
                  dblclick = dblclickOpts(id = "plotNoise_dblclick")),

                sliderInput(
                  'noiseTime',
                  'Breathing start / end, ms',
                  value = c(0, 300),
                  min = -pv['sylLen', 'high'] / 2,
                  max = pv['sylLen', 'high'],
                  step = pv['sylLen','step']) |>
                  tooltip("Timing of respiration noise relative to the voiced component",
                          options = tooltip_opt)
              )
            )
          )
        ),

        navbarMenu(
          "Tract",
          tabPanel(
            "Formants",
            sidebarLayout(
              sidebarPanel(
                tags$h3("Formants"),
                accordion(
                  sliderInput(
                    'formantDep',
                    'Formant prominence *hyper* ("formantDep")',
                    value = pv['formantDep','default'],
                    min = pv['formantDep', 'low'],
                    max = pv['formantDep', 'high'],
                    step = pv['formantDep','step']) |>
                    tooltip("Multiply formant amplitudes by ... (>1 = emphasize vowel quality)",
                            options = tooltip_opt),

                  sliderInput(
                    'formantWidth',
                    'Formant width *hyper* ("formantWidth")',
                    value = pv['formantWidth','default'],
                    min = pv['formantWidth', 'low'],
                    max = pv['formantWidth', 'high'],
                    step = pv['formantWidth','step']) |>
                    tooltip("Multiply formant bandwidths by ... (>1 = nasalized or muffled)",
                            options = tooltip_opt),

                  textInput(
                    'vowelString',
                    label = tooltip('String of vowel presets *hyper*',
                                    "Implemented presets: a, o, i, e, u, 0 (schwa)",
                                    options = tooltip_opt),
                    value = "a", width = NULL, placeholder = 'uaaao'),

                  accordion_panel(
                    "Show & modify formants manually",
                    tags$style(type="text/css", "textarea {width:100%}"),
                    # NB: this little hack ties the width of the following
                    # textarea to the width of the panel in which it is embedded;
                    # http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                    tags$textarea(
                      id = "formants",
                      label = 'Exact formants',
                      rows = 10, cols = 20, value = NA,
                      placeholder = "list()")
                  ),

                  accordion_panel(
                    "Advanced",
                    checkboxInput(
                      inputId = 'estimateVTL',
                      label = 'Estimate vocal tract length from formants?',
                      value = FALSE) |>
                      tooltip("If TRUE, user-specified formants trump user-specified
                            vocal tract length", options = tooltip_opt),

                    sliderInput(
                      'vocalTract',
                      'The length of vocal tract, cm ("vocalTract")',
                      value = pv['vocalTract', 'default'],
                      min = pv['vocalTract', 'low'],
                      max = pv['vocalTract', 'high'],
                      step = pv['vocalTract', 'step']) |>
                      tooltip("Affects default formant spacing at temperature>0",
                              options = tooltip_opt),

                    sliderInput(
                      'formantDepStoch',
                      'Added formants, dB ("formantDepStoch")',
                      value = pv['formantDepStoch','default'],
                      min = pv['formantDepStoch', 'low'],
                      max = pv['formantDepStoch', 'high'],
                      step = pv['formantDepStoch','step']) |>
                      tooltip("Amplitude of extra formants added on top of user-specified
                            ones based on the length of vocal tract", options = tooltip_opt),

                    sliderInput(
                      'lipRad', 'Lip radiation, dB/oct ("lipRad")',
                      value = pv['lipRad','default'],
                      min = pv['lipRad', 'low'],
                      max = pv['lipRad', 'high'],
                      step = pv['lipRad','step']) |>
                      tooltip("High-pass filter due to lip radiation", options = tooltip_opt),

                    sliderInput(
                      'noseRad', 'Nose radiation, dB/oct ("noseRad")',
                      value = pv['noseRad','default'],
                      min = pv['noseRad', 'low'],
                      max = pv['noseRad', 'high'],
                      step = pv['noseRad','step']) |>
                      tooltip("High-pass filter due to nose radiation: added instead of
                            lip radiation when the mouth is closed", options = tooltip_opt)
                  )
                ),
                width = 6
              ),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotFormants',
                  click = "plotFormants_click",
                  dblclick = dblclickOpts(id = "plotFormants_dblclick")),

                fluidRow(
                  radioButtons(
                    inputId= 'formants_spectrogram_or_spectrum',
                    label = "Filter preview (voiced)",
                    choices = c("Spectrogram" = "spectrogram",
                                "Spectrum" = "spectrum",
                                "Formant picker" = "formantPicker"),
                    selected = 'spectrum', inline = TRUE, width = NULL)
                )
              )
            )
          ),

          tabPanel(
            "Mouth opening",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Mouth opening"),
                actionButton(
                  inputId = "mouth_flatten",
                  label = "Flatten mouth opening contour") |>
                  tooltip("Revert to a flat mouth opening contour with opening degree
                        equal to the first (left) anchor", options = tooltip_opt),
                tableOutput("mouth_anchors"),

                sliderInput(
                  'mouthOpenThres', 'Open lips at',
                  value = pv['mouthOpenThres','default'],
                  min = pv['mouthOpenThres', 'low'],
                  max = pv['mouthOpenThres', 'high'],
                  step = pv['mouthOpenThres','step']) |>
                  tooltip("The degree of mouth opening at which lips separate and
                        start to radiate", options = tooltip_opt)
              ),

              mainPanel(
                width = 6,
                plotOutput(
                  'plotMouth',
                  click = "plotMouth_click",
                  dblclick = dblclickOpts(id = "plotMouth_dblclick"))
              )
            )
          ),

          tabPanel(
            "Noise type",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                tags$h3("Noise: type"),
                accordion(
                  selectInput(
                    inputId = 'noiseType',
                    label = tooltip("Presets",
                                    "Breathing = glottal noise (same formants as for
                                  voiced part); snuffling = breathing through the nose;
                                  h / s / sh / f = sibilants", options = tooltip_opt),
                    choices = c('Breathing' = 'b', 'Snuffling'= 'n',
                                'h'= 'h', 'sh' = 'x',
                                'f' = 'f', 's' = 's'),
                    selected= 'b'),

                  sliderInput(
                    'rolloffNoise',
                    'Linear noise rolloff, dB/oct ("rolloffNoise")',
                    value = pv['rolloffNoise','default'],
                    min = pv['rolloffNoise', 'low'],
                    max = pv['rolloffNoise', 'high'],
                    step = pv['rolloffNoise','step']) |>
                    tooltip("Linear rolloff of the noise component, dB/kHz above 2 kHz
                          (affects both breathing and supra-glottal noise)",
                            options = tooltip_opt),

                  sliderInput(
                    'rolloffNoiseExp',
                    'Exponential noise rolloff, dB/oct ("rolloffNoiseExp")',
                    value = pv['rolloffNoiseExp','default'],
                    min = pv['rolloffNoiseExp', 'low'],
                    max = pv['rolloffNoiseExp', 'high'],
                    step = pv['rolloffNoiseExp','step']) |>
                    tooltip("Exponential rolloff of the noise component, dB/oct
                          (affects both breathing and supra-glottal noise)",
                            options = tooltip_opt),

                  accordion_panel(
                    "Show & modify formants manually",
                    tags$style(type="text/css", "textarea {width:100%}"),
                    # NB: this little hack ties the width of the following
                    # textarea to the width of the panel in which it is embedded;
                    # http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                    tags$textarea(
                      id="formantsNoise",
                      label= 'Exact formants for aperiodic noise',
                      rows = 10, cols = 20, value = "",
                      placeholder ="list()")
                  )
                )
              ),

              mainPanel(
                width = 6,
                plotOutput('plotConsonant'),
                fluidRow(
                  radioButtons(
                    inputId = 'formantsNoise_spectrogram_or_spectrum',
                    label = "Filter preview (noise)",
                    choices = c("Spectrogram" = "spectrogram",
                                "Spectrum" = "spectrum"),
                    selected = 'spectrum', inline = TRUE, width = NULL)
                )
              )
            )
          )
        )
      )
    ),

    column(
      width = 4,
      fluidRow(
        column(
          width = 3,
          actionButton(
            inputId = "generateAudio",
            label = "Generate",
            style = "color: blue; background-color: orange;")
        ),
        column(
          width = 5,
          uiOutput("htmlAudio")
        )
      ),

      fluidRow(
        plotOutput('spectrogram')
      ),

      fluidRow(
        radioButtons(
          inputId = 'spectrogram_or_spectrum',
          label = "Generated sound",
          choices = c("Spectrogram" = "spectrogram",
                      "Spectrum" = "spectrum"),
          selected = 'spectrogram', inline = TRUE, width = NULL)
      ),

      fluidRow(
        accordion(
          id = "spec_controls",
          accordion_panel(
            "Show spectrogram controls",
            sliderInput(
              'spec_ylim', 'Frequency range, kHz',
              value = c(0, pv['spec_ylim', 'default']),
              min = pv['spec_ylim', 'low'],
              max = pv['spec_ylim', 'high'],
              step = pv['spec_ylim', 'step']),

            sliderInput(
              'specWindowLength', 'Window length, ms',
              value = pv['specWindowLength','default'],
              min = pv['specWindowLength', 'low'],
              max = pv['specWindowLength', 'high'],
              step = pv['specWindowLength','step']),

            sliderInput(
              'specContrast', 'Contrast',
              value = pv['specContrast','default'],
              min = pv['specContrast', 'low'],
              max = pv['specContrast', 'high'],
              step = pv['specContrast','step']),

            sliderInput(
              'specBrightness', 'Brightness',
              value = pv['specBrightness','default'],
              min = pv['specBrightness', 'low'],
              max = pv['specBrightness', 'high'],
              step = pv['specBrightness','step']),

            radioButtons(
              inputId = 'spec_colorTheme',
              label = "Color scheme",
              choices = c("Seewave" = "seewave",
                          "Heat" = "heat.colors",
                          "Black & white" = "bw"),
              selected = 'bw', inline = TRUE, width = NULL),

            radioButtons(
              inputId = 'spec_method', label = "Method",
              choices = c("Spectrum" = "spectrum",
                          "Derivative" = "derivative",
                          "Reassigned" = "reassigned"),
              selected = 'spectrum', inline = TRUE, width = NULL),

            radioButtons(
              inputId= 'osc', label ="Oscillogram type",
              choices=c("Linear" = "linear",
                        "dB" = "dB",
                        "None" = "none"),
              selected = 'linear', inline = TRUE, width = NULL),

            sliderInput(
              'osc_heights', 'Relative size of oscillogram',
              value = pv['oscHeight','default'],
              min = pv['oscHeight', 'low'],
              max = pv['oscHeight', 'high'],
              step = pv['oscHeight','step']) |>
              tooltip("Relative size of spectrogram vs oscillogram", options = tooltip_opt)
          ),
          open = FALSE
        )
      )
    ),

    column(
      width = 2,
      tags$h2('Presets'),
      accordion(
        selectInput(
          inputId = 'speaker', label = "Speaker",
          choices = names(presets),
          selected = names(presets)[1]),

        selectInput(
          inputId = 'callType', label = "Call",
          choices = names(presets[[1]]),
          selected = names(presets[[1]])[1]),

        accordion_panel(
          "Load new preset",
          tags$style(
            type = "text/css",
            "textarea {width:100%; font-size:50%}"),
          tags$textarea(
            id = "user_preset",
            label = 'Type in a new preset here',
            rows = 10, cols = 20, value = "",
            placeholder = "soundgen(...)"),
          actionButton(
            inputId = "import_preset",
            label = "Update sliders")
        ),

        tags$h2('Export'),
        downloadButton(outputId = "saveAudio", label = "Save audio"),
        tags$br(), tags$br(),
        accordion_panel(
          "Export R code",
          tags$style(
            type = "text/css",
            "textarea {width:100%; font-size:50%}"),
          tags$textarea(
            id = "mycall",
            label = 'Copy-paste function call',
            rows = 10, cols = 20, value = "",
            placeholder ="soundgen()")
        ),
        actionButton("about", "About")
      )
    )
  )
)
