library(bslib)
tooltip_opt = list(trigger = 'hover', delay = list(show = 500, hide = 100))

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
                  value = permittedValues['sylLen','default'],
                  min = permittedValues['sylLen', 'low'],
                  max = permittedValues['sylLen', 'high'],
                  step = permittedValues['sylLen','step']) |>
                  tooltip("Average duration of a continuous VOICED syllable
                          (aperiodic noise is added separately and may fill in
                          the pauses)", options = tooltip_opt),

                sliderInput(
                  'nSyl', 'Number of syllables ("nSyl")',
                  value = permittedValues['nSyl','default'],
                  min = permittedValues['nSyl', 'low'],
                  max = permittedValues['nSyl', 'high'],
                  step = permittedValues['nSyl','step']) |>
                  tooltip("Each sound consists of one or several syllables
                          separated by pauses", options = tooltip_opt),

                sliderInput(
                  'pauseLen', 'Pause, ms ("pauseLen")',
                  value = permittedValues['pauseLen','default'],
                  min = permittedValues['pauseLen', 'low'],
                  max = permittedValues['pauseLen', 'high'],
                  step = permittedValues['pauseLen','step']) |>
                  tooltip("Average pause between syllables", options = tooltip_opt),

                sliderInput(
                  'repeatBout', 'Repeat bout # times ("repeatBout")',
                  value = permittedValues['repeatBout','default'],
                  min = permittedValues['repeatBout', 'low'],
                  max = permittedValues['repeatBout', 'high'],
                  step = permittedValues['repeatBout','step']) |>
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
                  value = permittedValues['temperature','default'],
                  min = permittedValues['temperature', 'low'],
                  max = permittedValues['temperature', 'high'],
                  step = permittedValues['temperature','step']) |>
                  tooltip("The degree to which all specified parameters
                          vary stochastically", options = tooltip_opt),

                sliderInput(
                  'maleFemale', 'Male-female *hyper* ("maleFemale")',
                  value = permittedValues['maleFemale','default'],
                  min = permittedValues['maleFemale', 'low'],
                  max = permittedValues['maleFemale', 'high'],
                  step = permittedValues['maleFemale','step']) |>
                  tooltip("Adjusts vocal tract length, pitch contour, and formants
                          to imitate larger/smaller body size", options = tooltip_opt),

                sliderInput(
                  'creakyBreathy',
                  'Creaky-breathy *hyper* ("creakyBreathy")',
                  value = permittedValues['creakyBreathy','default'],
                  min = permittedValues['creakyBreathy', 'low'],
                  max = permittedValues['creakyBreathy', 'high'],
                  step = permittedValues['creakyBreathy','step']) |>
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
                  value = permittedValues['samplingRate','default'],
                  min = permittedValues['samplingRate', 'low'],
                  max = permittedValues['samplingRate', 'high'],
                  step = permittedValues['samplingRate','step']) |>
                  tooltip("The number of points per second of audio. Higher = better quality;
                          lower = faster. Can be any integer, not necessarily a power of two.",
                          options = tooltip_opt),

                numericInput(
                  'windowLength',
                  'FFT window length, ms ("windowLength")',
                  value = permittedValues['windowLength','default'],
                  min = permittedValues['windowLength', 'low'],
                  max = permittedValues['windowLength', 'high'],
                  step = permittedValues['windowLength','step']) |>
                  tooltip("The length of window for performing FFT - inverse FFT when
                          filtering the source", options = tooltip_opt),

                numericInput(
                  'pitchSamplingRate',
                  'Pitch sampling rate, Hz ("pitchSamplingRate")',
                  value = permittedValues['pitchSamplingRate', 'default'],
                  min = permittedValues['pitchSamplingRate', 'low'],
                  max = permittedValues['pitchSamplingRate', 'high'],
                  step = permittedValues['pitchSamplingRate', 'step']) |>
                  tooltip("The number of considered F0 values per s of audio. Set up to
                          samplingRate for max precision, but at least >= pitchCeiling",
                          options = tooltip_opt),

                numericInput(
                  'dynamicRange', 'Dynamic range, dB ("dynamicRange")',
                  value = permittedValues['dynamicRange', 'default'],
                  min = permittedValues['dynamicRange', 'low'],
                  max = permittedValues['dynamicRange', 'high'],
                  step = permittedValues['dynamicRange', 'step']) |>
                  tooltip("Discard everything more than dynamicRange dB under
                          maximum amplitude", options = tooltip_opt),

                sliderInput(
                  'pitchFloorCeiling',
                  'Synthesized pitch range, Hz ("pitchFloorCeiling")',
                  value=c(permittedValues['pitch', 'low'],
                          permittedValues['pitch', 'high']),
                  min = permittedValues['pitch', 'low'],
                  max = 8000,
                  step = permittedValues['pitch', 'step']) |>
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
                  min = permittedValues['pitch', 'low'],
                  max = permittedValues['pitch', 'high'],
                  step = permittedValues['pitch', 'step']) |>
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
                  value = permittedValues['vibratoFreq','default'],
                  min = permittedValues['vibratoFreq', 'low'],
                  max = permittedValues['vibratoFreq', 'high'],
                  step = permittedValues['vibratoFreq','step']) |>
                  tooltip("Frequency of regular FM", options = tooltip_opt),

                sliderInput(
                  'vibratoDep', 'Vibrato depth, semitones ("vibratoDep")',
                  value = permittedValues['vibratoDep','default'],
                  min = permittedValues['vibratoDep', 'low'],
                  max = permittedValues['vibratoDep', 'high'],
                  step = permittedValues['vibratoDep','step']) |>
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
                  value = permittedValues['attackLen','default'],
                  min = permittedValues['attackLen', 'low'],
                  max = permittedValues['attackLen', 'high'],
                  step = permittedValues['attackLen','step']) |>
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
                  value = permittedValues['amDep','default'],
                  min = permittedValues['amDep', 'low'],
                  max = permittedValues['amDep', 'high'],
                  step = permittedValues['amDep','step']) |>
                  tooltip("Depth of amplitude modulation", options = tooltip_opt),

                sliderInput(
                  'amFreq', 'AM frequency, Hz ("amFreq")',
                  value = permittedValues['amFreq','default'],
                  min = permittedValues['amFreq', 'low'],
                  max = permittedValues['amFreq', 'high'],
                  step = permittedValues['amFreq','step']) |>
                  tooltip("Frequency of amplitude modulation", options = tooltip_opt),

                sliderInput(
                  'amShape', 'AM shape ("amShape")',
                  value = permittedValues['amShape','default'],
                  min = permittedValues['amShape', 'low'],
                  max = permittedValues['amShape', 'high'],
                  step = permittedValues['amShape','step']) |>
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
                    value = permittedValues['rolloff','default'],
                    min = permittedValues['rolloff', 'low'],
                    max = permittedValues['rolloff', 'high'],
                    step = permittedValues['rolloff','step']) |>
                    tooltip("Loss of energy in harmonics relative to fundamental frequency (F0);
                            more negative values emphasize F0", options = tooltip_opt),

                  accordion_panel(
                    "Advanced",
                    sliderInput(
                      'rolloffOct',
                      'Change of rolloff with frequency, dB/octave ("rolloffOct")',
                      value = permittedValues['rolloffOct','default'],
                      min = permittedValues['rolloffOct', 'low'],
                      max = permittedValues['rolloffOct', 'high'],
                      step = permittedValues['rolloffOct','step']) |>
                      tooltip("Negative: rolloff is progressively steeper for higher
                              frequencies", options = tooltip_opt),

                    sliderInput(
                      'rolloffKHz',
                      'Adjust rolloff per f0,  dB/kHz ("rolloffKHz")',
                      value = permittedValues['rolloffKHz','default'],
                      min = permittedValues['rolloffKHz', 'low'],
                      max = permittedValues['rolloffKHz', 'high'],
                      step = permittedValues['rolloffKHz','step']) |>
                      tooltip("Steeper/gentler basic rolloff as f0 varies", options = tooltip_opt),

                    sliderInput(
                      'rolloffParab',
                      'Parabolic rolloff adjustment, dB/octave ("rolloffParab")',
                      value = permittedValues['rolloffParab','default'],
                      min = permittedValues['rolloffParab', 'low'],
                      max = permittedValues['rolloffParab', 'high'],
                      step = permittedValues['rolloffParab','step']) |>
                      tooltip("Parabolic boost to the first ... harmonics", options = tooltip_opt),

                    sliderInput(
                      'rolloffParabHarm',
                      'Harmonics boosted ("rolloffParabHarm")',
                      value = permittedValues['rolloffParabHarm','default'],
                      min = permittedValues['rolloffParabHarm', 'low'],
                      max = permittedValues['rolloffParabHarm', 'high'],
                      step = permittedValues['rolloffParabHarm','step']) |>
                      tooltip("Apply a parabolic boost to ... harmonics. See ?getRolloff",
                              options = tooltip_opt),

                    sliderInput(
                      'glottis', 'Closed glottis, % ("glottis")',
                      value = permittedValues['glottis', 'default'],
                      min = permittedValues['glottis', 'low'],
                      max = permittedValues['glottis', 'high'],
                      step = permittedValues['glottis','step']) |>
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
                    value = permittedValues['nonlinBalance','default'],
                    min = permittedValues['nonlinBalance', 'low'],
                    max = permittedValues['nonlinBalance', 'high'],
                    step = permittedValues['nonlinBalance','step']) |>
                    tooltip("3 regimes of nonlinear effects: none / subharmonics /
                            subharmonics + jitter", options = tooltip_opt),

                  sliderInput(
                    'shortestEpoch',
                    'Shortest epoch length, ms ("shortestEpoch")',
                    value = permittedValues['shortestEpoch','default'],
                    min = permittedValues['shortestEpoch', 'low'],
                    max = permittedValues['shortestEpoch', 'high'],
                    step = permittedValues['shortestEpoch','step']) |>
                    tooltip("Change nonlinear regime no sooner than after ... ms",
                            options = tooltip_opt),

                  accordion_panel(
                    "Subharmonics",
                    sliderInput(
                      'subRatio',
                      'Subharmonic frequency ratio ("subRatio")',
                      value = permittedValues['subRatio','default'],
                      min = permittedValues['subRatio', 'low'],
                      max = permittedValues['subRatio', 'high'],
                      step = permittedValues['subRatio','step']) |>
                      tooltip("f0/g0 ratio: 1 = no subharmonics, 2 = period doubling, etc.",
                              options = tooltip_opt),

                    sliderInput(
                      'subFreq',
                      'Target subharmonic frequency, Hz (overrides number of subharmonics) ("subFreq")',
                      value = permittedValues['subFreq','default'],
                      min = permittedValues['subFreq', 'low'],
                      max = permittedValues['subFreq', 'high'],
                      step = permittedValues['subFreq','step']) |>
                      tooltip("The approximate target frequency of subharmonics;
                              the actual frequency is forced to be a fraction of f0 at
                              every time point", options = tooltip_opt),

                    sliderInput(
                      'subDep', 'Depth of subharmonics, % ("subDep")',
                      value = permittedValues['subDep','default'],
                      min = permittedValues['subDep', 'low'],
                      max = permittedValues['subDep', 'high'],
                      step = permittedValues['subDep','step']) |>
                      tooltip("The depth of g-harmonics (subharmonics) vs f-harmonics
                              (main frequency component). 0: no subharmonics; 100:
                              as strong as f-harmonics", options = tooltip_opt),

                    sliderInput(
                      'subWidth',
                      'Width of sidebands, Hz ("subWidth")',
                      value = permittedValues['subWidth','default'],
                      min = permittedValues['subWidth', 'low'],
                      max = permittedValues['subWidth', 'high'],
                      step = permittedValues['subWidth','step']) |>
                      tooltip("Width of subharmonic sidebands - regulates how rapidly
                              g-harmonics weaken away from f-harmonics", options = tooltip_opt)
                  ),

                  accordion_panel(
                    "Chaos",
                    sliderInput(
                      'jitterDep',
                      'Jitter depth, semitones ("jitterDep")',
                      value = permittedValues['jitterDep','default'],
                      min = permittedValues['jitterDep', 'low'],
                      max = permittedValues['jitterDep', 'high'],
                      step = permittedValues['jitterDep','step']) |>
                      tooltip("Random variation in F0 per glottal cycle", options = tooltip_opt),

                    sliderInput(
                      'jitterLen', 'Jitter period, ms ("jitterLen")',
                      value = permittedValues['jitterLen','default'],
                      min = permittedValues['jitterLen', 'low'],
                      max = permittedValues['jitterLen', 'high'],
                      step = permittedValues['jitterLen','step']) |>
                      tooltip("f0 jumps every ... ms. Low ~ harsh noise, high ~ shaky voice",
                              options = tooltip_opt),

                    sliderInput(
                      'shimmerDep', 'Shimmer depth, % ("shimmerDep")',
                      value = permittedValues['shimmerDep','default'],
                      min = permittedValues['shimmerDep', 'low'],
                      max = permittedValues['shimmerDep', 'high'],
                      step = permittedValues['shimmerDep','step']) |>
                      tooltip("Random variation in amplitude per glottal cycle",
                              options = tooltip_opt),

                    sliderInput(
                      'shimmerLen', 'Shimmer period, ms ("shimmerLen")',
                      value = permittedValues['shimmerLen','default'],
                      min = permittedValues['shimmerLen', 'low'],
                      max = permittedValues['shimmerLen', 'high'],
                      step = permittedValues['shimmerLen','step']) |>
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
                  min = -permittedValues['sylLen', 'high'] / 2,
                  max = permittedValues['sylLen', 'high'],
                  step = permittedValues['sylLen','step']) |>
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
                    value = permittedValues['formantDep','default'],
                    min = permittedValues['formantDep', 'low'],
                    max = permittedValues['formantDep', 'high'],
                    step = permittedValues['formantDep','step']) |>
                    tooltip("Multiply formant amplitudes by ... (>1 = emphasize vowel quality)",
                            options = tooltip_opt),

                  sliderInput(
                    'formantWidth',
                    'Formant width *hyper* ("formantWidth")',
                    value = permittedValues['formantWidth','default'],
                    min = permittedValues['formantWidth', 'low'],
                    max = permittedValues['formantWidth', 'high'],
                    step = permittedValues['formantWidth','step']) |>
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
                      value = permittedValues['vocalTract', 'default'],
                      min = permittedValues['vocalTract', 'low'],
                      max = permittedValues['vocalTract', 'high'],
                      step = permittedValues['vocalTract', 'step']) |>
                      tooltip("Affects default formant spacing at temperature>0",
                              options = tooltip_opt),

                    sliderInput(
                      'formantDepStoch',
                      'Added formants, dB ("formantDepStoch")',
                      value = permittedValues['formantDepStoch','default'],
                      min = permittedValues['formantDepStoch', 'low'],
                      max = permittedValues['formantDepStoch', 'high'],
                      step = permittedValues['formantDepStoch','step']) |>
                      tooltip("Amplitude of extra formants added on top of user-specified
                            ones based on the length of vocal tract", options = tooltip_opt),

                    sliderInput(
                      'lipRad', 'Lip radiation, dB/oct ("lipRad")',
                      value = permittedValues['lipRad','default'],
                      min = permittedValues['lipRad', 'low'],
                      max = permittedValues['lipRad', 'high'],
                      step = permittedValues['lipRad','step']) |>
                      tooltip("High-pass filter due to lip radiation", options = tooltip_opt),

                    sliderInput(
                      'noseRad', 'Nose radiation, dB/oct ("noseRad")',
                      value = permittedValues['noseRad','default'],
                      min = permittedValues['noseRad', 'low'],
                      max = permittedValues['noseRad', 'high'],
                      step = permittedValues['noseRad','step']) |>
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
                  value = permittedValues['mouthOpenThres','default'],
                  min = permittedValues['mouthOpenThres', 'low'],
                  max = permittedValues['mouthOpenThres', 'high'],
                  step = permittedValues['mouthOpenThres','step']) |>
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
                    value = permittedValues['rolloffNoise','default'],
                    min = permittedValues['rolloffNoise', 'low'],
                    max = permittedValues['rolloffNoise', 'high'],
                    step = permittedValues['rolloffNoise','step']) |>
                    tooltip("Linear rolloff of the noise component, dB/kHz above 2 kHz
                          (affects both breathing and supra-glottal noise)",
                            options = tooltip_opt),

                  sliderInput(
                    'rolloffNoiseExp',
                    'Exponential noise rolloff, dB/oct ("rolloffNoiseExp")',
                    value = permittedValues['rolloffNoiseExp','default'],
                    min = permittedValues['rolloffNoiseExp', 'low'],
                    max = permittedValues['rolloffNoiseExp', 'high'],
                    step = permittedValues['rolloffNoiseExp','step']) |>
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
              value = c(0, permittedValues['spec_ylim', 'default']),
              min = permittedValues['spec_ylim', 'low'],
              max = permittedValues['spec_ylim', 'high'],
              step = permittedValues['spec_ylim', 'step']),

            sliderInput(
              'specWindowLength', 'Window length, ms',
              value = permittedValues['specWindowLength','default'],
              min = permittedValues['specWindowLength', 'low'],
              max = permittedValues['specWindowLength', 'high'],
              step = permittedValues['specWindowLength','step']),

            sliderInput(
              'specContrast', 'Contrast',
              value = permittedValues['specContrast','default'],
              min = permittedValues['specContrast', 'low'],
              max = permittedValues['specContrast', 'high'],
              step = permittedValues['specContrast','step']),

            sliderInput(
              'specBrightness', 'Brightness',
              value = permittedValues['specBrightness','default'],
              min = permittedValues['specBrightness', 'low'],
              max = permittedValues['specBrightness', 'high'],
              step = permittedValues['specBrightness','step']),

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
                          "Spectral derivative" = "spectralDerivative",
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
              value = permittedValues['oscHeight','default'],
              min = permittedValues['oscHeight', 'low'],
              max = permittedValues['oscHeight', 'high'],
              step = permittedValues['oscHeight','step']) |>
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
