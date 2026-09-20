#' Generate a sound
#'
#' Generates a sequence ("bout") of one or more vocalizations ("syllables") with
#' pauses between them. Two basic components are synthesized: a periodic
#' component (the sum of sine waves with frequencies that are multiples of the
#' fundamental frequency) and an aperiodic noise component. Both components can
#' be filtered with independently specified vocal tract transfer functions
#' ("formants"). Intonation and amplitude contours can be applied both within
#' each syllable and across multiple syllables. Suggested application: synthesis
#' of animal calls and human nonverbal vocalizations (not speech). For more
#' information, see \url{https://cogsci.se/soundgen/sound_generation.html}.
#'
#' @seealso \code{\link{getFormantFilter}} \code{\link{beat}}
#' @return The synthesized waveform as a numeric vector normalized to [-1, 1]
#'   and sampled at the specified \code{samplingRate}. Note that the sampling
#'   rate may be increased internally in case the pitch is high, with a warning.
#'
#' @section Parameter groups:
#' \describe{
#'   \item{Temporal structure}{repeatBout, nSyl, sylLen, pauseLen,
#'   addSilence}
#'   \item{Amplitude}{ampl, amplGlobal, attackLen}
#'   \item{Pitch & periodic source}{pitch, pitchGlobal, rolloff,
#'   rolloffOct, rolloffKHz, rolloffExact, glottis, pitchFloor,
#'   pitchCeiling, pitchSamplingRate}
#'   \item{Aperiodic source}{noise, rolloffNoise, rolloffNoiseExp,
#'    noiseFlatSpec}
#'   \item{Filter}{formants, formantsNoise, formantDep, formantDepStoch,
#'   formantWidth, formantCeiling, formantLocking, vocalTract, lipRad,
#'   noseRad, mouth, mouthOpenThres}
#'   \item{Amplitude & frequency modulation}{amDep, amFreq, amType,
#'   amShape, vibratoFreq, vibratoDep}
#'   \item{Nonlinear phenomena}{jitterDep, jitterLen, shimmerDep,
#'   shimmerLen, subRatio, subDep, nonlinBalance, nonlinRandomWalk,
#'   shortestEpoch}
#'   \item{Stochasticity & hyper-parameters}{temperature, tempEffects,
#'   maleFemale, creakyBreathy}
#'   \item{I/O}{plot, play, saveAudio, ...}
#'   \item{Technical}{smoothing, invalidArgAction, samplingRate,
#'   windowLength, step, overlap, wn, dynamicRange}
#' }
#'
#' @section Anchor format:
#' soundgen() and some other functions in this package accept arguments that
#' define a time series (e.g., pitch or amplitude contour). These arguments can
#' be provided as either numeric vectors or a list of anchors - points through
#' which the contour is interpolated. This "anchor format" can be a dataframe or
#' list with two elements: \code{$time} (ms or 0 to 1) and \code{value} of each
#' anchor. Ex.: \code{soundgen(pitch = list(time = c(0, 0.2, 1),
#' value = c(310, 340, 280)))}.
#'
#' @param repeatBout number of times the whole bout should be repeated
#' @param nSyl number of syllables in the bout; pitchGlobal, amplGlobal, and
#'   formants span multiple syllables, but not multiple bouts
#' @param sylLen average duration of each syllable, ms (vectorized)
#' @param pauseLen average duration of pauses between syllables, ms (can be
#'   negative between bouts to overlap them: force with invalidArgAction =
#'   'ignore') (vectorized). If there are multiple bouts, the first value of
#'   \code{pauseLen} is used at the pause between bouts
#' @param addSilence silence before and after the bout, ms: a vector of length 1
#'   (symmetric) or 2 (different duration of silence before/after the sound)
#'
#' @param ampl amplitude envelope (dB, 0 = max amplitude): a numeric vector or
#'   anchor format
#' @param amplGlobal global amplitude envelope spanning multiple syllables (dB,
#'   0 = no change) (anchor format)
#' @param attackLen duration of fade-in / fade-out at each end of syllables and
#'   noise (ms): a vector of length 1 (symmetric) or 2 (separately for fade-in
#'   and fade-out)
#'
#' @param pitch fundamental frequency within one syllable (anchor format). NAs
#'   in pitch vectors are accepted and are converted into voiceless fragments,
#'   but then set \code{nSyl = 1}
#' @param pitchGlobal unlike \code{pitch}, these anchors are
#'   used to create a smooth contour of average f0 across multiple syllables.
#'   The values are in semitones relative to the existing pitch, i.e. 0 = no
#'   change (anchor format)
#' @param rolloff the rate at which f0 harmonics in the spectrum become weaker,
#'   dB/oct (anchor format for all rolloff-related parameters). More negative
#'   rolloff = weaker upper harmonics; see \code{\link{getRolloff}} for more
#'   details
#' @param rolloffOct,rolloffKHz rolloff may be constant throughout the spectrum,
#'   or it may vary with each octave above f0 (rolloffOct) or per kHz increase
#'   in f0 above the baseline of 200 Hz (rolloffKHz); for both parameters,
#'   positive values mean the rate of rolloff increases toward upper
#'   frequencies)
#' @param rolloffExact user-specified relative amplitude of harmonics: a vector
#'   or matrix with one row per harmonic, scale 0 to 1 (overrides all other
#'   rolloff parameters)
#' @param glottis duration of the closed phase of a glottal cycle (silent) in
#'   relation to the open phase, \% (0 = no closed phase, 100 = closed phase as
#'   long as open phase, 200 = twice as long as the open phase, etc.); numeric
#'   vector or dataframe specifying time and value (anchor format). Use this
#'   effect sparingly: it is slow, and high values may affect harmonic
#'   composition and require a high sampling rate, especially in combination
#'   with high pitch
#' @param pitchFloor,pitchCeiling lower & upper bounds of f0
#' @param pitchSamplingRate sampling frequency of the pitch contour only, Hz.
#'   Low values reduce processing time. Set to \code{pitchCeiling} for optimal
#'   speed or to \code{samplingRate} for optimal quality
#'
#' @param noise intensity of turbulent noise (0 dB = same RMS as that of the
#'   periodic (voiced) component, negative values = less intense; anchor
#'   format). In soundgen 3.0, the noise component is always calibrated relative
#'   to the filtered harmonic component. When noise is present, the harmonic and
#'   noise components are filtered separately, their RMS amplitudes are
#'   normalized after filtering, and they are then mixed. Because noise can
#'   begin before the voiced part and continue after it, the time of noise
#'   anchors MUST be in ms, not [0, 1]; this is different from all other
#'   soundgen arguments that accept the anchor format with time either in ms or
#'   [0, 1]
#' @param rolloffNoise,rolloffNoiseExp,noiseFlatSpec linear (rolloffNoise,
#'   dB/kHz, anchor format) or exponential (rolloffNoiseExp, dB/oct, anchor
#'   format) rolloff of the excitation source for the noise component (anchor
#'   format) applied above \code{noiseFlatSpec} (Hz, scalar). More negative
#'   rolloff = less high-frequency noise
#'
#' @param formants a vector of formant frequencies (assuming formants are static
#'   throughout the sound); a list of formant times, frequencies, amplitudes,
#'   and bandwidths; or a character string referring to default presets for
#'   speaker "M1" (implemented: "aoieu0"). NA or NULL means no formants, only
#'   lip radiation (but a schwa is generated if \code{vocalTract} is specified).
#'   Time stamps for formants and mouth can be specified in ms relative to
#'    \code{sylLen} or on a scale of \code{[0, 1]}. See
#'    \code{\link{getFormantFilter}} for more details
#' @param formantsNoise the same as \code{formants}, but for the aperiodic noise
#'   rather than for the periodic component. If NA (default), the noise will be
#'   filtered through the same formants as the periodic component, approximating
#'   aspiration noise \code{[h]}
#' @param formantDep scale factor of formant amplitude (1 = no change relative
#'   to amplitudes in \code{formants})
#' @param formantDepStoch the amplitude of additional stochastic formants added
#'   above the highest specified formant, dB (only if temperature > 0)
#' @param formantWidth scale factor of formant bandwidth (1 = no change)
#' @param formantCeiling frequency to which stochastic formants are calculated
#'   to avoid losing energy in the upper part of the spectrum due to unmodeled
#'   resonances above the Nyquist frequencies, specified in multiples of
#'   Nyquist. If NULL (default), a faster theoretical correction is used (may
#'   fail for unusual sounds)
#' @param formantLocking the approximate proportion of sound in which one of the
#'   harmonics is locked to the nearest formant, 0 = none, 1 = the entire sound
#'   (anchor format). In multi-syllable sounds, formant locking is applied
#'   separately to each syllable using the corresponding portion of the
#'   bout-level formant filter
#' @param vocalTract the length of vocal tract, cm. Used for calculating formant
#'   dispersion (for adding extra formants) and formant transitions as the mouth
#'   opens and closes. If \code{NULL} or \code{NA}, the length is estimated
#'   based on specified formant frequencies, if any (anchor format)
#' @param mouth mouth opening (0 to 1, 0.5 = neutral, i.e. no
#'   modification) (anchor format)
#' @param lipRad the effect of lip radiation on source spectrum, dB/oct (the
#'   default of +6 dB/oct produces a high-frequency boost when the mouth is
#'   open)
#' @param noseRad the effect of radiation through the nose on source spectrum,
#'   dB/oct (the alternative to \code{lipRad} when the mouth is closed)
#' @param mouthOpenThres open the lips (switch from nose radiation to lip
#'   radiation) when the mouth is open \code{>mouthOpenThres}, 0 to 1
#'
#' @param amDep amplitude modulation (AM) depth, \%. 0: no change; 100: AM with
#'   amplitude range equal to the dynamic range of the sound (anchor format)
#' @param amFreq AM frequency, Hz (anchor format)
#' @param amType "logistic" = logistic (default), "sine" = sinusoidal
#' @param amShape ignored if amType = "sine", otherwise determines the shape of
#'   non-sinusoidal AM: 0 = ~sine, -1 = notches, +1 = clicks (anchor format)
#' @param vibratoFreq the rate of regular pitch modulation, or vibrato, Hz
#'   (anchor format)
#' @param vibratoDep the depth of vibrato, semitones (anchor format)
#'
#' @param jitterDep cycle-to-cycle random pitch variation, semitones (anchor
#'   format)
#' @param jitterLen duration of stable periods between pitch jumps, ms. Use a
#'   low value for harsh noise, a high value for irregular vibrato or shaky
#'   voice (anchor format)
#' @param shimmerDep random variation in amplitude between individual glottal
#'   cycles (0 to 100\% of original amplitude of each cycle) (anchor format)
#' @param shimmerLen duration of stable periods between amplitude jumps, ms. Use
#'   a low value for harsh noise, a high value for shaky voice (anchor format)
#' @param subRatio a positive integer giving the ratio of f0 (the main
#'   fundamental) to g0 (a lower frequency): 1 = no subharmonics, 2 = period
#'   doubling regardless of pitch changes, 3 = period tripling, etc.
#' @param subDep the depth of subharmonics relative to the main frequency
#'   component (f0), \%. 0: no subharmonics; 100: g0 harmonics are as strong as
#'   the nearest f0 harmonic (anchor format)
#' @param nonlinBalance hyperparameter for regulating the (approximate)
#'   proportion of sound with different regimes of pitch effects (none /
#'   subharmonics only / subharmonics and jitter). 0\% = no nonlinear phenomena;
#'   100\% = the entire sound has jitter + subharmonics. Ignored if temperature
#'   = 0
#' @param nonlinRandomWalk a numeric vector specifying the timing of nonlinear
#'   regimes: 0 = none, 1 = subharmonics, 2 = subharmonics + jitter + shimmer
#' @param shortestEpoch minimum duration of each epoch with unchanging
#'   subharmonics regime or formant locking, in ms
#'
#' @param temperature hyperparameter for regulating the amount of stochasticity
#'   in sound generation
#' @param tempEffects a list of scaling coefficients regulating the effect of
#'   temperature on particular parameters. To change, specify just those pars
#'   that you want to modify (1 = default, 0 = no stochastic behavior).
#' \describe{
#'   \item{\code{amplDep, pitchDep, noiseDep}}{random fluctuations of
#'   user-specified amplitude / pitch / noise anchors}
#'   \item{\code{amplDriftDep}}{drift of amplitude mirroring pitch drift}
#'   \item{\code{formDisp}}{dispersion of stochastic formants}
#'   \item{\code{formDrift}}{formant frequencies}
#'   \item{\code{glottisDep}}{proportion of glottal cycle with closed glottis}
#'   \item{\code{pitchDriftDep}}{amount of slow random drift of f0}
#'   \item{\code{pitchDriftFreq}}{frequency of slow random drift of f0}
#'   \item{\code{rolloffDriftDep}}{drift of rolloff mirroring pitch drift}
#'   \item{\code{specDep}}{rolloff, rolloffNoise, nonlinear effects, attack}
#'   \item{\code{subDriftDep}}{drift of subharmonic frequency and bandwidth
#'   mirroring pitch drift}
#'   \item{\code{sylLenDep}}{duration of syllables and pauses}
#' }
#' @param maleFemale hyperparameter for shifting f0 contour, formants, and
#'   vocalTract to make the speaker appear more male (-1...0) or more female
#'   (0...+1); 0 = no change
#' @param creakyBreathy hyperparameter for an adjustment of voice quality from
#'   creaky (-1) to breathy (+1); 0 = no change
#'
#' @param plot if TRUE, plots a spectrogram
#' @param play if TRUE, plays the synthesized sound using the default player on
#'   your system. If character, passed to \code{\link[tuneR]{play}} as the name
#'   of player to use, eg "aplay", "play", "vlc", etc. In case of errors, try
#'   setting another default player for \code{\link[tuneR]{play}}
#' @param saveAudio if TRUE, saves the result as "soundgen.wav" in the working
#'   directory
#' @param ... other plotting parameters passed to \code{\link{spectrogram}}
#'
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   permitted range: 'adjust' = reset to default value, 'abort' = stop
#'   execution, 'ignore' = throw a warning and continue (may crash)
#' @param smoothing a list of parameters passed to \code{\link{interpolate}} to
#'   control the interpolation and smoothing of contours drawn through anchors
#' @param samplingRate sampling rate of the output (Hz)
#' @param windowLength length of the FFT window (ms)
#' @param step step between successive windows (ms); if provided, overrides
#'   \code{overlap}
#' @param overlap overlap between successive windows (0–100\%)
#' @param wn wn window type accepted by \code{\link{winFun}}: character string
#'   or function
#' @param dynamicRange dynamic range (dB). Harmonics and noise more than
#'   dynamicRange under maximum amplitude are discarded to save computational
#'   resources
#'
#' @export
#' @examples
#' # Detailed documentation: https://cogsci.se/soundgen/sound_generation.html
#'
#' # A gallery of examples with code: https://cogsci.se/soundgen/demos.html
#'
#' # A GUI for soundgen is available as a Shiny app.
#' # Type "soundgen_app()" to open it in your default browser
#'
#'# Set "playback" to TRUE for default system player or the name of preferred
#' # player (eg "aplay") to play back the audio from examples
#' playback = FALSE # or TRUE, 'aplay', 'vlc', etc. (see ?playme)
#'
#' sound = soundgen(play = playback)
#' # spectrogram(sound, 16000)
#' # playme(sound)
#'
#' # Control of intonation, amplitude envelope, formants
#' s0 = soundgen(
#'   pitch = c(300, 390, 250),
#'   ampl = data.frame(time = c(0, 50, 300), value = c(-5, -10, 0)),
#'   attackLen = c(10, 50),
#'   formants = c(600, 900, 2200),
#'   play = playback
#' )
#'
#' # Use the in-built collection of presets:
#' # names(presets)  # speakers
#' # names(presets$Chimpanzee)  # calls per speaker
#' s1 = eval(parse(text = presets$Chimpanzee$Scream_conflict))  # screaming chimp
#' # playme(s1)
#' s2 = eval(parse(text = presets$F1$Scream))  # screaming woman
#' # playme(s2, 18320)
#'
#' # presets of some vowels and consonants
#' names(presets$M1$Formants$vowels)
#' soundgen(sylLen = 500, formants = 'aoieu0', play = playback)
#'
#' \dontrun{
#' # Ultrasound - need to adjust some defaults:
#'  soundgen(
#'    sylLen = 10,  # just 10 ms
#'    attackLen = 1,  # should be very short for short vocalizations
#'    addSilence = 2,
#'    pitch = c(45000, 35000, 65000, 60000),  # 35-60 kHz
#'    rolloff = -12,
#'    rolloffKHz = 0,
#'    formants = NA,  # no formants (or set vocal tract length)
#'    samplingRate = 350000,  # at least ~10 times the max f0
#'    pitchSamplingRate = 350000,  # the same as samplingRate
#'    windowLength = .25,  # need very short window lengths for USV
#'    pitchCeiling = 90000, # max allowed pitch
#'    invalidArgAction = 'ignore', # override the ranges allowed by default
#'    temperature = 1e-4,
#'    plot = TRUE
#'  )
#'
#' # soundgen plays Bach
#' dt = otherToHz(
#'   c('E4', 'D4', 'E4', 'C4', 'E4', 'B3', 'E4', 'A3', 'E4', 'G#3', 'E4',
#'   'A3', 'E4', 'B3', 'E4', 'C4', 'E4', 'E3', 'E4', 'F#3', 'E4', 'G#3',
#'   'E4', 'A3', 'E4', 'G#3', 'E4', 'A3', 'E4', 'B3', 'E4', 'C4'), 'notes')
#' out = numeric(0)
#' for (s in 1:length(dt)) {
#'   syl_s = soundgen(sylLen = 100, pitch = dt[s], rolloff = -15,
#'                    formants = c(750, 1400, 2900, 3800), noise = -45,
#'                    attackLen = 50, addSilence = 0, temperature = .01)
#'   syl_s = fade(soundgen:::matchLengths(syl_s, 0.1 * 16000), samplingRate = 16000)
#'   out = c(out, syl_s[1:(0.1 * 16000)])
#' }
#' spectrogram(out, 16000, yScale = 'ERB')
#' playme(out, 16000)
#' }
soundgen = function(
  # Temporal structure
  repeatBout = 1,
  nSyl = 1,
  sylLen = 500,
  pauseLen = 200,
  addSilence = 100,

  # Amplitude
  ampl = NA,
  amplGlobal = NA,
  attackLen = 50,

  # Pitch & periodic source
  pitch = c(100, 150, 100),
  pitchGlobal = NA,
  rolloff = -12,
  rolloffOct = 0,
  rolloffKHz = 0,
  rolloffExact = NULL,
  glottis = 0,
  pitchFloor = 1,
  pitchCeiling = 3500,
  pitchSamplingRate = 16000,

  # Aperiodic source
  noise = NULL,
  rolloffNoise = -4,
  noiseFlatSpec = 1200,
  rolloffNoiseExp = 0,

  # Filter
  formants = c(860, 1430, 2900, 4100),
  formantsNoise = NA,
  formantDep = 1,
  formantDepStoch = 1,
  formantWidth = 1,
  formantCeiling = NULL,
  formantLocking = 0,
  vocalTract = NA,
  mouth = NULL,
  lipRad = 6,
  noseRad = 4,
  mouthOpenThres = 0,

  # Amplitude & frequency modulation
  amDep = 0,
  amFreq = 30,
  amType = c('logistic', 'sine'),
  amShape = 0,
  vibratoFreq = 5,
  vibratoDep = 0,

  # Nonlinear phenomena
  jitterDep = 0,
  jitterLen = 1,
  shimmerDep = 0,
  shimmerLen = 1,
  subRatio = 2,
  subDep = 0,
  nonlinBalance = 100,
  nonlinRandomWalk = NULL,
  shortestEpoch = 300,

  # Stochasticity & hyper-parameters
  temperature = 0.025,
  tempEffects = list(),
  maleFemale = 0,
  creakyBreathy = 0,

  # I/O
  plot = FALSE,
  play = FALSE,
  saveAudio = FALSE,

  # Technical / I/O
  invalidArgAction = c('adjust', 'abort', 'ignore'),
  smoothing = list(interpol = 'splineFC',
                   discontThres = 0.05,
                   jumpThres = 0.01),
  samplingRate = 16000,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = 'gaussian',
  dynamicRange = 80,
  ...
) {
  # deprecated parameters
  call_arg_names = names(match.call())
  if (any(c('rolloffParab', 'rolloffParabHarm', 'rolloffParabCeiling') %in%
          call_arg_names))
    message('parabolic interpolation of rolloff is deprecated')
  if (any(c('subFreq', 'subWidth') %in% call_arg_names))
    message('subFreq and subWidth are deprecated')
  if ('noiseAmpRef' %in% call_arg_names)
    message('noiseAmpRef is deprecated')

  # match character arguments
  amType = match.arg(amType)
  invalidArgAction = match.arg(invalidArgAction)

  # check that values of numeric non-anchor arguments are valid and within range
  pars_to_check = c(
    'repeatBout', 'nSyl', 'sylLen', 'pauseLen', 'temperature', 'maleFemale',
    'creakyBreathy', 'nonlinBalance', 'subFreq', 'subWidth', 'shortestEpoch',
    'attackLen', 'rolloffParab', 'rolloffParabHarm', 'lipRad', 'noseRad',
    'mouthOpenThres', 'formantDep', 'formantDepStoch', 'formantWidth',
    'formantCeiling', 'samplingRate', 'windowLength', 'dynamicRange',  'overlap',
    'addSilence', 'pitchFloor', 'pitchCeiling', 'pitchSamplingRate', 'noiseFlatSpec')
  for (p in pars_to_check) {
    if (p %in% rownames(permittedValues)) {
      gp = get(p, inherits = FALSE)
      if (is.numeric(gp)) {
        assign(p, validatePars(p, gp, permittedValues, invalidArgAction))
      }
    }
  }

  ## stochastic rounding of the number of syllables and repeatBouts
  #   (eg for nSyl = 2.5, we'll have 2 or 3 syllables with equal probs)
  #   NB: this is very useful for morphing
  idx_nonInt_nSyl = which(nSyl != floor(nSyl))  # not integers
  if (any(idx_nonInt_nSyl)) {
    nSyl[idx_nonInt_nSyl] = floor(nSyl[idx_nonInt_nSyl]) +
      rbinom(length(idx_nonInt_nSyl), 1, nSyl[idx_nonInt_nSyl] -
               floor(nSyl[idx_nonInt_nSyl]))
  }
  if (repeatBout != floor(repeatBout)) {  # not an integer
    repeatBout = floor(repeatBout) +
      rbinom(1, 1, repeatBout - floor(repeatBout))
  }
  nSyl = pmax(1, nSyl)
  repeatBout = max(1, repeatBout)

  # make sure sylLen and pauseLen are vectors of appropriate length
  len_sylLen = length(sylLen)
  if (len_sylLen > 1 || nSyl > 1) {
    if (nSyl == 1) nSyl = len_sylLen
    if (len_sylLen != nSyl) {
      if (len_sylLen == 1) {
        sylLen = rep(sylLen, nSyl)
      } else {
        sylLen = approx(sylLen, n = nSyl)$y
      }
      len_sylLen = nSyl
    }
    if (length(pauseLen) != len_sylLen - 1) {
      if (len_sylLen == 2) {
        pauseLen = pauseLen[1]
      } else {
        if (length(pauseLen) == 1) {
          pauseLen = rep(pauseLen, nSyl - 1)
        } else {
          pauseLen = approx(pauseLen, n = nSyl - 1)$y
        }
      }
    }
  }
  if (any(pauseLen < 0) && nSyl > 1) {
    stop(paste(
      'Negative pauseLen is allowed between bouts, but not between syllables.',
      'Use repeatBout instead of nSyl if you need syllables to overlap'
    ))
  }

  # deal with NAs in pitch contour: save NA location, then interpolate
  if (nSyl == 1 && is.numeric(pitch) && any(is.na(pitch))) {
    lp = length(pitch)
    change_idx = which(diff(is.na(pitch)) != 0)  # last idx before change
    na_seg = data.frame(
      start = c(1, change_idx + 1),
      end = c(change_idx, lp)
    )
    na_seg = na_seg[is.na(pitch[na_seg$start]), ]
    na_seg$prop_start = (na_seg$start - 1) / lp
    na_seg$prop_end = na_seg$end / lp
    pitch = interpolateNA(pitch)  # fill in NA by interpolation
  } else {
    na_seg = NULL
  }

  # Validate smoothing par-s
  if (!is.list(pitch) || is.null(smoothing$discontThres))
    smoothing$discontThres = 0
  for (s in c('discontThres', 'jumpThres')) {
    if (is.numeric(smoothing[[s]])) {
      smoothing[[s]] = validatePars(
        s, smoothing[[s]], permittedValues, invalidArgAction
      )
    } else {
      smoothing[[s]] = defaults[[s]]
    }
  }

  # check and, if necessary, reformat anchors to dataframes
  anchors = c('pitch', 'pitchGlobal', 'glottis',
              'ampl', 'amplGlobal',
              'mouth', 'vocalTract', 'formantLocking',
              'vibratoFreq', 'vibratoDep',
              'subRatio', 'subDep',
              'jitterLen', 'jitterDep', 'shimmerLen', 'shimmerDep',
              'rolloff', 'rolloffOct', 'rolloffKHz',
              'rolloffNoise', 'rolloffNoiseExp',
              'amDep', 'amFreq', 'amShape')
  for (anchor in anchors) {
    assign(anchor, reformatAnchors(get(anchor), name = anchor,
                                   invalidArgAction = invalidArgAction))
  }

  # reformat noise
  if (is.numeric(noise)) {
    # if noise is numeric, fix exactly same timing as voiced
    lockNoiseToVoiced = TRUE
    if (length(noise) > 0) {
      noise = data.frame(
        time = seq(0, sylLen[1], length.out = max(2, length(noise))),
        value = noise
      )
    }
  } else {
    lockNoiseToVoiced = FALSE
    if (!is.list(noise)) {
      noise = data.frame(
        time = c(0, sylLen[1]),
        value = c(-dynamicRange, -dynamicRange)
      )
    }
  }
  # throw a warning if noise$time seems to be [0, 1] instead of ms
  if (all(noise$time >= 0 & noise$time <= 1))
    warning('The time of noise anchors must be in ms, not [0, 1]')

  # validate pitch floor/ceiling
  if (is.list(pitch) && invalidArgAction != 'ignore') {
    if (any(pitch$value < pitchFloor, na.rm = TRUE)) {
      pitchFloor = 0.1
      message('Some pitch values are lower than pitchFloor; lowering to 0.1 Hz')
    }
    if (any(pitch$value >= samplingRate / 2, na.rm = TRUE)) {
      samplingRate = max(pitch$value * 4)
      message(paste('Some pitch values exceed Nyquist frequency;',
                    'raising samplingRate to', samplingRate, 'Hz'))
    }
    if (any(pitch$value > pitchSamplingRate, na.rm = TRUE)) {
      pitchSamplingRate = samplingRate
      message(paste('Some pitch values exceed pitchSamplingRate.',
                    'Resetting pitchSamplingRate to samplingRate.'))
    }
    if (any(pitch$value > pitchCeiling, na.rm = TRUE)) {
      pitchCeiling = samplingRate / 2
      message(paste('Some pitch values exceed pitchCeiling.',
                    'Resetting pitchCeiling to Nyquist frequency (samplingRate / 2).'))
    }
    mp = max(pitch$value)
    if (mp * 10 > pitchSamplingRate && invalidArgAction != 'ignore') {
      pitchSamplingRate = mp * 10
      message(paste0('pitchSamplingRate should be much higher than the ',
                     'highest pitch; resetting to ', mp * 10, ' Hz'))
    }
    if (pitchSamplingRate > samplingRate && invalidArgAction != 'ignore') {
      samplingRate = pitchSamplingRate
      message(paste0('Resetting samplingRate to ',
                     samplingRate, ' Hz because of high pitch'))
    }
  }

  # check amplitude anchors and make all values negative
  if (is.list(ampl)) {
    if (any(ampl$value > 0)) {
      ampl$value = ampl$value - max(ampl$value)
      message(paste('The recommended range for ampl is (-dynamicRange, 0).',
                    'If positive, values are transformed to be non-positive'))
    }
  }

  # for amplGlobal, make the first value 0
  if (is.list(amplGlobal)) {
    if (any(amplGlobal$value != 0)) {
      amplGlobal$value = amplGlobal$value - amplGlobal$value[1]
    }
  }

  # wl / step validation
  dur = (sum(sylLen) + sum(pauseLen)) / 1000
  val = validateWlOvlp(list(samplingRate = samplingRate, dur = dur),
                       windowLength, step, overlap)
  windowLength = val$windowLength; step = val$step; step_points = val$step_points
  wl = val$wl

  # tempEffects are either left at default levels or multiplied by user-supplied
  # scaling coefficients (1 = no change)
  es = c('amplDep', 'amplDriftDep', 'formDisp', 'formDrift', 'glottisDep',
         'noiseDep', 'pitchDep', 'pitchDriftDep', 'pitchDriftFreq',
         'rolloffDriftDep', 'specDep', 'subDriftDep', 'sylLenDep')
  for (e in es) {
    if (!is.numeric(tempEffects[[e]])) {
      tempEffects[[e]] = defaults[[e]]
    } else {
      tempEffects[[e]] = defaults[[e]] * tempEffects[[e]]
    }
  }
  for (name_s in setdiff(names(tempEffects), es)) {
    message(paste0('"', name_s, '" is not among valid tempEffects parameters (',
                   paste(es, collapse = ', '),
                   '). See ?soundgen'))
  }

  # expand formants to full format for adjusting bandwidth if creakyBreathy > 0
  formants = reformatFormants(formants)
  formantsNoise = reformatFormants(formantsNoise)
  if (is.list(formantsNoise) && !is.numeric(vocalTract) && !is.list(vocalTract)) {
    # the only cond in which we do not create extra stochastic formants for
    # noise is when we have user-specified formantsNoise (ie not just breathing)
    # and we don't know VTL
    formantDepStoch_noise = 0
  } else {
    formantDepStoch_noise = formantDepStoch
  }

  # Do we have source-filter interaction?
  sourceFilterInter = FALSE
  if (is.list(formantLocking)) {
    if (any(formantLocking$value > 0)) {
      sourceFilterInter = TRUE
    }
  }

  ## adjust parameters according to the specified hyperparameters
  # effects of creakyBreathy hyper
  if (creakyBreathy < 0) {
    # for creaky voice
    nonlinBalance = min(100, nonlinBalance - creakyBreathy * 100)
    jitterDep$value = jitterDep$value - creakyBreathy / 2
    jitterDep$value[jitterDep$value < 0] = 0
    shimmerDep$value = shimmerDep$value - creakyBreathy * 5
    shimmerDep$value[shimmerDep$value < 0] = 0
    subDep$value = subDep$value - 100 * creakyBreathy
  } else if (creakyBreathy > 0) {
    # for breathy voice, add breathing
    noise$value = pmin(
      permittedValues['noiseAmpl', 'high'],
      noise$value + creakyBreathy * (dynamicRange + permittedValues['noiseAmpl', 'high']))
    # increase formant bandwidths by up to 100%
    if (is.list(formants)) {
      for (f in seq_along(formants)) {
        formants[[f]]$width = formants[[f]]$width * (creakyBreathy + 1)
      }
    }
  }
  # adjust rolloff for both creaky and breathy voices
  rolloff$value = rolloff$value - creakyBreathy * 10
  rolloffOct$value = rolloffOct$value - creakyBreathy * 1
  if (invalidArgAction != 'ignore') {
    rolloff$value[rolloff$value < permittedValues['rolloff', 'low']] =
      permittedValues['rolloff', 'low']
    rolloff$value[rolloff$value > permittedValues['rolloff', 'high']] =
      permittedValues['rolloff', 'high']
    rolloffOct$value[rolloffOct$value < permittedValues['rolloffOct', 'low']] =
      permittedValues['rolloffOct', 'low']
    rolloffOct$value[rolloffOct$value > permittedValues['rolloffOct', 'high']] =
      permittedValues['rolloffOct', 'high']
  }

  # effects of maleFemale hyper
  if (maleFemale != 0) {
    # adjust pitch and formants along the male-female dimension
    # pitch varies by 1 octave up or down
    if (is.list(pitch)) {
      pitch$value = pitch$value * 2 ^ maleFemale
    }
    if (is.list(formants)) {
      for (f in seq_along(formants)) {
        # formants vary by 25% up or down:
        #   see http://www.santiagobarreda.com/vignettes/v1/v1.html)
        if (!is.null(formants[[f]]$freq)) {
          formants[[f]]$freq = formants[[f]]$freq * 1.25 ^ maleFemale
        }
      }
    }
    # vocalTract varies by 25% from the average
    if (is.list(vocalTract)) {
      if (is.numeric(vocalTract$value)) {
        vocalTract$value = vocalTract$value * (1 - .25 * maleFemale)
      }
    }
  }

  # prepare a list of pars for calling generateHarmonics()
  pars_to_vary = c(
    'attackLen',
    'shortestEpoch'
  )  # don't add nonlinBalance, otherwise there is no simple way to remove noise at temp>0
  anchors_to_wiggle = c(
    'pitch', 'ampl', 'glottis',
    'vibratoFreq', 'vibratoDep',
    'subRatio', 'subDep',
    'jitterLen', 'jitterDep',
    'shimmerLen', 'shimmerDep',
    'rolloff', 'rolloffKHz', 'rolloffOct',
    'rolloffNoise', 'rolloffNoiseExp')
  pars_to_round = c('attackLen', 'subRatio')
  pars_list = list(
    'attackLen' = attackLen,
    'jitterDep' = jitterDep,
    'jitterLen' = jitterLen,
    'vibratoFreq' = vibratoFreq,
    'vibratoDep' = vibratoDep,
    'shimmerDep' = shimmerDep,
    'shimmerLen' = shimmerLen,
    'rolloff' = rolloff,
    'rolloffOct' = rolloffOct,
    'rolloffKHz' = rolloffKHz,
    'rolloffExact' = rolloffExact,
    'temperature' = temperature,
    'pitchDriftDep' = tempEffects$pitchDriftDep,
    'pitchDriftFreq' = tempEffects$pitchDriftFreq,
    'amplDriftDep' = tempEffects$amplDriftDep,
    'subDriftDep' = tempEffects$subDriftDep,
    'rolloffDriftDep' = tempEffects$rolloffDriftDep,
    'shortestEpoch' = shortestEpoch,
    'subRatio' = subRatio,
    'subDep' = subDep,
    'nonlinBalance' = nonlinBalance,
    'nonlinRandomWalk' = nonlinRandomWalk,
    'pitchFloor' = pitchFloor,
    'pitchCeiling' = pitchCeiling,
    'pitchSamplingRate' = pitchSamplingRate,
    'dynamicRange' = dynamicRange,
    'samplingRate' = samplingRate,
    'smoothing' = smoothing,
    'rolloffNoise' = rolloffNoise,
    'rolloffNoiseExp' = rolloffNoiseExp
  )
  pars_syllable = pars_list
  pitchDeltas = rep(1, nSyl)
  if (is.list(pitchGlobal)) {
    if (any(!is.na(pitchGlobal))) {
      if (any(pitchGlobal$value != 0) && nSyl > 1) {
        pitchDeltas = 2 ^ (
          getDiscreteContour(
            len = nSyl,
            anchors = pitchGlobal,
            plot = FALSE
          ) / 12
        )
      }
    }
  }

  wiggleNoise = FALSE
  if (is.list(noise)) {
    if (temperature > 0 &&
        any(noise$value > -dynamicRange)) {
      wiggleNoise = TRUE
    }
  }

  wiggleAmpl_per_syl = FALSE
  if (is.numeric(ampl) || is.list(ampl)) {
    if (temperature > 0 && any(ampl$value > (-dynamicRange))) {
      wiggleAmpl_per_syl = TRUE
    }
  }

  wiggleGlottis = FALSE
  if (is.numeric(glottis) || is.list(glottis)) {
    if (temperature > 0 && any(glottis$value > 0)) {
      wiggleGlottis = TRUE
    }
  }

  # For polysyllabic vocalizations, calculate amplitude envelope correction
  # per voiced syllable
  if (is.list(amplGlobal)) {
    if (any(amplGlobal$value != 0)) {
      amplEnvelope = do.call(getSmoothContour, c(smoothing, list(
        anchors = amplGlobal,
        len = nSyl,
        valueFloor = -dynamicRange,
        valueCeiling = dynamicRange,
        samplingRate = samplingRate
      )))
      # convert from dB to linear multiplier
      amplEnvelope = 10 ^ (amplEnvelope / 20)
    } else {
      amplEnvelope = rep(1, nSyl)
    }
  } else {
    amplEnvelope = rep(1, nSyl)
  }

  # prepare a list of formantPars in case we add source-filter interaction
  formantPars = list(
    vocalTract = vocalTract,
    formantDep = formantDep,
    formantWidth = formantWidth,
    formantCeiling = formantCeiling,
    lipRad = lipRad,
    noseRad = noseRad,
    mouthOpenThres = mouthOpenThres,
    mouth = mouth,
    temperature = temperature,
    formDrift = tempEffects$formDrift,
    formDisp = tempEffects$formDisp,
    samplingRate = samplingRate,
    windowLength = windowLength,
    step = step,
    wn = wn,
    smoothing = smoothing
  )

  if (!is.list(formantsNoise)) {
    formantsNoise = formants
  }

  # START OF BOUT GENERATION
  for (b in 1:repeatBout) {
    # syllable segmentation
    syllables = divideIntoSyllables(
      sylLen = sylLen,
      nSyl = nSyl,
      pauseLen = pauseLen,
      sylDur_min = permittedValues['sylLen', 'low'],
      sylDur_max = permittedValues['sylLen', 'high'],
      pauseDur_min = permittedValues['pauseLen', 'low'],
      pauseDur_max = permittedValues['pauseLen', 'high'],
      temperature = temperature * tempEffects$sylLenDep
    )
    # end of syllable segmentation

    # Prepare a spectral envelope. It's added after syllable generation,
    # but we set it up here to be able to add source-filter interactions
    # such as formant-locking
    if (sourceFilterInter) {
      approx_dur_points = syllables$end[nrow(syllables)] / 1000 * samplingRate

      specEnv_list = do.call(getFormantFilter, c(
        formantPars,
        list(nc = round(approx_dur_points / step_points), # approximate n of windows for fft
             nr = max(2, wl %/% 2 + 1), # n of frequency bins for fft
             formants = formants,
             formantDepStoch = formantDepStoch,
             output = 'detailed')
      ))
      # image(t(specEnv_list$specEnv))
    }


    # START OF SYLLABLE GENERATION
    voiced_list = vector('list', nrow(syllables) * 2)  # for both syls and pauses
    aperiodic = list()
    noise_syl = list()

    for (s in 1:nrow(syllables)) {
      # scale noise anchors for polysyllabic sounds with length(sylLen) > 1
      noise_syl[[s]] = noise
      noise_syl[[s]]$time = scaleNoiseAnchors(
        noiseTime = noise_syl[[s]]$time,
        sylLen_old = sylLen[1],
        sylLen_new = syllables$dur[s]
      )

      # wiggle par values for this particular syllable, making sure
      #   they are within the permitted range for each variable
      for (anchor in c('pitch', 'ampl', 'glottis')) {  # the first 3 in anchors_to_wiggle
        newName = paste0(anchor, '_per_syl')
        assign(newName, get(anchor))  # just create a copy with name _per_syl
      }

      if (temperature > 0) {
        # OR if (temperature>0 & nrow(syllables)>1)
        # if you don't want to mess with single-syllable vocalizations
        for (p in seq_along(pars_to_vary)) {
          par_value = as.numeric(unlist(pars_list[pars_to_vary[p]]))
          l = permittedValues[pars_to_vary[p], 'low']
          h = permittedValues[pars_to_vary[p], 'high']
          sd = (h - l) * temperature * tempEffects$specDep
          pars_syllable[[pars_to_vary[p]]] = rnorm_truncated2(
            n = length(par_value),
            mean = par_value,
            low = l,
            high = h,
            sd = sd,
            roundToInteger = (pars_to_vary[p] %in% pars_to_round),
            invalidArgAction = invalidArgAction
          )
        }
        if (is.list(pitch_per_syl)) {
          pitch_per_syl = wiggleAnchors(
            df = pitch_per_syl,
            temperature = temperature,
            low = c(0, pitchFloor),
            high = c(1, pitchCeiling),
            temp_coef = tempEffects$pitchDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleNoise) {
          noise_syl[[s]] = wiggleAnchors(
            df = noise_syl[[s]],
            temperature = temperature,
            low = c(-Inf, -dynamicRange),
            high = c(+Inf, permittedValues['noiseAmpl', 'high']),
            wiggleAllRows = if (lockNoiseToVoiced) FALSE else TRUE,
            temp_coef = tempEffects$noiseDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleAmpl_per_syl) {
          ampl_per_syl = wiggleAnchors(
            df = ampl_per_syl,
            temperature = temperature,
            low = c(0, -dynamicRange),
            high = c(1, 0),
            temp_coef = tempEffects$amplDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleGlottis) {
          glottis_per_syl = wiggleAnchors(
            df = glottis_per_syl,
            temperature = temperature,
            low = c(0, 0),
            high = c(1, Inf),
            temp_coef = tempEffects$glottisDep,
            invalidArgAction = invalidArgAction
          )
        }
        # wiggle anchors except special cases (pitch, ampl, glottis)
        for (i in 4:length(anchors_to_wiggle)) {
          anchor = anchors_to_wiggle[i]
          l = permittedValues[anchor, 'low']
          h = permittedValues[anchor, 'high']
          anchor_new = wiggleAnchors(
            df = get(anchor),
            temperature = temperature,
            low = c(0, l),
            high = c(1, h),
            temp_coef = tempEffects$specDep,
            sd_values = (h - l) * temperature * tempEffects$specDep,
            roundToInteger = (any(pars_to_round == anchor)),
            invalidArgAction = invalidArgAction
          )
          pars_syllable[[anchor]] = anchor_new
        }
      }

      # generate the voiced part only if noise is weaker than 40 dB
      # and the voiced part is long enough to bother synthesizing it
      dur_syl = as.numeric(syllables[s, 'end'] - syllables[s, 'start'])
      generateVoiced = is.list(pitch_per_syl)
      if (invalidArgAction != 'ignore' &&
          dur_syl < permittedValues['sylLen', 'low']) {
        generateVoiced = FALSE
      }
      if (is.list(noise)) {
        if (min(noise$value) >= 40) {
          generateVoiced = FALSE
        }
      }

      if (!generateVoiced) {
        syllable = rep(0, round(dur_syl * samplingRate / 1000))
      } else {
        # generate smooth pitch contour for this particular syllable
        if (is.list(pitch_per_syl) || is.numeric(pitch_per_syl)) {
          pitchContour_syl = do.call(getSmoothContour, c(
            smoothing, list(
              anchors = pitch_per_syl,
              len = round(dur_syl * pitchSamplingRate / 1000),
              samplingRate = pitchSamplingRate,
              valueFloor = pitchFloor,
              valueCeiling = pitchCeiling,
              thisIsPitch = TRUE
            ))) * pitchDeltas[s]
          # plot(pitchContour_syl, type = 'l')
        }

        # if doing formant locking: cut the syllable-specific part of the
        # bout-level spectral envelope
        if (sourceFilterInter) {
          total_dur_ms = syllables$end[nrow(syllables)]

          frame_times_ms = suppressWarnings(
            as.numeric(colnames(specEnv_list$specEnv))
          )

          if (length(frame_times_ms) != ncol(specEnv_list$specEnv) ||
              any(is.na(frame_times_ms))) {
            frame_times_ms = seq(0, total_dur_ms,
                                 length.out = ncol(specEnv_list$specEnv))
          } else if (max(frame_times_ms, na.rm = TRUE) <= 1 && total_dur_ms > 1) {
            # time stamps are proportional (0 to 1), not ms
            frame_times_ms = frame_times_ms * total_dur_ms
          }

          keep_frames = frame_times_ms >= syllables[s, 'start'] &
            frame_times_ms <= syllables[s, 'end']

          # include boundary frames so the slice spans the whole syllable
          before = which(frame_times_ms <= syllables[s, 'start'])
          after = which(frame_times_ms >= syllables[s, 'end'])

          if (length(before) > 0) keep_frames[max(before)] = TRUE
          if (length(after) > 0) keep_frames[min(after)] = TRUE

          # safety net: at least one frame must be kept
          if (!any(keep_frames)) {
            mid_ms = (syllables[s, 'start'] + syllables[s, 'end']) / 2
            keep_frames[which.min(abs(frame_times_ms - mid_ms))] = TRUE
          }

          specEnv_syl = specEnv_list$specEnv[, keep_frames, drop = FALSE]

          if (!is.null(specEnv_list$formantSummary)) {
            formantSummary_syl = specEnv_list$formantSummary[, keep_frames, drop = FALSE]
          } else {
            formantSummary_syl = NULL
          }
        } else {
          specEnv_syl = NULL
          formantSummary_syl = NULL
        }

        # ***THE ACTUAL SYNTHESIS IS HERE***
        # print(pars_syllable)
        # plot(pitchContour_syl, type = 'l')
        syllable = try(do.call(generateHarmonics, c(
          pars_syllable[which(!names(pars_syllable) %in% c('rolloffNoise', 'rolloffNoiseExp'))],
          list(pitch = pitchContour_syl,
               ampl = ampl_per_syl,
               glottis = glottis_per_syl,
               normalize = TRUE,
               formantLocking = switch(sourceFilterInter + 1, NULL, formantLocking),
               # ifelse doesn't return NULL properly, thus switch() instead
               specEnv = switch(sourceFilterInter + 1, NULL, specEnv_syl),
               formantSummary = switch(sourceFilterInter + 1, NULL, formantSummary_syl))
        )) * amplEnvelope[s]  # correction of amplitude per syllable
        )
      }
      # plot(syllable, type = 'l')
      # spectrogram(syllable, samplingRate = samplingRate)
      # playme(syllable, samplingRate = samplingRate)
      # ***THE ACTUAL SYNTHESIS IS HERE***

      if (inherits(syllable, 'try-error')) {
        stop('Failed to generate the new syllable!')
      }
      # if (any(is.na(syllable))) {
      #   stop('The new syllable contains NA values!')
      # }

      # silence the syllable at the former location of NAs in pitch contour, if any
      if (!is.null(na_seg)) {
        syllable = silenceSegments(
          x = syllable,
          samplingRate = samplingRate,
          na_seg = na_seg,
          attackLen = attackLen
        )
        # spectrogram(syllable, samplingRate)
      }

      # save the generated syllable in syls_list
      voiced_list[[2 * s - 1]] = syllable
      actualSylLen = length(syllable) / samplingRate * 1000

      # generate a pause for all but the last syllable
      if (s < nrow(syllables)) {
        voiced_list[[2 * s]] = rep(0, floor((syllables[s + 1, 'start'] - syllables[s, 'end']) *
                                              samplingRate / 1000))
      } else {
        voiced_list[[2 * s]] = numeric(0)
      }

      # update syllable timing info, b/c with temperature > 0, jitter etc
      # there may be deviations from the target duration
      if (s < nrow(syllables)) {
        actualBoutLen = sum(lengths(voiced_list)) * 1000 / samplingRate
        correction = actualBoutLen - syllables[s + 1, 'start']
        syllables[(s + 1):nrow(syllables), c('start', 'end')] =
          syllables[(s + 1):nrow(syllables), c('start', 'end')] + correction
      }

      # scale noise anchors again to take into account the actual sylLen
      if (lockNoiseToVoiced) {
        noise_syl[[s]]$time = scaleNoiseAnchors(
          noiseTime = noise_syl[[s]]$time,
          sylLen_old = max(noise_syl[[s]]$time),
          sylLen_new = actualSylLen # syllables$dur[s]
        )
      }

      # generate the aperiodic part, but don't add it to the sound just yet
      if (is.list(noise) && any(noise$value > -dynamicRange)) {
        # synthesize the aperiodic noise
        aperiodic[[s]] = generateNoise(
          len = max(1, round(diff(range(noise_syl[[s]]$time)) * samplingRate / 1000)),
          noise = noise_syl[[s]],
          rolloffNoise = pars_syllable[['rolloffNoise']],
          rolloffNoiseExp = pars_syllable[['rolloffNoiseExp']],
          noiseFlatSpec = noiseFlatSpec,
          attackLen = pars_syllable$attackLen,
          samplingRate = samplingRate,
          windowLength = windowLength,
          step = step,
          smoothing = smoothing,
          formantFilter = NULL
        ) * amplEnvelope[s]  # correction of amplitude per syllable
        # plot(aperiodic[[s]], type = 'l')
      }
    }

    # concatenate all syllables and pauses
    voiced = do.call(c, voiced_list)
    # plot(voiced, type = 'l')
    # spectrogram(voiced, samplingRate = samplingRate)
    # playme(voiced, samplingRate = samplingRate)
    # END OF SYLLABLE GENERATION


    ## Add noise fragments together
    sound_aperiodic = rep(0, length(voiced))
    for (s in seq_along(aperiodic)) {
      # calculate where syllable s begins
      syllableStartIdx = round(syllables[s, 'start'] * samplingRate / 1000) + 1

      # calculate where noise is to be inserted
      insertionIdx = syllableStartIdx +
        round(noise_syl[[s]]$time[1] * samplingRate / 1000)
      sound_aperiodic = addVectors(sound_aperiodic,
                                   aperiodic[[s]],
                                   insertionPoint = insertionIdx,
                                   normalize = FALSE)

      # update syllable timing if inserting before the bout
      # (increasing its length)
      if (insertionIdx < 0) {
        syllables[, c('start', 'end')] =
          syllables[, c('start', 'end')] - insertionIdx / samplingRate * 1000
      }
    }
    any_aperiodic = any(sound_aperiodic != 0)
    any_voiced = any(voiced != 0)

    ## Adding formants: filter periodic and aperiodic components separately
    if (any_voiced &&
        length(voiced) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
      voiced = do.call(.addFormants, c(
        formantPars,
        list(audio = list(
          sound = voiced,
          samplingRate = samplingRate,
          scale = 1
        ),
        formants = formants,
        formantDepStoch = formantDepStoch,
        normalize = 'none')
      ))
    }

    if (any_aperiodic &&
        length(sound_aperiodic) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
      sound_aperiodic = do.call(.addFormants, c(
        formantPars,
        list(audio = list(
          sound = sound_aperiodic,
          samplingRate = samplingRate,
          scale = 1
        ),
        formants = formantsNoise,
        formantDepStoch = formantDepStoch_noise,
        normalize = 'none')
      ))
    }


    # Add periodic and aperiodic components at SNR corresponding to max noise
    # value (dB)
    if (any_voiced && any_aperiodic) {
      # both periodic and aperiodic
      noise_values = unlist(lapply(noise_syl, function(x) x$value))
      noise_db = if (length(noise_values) > 0) {
        max(noise_values, na.rm = TRUE)
      } else {
        max(noise$value, na.rm = TRUE)
      }
      if (!is.finite(noise_db)) noise_db = -dynamicRange
      soundFiltered = addVectors(
        voiced,
        sound_aperiodic,
        insertionPoint = -syllables$start[1] * samplingRate / 1000,
        SNR = -noise_db,
        type = 'rms',
        normalize = FALSE
      )
    } else if (any_voiced) {
      # only periodic
      soundFiltered = voiced
    } else if (any_aperiodic) {
      # only aperiodic
      soundFiltered = sound_aperiodic
    } else {
      # nothing at all (shouldn't happen)
      soundFiltered = numeric(0)
    }
    # spectrogram(soundFiltered, samplingRate)

    ## Add amplitude modulation (affects both voiced and aperiodic)
    if (is.list(amDep)) {
      if (any(amDep$value > 0)) {
        soundFiltered = .addAM(
          audio = list(
            sound = soundFiltered,
            samplingRate = samplingRate,
            ls = length(soundFiltered)
          ),
          amDep = amDep,
          amFreq = amFreq,
          amType = amType,
          amShape = amShape,
          invalidArgAction = invalidArgAction,
          plot = FALSE,
          play = FALSE
        )
        # spectrogram(soundFiltered, samplingRate)
      }
    }

    # grow bout
    if (b == 1) {
      bout = soundFiltered
    } else {
      bout = addVectors(
        bout,
        soundFiltered,
        insertionPoint = length(bout) + round(pauseLen[1] * samplingRate / 1000),
        normalize = FALSE
      )
    }
  }

  # normalize
  if (any(is.na(bout))) stop('sound generation failed')
  m = max(abs(bout))
  if (m != 0) bout = bout / m

  # add some silence before and after the entire bout
  if (is.numeric(addSilence)) {
    n = round(samplingRate / 1000 * addSilence)
    if (length(n) == 1) n = rep(n, 2)
    bout = c(rep(0, n[1]), bout, rep(0, n[2]))
  }

  if (isTRUE(play)) {
    playme(bout, samplingRate = samplingRate)
  } else if (is.character(play)) {
    playme(bout, samplingRate = samplingRate, player = play)
  }
  if (isTRUE(saveAudio)) {
    audio = list(samplingRate = samplingRate, bit = 16, scale = 1,
                 scale_used = max(abs(range(bout))))
    writeAudio(bout, audio = audio, filename = 'soundgen.wav')
  }
  if (plot) {
    spectrogram(bout, samplingRate = samplingRate,
                windowLength = windowLength, step = step,
                dynamicRange = dynamicRange, ...)
  }
  invisible(bout)
}
