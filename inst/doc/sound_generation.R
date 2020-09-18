## -----------------------------------------------------------------------------
playback = list(TRUE, FALSE, 'vlc', 'my-awesome-player')[[2]]
# TRUE means defaulting to "play" on Linux, "afplay" on Mac, 
# and the defaults of tuneR::play on Windows
# FALSE means no sound playback

## -----------------------------------------------------------------------------
library(soundgen)
s = soundgen(play = playback)  # default sound: a short [a] by a male speaker
# 's' is a numeric vector - the waveform. You can save it, play it, plot it, ...

# names(presets)  # speakers in the preset library
# names(presets$Chimpanzee)  # presets per speaker
s = eval(parse(text = presets$Chimpanzee$Scream_conflict))  # a screaming chimp
# playme(s)

## -----------------------------------------------------------------------------
s = soundgen(formants = 'uai', repeatBout = 1, nSyl = 3, play = playback)
# to replay without re-generating the sound, type "playme(s)"

## -----------------------------------------------------------------------------
s = soundgen(formants = 'uai', repeatBout = 3, nSyl = 1, play = playback)
# playme(s)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(nSyl = 5, 
             sylLen = c(300, 100),   # linearly decreasing from 300 to 100 ms
             pauseLen = c(50, 150),  # increasing from 50 to 150 ms
             plot = TRUE,
             play = playback)
# playme(s)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(
  nSyl = 10, 
  sylLen = c(60, 200, 90, 50, 50),  # quickly up to 200 and down to 50
  pauseLen = c(50, 60, 80, 150),    # growing ~exponentially
  plot = TRUE,
  play = playback
)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(
  nSyl = 5, 
  sylLen = c(300, 100, 400, 50, 100),  # 5 syllables, 5 values
  pauseLen = c(50, 150, 50, 100),      # 4 pauses, 4 values
  plot = TRUE,
  play = playback
)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(
  repeatBout = 2,
  nSyl = 3, 
  sylLen = c(300, 100), 
  pauseLen = c(100, 50),     
  plot = TRUE,
  play = playback
)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(
  repeatBout = 2,
  sylLen = c(300, 100), 
  pauseLen = -50,     
  plot = TRUE,
  play = playback,
  invalidArgAction = 'ignore'
)

## -----------------------------------------------------------------------------
sound = soundgen(pitch = 440, play = playback)  # steady pitch at 440 Hz
sound = soundgen(pitch = 3000:2000, play = playback)  # downward chirp
sound = soundgen(pitch = c(150, 250, 100), sylLen = 700, play = playback)  # up and down

## ----fig.show = "hold", fig.width = 7, fig.height = 5-------------------------
anchors = (sin(1:70 / 3) * .25 + 1) * 350
par(mfrow = c(1, 2))
plot(anchors, type = 'l', xlab = 'Time (points)', ylab = 'Pitch (Hz)')
sound = soundgen(pitch = anchors, sylLen = 1000, 
                 play = playback, plot = TRUE)
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
sound = soundgen(sylLen = 900, play = playback,
                 pitch = list(time = c(0, .1, 1),  # or (c(0, 30, 300)) - in ms
                              value = c(350, 700, 350)))

## ----fig.show = "hold", fig.width = 7, fig.height = 4-------------------------
sylLen = 500  # desired syllable length, in ms
samplingRate = 16000
sylLen_points = sylLen / 1000 * samplingRate
anchors = data.frame(time = c(0, .1, 1), 
                       value = c(350, 700, 350))

par(mfrow = c(1, 3))
getSmoothContour(
  anchors = anchors,
  len = sylLen_points,
  interpol = 'approx',
  thisIsPitch = TRUE, plot = TRUE, 
  main = 'No smoothing', samplingRate = samplingRate
)
getSmoothContour(
  anchors = anchors,
  len = sylLen_points,
  loessSpan = 0.75,
  thisIsPitch = TRUE, plot = TRUE, 
  main = 'loessSpan = .75', samplingRate = samplingRate
)
getSmoothContour(
  anchors = anchors,
  len = sylLen_points,
  loessSpan = 1,
  thisIsPitch = TRUE, plot = TRUE, 
  main = 'loessSpan = 1', samplingRate = samplingRate
)
par(mfrow = c(1, 1))
# likewise: soundgen(smoothing = list(interpol = 'loess', loessSpan = 1))

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(sylLen = 800, plot = TRUE, play = playback,
             pitch = list(time = c(0, .2, .201, .4, 1), 
                          value = c(900, 1200, 1800, 2000, 1500)))

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(nSyl = 5, sylLen = 200, pauseLen = 140, plot = TRUE, play = playback,
             pitch = data.frame(time = c(0, 0.65, 1), 
                                value = c(977, 1540, 826)),
             pitchGlobal = data.frame(time = c(0, .5, 1), 
                                      value = c(-6, 7, 0)))
# pitchGlobal = c(-6, 7, 0) is equivalent, since time steps are equal

## ----fig.width = 5, fig.height = 5--------------------------------------------
# variable, but deterministic vibrato
s1 = soundgen(vibratoDep = 0:3, vibratoFreq = 7:5, 
              sylLen = 2000, pitch = c(300, 280), 
              play = playback, plot = TRUE)

# stochastic vibrato
s2 = soundgen(vibratoDep = rnorm(n = 10, mean = .5, sd = .1), 
              vibratoFreq = rnorm(n = 10, mean = 5, sd = .5), 
              sylLen = 2000, pitch = c(300, 280), 
              play = playback, plot = TRUE)

## -----------------------------------------------------------------------------
# the sound is a bit different each time, because temperature is above zero
s = soundgen(repeatBout = 3, temperature = 0.3, play = playback)
# Setting repeatBout = 3 is equivalent to:
# for (i in 1:3) soundgen(temperature = 0.3, play = playback)

## -----------------------------------------------------------------------------
# despite the high temperature, temporal structure does not vary at all, 
# while formants are more variable than the default
s = soundgen(repeatBout = 3, nSyl = 2, temperature = .3, play = playback,
             tempEffects = list(sylLenDep = 0, formDrift = 3))

## -----------------------------------------------------------------------------
mf = c(-1,  # male: 100% lower f0, 25% lower formants, 25% longer vocal tract
       0,   # neutral (default)
       1)   # female: 100% higher f0, 25% higher formants, 25% shorter vocal tract
# See e.g. http://www.santiagobarreda.com/vignettes/v1/v1.html

for (i in mf) {
  s = soundgen(maleFemale = i, formants = NA, pitch = 220, 
               vocalTract = 15, play = playback)
  # Since `formants` are not specified, but temperature is above zero, a 
  # schwa-like sound with approximately equidistant formants is generated using
  # `vocalTract` (cm) to calculate the expected formant dispersion.
}

## -----------------------------------------------------------------------------
cb = c(-1,  # max creaky
       -.5, # moderately creaky
       0,   # neutral (default)
       .5,  # moderately breathy
       1)   # max breathy (no tonal component)
for (i in cb) {
  soundgen(creakyBreathy = i, play = playback)
}

## ----fig.width = 5, fig.height = 5--------------------------------------------
# each syllable has a 10-dB dip in the middle (note the dumbbell shapes 
# in the oscillogram under the spectrogram), and there is an overall fade-out
# over the entire bout
s = soundgen(nSyl = 4, 
             ampl = data.frame(time = c(0, .3, 1),  # unequal time steps
                               value = c(0, -10, 0)),
             amplGlobal = c(0, -20),  # this fade-out applies to noise as well
             noise = -10,
             plot = TRUE, heights = c(1, 1), play = playback)

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(sylLen = 1000, formants = NA,
             # set the depth of AM (0% = none, 100% = max)
             amDep = c(0, 100),   
             # set AM frequency in Hz (vectorized)
             amFreq = c(50, 25),  
             # set the shape: 0 = close to sine, -1 = notches, +1 = clicks
             amShape = 0,  
             # asymmetrical attack: 20 ms at the beginning and 140 ms at the end
             attackLen = c(20, 140),
             plot = TRUE, heights = c(1, 1), play = playback)

## ----fig.width = 7, fig.height = 3--------------------------------------------
s = rnorm(500) * seq(1, 0, length.out = 500)
s1 = flatEnv(s, plot = TRUE, killDC = TRUE, windowLength_points = 50)

## ----fig.show = "hold", fig.width = 7, fig.height = 3-------------------------
# Create a sound with sharp attack
s = soundgen(sylLen = 300, pitch = 800, addSilence = 0, attackLen = 10)  
# playme(s)
s1 = fade(s, fadeIn = 50, fadeOut = 100, samplingRate = 16000,
          shape = 'logistic', steepness = 1, plot = TRUE)
# playme(s1)
# different fades are available: linear, logarithmic, etc

## -----------------------------------------------------------------------------
soundgen(formants = 'ai', play = playback)
soundgen(formants = 'aaai', play = playback)

## ----fig.show = "hold", fig.width = 4, fig.height = 4-------------------------
freqs = 2 ^ seq(log2(20), log2(20000), length.out = 500)
plot(freqs, soundgen:::getBandwidth(freqs), type = 'l', 
     log = 'xy', xlab = 'Center frequency, Hz',
     ylab = 'Bandwidth, Hz', 
     main = 'Default formant bandwidths')
abline(v = 250, lty = 3)
abline(v = 500, lty = 3)

## -----------------------------------------------------------------------------
# shorthand specification with three stationary formants
formants = c(300, 2500, 3200)

# shorthand specification with two moving formants
formants = list(f1 = c(300, 900), f2 = c(2500, 1500))

# full specification with two moving formants and non-default amplitude and bandwidth
formants = list(
  f1 = list(freq = c(300, 900), 
            amp = c(30, 10), 
            width = 120),
  f2 = list(time = c(0, .2, 1),  # "time" is only needed for non-equidistant anchors
            freq = c(2500, 2400, 1500), 
            amp = 30, 
            width = c(0, 220, 240)))

## ----fig.width = 7, fig.height = 4--------------------------------------------
estimateVTL(formants = c(400, 1800, 2550, 4100), plot = TRUE)  
# 17.5 cm

## ----fig.width = 7, fig.height = 5--------------------------------------------
soundgen(formants = c(800, 1200), play = playback, plot = TRUE)
soundgen(formants = c(800, 1200), formantDepStoch = 0,
              play = playback, plot = TRUE)

## ----fig.width = 7, fig.height = 5--------------------------------------------
soundgen(sylLen = 800, formants = NULL, rolloff = -6,
         vocalTract = c(12, 18, 19), formantCeiling = 5, 
         play = playback, plot = TRUE)

## ----fig.width = 7, fig.height = 5--------------------------------------------
formants = list(f1 = c(500, 250), f2 = c(1500, 2800), f3 = 3500, f4 = 4300)
estimateVTL(formants)  # 13.7 cm - so the initial VTL should be close to 13.7
soundgen(sylLen = 800, rolloff = -6, formants = formants, 
         vocalTract = list(time = c(0, .3, 1), value = c(14, 28, 25)), 
         play = playback, plot = TRUE, main = 'Good initial VTL')
# wrong: VTL too high for these formants, causing an overlap
soundgen(sylLen = 800, rolloff = -6, formants = formants, 
         vocalTract = list(time = c(0, .3, 1), value = c(18, 28, 25)), 
         play = playback, plot = TRUE, main = 'Bad initial VTL')

## ----fig.width = 4, fig.height = 4--------------------------------------------
# plotting directly from getSpectralEnvelope() in spectrogram form
s = getSpectralEnvelope(nr = 1024,  # freq bins in FFT frame (window_length / 2)
                        nc = 50,    # time bins
                        samplingRate = 16000, 
                        formants = list(f1 = c(500, 250), 
                                        f2 = c(1500, 2800), 
                                        f3 = 3500, f4 = 4300),
                        plot = TRUE, 
                        dur = 1500,   # just an example
                        colorTheme = 'seewave',
                        lipRad = 6)  # lip radiation, dB/octave

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
formants = list(
  f1   = list(time = c(0, 1), freq = c(880, 900), 
              amp = c(25, 15), width = c(80, 120)), 
  f1.5 = list(time = c(0, 1), freq = 600,
              amp = c(0, 15), width = 80),   # additional pole
  f1.7 = list(time=c(0, 1), freq = 750,
              amp = c(0, -15), width = 80),  # zero
  f2   = list(time = c(0, 1), freq = c(1480, 1250), 
              amp = c(30, 20), width = c(120, 200)), 
  f3   = list(time=c(0, 1), freq = c(2900, 3100), 
              amp = 25, width = 200))
s = soundgen(sylLen = 1500, play = playback, pitch = 140, formants = formants)
spectrogram(s, samplingRate = 16000, ylim = c(0, 4), contrast = .5, 
            windowLength = 10, step = 5, colorTheme = 'seewave')
# long-term average spectrum (less helpful for moving formants but very good for stationary):
# seewave::meanspec(s, f = 16000, wl = 256)  

## ----fig.show = "hold", fig.width = 5, fig.height = 4-------------------------
se = getSpectralEnvelope(nr = 512, nc = 100, formants = formants)
plot(as.numeric(rownames(se)), 20 * log10(se[, ncol(se)]), 
     type = 'l', xlab = 'KHz', ylab = 'dB')

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(sylLen = 1200, play = playback, pitch = 140, 
             mouth = list(time = c(0, .3, .75, 1), 
                          value = c(0, 0, .7, 0)))
spectrogram(s, samplingRate = 16000, 
            ylim = c(0, 4), contrast = .5, 
            windowLength = 10, step = 5, 
            colorTheme = 'seewave')

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(
  sylLen = 1500, rolloff = -20,
  pitch = c(500, 1000, 1800, 1700, 500),
  formants = NULL, vocalTract = 55,
  formantLocking = c(0, 1, 1, 1, 1, 0),  # except the beginning and end
  shortestEpoch = 200,  # affects both subharmonics (if any) and formantLocking
  noise = -20,  # just to make formants visible in this example
  temperature = .1,
  samplingRate = 22000, pitchSamplingRate = 22000,
  play = playback, plot = TRUE, ylim = c(0, 8)
)

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
# strong F0, rolloff with a "shoulder"
r = getRolloff(rolloff = c(-5, -20),  # rolloff parameters are vectorized
               rolloffParab = -10, rolloffParabHarm = 13, 
               pitch_per_gc = c(170, 340), plot = TRUE)

# to generate the corresponding sound:
s = soundgen(sylLen = 1000, rolloff = c(-5, -20), rolloffOct = 0,
             rolloffParab = -10, rolloffParabHarm = 13,
             pitch = c(170, 340),  play = playback)

## ----fig.width = 5, fig.height = 5--------------------------------------------
rolloffExact = matrix(c(.1, .2, 1, .02, .2,  # strength of H1-H5 at time 0
                        1, .2, .01, .1, .4), # strength of H1-H5 at time 1000
                      ncol = 2)
s = soundgen(sylLen = 1000, pitch = c(400, 430), formants = NULL,
             rolloffExact = rolloffExact, 
             plot = TRUE, ylim = c(0, 4), play = playback)

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(sylLen = 1000, pitch = c(400, 430), formants = NULL,
             rolloffExact = c(.1, .5, .25, 1, .25, .08, .05, .02), 
             plot = TRUE, ylim = c(0, 4), play = playback)

## ----fig.width = 7, fig.height = 7--------------------------------------------
# Not a good idea: samplingRate is too low
s1 = soundgen(pitch = c(1500, 800), glottis = 75, 
              samplingRate = 16000, play = playback)

# This sounds better but takes a long time to synthesize:
s2 = suppressWarnings(soundgen(pitch = c(1500, 800), glottis = 75, 
                               samplingRate = 80000, play = playback, 
                               invalidArgAction = 'ignore'))
# NB: invalidArgAction = 'ignore' forces a "weird" samplingRate value
# to be accepted without question

# Now this is what this feature is meant for: vocal fry
s3 = soundgen(sylLen = 1500, pitch = c(110, 90), rolloff = -12,
              glottis = c(0, 500), 
              jitterDep = 1, shimmerDep = 20,
              # subharmonics not implemented with "glottis" 
              play = playback)
spectrogram(s3, samplingRate = 16000, heights = c(1, 1))

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
soundgen(sylLen = 1500, pitch = c(170, 420, 400, 190),
         nonlinBalance = 60,
         subDep = 10, jitterDep = 1.5, shimmerDep = 25,
         play = playback, plot = TRUE, ylim = c(0, 5))

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
soundgen(subRatio = 2, subDep = c(5, 20),
         sylLen = 800, pitch = c(700, 1300), formants = NULL,
         play = playback, plot = TRUE, ylim = c(0, 3))

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
soundgen(subFreq = 400, subDep = c(5, 20),
         sylLen = 800, pitch = c(700, 1300), formants = NULL,
         play = playback, plot = TRUE, ylim = c(0, 3))

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(
  sylLen = 800, 
  pitch = list(time=c(0, .3, .9, 1), 
               value = c(1200, 1547, 1487, 1154)),
  rolloff = -3, rolloffKHz = 0,
  # gradually increasing width of sidebands at 0-600 ms
  subFreq = 75, subDep = 25,
  subWidth = data.frame(time = c(0, 600, 650, 800), 
                        value = c(0, 130, 0, 0)),  
  vocalTract = 12, mouth = c(.1, .8, .1),
  temperature = .001,
  pitchSamplingRate = 22050, samplingRate = 22050,
  play = playback, plot = TRUE, ylim = c(0, 5))

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(sylLen = 800, 
             pitch = data.frame(time=c(0, .3, .9, 1), 
                                value = c(1200, 1547, 1487, 1154)),
             rolloff = -3, rolloffKHz = 0,
             # gradually increasing width of sidebands at 0-600 ms
             amFreq = 75, amShape = .1,
             amDep = data.frame(time = c(0, 600, 650, 800), 
                                value = c(0, 100, 0, 0)),  
             vocalTract = 12, mouth = c(.1, .8, .1),
             temperature = .001,
             pitchSamplingRate = 22050, samplingRate = 22050,
             play = playback, plot = TRUE, ylim = c(0, 5))

## ----fig.width = 5, fig.height = 5--------------------------------------------
s1 = soundgen(jitterLen = 40, jitterDep = 1,  # shaky voice
              shimmerLen = 30, shimmerDep = 30,   
              sylLen = 1000, pitch =  c(150, 170), 
              play = playback, plot = TRUE, ylim = c(0, 3))
s2 = soundgen(jitterLen = 1, jitterDep = 1,   # harsh voice
              shimmerLen = 1, shimmerDep = 10, 
              sylLen = 1000, pitch =  c(150, 170), 
              play = playback, plot = TRUE, ylim = c(0, 3))

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(repeatBout = 2, sylLen = 140, pauseLen = 100, 
             vocalTract = 8, formants = NULL, rolloff = 0,
             pitch = c(1100, 1600, 1100), mouth =  c(0, 0.5, 0),
             jitterDep = 1, subDep = 60, play = playback) 

## ----fig.width = 5, fig.height = 5--------------------------------------------
# slight and slow (slightly unsteady voice)
soundgen(sylLen = 1500, pitch = 300,
         tempEffects = list(pitchDriftDep = 1, pitchDriftFreq = .5),
         play = playback, plot = TRUE, ylim = c(0, 2))
# strong and rapid (trembling voice, similar to jitter)
soundgen(sylLen = 1500, pitch = 300,
         tempEffects = list(pitchDriftDep = 5, pitchDriftFreq = 5),
         play = playback, plot = TRUE, ylim = c(0, 2))
# both drift and jitter (trembling voice ending with some "chaos")
soundgen(sylLen = 1500, pitch = 300,
         tempEffects = list(pitchDriftDep = 5, pitchDriftFreq = 5),
         jitterDep = c(0, 0, 0, 2),
         play = playback, plot = TRUE, ylim = c(0, 2))

## ----fig.width = 7, fig.height = 7--------------------------------------------
s = soundgen(sylLen = 1200, 
             pitch = list(
               time = c(0, 110, 111, 180, 350, 940, 941, 1100, 1200),
               value = c(700, 1150, 1550, 2000, 2240, 1940, 1180, 900, 500)),
             temperature = 0.05, tempEffects = list(pitchDep = 0),
             jitterDep = data.frame(time = c(0, 200, 201, 900, 901, 1200),
                                    value = c(0, 0,  1.7, 1.2, 0,   0)),
             formants = c(900, 1300, 3300, 4300),
             attackLen = c(10, 200),
             samplingRate = 22000, play = playback, plot = TRUE)

## ----fig.width = 7, fig.height = 7--------------------------------------------
s = soundgen(sylLen = 1200, 
             pitch = list(
               time = c(0, 80, 81, 230, 231, 385, 
                        # 500 time anchors here - an episode of "chaos"
                        seq(385, 850, length.out = 500), 
                        851, 1020, 1021, 1085),
               value = c(700, 1130, 1000, 1200, 1860, 1840, 
                         # random f0 jumps b/w 1.2 & 1.8 KHz 
                         sample(c(1200, 1800), size = 500, replace = TRUE), 
                         1620, 1540, 1220, 900)),
             temperature = 0.05, 
             tempEffects = list(pitchDep = 0),
             jitterDep = .3,
             rolloffKHz = 0, rolloff = 0, formants = c(900, 1300, 3300, 4300),
             samplingRate = 22000, play = playback, plot = TRUE)

## -----------------------------------------------------------------------------
# run several times to appreciate the randomness
for (i in 1:5) s = soundgen(sylLen = 800, 
             mouth = rnorm(n = 5, mean = .5, sd = .3),
             play = playback)

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
s = soundgen(
  # nonlinear settings
  jitterDep = c(0, 0, 1.5, .5), shimmerDep = c(0, 0, 15, 5),
  # settings for high precision
  temperature = .001, dynamicRange = 120,             
  samplingRate = 22050, pitchSamplingRate = 22050,  
  # other settings
  sylLen = 1000, pitch = c(240, 200),
  rolloff = c(-20, -18, -23, -28) + 4, vibratoDep = .2,
  formants = c(800, 1400, 2500, 3700, 5000, 6800),
  noise = data.frame(time = c(0, 340, 900, 1000), 
                     value = c(-60, -45, -60, -80) + 10),
  rolloffNoise = 0,
  mouth = c(.55, .5, .45, .6),
  play = playback, plot = TRUE, ylim = c(0, 4)
)

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
s = soundgen(
  # nonlinear settings
  jitterDep = data.frame(
    time = c(0, 300, 301, 500, 501, 1000), 
    value = c(0, 0, 1.5, 1.5, 0, 0)
  ),   
  shimmerDep = data.frame(
    time = c(0, 600, 601, 800, 801, 1000), 
    value = c(0, 0, 40, 40, 0, 0)
  ),
  # settings for high precision
  temperature = .001, dynamicRange = 120,             
  samplingRate = 22050, pitchSamplingRate = 22050,  
  # other settings
  addSilence = 0,  # easier to check timing
  sylLen = 1000, pitch = c(240, 200),
  rolloff = c(-20, -18, -23, -28), vibratoDep = .2,
  formants = c(800, 1400, 2500, 3700, 5000, 6800),
  noise = data.frame(time = c(0, 340, 900, 1000), 
                     value = c(-60, -45, -60, -80) + 30),
  rolloffNoise = -8,
  mouth = c(.55, .5, .45, .6),
  play = playback, plot = TRUE, ylim = c(0, 4)
)

## ----fig.width = 5, fig.height = 5--------------------------------------------
rw_bin = c(rep(0, 100), rep(1, 100), rep(2, 100))
s = soundgen(sylLen = 800, pitch = 300, temperature = 0.001,
             subFreq = 100, subDep = 70, jitterDep = 1,
             nonlinRandomWalk = rw_bin, 
             play = playback, plot = TRUE, ylim = c(0, 4))

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
# set up a random walk (repeat until satisfied with the contour)
rw = getRandomWalk(len = 1000, rw_range = 100, 
                   trend = c(0.5, -0.5), rw_smoothing = .95)
rw_bin = getIntegerRandomWalk(rw, minLength = 100, plot = TRUE)
# synthesize two sounds with identical nonlinear effects but different f0
s1 = soundgen(sylLen = 800, pitch = 300, temperature = 0.001,
              subFreq = 100, subDep = 70, jitterDep = 1,
              nonlinRandomWalk = rw_bin, 
              play = playback, plot = TRUE, ylim = c(0, 4))
s2 = soundgen(sylLen = 800, pitch = 500, temperature = 0.001,
              subFreq = 100, subDep = 70, jitterDep = 1,
              nonlinRandomWalk = rw_bin, 
              play = playback, plot = TRUE, ylim = c(0, 4))

## ----fig.width = 5, fig.height = 5--------------------------------------------
soundgen(sylLen = 500, 
         noise = data.frame(time = c(0, 800), value = c(-20, -10)),
         formantsNoise = NA,  # breathing - same formants as for voiced
         play = playback, plot = TRUE)
# observe that the voiced and unvoiced components have exactly the same formants

## ----fig.width = 5, fig.height = 5--------------------------------------------
s = soundgen(sylLen = 200, pitch = c(150, 120),
             noise = data.frame(time = c(180, 250, 400), value = c(-20, -10, -50)),
             # specify noise filter ≠ voiced filter to get ~[s]
             formantsNoise = list(
               f1 = data.frame(freq = 7000,
                               amp = 40, 
                               width = 1500)
             ), 
             rolloffNoise = 0,
             play = playback, plot = TRUE)
# observe that the voiced and unvoiced components have different formants

## -----------------------------------------------------------------------------
s1 = soundgen(vocalTract = 15.5,  # ~human throat (15.5 cm)
              formants = NULL, attackLen = 200, play = playback,
              noise = list(time = c(0, 800), value = c(40, 40)))
# NB: since there is no voiced component, we control syllable length
# by specifying the appropriate noise$time, in this case 0 to 800 ms

s2 = soundgen(vocalTract = 30,    # a large animal
              formants = NULL, attackLen = 200, play = playback,
              sylLen = 800, noise = 40)  # another way to specify the length
# NB: voiced component is not generated if noise$value >= 40 dB

s3 = soundgen(vocalTract = 100, invalidArgAction = 'ignore',    # a whale
              formants = NULL, attackLen = 200, play = playback,
              sylLen = 800, pitch = NULL, noise = 0) 
# Another way to remove the voiced component is to write pitch = NULL

## ----fig.width = 5, fig.height = 5--------------------------------------------
# only two specified formants
s3 = soundgen(pitch = NULL, 
              formantsNoise = c(1000, 2000),  
              noise = 40, sylLen = 800,
              play = playback, plot = TRUE)
# two specified formants plus extra formants based on vocalTract
s4 = soundgen(vocalTract = 15.5, 
              pitch = NULL,
              formantsNoise = c(1000, 2000),  
              noise = 40, sylLen = 800,
              play = playback, plot = TRUE)

## ----fig.width = 6, fig.height = 6--------------------------------------------
s1 = soundgen(vocalTract = 17.5, 
              noise = 40, rolloffNoise = c(5, -20),
              formants = NULL, attackLen = 200, 
              play = playback, plot = TRUE)
# NB: noise amplitude may change as rolloffNoise changes

## -----------------------------------------------------------------------------
s1 = soundgen(noiseAmpRef = 'f0', rolloff = -1, 
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)
s2 = soundgen(noiseAmpRef = 'f0', rolloff = -15, 
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)

## -----------------------------------------------------------------------------
# Harmonics-noise balance doesn't depend on rolloff...
s3 = soundgen(noiseAmpRef = 'source', rolloff = -15, rolloffNoise = 0,
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)
s4 = soundgen(noiseAmpRef = 'source', rolloff = -1, rolloffNoise = -20,
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)

# ...but it does depend on the formant structure
s5 = soundgen(noiseAmpRef = 'source', formants = 'a', 
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)
s6 = soundgen(noiseAmpRef = 'source', formants = 'u',
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)

## -----------------------------------------------------------------------------
s7 = soundgen(noiseAmpRef = 'filtered', formants = 'a', 
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)
s8 = soundgen(noiseAmpRef = 'filtered', formants = 'u',
              noise = data.frame(time = c(-100, 400), value = c(0, 0)), 
              play = playback)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(nSyl = 2,
             noise = c(-10, 0),
             plot = TRUE, ylim = c(0, 4), play = playback)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(nSyl = 2, sylLen = 120, pauseLen = 120, 
             temperature = 0.001, rolloffNoise = -2, 
             noise = data.frame(time = c(39, 56, 209), 
                                value = c(-40, 0, -20)),
             formants = list(f1 = c(860, 530),  f2 = c(1280, 2400)),
             formantsNoise = list(f1 = c(420, 1200)),
             plot = TRUE, ylim = c(0, 4), play = playback)

## ----fig.width = 7, fig.height = 5--------------------------------------------
s = soundgen(repeatBout = 2, nSyl = 2,
             sylLen = 120, pauseLen = 120, 
             temperature = 0.001, rolloffNoise = -2, 
             noise = data.frame(time = c(39, 56, 209), 
                                value = c(-40, 0, -20)),
             formants = list(f1 = c(860, 530),  f2 = c(1280, 2400)),
             formantsNoise = list(f1 = c(420, 1200)),
             plot = TRUE, ylim = c(0, 4), play = playback)

## ----fig.width = 5, fig.height = 5--------------------------------------------
a = getSmoothContour(anchors = data.frame(time = c(-50, 200, 300), 
                                          value = c(-80, 20, -80)),
                     voiced = 200, 
                     normalizeTime = FALSE,  # keep time in ms
                     plot = TRUE, ylim = c(-80, 40), main = '')

## ----fig.show = "hold", fig.width = 7, fig.height = 3-------------------------
par(mfrow = c(1, 2))
sound1 = sin(2 * pi * 1:5000 * 100 / 16000) # pure tone, 100 Hz
sound2 = sin(2 * pi * 1:5000 * 200 / 16000) # pure tone, 200 Hz

# simple concatenation
comb1 = c(sound1, sound2)
# playme(comb1)  # note the click
plot(comb1[4000:5500], type = 'l', xlab = '', ylab = '')  
# note the abrupt transition, which creates the click
# spectrogram(comb1, 16000)  

# cross-fade
comb2 = crossFade(sound1, sound2, samplingRate = 16000, crossLen = 50)
# playme(comb2)  # no click
plot(comb2[4000:5500], type = 'l', xlab = '', ylab = '')  
# gradual transition
# spectrogram(comb2, 16000)
par(mfrow = c(1, 1))

## ----fig.width = 5, fig.height = 5--------------------------------------------
cow1 = soundgen(sylLen = 1400, 
                pitch = list(time = c(0, 11/14, 1), 
                             value = c(75, 130, 200)), 
                temperature = 0.1, 
                rolloff = -6, rolloffOct = -3, rolloffParab = 12,
                mouthOpenThres = 0.6, 
                formants = NULL, vocalTract = 36.5, 
                mouth = list(time = c(0, 0.82, 1), 
                             value = c(0.6, 0, 1)), 
                noise = list(time = c(0, 1400), 
                             value = c(-45, -45)),
                rolloffNoise = -4, addSilence = 0)
cow2 = soundgen(sylLen = 310, pitch = c(359, 359), 
                temperature = 0.05,
                subFreq = 150, subDep = 70, jitterDep = 1.3, 
                rolloff = -6, rolloffOct = -3, rolloffKHz = -0, 
                formants = NULL, vocalTract = 36.5, 
                noise = list(time = c(0, 26, 317, 562), 
                             value = c(-80, -33, -32, -80)), 
                rolloffNoise = -6,
                attackLen = 0, addSilence = 0)
s = crossFade(cow1 * 3, cow2,  # adjust the relative volume by scaling
              samplingRate = 16000, crossLen = 150)
# playme(s, 16000)
spectrogram(s, 16000, ylim = c(0, 4))

## ----fig.show = "hold", fig.width = 5, fig.height = 5-------------------------
samplingRate = 48000  # >10 times the highest pitch 
sound1 = soundgen(sylLen = 700, pitch = 250:180, 
                  formants = 'aaao',  addSilence = 100, 
                  samplingRate = samplingRate, play = playback)
sound2 = soundgen(nSyl = 2, sylLen = 150, 
                  pitch = 4300:2200, attackLen = 10,
                  formants = NA, temperature = .001, 
                  pitchCeiling = samplingRate, pitchSamplingRate = samplingRate,  
                  addSilence = 0, play = playback)

insertionTime = .1 + .15  # silence + 150 ms
insertionPoint = insertionTime * samplingRate
comb = addVectors(sound1, 
                  sound2 * .05,  # to make sound2 quieter relative to sound1
                  insertionPoint = insertionPoint)
# sound1 and sound2 have attack of 50 and 10 ms, so no clicks
# playme(comb, samplingRate)
spectrogram(comb, samplingRate, windowLength = 10, ylim = c(0, 5), 
            contrast = .5, colorTheme = 'seewave')

## ----fig.show = "hold", fig.width = 7, fig.height = 3-------------------------
a = data.frame(time=c(0, .2, .9, 1), value=c(100, 110, 180, 110))
b = data.frame(time=c(0, .3, .5, .8, 1), value=c(300, 220, 190, 400, 350))
par(mfrow = c(1, 3))
plot (a, type = 'b', ylim = c(100, 400), main = 'Original curves')
points (b, type = 'b', col = 'blue')
m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'smooth', 
                       plot = TRUE, main = 'Morphing curves')
m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'perAnchor', 
                       plot = TRUE, main = 'Morphing anchors')
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
m = suppressMessages(morph(formula1 = list(repeatBout = 2),
          # equivalently: formula1 = 'soundgen(repeatBout = 2)',
          formula2 = presets$Misc$Dog_bark,
          nMorphs = 5, playMorphs = playback))
# use $formulas to access formulas for each morph, $sounds for waveforms
# m$formulas[[4]]
# playme(m$sounds[[3]])

## -----------------------------------------------------------------------------
target = soundgen(repeatBout = 3, sylLen = 120, pauseLen = 70,
                  pitch = c(300, 200),
                  rolloff = -5, play = playback)  # we hope to reproduce this sound
# playme(target)

m1 = matchPars(target = target,
               samplingRate = 16000,
               maxIter = 0)  # no optimization, only acoustic analysis
# ignore the warning about failing to improve the fit: we don't want to optimize yet

# m1$pars contains a list of soundgen settings
cand1 = do.call(soundgen, c(m1$pars, list(play = playback, temperature = 0.001)))
# playme(cand1)

## ---- eval = FALSE------------------------------------------------------------
#  call('soundgen', m1$pars)
#  # copy-paste from the console and remove "list(...)" to get your call to soundgen():
#  # soundgen(samplingRate = 16000, nSyl = 3, sylLen = 79, pauseLen = 114,
#  #     pitch = list(time = c(0, 0.5, 1), value = c(274, 253, 216)),
#  #     formants = list(f1 = list(freq = 821, width = 122),
#  #                     f2 = list(freq = 1266, width = 36),
#  #                     f3 = list(freq = 2888, width = 117)))

## ---- eval = FALSE------------------------------------------------------------
#  m2 = matchPars(target = target,
#                 samplingRate = 16000,
#                 pars = 'rolloff',
#                 maxIter = 100)
#  
#  # rolloff should be moving from default (-9) to target (-5):
#  sapply(m2$history, function(x) {
#    paste('Rolloff:', round(x$pars$rolloff, 1),
#          '; fit to target:', round(x$sim, 2))
#  })
#  
#  cand2 = do.call(soundgen, c(m2$pars, list(play = playback, temperature = 0.001)))

