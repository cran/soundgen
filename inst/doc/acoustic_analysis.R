## ----fig.width = 5, fig.height = 5--------------------------------------------
library(soundgen)
s1 = soundgen(sylLen = 900, temperature = 0.001,
              pitch = list(time = c(0, .3, .8, 1), 
                           value = c(300, 900, 400, 1300)),
              noise = c(-40, -20), 
              subFreq = 100, subDep = 20, jitterDep = 0.5, 
              plot = TRUE, ylim = c(0, 4))
# playme(s1)  # to replay w/o re-synthesizing the sound

## ----fig.width = 5, fig.height = 3--------------------------------------------
osc(s1, samplingRate = 16000, dB = TRUE)
# Or just plain base R: plot(s1, type = 'l')

## ----fig.width = 5, fig.height = 3--------------------------------------------
seewave::meanspec(s1, f = 16000, dB = 'max0', main = 'Spectrum')

## ----fig.width = 5, fig.height = 5--------------------------------------------
spectrogram(
  s1, samplingRate = 16000,
  osc = 'dB',  # plot oscillogram in dB
  heights = c(2, 1),  # spectro/osc height ratio
  noiseReduction = .5,  # subtract the spectrum of noisy parts
  brightness = -1,  # reduce brightness
  contrast = .5,  # increase contrast
  colorTheme = 'heat.colors',  # pick color theme
  cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
  grid = 5,  # lines per kHz; to customize, add manually with graphics::grid()
  ylim = c(0, 5),  # always in kHzmain = 'Spectrogram')
  main = 'Spectrogram'
)
# see ?spectrogram for explanation of settings and more examples

## ----fig.width = 5, fig.height = 5--------------------------------------------
audSpectrogram(s1, samplingRate = 16000, 
               nFilters = 128, step = 5,
               colorTheme = 'terrain.colors',
               main = 'Auditory spectrogram')

## ----fig.width = 5, fig.height = 5--------------------------------------------
modulationSpectrum(s1, samplingRate = 16000, colorTheme = 'seewave',
                   plot = TRUE, main = 'Modulation spectrum')

## ----fig.width = 5, fig.height = 5--------------------------------------------
ssm(s1, samplingRate = 16000, main = 'Self-similarity matrix')

## ----fig.show = "hold", fig.height = 5, fig.width = 7-------------------------
a1 = analyze(s1, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
# a1$detailed  # many acoustic predictors measured for each STFT frame
median(a1$detailed$pitch, na.rm = TRUE)  # our estimate of median pitch
# Pitch postprocessing is stochastic (see below), so the contour may vary.

## -----------------------------------------------------------------------------
spec = seewave::spec(s1, f = 16000, plot = FALSE)  # FFT of the entire sound
avSpec = seewave::meanspec(s1, f = 16000, plot = FALSE)  # STFT followed by averaging
# either way, you get a dataframe with two columns: frequencies and their strength
head(avSpec)

## -----------------------------------------------------------------------------
spgm = spectrogram(s1, samplingRate = 16000, output = 'original', plot = FALSE)
# rownames give you frequencies in KHz, colnames are time stamps in ms
str(spgm)

## -----------------------------------------------------------------------------
# Transform spectrum to pdf (all columns should sum to 1):
spgm_norm = apply(spgm, 2, function(x) x / sum(x))
# Set up a dataframe to store the output
out = data.frame(skew = rep(NA, ncol(spgm)),
                 quantile66 = NA,
                 ratio500 = NA)
# Process each STFT frame
for (i in 1:ncol(spgm_norm)) {
  # Absolute spectrum for this frame
  df = data.frame(
    freq = as.numeric(rownames(spgm_norm)),  # frequency (kHz)
    d = spgm_norm[, i]                       # density
  )
  # plot(df, type = 'l')
  
  # Skewness (see https://en.wikipedia.org/wiki/Central_moment)
  m = sum(df$freq * df$d)  # spectral centroid, kHz
  out$skew[i] = sum((df$freq - m)^3 * df$d)
  
  # 66.6th percentile (2/3 of density below this frequency)
  out$quantile66[i] = df$freq[min(which(cumsum(df$d) >= 2/3))]  # in kHz
  
  # Energy above/below 500 Hz
  out$ratio500[i] = sum(df$d[df$freq >= .5]) / sum(df$d[df$freq < .5])
}
summary(out)

## ----fig.show = "hold", fig.height = 5, fig.width = 7-------------------------
a = analyze(s1, samplingRate = 16000, priorSD = 24, 
            pitchMethods = c('autocor', 'cep', 'dom', 'spec', 'hps', 'zc'),
            plot = TRUE, ylim = c(0, 4))

## ----fig.show = "hold", fig.height = 5, fig.width = 7-------------------------
par(mfrow = c(1, 2))
# default prior in soundgen
getPrior(priorMean = 300, priorSD = 6)
# narrow peak at 2 kHz
getPrior(priorMean = 2000, priorSD = 1)
par(mfrow = c(1, 1))

## ----fig.show = "hold", fig.height = 4, fig.width = 7-------------------------
a = analyze(s1, samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 2), priorMean = NA, osc = FALSE,
            pitchMethods = 'autocor',
            pitchAutocor = list(autocorThres = .45, 
                                # + plot pars if needed
                                col = 'green'),
            nCands = 3)

## ----fig.show = "hold", fig.height = 4, fig.width = 7-------------------------
a = analyze(s1,  samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 2), priorMean = NA, osc = FALSE,
            pitchMethods = 'dom',
            pitchDom = list(domThres = .1, domSmooth = 500, cex = 1.5))

## ----fig.show = "hold", fig.height = 4, fig.width = 7-------------------------
a = analyze(s1, samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 2), priorMean = NA, osc = FALSE,
            pitchMethods = 'cep',
            pitchCep = list(cepThres = .3),
            nCands = 2)

## ----fig.show = "hold", fig.height = 4, fig.width = 7-------------------------
a = analyze(s1, samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 2), priorMean = NA, osc = FALSE,
            pitchMethods = 'spec',
            pitchSpec = list(specPeak = .025, specSmooth = 400, cex = 2))

## ----fig.show = "hold", fig.height = 4, fig.width = 7-------------------------
a = analyze(s1, samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 2), priorMean = NA, osc = FALSE,
            pitchMethods = 'hps',
            pitchHps = list(hpsNum = 2, # try 8 or so to measure subharmonics
                            hpsThres = .2))

## ----fig.show = "hold", fig.height = 4, fig.width = 7-------------------------
a = analyze(s1, samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 2), priorMean = NA, osc = FALSE,
            pitchMethods = 'zc',
            pitchCeiling = 600,  # higher pitch values blend with F1
            pitchZc = list(zcWin = 3, zcThres = 0))

## ----fig.show = "hold", fig.height = 3, fig.width = 4-------------------------
s_withoutf0 = soundgen(sylLen = 600, pitch = 300,
              rolloffExact = c(0, 1, 1, 1), formants = NULL, lipRad = 0)
# playme(s_withoutf0)  # you can clearly hear the difference
seewave::meanspec(s_withoutf0, f = 16000, dB = 'max0', flim = c(0, 3))

## ----fig.show = "hold", fig.height = 4, fig.width = 6-------------------------
a_withoutf0 = analyze(s_withoutf0, 16000, priorMean = NA, priorAdapt = FALSE,
             pitchMethods = c('autocor', 'dom', 'cep', 'spec', 'hps', 'zc'),
             pitchSpec = list(specMethod = 'BaNa'),
             plot = TRUE, ylim = c(0, 2), dynamicRange = 60, osc = FALSE)

## ----fig.height = 5, fig.width = 7--------------------------------------------
a = analyze(
  s1, 
  samplingRate = 16000, plot = TRUE, ylim = c(0, 4), priorMean = NA,
  shortestSyl = 0,        # any length of voiced fragments
  interpol = NULL,        # don't interpolate missing f0 values
  pathfinding = 'none',   # don't look for optimal path through candidates
  snakeStep = 0,          # don't run the snake
  smooth = 0              # don't run median smoothing
)       

## ----fig.show = "hold", fig.height = 3, fig.width = 7-------------------------
a1 = analyze(s1, samplingRate = 16000, step = 10, priorMean = NA,
             pitchMethods = 'cep', pitchCep = list(cepThres = .25),
             interpol = NULL,   # disable interpolation
             pathfinding = 'none',  
             snakeStep = 0, smooth = 0, plot = FALSE)
a2 = analyze(s1, samplingRate = 16000, step = 10, priorMean = NA,
             pitchMethods = 'cep', pitchCep = list(cepThres = .25),
             interpol = list(win = 100, tol = .1),
             pathfinding = 'none', 
             snakeStep = 0, smooth = 0, plot = FALSE)  
plot(a1$detailed$time, a1$detailed$pitch, type = 'l', 
     xlab = 'Time, ms', ylab = 'Pitch, Hz')
points(a2$detailed$time, a2$detailed$pitch, type = 'l', 
       col = 'red', lty = 3)


## ----fig.show = "hold", fig.height = 3, fig.width = 7-------------------------
a1 = analyze(s1, samplingRate = 16000, priorMean = NA,
             pitchMethods = 'cep', pitchCep = list(cepThres = .1), nCands = 3,
             pathfinding = 'none', snakeStep = 0, interpolTol = Inf,
             smooth = 0,  # no smoothing
             summaryFun = NULL, plot = FALSE)
a1$detailed$medianSmooth = soundgen:::medianSmoother(
  data.frame(pitch = a1$detailed$pitch), smoothing_ww = 3, smoothingThres = 1
)$pitch
plot(pitch ~ time, data = a1$detailed, type = 'l', xlab = 'Time, ms', ylab = 'Pitch, Hz')
points(medianSmooth ~ time, data = a1$detailed, type = 'l', col = 'red', lty = 3, lwd = 2)
# dotted line = with median smoothing

## ----fig.show = "hold", fig.height = 3, fig.width = 7-------------------------
a1$detailed$ps = pitchSmoothPraat(
  a1$detailed$pitch, 
  bandwidth = 5, # cutoff above 5 Hz (5 pitch values per s)
  samplingRate = 1000/25)  # 1/step = number of pitch values per s
plot(pitch ~ time, data = a1$detailed, type = 'l', xlab = 'Time, ms', ylab = 'Pitch, Hz')
points(ps ~ time, data = a1$detailed, type = 'l', col = 'red', lty = 3, lwd = 2)
# dotted line = after low-pass smoothing

## ----fig.height = 5, fig.width = 7--------------------------------------------
a = analyze(
  s1, samplingRate = 16000, plot = TRUE, priorMean = NA,
  # options for spectrogram(): see ?spectrogram
  xlab = 'Time (ms)',
  main = 'My spectrogram',
  dynamicRange = 90,
  contrast = .5,
  brightness = -0.3,
  colorTheme = 'seewave',
  ylim = c(0, 4),
  # + other pars passed to soundgen:::filled.contour.mod()
  
  # options for oscillogram
  osc = 'dB', 
  heights = c(3, 1),
  
  # options for plotting the final pitch contour (line)
  pitchPlot = list(       
    col = 'black',
    lwd = 5,
    lty = 3
    # + other pars passed to base::lines()
  ),
  
  # options for plotting pitch candidates (points)
  pitchAutocor = list(col = rgb(0, 1, 0, .5), pch = 16, cex = 2),
  pitchDom = list(col = 'red', cex = 4)
)

## ----eval = FALSE-------------------------------------------------------------
#  a = analyze('~/Downloads/temp2', savePlots = '',
#              width = 900, height = 500, units = 'px')

## ----fig.height = 6, fig.width = 5--------------------------------------------
pd = pitchDescriptives(
  a1$detailed$pitch, step = a1$detailed$time[2] - a1$detailed$time[1],
  timeUnit = 'ms',
  smoothBW = c(10, 1),   # original + smoothed at 10 Hz and 1 Hz
  inflThres = .2,        # min amplitude that counts as inflections
  plot = TRUE
)
colnames(pd)

## ----fig.height = 5, fig.width = 7--------------------------------------------
# for info on using soundgen() function, see the vignette on sound synthesis 
s2 = soundgen(nSyl = 8, sylLen = 50, pauseLen = 70, temperature = 0,
              pitch = c(368, 284), amplGlobal = c(0, -20))
# add noise so SNR decreases from 20 to 0 dB from syl1 to syl8
s2 = s2 + runif(length(s2), -10 ^ (-20 / 20), 10 ^ (-20 / 20))
# playme(s2, samplingRate = 16000)
a = segment(s2, samplingRate = 16000, plot = TRUE)

## ----fig.show = "hold", fig.height = 4, fig.width = 5-------------------------
a1 = segment(s2, samplingRate = 16000, plot = TRUE, 
             SNR = 0.1, main = 'SNR very low')
a2 = suppressWarnings(segment(s2, samplingRate = 16000, plot = TRUE, 
            SNR = 14, main = 'SNR very high'))

## ----fig.show = "hold", fig.height = 4, fig.width = 5-------------------------
a1 = segment(s2, samplingRate = 16000, plot = TRUE, 
             windowLength = 40, overlap = 0, main = 'overlap too low')
a2 = suppressWarnings(segment(s2, samplingRate = 16000, plot = TRUE, 
             windowLength = 5, overlap = 80, main = 'window too short'))
a3 = segment(s2, samplingRate = 16000, plot = TRUE, 
             windowLength = 150, overlap = 80, main = 'window too long')

## ----fig.show = "hold", fig.height = 4, fig.width = 5-------------------------
# too long, but at least bursts are detected
a1 = segment(s2, samplingRate = 16000, plot = TRUE, 
             shortestSyl = 80, main = 'shortestSyl too long')    
# merges syllables
a2 = segment(s2, samplingRate = 16000, plot = TRUE, 
             shortestPause = 80, main = 'shortestPause too long')  

## ----fig.show = "hold", fig.height = 5, fig.width = 7-------------------------
dur = .5  # .5 s duration
samplingRate = 16000
f0 = getSmoothContour(c(70, 8000), len = samplingRate * dur, thisIsPitch = TRUE)
sweep = sin(2 * pi * cumsum(f0) / samplingRate)
# playme(sweep)
# spectrogram(sweep, 16000)
# plot(sweep, type = 'l')

## ----fig.height = 3, fig.width = 7--------------------------------------------
a = analyze(sweep, samplingRate = samplingRate, pitchMethods = NULL, plot = FALSE)
par(mfrow = c(1, 2))
plot(a$detailed$time, a$detailed$ampl, type = 'l', ylim = c(0, 1), main = 'RMS')
plot(a$detailed$time, a$detailed$loudness, type = 'l', main = 'Loudness')
par(mfrow = c(1, 1))

## ----fig.height = 5, fig.width = 7--------------------------------------------
l = getLoudness(sweep, samplingRate = 16000)

## ----fig.show = "hold", fig.height = 5, fig.width = 5-------------------------
s = soundgen(pitch = 70, amFreq = 25, amDep = 80, rolloff = -15)
ms = modulationSpectrum(s, samplingRate = 16000, logWarp = NULL,
                        windowLength = 25, step = 25, amRes = NULL)

## ----fig.show = "hold", fig.height = 5, fig.width = 5-------------------------
ms = modulationSpectrum(s, samplingRate = 16000, logWarp = NULL,
                        windowLength = 40, step = 10, amRes = NULL)

## ----fig.show = "hold", fig.height = 5, fig.width = 5-------------------------
ms = modulationSpectrum(
  s, samplingRate = 16000, windowLength = 15, step = 5, 
  amRes = NULL,  # analyze the entire sound at once (see TIP below)
  logSpec = FALSE,  # log-transform the spectrogram before 2D FFT?
  power = 2,  # square amplitudes in modulation spectrum ("power" spectrum)
  roughRange = c(30, 150),  # temporal modulations in the "roughness" range
  amRange = c(10, 70),    # AM frequencies of interest
  logWarp = 2,  # log-transform axes for plotting
  kernelSize = 7,  # apply Gaussian blur for smoothing
  quantiles = c(.5, .8, .95, .99),  # customize contour lines
  colorTheme = 'terrain.colors'  # alternative palette
)
ms[c('roughness', 'amFreq', 'amDep')]

## ----fig.show = "hold", fig.height = 6, fig.width = 7-------------------------
s3 = c(soundgen(), soundgen(nSyl = 4, sylLen = 50, pauseLen = 70, 
       formants = NA, pitch = c(500, 330)))
# playme(s3, 16000)
m = ssm(s3, samplingRate = 16000)

## ----fig.show = "hold", fig.height = 6, fig.width = 7-------------------------
par(mfrow = c(2, 1))
m1 = ssm(s3, samplingRate = 16000,
         input = 'melspec', simil = 'cor', norm = FALSE, 
         ssmWin = 10, kernelLen = 150)  # detailed, local features
m2 = ssm(s3, samplingRate = 16000,
         input = 'mfcc', simil = 'cosine', norm = TRUE, 
         ssmWin = 50, kernelLen = 600)  # more global
par(mfrow = c(1, 1))

## ----eval = FALSE-------------------------------------------------------------
#  # checking combinations of pitch tracking methods
#  myfolder = 'path.to.260.wav.files'
#  key = log(pitchManual)
#  p = c('autocor', 'cep', 'spec', 'dom')
#  pp = c(list(p),
#         combn(p, 3, simplify = FALSE),
#         combn(p, 2, simplify = FALSE),
#         combn(p, 1, simplify = FALSE))
#  out = list()
#  res = data.frame('pars' = sapply(pp, function(x) paste(x, collapse = ',')),
#                   cor1 = rep(NA, length(pp)),
#                   cor2 = rep(NA, length(pp)))
#  # repeating the analysis for each combination of methods in pp
#  for (i in 1:length(pp)) {
#    out[[i]] = analyze(myfolder, plot = FALSE, step = 50,
#                             pitchMethods = pp[[i]])$summary$pitch_median
#    res$cor1[i] = cor(log(out[[i]]), log(pitchManual), use = 'pairwise.complete.obs')
#    res$cor2[i] = cor(log(out[[i]]), log(pitchManual), use = 'pairwise.complete.obs') *
#      (1 - mean(is.na(out[[i]]) & !is.na(key)))
#    print(res[i, ])
#  }
#  res[order(res$cor1, decreasing = TRUE), ]  # max correlation regardless of NA
#  res[order(res$cor2, decreasing = TRUE), ]  # max correlation penalized for NA

## ----eval = FALSE-------------------------------------------------------------
#  myfolder = 'path.to.260.wav.files'
#  key = log(pitchManual)
#  out = list()
#  pars = expand.grid(windowLength = c(17, 35, 50),
#                     smooth = c(0, 1, 2))
#  for (i in 1:nrow(pars)) {
#    out[[i]] = suppressWarnings(analyze(myfolder, plot = FALSE,
#                 step = 25,
#                 pitchMethods = c('autocor','dom'),
#                 windowLength = pars$windowLength[i],
#                 smooth = pars$smooth[i]))$summary$pitch_median
#    print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs'))
#    print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs') *
#            (1 - mean(is.na(out[[i]]) & !is.na(key))))
#  }
#  pars$r1 = sapply(out, function(x) {
#    cor(log(x), key, use = 'pairwise.complete.obs')
#  })
#  pars$r2 = sapply(out, function(x) {
#    cor(log(x), key, use = 'pairwise.complete.obs') *
#      (1 - mean(is.na(x) & !is.na(key)))
#  })
#  pars
#  
#  v = 6  # pick some combination of par values to explore
#  trial = log(out[[v]])
#  cor(key, trial, use = 'pairwise.complete.obs')
#  cor(key, trial, use = 'pairwise.complete.obs') *
#    (1 - mean(is.na(trial) & !is.na(key)))
#  plot (key, trial)
#  abline(a=0, b=1, col='red')

