# -------------------------------------------------------------
# A library of presets for easy generation of a few nice sounds
# -------------------------------------------------------------
#' Presets
#'
#' A library of presets for easy generation of a few nice sounds.
#'
#' @format A list of length 4.
"presets"
presets = list(
  M1 = list(
    Vowel1 = 'soundgen()', # these are just the global defaults

    Gasp = 'soundgen(sylLen = 240, pitch = c(160, 150), jitterDep = 0.6, rolloff = -24, rolloffParab = 15, rolloffParabHarm = 2, formantDep = 1.5, shortestEpoch = 100, noise = list(time = c(-1, 170, 362), value = c(-14, 0, -26)), rolloffNoise = -7, mouth = list(time = c(0, 0.07, 1), value = c(0, 0.48, 0.32)))',

    Roar = 'soundgen(sylLen = 960, pitch = list(time = c(0, 0.13, 0.9, 1), value = c(151, 187, 139, 120)), temperature = 0.1, nonlinBalance = 60, jitterDep = 0.7, rolloff = -12, rolloffOct = 1, formants = c(500, 1000, 1500), vocalTract = 19, subFreq = 90, subDep = 20, shortestEpoch = 150, mouth = list(time = c(0, 0.1, 1), value = c(0.38, 0.71, 0.42)))',

    Moan = 'soundgen(sylLen = 800, pitch = list(time = c(0, 0.21, 1), value = c(230, 181, 143)), jitterDep = 1, jitterLen = 60, attackLen = 100, rolloff = -12, formants = list(f1 = c(630, 860), f2 = c(900, 1430), f3 = c(3000, 2900), f4 = c(3960, 3960)), noise = list(time = c(-20, 801, 911), value = c(-30, -15, -80)), rolloffNoise = -8, mouth = c(0.54, 0.3), ampl = c(0, -9))',

    Sigh = 'soundgen(sylLen = 50, pitch = NA, temperature = 0.1, formants = c(860, 1430, 2900), vocalTract = 17, noise = list(time = c(-20, 104, 756, 1252), value = c(26, 40, 23, -22)))',

    Laugh = 'soundgen(nSyl = 5, sylLen = 120, pauseLen = 120, pitch = c(180, 166), pitchGlobal = list(time = c(0, 0.3, 1), value = c(1, 2, 0)), temperature = 0.1, nonlinBalance = 50, jitterDep = 0.8, attackLen = 10, formants = list(f1 = c(860, 550, 550), f2 = c(1430, 2300, 2300), f3 = c(2900, 2800, 2800), f4 = 4000), shortestEpoch = 50, noise = list(time = c(-10, 57, 237), value = c(-60, -20, -60)), rolloffNoise = -6, ampl = list(time = c(0, 0.32, 1), value = c(0, -20, 0)), amplGlobal = list(time = c(0, 0.77, 1), value = c(0, -11, -40)))',

    Snore = 'soundgen(sylLen = 960, pitch = list(time = c(0, 0.15, 0.87, 1), value = c(175, 199, 188, 140)), temperature = 0.1, jitterDep = 0.75, rolloffOct = -5, formants = c(560, 1000, 1450, 3800), subDep = 80, shortestEpoch = 200, noise = list(time = c(-18, 991), value = c(-28, -28)), mouth = 0)',

    # 'Formants' is a reserved name. The list of presets for every caller should end with
    # a list of 'Formants' presets for each vowel and consonant, otherwise you won't be
    # able to specify formants in a string like 'aui' for this speaker
    Formants = list(
      vowels = list(
        'a' = list(f1 = 860, f2 = 1430, f3 = 2900),
        'o' = list(f1 = 630, f2 = 900, f3 = 3000, f4 = 3960),
        'i' = list(f1 = 300, f2 = 2700, f3 = 3400),
        'e' = list(f1 = 550, f2 =2300, f3 = 2800,  f4 = 4000),
        'u' = list(f1 = 300, f2 = 610, f3 = 2100, f4 = 4200),
        '0' = list(f1 = 640, f2 = 1670, f3 = 2700, f4 = 3880)  # schwa
      ),
      consonants = list(
        'h' = list(
          label = 'h',
          rolloffNoise = 0,
          f1=list(freq=420, amp=10),
          f2=list(freq=1200, amp=30),
          f3=list(freq=3400, amp=25),
          f4=list(freq=5000, amp=25),
          f5=list(freq=8500, amp=20)
        ),
        's' = list(
          label = 's',
          rolloffNoise = 0,
          f1=list(freq=8000, amp = 25)
        ),
        'x' = list(
          label = 'sh',
          rolloffNoise = -9,
          f1=list(freq=1700, amp=15, width=80),
          f2=list(freq=2600, amp=30, width=300),
          f3=list(freq=3400, amp=25, width=200),
          f4=list(freq=4800, amp=10, width=300)
        ),
        'f' = list(
          label = 'f',
          rolloffNoise = 0,
          f1 = list(freq = 1400, amp = 20, width = 300)
        ),
        'n' = list(
          label = 'snuffle',  # sNuffle (breathing through the nose)
          rolloffNoise = 0,
          f1=list(freq=5400, amp=25, width=2000)
        )
      )
    )
  ),

  F1 = list(
    Vowel2 = 'soundgen(sylLen = 500, pitch = list(time = c(0, 0.6, 1), value = c(340, 370, 340)), formants = c(900, 1300, 3300, 4340))',

    Scream = 'soundgen(sylLen = 1110, pitch = list(time = c(0, 0.1, 0.85, 1), value = c(900, 1832, 1618, 1200)), temperature = 0.1, nonlinBalance = 70, jitterDep = 1, shimmerDep = 10, rolloff = -6, formants = NULL, vocalTract = 13.5, subFreq = 400, rolloffNoise = 0)',

    Growl = 'soundgen(sylLen = 1100, pitch = list(time = c(0, 0.12, 0.34, 1), value = c(238, 254, 448, 203)), temperature = 0.1, nonlinBalance = 71, jitterDep = 1.8, rolloff = c(-6, -9), rolloffParab = -20, rolloffParabHarm = 20, formants = NULL, vocalTract = 13.5, shortestEpoch = 150, amDep = 40, amFreq = 35, amShape = -0.4, noise = list(time = c(-6, 550, 1122), value = c(-34, -23, -36)), rolloffNoise = 0, mouth = 0)',

    Moan = 'soundgen(sylLen = 360, pitch = c(380, 260), attackLen = 100, rolloff = -24, formants = c(900, 1300, 3300, 4340), vocalTract = 13.5, noise = list(time = c(0, 417, 508), value = c(-28, -12, -80)), mouth = list(time = c(0, 0.09, 1), value = c(0.12, 0.37, 0.5)), ampl = c(0, -40))',

    Laugh = 'soundgen(nSyl = 3, sylLen = 60, pauseLen = 90, pitch = c(368, 284), temperature = 0.075, attackLen = 10, formants = c(900, 1300, 3300, 4340), noise = list(time = c(0, 67, 86, 186), value = c(-15, -17, -59, -80)), rolloffNoise = -8, ampl = c(0, -10))',

    Cry = 'soundgen(sylLen = 1600, pitch = c(610, 511), temperature = 0.2, nonlinBalance = 40, formants = NULL, vocalTract = 13.5, subFreq = 125, subDep = 10, subWidth = 25, jitterDep = 1.5, mouth = 0, ampl = c(0, -20))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      vowels = list(
        'a' = list(f1 = 900, f2 = 1300, f3 = 3300, f4 = 4340),
        'o' = list(f1 = 800, f2 = 1100, f3 = 3560, f4 = 5830),
        'i' = list(f1 = 330, f2 = 2700, f3 = 3580, f4 = 4710, f5 = 5800),
        'e' = list(f1 = 930, f2 = 2470, f3 = 3300, f4 = 4200),
        'u' = list(f1 = 450, f2 = 850, f3 = 2900, f4 = 4100),
        '0' = list(f1 = 790, f2 = 1600, f3 = 3100, f4 = 3900)  # schwa
      ),
      consonants = list(
        'h' = list(
          label = 'h',
          rolloffNoise = -13,
          f1=list(time=0, freq=420, amp=25, width=150),
          f2=list(time=0, freq=1200, amp=50, width=250),
          f3=list(time=0, freq=5000, amp=10, width=200),
          f4=list(time=0, freq=8500, amp=10, width=300)
        ),
        's' = list(
          label = 's',
          rolloffNoise = 0,
          f1=list(time=0, freq=5500, amp=25, width=200), # NB: amplitude in dB for consonants
          f2=list(time=0, freq=7000, amp=30, width=1000),
          f3=list(time=0, freq=9000, amp=30, width=1000)
        ),
        'x' = list(
          label = 'sh',
          rolloffNoise = -9,
          f1=list(time=0, freq=1700, amp=15, width=80),
          f2=list(time=0, freq=2600, amp=30, width=300),
          f3=list(time=0, freq=3400, amp=25, width=200),
          f4=list(time=0, freq=4800, amp=10, width=300)
        ),
        'f' = list(
          label = 'f',
          rolloffNoise = -10,
          f1=list(time=0, freq=1400, amp=30, width=200),
          f2=list(time=0, freq=2000, amp=10, width=80),
          f3=list(time=0, freq=2900, amp=25, width=1000)
        ),
        'n' = list(
          label = 'snuffle',  # sNuffle (breathing through the nose)
          rolloffNoise=0,
          f1=list(time=0, freq=5400, amp=25, width=2000)
        )
      )
    )
  ),

  Chimpanzee = list(
    Bark_alarm = 'soundgen(sylLen = 160, pitch = c(232, 185), jitterDep = 2.8, attackLen = 61, rolloff = -19, formants = c(400, 1000), vocalTract = 14, subFreq = 125, subDep = 100, subWidth = 60, noise = list(time = c(0, 63, 344), value = c(-80, -5, -80)), rolloffNoise = -6)',

    Scream_conflict = 'soundgen(sylLen = 740, pitch = list(time = c(0, 0.3, 0.9, 1), value = c(1200, 1547, 1487, 1154)), formants = NULL, vocalTract = 14, temperature = 0.05, jitterDep = 0.3, rolloff = -6, subFreq = 75, subDep = 60, subWidth = 100)',

    Grunt_excited = 'soundgen(nSyl = 6, sylLen = 100, pauseLen = 220, pitch = c(216, 164), pitchGlobal = list(time = c(0, 0.4, 1), value = c(0, 1, -8.4)), temperature = 0.15, jitterDep = 3.1, attackLen = 10, formants = list(f1 = c(410, 250), f2 = c(990, 650)), vocalTract = 20.5, noise = list(time = c(-8, 22, 298), value = c(-80, -25, -80)), rolloffNoise = -7, ampl = c(0, -30), amplGlobal = list(time = c(0, 0.35, 1), value = c(-10, 0, -20)))',

    Hoot_excited = 'soundgen(sylLen = 730, pitch = list(time = c(0, 0.21, 0.79, 1), value = c(440, 469, 402, 440)), temperature = 0.075, jitterDep = 0.4, shimmerDep = 11, attackLen = 0, rolloff = -6, formants = c(400, 850, 2600, 3700, 5500), noise = list(time = c(-19, 26, 173, 738), value = c(-44, -4, -37, -39)), mouth = list(time = c(0, 0.12, 1), value = c(0.01, 0.5, 0.5)))',

    Laugh_playing = 'soundgen(nSyl = 6, sylLen = 120, pauseLen = 120, pitch = c(127, 102), temperature = 0.05, rolloff = -12, rolloffParab = 15, rolloffParabHarm = 1, formants = c(400, 900, 1500), vocalTract = 23, noise = list(time = c(-6, 30, 110, 168, 219), value = c(-32, -53, -52, -51, -80)), rolloffNoise = -20, amplGlobal = list(time = c(0, 0.35, 1), value = c(-10, 0, -25)))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Cat = list(
    Chirp = 'soundgen(sylLen = 120, pitch = c(828, 768, 423), jitterDep = 0.3, shimmerDep = 11, attackLen = 1, rolloff = -3, formants = c(875, 1944, 2409), noise = list(time = c(-1, 80, 161), value = c(-60, 14, -47)), ampl = list(time = c(0, 0.33, 1), value = c(0, 0, -23)))',

    Growl = 'soundgen(sylLen = 2700, pitch = c(154, 121), jitterDep = 1.3, jitterLen = 6, formants = c(200, 400, 700), vocalTract = 8, subFreq = 80, subDep = 100, subWidth = 50, shortestEpoch = 50)',

    Hiss = 'soundgen(pitch = NULL, formants = c(1112, 2610, 4587), formantDep = 0.8, noise = list(time = c(-185, 74, 506), value = c(-5, 40, 0)), rolloffNoise = -8, mouth = list(time = c(0, 0.13, 1), value = c(0, 0.5, 0.5)))',

    Howl = 'soundgen(sylLen = 3150, pitch = list(time = c(0, 0.05, 0.35, 0.89, 1), value = c(218, 339, 352, 291, 229)), temperature = 0.125, rolloff = -5, rolloffParabHarm = 1, formants = list(f1 = 895, f2 = c(1500, 2000), f3 = 3287), vocalTract = 10, mouth = list(time = c(0, 0.18, 0.25, 0.41, 0.89, 1), value = c(0.32, 0.24, 0.66, 0.37, 0.39, 0.27)))',

    Heat = 'soundgen(sylLen = 800, pitch = c(480, 552, 501), temperature = 0.1, jitterDep = 0.3, jitterLen = 60, rolloff = -6, rolloffParabHarm = 1, formants = list(f1 = c(1500, 500), f2 = 2100, f3 = 5500), amDep = 20, amFreq = 40, mouth = list(time = c(0, 0.12, 0.86, 1), value = c(0, 0.52, 0.57, 0)))',

    Meow = 'soundgen(sylLen = 920, pitch = list(time = c(0, 0.2, 1), value = c(480, 550, 515)), attackLen = 150, rolloff = -12, formants = c(1594, 3600), mouth = list(time = c(0, 0.16, 0.91, 1), value = c(0, 1, 0.31, 0.06)))',

    Purr = 'soundgen(repeatBout = 2, nSyl = 2, sylLen = 800, pauseLen = 20, pitch = 25, rolloff = -3, jitterDep = .5, shimmerDep = 20, formants = c(85, 230, 450, 1200, 2600, 5200, 7000), noise = list(time = c(-113, 96, 839), value = c(-65, -30, -65)), formantsNoise = c(1200, 2600, 5200, 7000), rolloffNoise = -3, temperature = 0.15, tempEffects = list(pitchDriftDep = 1))',

    Scream = 'soundgen(repeatBout = 2, sylLen = 1610, pauseLen = 500, pitch = list(time = c(0, 0.28, 0.53, 0.88, 1), value = c(388, 385, 669, 663, 392)), temperature = 0.05, nonlinBalance = 35, jitterDep = 1.9, rolloff = -6, formants = list(f1 = c(1800, 1000), f2 = 2400), subFreq = 150, subDep = 100, subWidth = 80)',

    Snarl = 'soundgen(sylLen = 450, pitch = list(time = c(0, 0.08, 1), value = c(193, 454, 434)), temperature = 0.1, nonlinBalance = 75, jitterDep = 1.1, attackLen = 0, rolloff = -6, rolloffOct = -1, formants = list(f1 = list(freq = c(1500, 2000), width = 300), f2 = list(freq = 2500, width = 350)), subFreq = 230, subDep = 100, subWidth = 170, shortestEpoch = 75, noise = -15, mouth = list(time = c(0, 0.3, 0.63, 1), value = c(0, 1, 1, 0.5)))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Misc = list(
    Crocodile = "soundgen(sylLen = 2570, pitch = list(time = c(0, 0.36, 1), value = c(25, 42, 25)), glottis = c(15, 80), rolloff = c(-40, -33, -32, -35), jitterDep = .5, shimmerDep = 20, formants = c(180, 500, 1200, 1700, 2280, 2680, 3350), noise = c(-50, -40, -50), ampl = c(-10, 0, -10), dynamicRange = 120, temperature = .01, invalidArgAction = 'ignore')",

    Dog_bark = 'soundgen(repeatBout = 2, sylLen = 140, pauseLen = 100, pitch = list(time = c(0, 0.29, 1), value = c(559, 716, 647)), temperature = 0.05, jitterDep = 2.1, formants = c(1500, 3300, 6000), vocalTract = 8.5, noise = list(time = c(0, 78, 160), value = c(-50, 10, -50)), rolloffNoise = -9, mouth = c(0, 0.5, 0))',

    Duck = 'soundgen(repeatBout = 5, sylLen = 110, pauseLen = 170, pitch = c(119, 110), temperature = 0.1, rolloff = -3, rolloffOct = -1, formants = c(1600, 2700, 5600, 6400), noise = -13, mouth = c(0.34, 0.57, 0.35))',

    Elephant = 'soundgen(sylLen = 510, pitch = list(time = c(0, 0.36, 1), value = c(450, 485, 328)), nonlinBalance = 50, jitterDep = 0.3, rolloff = -3, rolloffOct = -2, rolloffKHz = 0, formants = NULL, formantDepStoch = 0, subFreq = 75, subDep = 30, subWidth = 40, shortestEpoch = 50, noise = list(time = c(0, 510), value = c(-19, -19)), ampl = list(time = c(0, 0.5, 1), value = c(0, 0, -10)))',

    Seagull = 'soundgen(nSyl = 8, sylLen = 200, pauseLen = 140, pitch = list(time = c(0, 0.71, 1), value = c(977, 1530, 826)), jitterDep = 0, rolloff = -6, rolloffParabHarm = 6, rolloffKHz = 0, formants = c(2500, 4500), subRatio = 2, subDep = 100, subWidth = 220, noise = list(time = c(0, 44, 141, 201), value = c(-21, -22, -80, -22)), samplingRate = 24000)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  )

  # # empty container for adding new speakers
  # NewSpeaker = list(
  #   Formants = list()
  # )
) # END of presets / dictionaries
# usethis::use_data(presets, overwrite = TRUE)
#


## extras
# Whale = 'soundgen(sylLen = 1000, pitch = c(330, 180), amFreq = c(30, 15), amDep = 100, formants = NULL, vocalTract = 23)'

# cow1 = soundgen(sylLen = 1400, pitch = list(time = c(0, 11/14, 1), value = c(75, 130, 200)), temperature = 0.1, rolloff = -6, rolloffOct = -3, rolloffParab = 12, mouthOpenThres = 0.6, formants = NULL, vocalTract = 36.5, mouth = list(time = c(0, 0.82, 1), value = c(0.6, 0, 1)), noise = list(time = c(0, 1400), value = c(-25, -25)), rolloffNoise = -4, addSilence = 0)
# cow2 = soundgen(sylLen = 310, pitch = list(time = c(0, 1), value = c(359, 359)), temperature = 0.05, nonlinBalance = 100, jitterDep = 1.3, attackLen = 0, rolloff = -6, rolloffOct = -3, rolloffKHz = -0, formants = NULL, vocalTract = 36.5, subFreq = 150, subDep = 100, subWidth = 70, noise = list(time = c(0, 26, 317, 562), value = c(-80, -23, -22, -80)), rolloffNoise = -6, addSilence = 0)
# s = crossFade(cow1, cow2, samplingRate = 16000, crossLen = 150)
# # playme(s, 16000)
# # spectrogram(s, 16000, osc=T)
# # seewave::savewav(s, f = 16000, '~/Downloads/cow_soundgen.wav')

# a good ex. of a breathy [a]: s = soundgen(ampl = c(0, -20), sylLen = 1200, pitch = 200, formants = c(720, 1370, 2900, 3800, 4900, 5500, 6500), noise = -15, rolloffNoise = 0, rolloff = -24, plot = T, play = T, formantDep = 1.5, formantCeiling = 5)
