soundgen readme
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN
Version](http://www.r-pkg.org/badges/version/soundgen)](https://cran.r-project.org/package=soundgen)
[![Downloads](http://cranlogs.r-pkg.org/badges/soundgen)](https://CRAN.R-project.org/package=soundgen)

R package for sound synthesis and acoustic analysis.  
Homepage with help, demos, etc: <http://cogsci.se/soundgen.html>

Performs parametric synthesis of sounds with harmonic and noise
components such as animal vocalizations or human voice. Also includes
tools for spectral analysis, pitch tracking, audio segmentation,
self-similarity matrices, morphing, etc.

# Key functions

- Sound synthesis from R console: `soundgen()`
- Shiny app for sound synthesis (opens in a browser): `soungen_app()`
- Acoustic analysis of a wav/mp3 file: `analyze()`
- Shiny app for editing intonation contours (opens in a browser):
  `pitch_app()`
- Measuring syllables, pauses, and bursts in a wav/mp3 file: `segment()`

For more information, please see the vignettes on sound synthesis and
acoustic analysis and other tips at
<a href="https://cogsci.se/soundgen.html">https://cogsci.se/soundgen.html</a>

# Example of sound synthesis

Use the `soundgen()` function to create a breathy moan:

``` r
s = soundgen(
  sylLen = 230,               # duration of voiced part, ms
  pitch = c(305, 280),        # pitch, Hz (goes down from 305 to 380 Hz)
  ampl = c(0, -20),           # amplitude, dB (gradual fade-out by 20 dB)
  rolloff = -30,              # strong f0, weak harmonics
  temperature = 0.05,         # some stochasticity in generation
  formants = c(260, 960, 1500, 2200,  # F1-F8 formant frequencies, Hz
               2600, 3600, 4200, 4500),
  noise = data.frame(
    time = c(-50, 120, 700),  # time of noise anchors
    value = c(-35, -25, -50)   # noise amplitude, dB
  ),
  rolloffNoise = 0,           # flat noise spectrum before adding formants
  addSilence = 0, samplingRate = 44100, pitchSamplingRate = 44100,
  play = TRUE, plot = TRUE, osc = TRUE, ylim = c(0, 6)
)
```

<img src="man/figures/README-synthsesis-1.png" width="80%" style="display: block; margin: auto;" />

<p>

<audio controls style = "display: block">

<source src="man/figures/s.mp3" type="audio/mp3">

</audio>

</p>

# Example of acoustic analysis

Use the `analyze()` function to detect pitch and obtain other spectral
descriptives of the sound we have just synthesized:

``` r
a = analyze(s, 44100, plot = TRUE, ylim = c(0, 6))
```

<img src="man/figures/README-analysis-1.png" width="80%" style="display: block; margin: auto;" />

``` r
a$detailed[1:5, c('pitch', 'peakFreq', 'harmHeight', 'HNR', 'ampl', 'loudness')]
#>      pitch peakFreq harmHeight      HNR      ampl  loudness
#> 1       NA       NA         NA       NA 0.0352417  3.361811
#> 2 304.0203 300.1361   1368.385 13.57148 0.2611070 11.217886
#> 3 302.1499 300.1361         NA 20.99729 0.5078240 18.005310
#> 4 299.4880 300.1361   1049.828 19.95427 0.5918217 20.944904
#> 5 296.4051 300.1361   1629.551 19.15678 0.5082565 19.372810
colnames(a$detailed)
#>  [1] "duration"           "duration_noSilence" "time"               "amEnvDep"          
#>  [5] "amEnvDepVoiced"     "amEnvFreq"          "amEnvFreqVoiced"    "amMsFreq"          
#>  [9] "amMsFreqVoiced"     "amMsPurity"         "amMsPurityVoiced"   "ampl"              
#> [13] "ampl_noSilence"     "amplVoiced"         "CPP"                "dom"               
#> [17] "domVoiced"          "entropy"            "entropySh"          "entropyShVoiced"   
#> [21] "entropyVoiced"      "epoch"              "f1_freq"            "f1_width"          
#> [25] "f2_freq"            "f2_width"           "f3_freq"            "f3_width"          
#> [29] "flux"               "fmDep"              "fmFreq"             "fmPurity"          
#> [33] "harmEnergy"         "harmHeight"         "HNR"                "HNRVoiced"         
#> [37] "loudness"           "loudnessVoiced"     "novelty"            "noveltyVoiced"     
#> [41] "peakFreq"           "peakFreqVoiced"     "pitch"              "quartile25"        
#> [45] "quartile25Voiced"   "quartile50"         "quartile50Voiced"   "quartile75"        
#> [49] "quartile75Voiced"   "roughness"          "roughnessVoiced"    "specCentroid"      
#> [53] "specCentroidVoiced" "specSlope"          "specSlopeVoiced"    "subDep"            
#> [57] "subRatio"           "voiced"
```

# Installation

To install the current release from CRAN: `install.packages("soundgen")`

NB: Make sure all dependencies have been installed correctly! On Macs,
you may need to do the following:

- First install brew according to the instructions here:
  <https://brew.sh/>
- Then run the following from the terminal  
  `brew install libsndfile`  
  `brew install fftw`
- Finally, install soundgen in R:  
  `install.packages("soundgen")`
