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

-   Sound synthesis from R console: `soundgen()`
-   Shiny app for sound synthesis (opens in a browser): `soungen_app()`
-   Acoustic analysis of a wav/mp3 file: `analyze()`
-   Shiny app for editing intonation contours (opens in a browser):
    `pitch_app()`
-   Measuring syllables, pauses, and bursts in a wav/mp3 file:
    `segment()`

For more information, please see the vignettes on sound synthesis and
acoustic analysis and other tips at
<a href="https://cogsci.se/soundgen.html">https://cogsci.se/soundgen.html</a>

# Example of sound synthesis

Use the `soundgen()` function to create a breathy moan:

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

<img src="man/figures/README-synthsesis-1.png" width="80%" style="display: block; margin: auto;" />

<p>
<audio controls style = "display: block">
<source src="man/figures/s.mp3" type="audio/mp3">
</audio>
</p>

# Example of acoustic analysis

Use the `analyze()` function to detect pitch and obtain other spectral
descriptives of the sound we have just synthesized:

    a = analyze(s, 44100, plot = TRUE, ylim = c(0, 6))

<img src="man/figures/README-analysis-1.png" width="80%" style="display: block; margin: auto;" />

    a$detailed[1:5, c('pitch', 'peakFreq', 'harmHeight', 'HNR', 'ampl', 'loudness')]
    #>      pitch peakFreq harmHeight      HNR       ampl  loudness
    #> 1       NA       NA         NA       NA 0.02203151  2.971372
    #> 2 303.0399 300.1361   1063.385 12.61768 0.23630756 10.772501
    #> 3 302.0111 300.1361   1052.351 21.29106 0.48501199 17.973918
    #> 4 299.5857 300.1361   1049.926 21.05983 0.57804421 20.946816
    #> 5 296.4350 300.1361   1036.771 19.61540 0.50244285 19.852580
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

# Installation

To install the current release from CRAN: `install.packages("soundgen")`

NB: Make sure all dependencies have been installed correctly! On Macs,
you may need to do the following:

-   First install brew according to the instructions here:
    <https://brew.sh/>
-   Then run the following from the terminal  
    `brew install libsndfile`  
    `brew install fftw`
-   Finally, install soundgen in R:  
    `install.packages("soundgen")`
