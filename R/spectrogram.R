### FUNCTIONS FOR PREPARING AND PLOTTING A SPECTROGRAM ###

#' Spectrogram
#'
#' Produces the spectrogram of a sound using short-time Fourier transform with
#' added routines for reassignment, multi-resolution spectrograms, noise
#' reduction, smoothing in time and frequency domains, manual control of
#' contrast and brightness, plotting the oscillogram on a dB scale, etc.
#' Gallery of examples: \url{https://cogsci.se/soundgen/spectrograms.html}.
#'
#' Many soundgen functions call \code{spectrogram}, and you can pass along most
#' of its graphical parameters from functions like \code{\link{soundgen}},
#' \code{\link{analyze}}, etc. However, in some cases this will not work (eg for
#' "units") or may produce unexpected results. If in doubt, omit extra graphical
#' parameters or save your sound first, then call spectrogram() explicitly.
#' Reassigned spectrograms are not affected by noise reduction or blurring.
#'
#' @seealso \code{\link{osc}} \code{\link{modulationSpectrum}} \code{\link{ssm}}
#'
#' @inheritParams .roxygen_defaults
#' @param windowLength length of FFT window, ms (multiple values in a vector
#'   produce a multi-resolution spectrogram, in which case \code{step} should
#'   have the same length)
#' @param specType plot the original FFT ('spectrum'), reassigned spectrogram
#'   ('reassigned'), or spectral derivatives ('derivative')
#' @param logSpec if TRUE, log-transforms the spectrogram; no effect if
#'   \code{specType = 'derivative'} because then the spectrogram is always
#'   log-transformed
#' @param rasterize (only applies if specType = 'reassigned') if TRUE, the
#'   reassigned spectrogram is plotted after rasterizing it: that is, showing
#'   density per time-frequency bins with the same resolution as an ordinary
#'   spectrogram
#' @param zp window length after zero padding, samples (a vector of the same
#'   length as \code{windowLength}). No padding is performed if zp < analysis
#'   window length in samples. If NULL, the signal is padded to a good number
#'   for speeding up the FFT. For ex., if \code{windowLength = 10} and
#'   \code{samplingRate = 22050}, each frame is \code{22050 * 0.01 = 220.5}
#'   samples long, so we can set \code{zp = 512} or just default to
#'   \code{nextn(22050 * 0.01)} by setting \code{zp = NULL}. NB: do not use
#'   zero-padding (set \code{zp = 0}) if you need to invert the spectrogram
#'   afterwards with \code{\link{istft_simple}} or
#'   \code{\link{invertSpectrogram}}
#' @param smoothFreq,smoothTime length of the window for median smoothing in
#'   frequency (Hz) and time domains (ms), respectively
#' @param qTime the quantile to be subtracted for each frequency bin. For ex.,
#'   if qTime = 0.5, the median of each frequency bin (over the entire sound
#'   duration) will be calculated and subtracted from each frame (see examples)
#' @param noiseReduction,percentNoise how much noise to remove (non-negative
#'   number, recommended 0 to 2). 0 = no noise reduction, 2 = strong noise
#'   reduction: \eqn{spectrum - (noiseReduction * noiseSpectrum)}, where
#'   noiseSpectrum is the average spectrum of frames with entropy exceeding the
#'   quantile set by \code{percentNoise}. \code{percentNoise} can range from 0
#'   to 100 (default 10), and it gives the percentage of frames classified as
#'   noise
#' @param contrast controls the sharpness or contrast of the image: <0 =
#'   decrease contrast, 0 = no change, >0 increase contrast. Recommended range
#'   approximately (-1, 1). The spectrogram is raised to the power of
#'   \code{exp(3 * contrast)}
#' @param brightness makes the image lighter or darker, range [-1, 1] (default 0
#'   = no change); for colorTheme = "bw", <0 = darker, >0 = lighter, range [-1,
#'   1]. Values are remapped through a smooth sigmoid transfer curve that
#'   preserves the full color palette. To lighten or darken the palette itself,
#'   change the colors
#' @param blur apply a Gaussian filter to blur or sharpen the image, two
#'   numbers: frequency (Hz), time (ms). A single number is interpreted as
#'   frequency, and a square filter is applied. NA / NULL / 0 means no blurring
#'   in that dimension. Negative numbers lead to unsharp masking with the
#'   default amount of 0.5 instead of blurring - see
#'   \code{\link{gaussianSmooth2D}}
#' @param maxPoints the maximum number of "pixels" in the oscillogram (if any)
#'   and spectrogram; good for quickly plotting long audio files; defaults to
#'   c(1e5, 5e5); does not affect reassigned spectrograms
#' @param output specifies what to return: unmodified spectrogram ('original'),
#'   denoised and/or smoothed spectrogram ('processed'), unmodified spectrogram
#'   with the imaginary part ('complex'), all of the above ('all'), or nothing
#'   ('none')
#' @param specManual manually calculated spectrogram-like representation in the
#'   same format as the output of spectrogram(): rows = frequency in kHz,
#'   columns = time in ms
#' @param osc "none" = no oscillogram; "linear" = on the original scale; "dB" =
#'   in decibels
#' @param ylim frequency range to plot, kHz (defaults to 0 to Nyquist
#'   frequency). NB: still in kHz, even if yScale = bark, mel, or ERB
#' @param yScale scale of the frequency axis: 'linear' = linear, 'log' =
#'   logarithmic (musical), 'bark' = bark, 'mel' = mel, 'ERB' = Equivalent
#'   Rectangular Bandwidths
#' @param heights a vector of length two specifying the relative height of the
#'   spectrogram and the oscillogram (including time axes labels)
#' @param padWithSilence if TRUE, pads the sound with half a window length of
#'   silence to resolve the edges properly (not needed if your sound is already
#'   padded with some silence)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   matlab-type palette ('matlab'), or any palette from
#'   \code{\link[grDevices]{palette}} such as 'heat.colors', 'cm.colors', etc
#' @param col actual colors, eg rev(rainbow(100)) - see ?hcl.colors for colors
#'   in base R (overrides colorTheme)
#' @param extraContour a vector of arbitrary length scaled in Hz (regardless of
#'   yScale, but nonlinear yScale also warps the contour) that will be plotted
#'   over the spectrogram (eg pitch contour); can also be a list with extra
#'   graphical parameters such as \code{lwd, col, warp} (FALSE = plot as is,
#'   TRUE = warp to conform to nonlinear yScale), etc. (see examples)
#' @param xlab,ylab,main,mar,xaxp graphical parameters for plotting
#' @param grid if numeric, adds n = \code{grid} dotted lines per kHz and the
#'   same number of lines along the time axis
#' @param ... other graphical parameters
#' @export
#' @return Depends on the \code{output} argument. All matrices have frequency
#'   in rows (rownames in kHz) and time in columns (colnames in ms):
#'   \describe{
#'     \item{'none'}{Nothing: only produces and/or saves the plot.}
#'     \item{'original'}{Unprocessed spectral magnitudes (not power!), with
#'       frequency bins from 0 up to and including Nyquist. If
#'       \code{specType = 'derivative'}, the matrix contains spectral
#'       derivatives; if \code{specType = 'reassigned'}, it contains either the
#'       rasterized reassigned spectrogram (matrix) or the unrasterized
#'       reassigned spectrogram - a dataframe giving time (ms), frequency (KHz),
#'       and magnitude of each bin).}
#'     \item{'processed'}{The same matrix as \code{'original'} after
#'       denoising, time- / frequency-smoothing, blurring, and applying contrast
#'       and brightness.}
#'     \item{'complex'}{The complete complex STFT spectrogram with both
#'       positive and negative frequencies (rows beyond Nyquist correspond
#'       to negative frequencies, as in \code{\link{stft_simple}}). This is
#'       the raw Fourier transform prior to reassignment and any
#'       post-processing, suitable for custom frequency-domain manipulation
#'       followed by inverse STFT with \code{\link{istft_simple}} (NB: use
#'       \code{zp = 0} if you intend to invert the spectrogram).}
#'     \item{'all'}{A list with all of the above.}
#'   }
#'   For multi-resolution spectrograms (\code{windowLength} with several
#'   values), the plot combines all resolutions, but the returned matrices
#'   correspond to the last value of \code{windowLength}.
#' @examples
#' # Gallery of examples: https://cogsci.se/soundgen/spectrograms.html
#'
#' # synthesize a sound 500 ms long, with gradually increasing hissing noise
#' sound = soundgen(sylLen = 500, temperature = 0.001, noise = list(
#'   time = c(0, 650), value = c(-40, 0)), formantsNoise = list(
#'   f1 = list(freq = 5000, width = 10000)))
#' # playme(sound, samplingRate = 16000)
#'
#' # basic spectrogram
#' spectrogram(sound, samplingRate = 16000, yScale = 'bark')
#'
#' # add bells and whistles
#' spectrogram(sound, samplingRate = 16000,
#'   windowLength = c(5, 40),  # multi-resolution
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   contrast = .25,  # increase contrast
#'   brightness = -.3,  # reduce brightness
#'   # pick color theme - see ?hcl.colors
#'   # colorTheme = 'heat.colors',
#'   # ...or just specify the actual colors
#'   col = colorRampPalette(c('white', 'yellow', 'red'))(50),
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   grid = 5,  # lines per kHz; to customize, add manually with graphics::grid()
#'   ylim = c(0, 5),  # always in kHz
#'   main = 'My spectrogram' # title
#'   # + axis labels, etc
#' )
#' \dontrun{
#' # save spectrograms of all sounds in a folder
#' spectrogram('~/Downloads/temp', savePlots = TRUE, cores = 2)
#'
#' # change dynamic range
#' spectrogram(sound, samplingRate = 16000, dynamicRange = 40)
#' spectrogram(sound, samplingRate = 16000, dynamicRange = 120)
#'
#' # remove the oscillogram
#' spectrogram(sound, samplingRate = 16000, osc = 'none')
#'
#' # frequencies on a logarithmic (musical) scale (mel/bark/ERB also available)
#' spectrogram(sound, samplingRate = 16000,
#'             yScale = 'log', ylim = c(.05, 8))
#'
#' # broad-band instead of narrow-band
#' spectrogram(sound, samplingRate = 16000, windowLength = 5)
#'
#' # reassigned spectrograms can be plotted without rasterizing, as a
#' # scatterplot instead of a contour plot
#' s = soundgen(sylLen = 500, pitch = c(100, 1100, 120, 1200, 90, 900, 110, 700),
#'   samplingRate = 22050, formants = NULL, lipRad = 0, rolloff = -20)
#' spectrogram(s, 22050, windowLength = 5, step = 1, yScale = 'bark')
#' sp = spectrogram(s, 22050, specType = 'reassigned', windowLength = 5,
#'   step = 1, yScale = 'bark', output = 'original')
#' # ...or it can be rasterized, but that sacrifices frequency resolution:
#' sp2 = spectrogram(s, 22050, specType = 'reassigned', rasterize = TRUE,
#'                  windowLength = 5, step = 1, yScale = 'bark')
#'
#' # an unrasterized reassigned spectrogram is just a dataframe of points
#' sp$z1 = soundgen:::zeroOne(log(sp$magn))
#' plot(sp$time, sp$freq, col = rgb(sp$z1, sp$z1, 1 - sp$z1, 1),
#'   pch = 16, cex = 0.5, ylim = c(0, 2))
#'
#' # multi-resolution spectrograms
#' spectrogram(s, 22050, windowLength = c(1, 10, 20, 50), yScale = 'bark')
#' # (works well in combination with de-blurring)
#' spectrogram(s, 22050, windowLength = c(1, 10, 20, 50), yScale = 'bark',
#'   blur = c(-50, -50))
#' spectrogram(s, 22050, windowLength = 1:10, yScale = 'bark',
#'   specType = 'reassigned', dynamicRange = 50)
#' spectrogram(s, 22050, windowLength = 1:10, yScale = 'bark',
#'   specType = 'reassigned', dynamicRange = 50, rasterize = TRUE)
#'
#' # Different combinations of specType, mono/multiresolution, and rasterization
#' spectrogram(s, 22050, windowLength = 5)
#' spectrogram(s, 22050, windowLength = c(5, 10))
#'
#' spectrogram(s, 22050, windowLength = 5, specType = 'reassigned',
#'   rasterize = FALSE)
#' spectrogram(s, 22050, windowLength = c(5, 10), specType = 'reassigned',
#'   rasterize = FALSE)
#'
#' spectrogram(s, 22050, windowLength = 5, specType = 'reassigned',
#'   rasterize = TRUE)
#' spectrogram(s, 22050, windowLength = c(5, 10), specType = 'reassigned',
#'   rasterize = TRUE)
#'
#' # focus only on values in the upper 5% for each frequency bin
#' spectrogram(sound, samplingRate = 16000, qTime = 0.95)
#'
#' # detect 10% of the noisiest frames based on entropy and remove the pattern
#' # found in those frames (in this case, breathing)
#' spectrogram(sound, samplingRate = 16000, noiseReduction = 0.2)
#'
#' # increase contrast, reduce brightness
#' spectrogram(sound, samplingRate = 16000, contrast = .7, brightness = -.7)
#'
#' # increase brightness (drops quiet bins with the same color palette)
#' spectrogram(sound, samplingRate = 16000, brightness = .5)
#'
#' # another approach is to just make the palette lighter:
#' spectrogram(sound, samplingRate = 16000, col = gray.colors(30, 1, .5))
#'
#' # median smoothing in time and/or frequency
#' spectrogram(sound, samplingRate = 16000, smoothFreq = 250)
#' spectrogram(sound, samplingRate = 16000, smoothTime = 100)
#' spectrogram(sound, samplingRate = 16000, smoothFreq = 250, smoothTime = 100)
#'
#' # Gaussian filter to blur or sharpen the image in time and/or frequency
#' spectrogram(sound, samplingRate = 16000, blur = c(100, 50))
#' # TIP: when unblurring, set the first (frequency) parameter to the
#' # frequency resolution of interest, eg ~500-1000 Hz for human formants
#' spectrogram(sound, samplingRate = 16000, windowLength = 10, blur = c(-500, 50))
#'
#' # specify location of tick marks etc - see ?par() for base graphics
#' spectrogram(sound, samplingRate = 16000,
#'             ylim = c(0, 3), yaxp = c(0, 3, 5), xaxp = c(0, .8, 10))
#'
#' # Plot long audio files with reduced resolution
#' data(speechEx, package = 'soundgen')
#' sp = spectrogram(speechEx, windowLength = c(5, 25), yScale = 'ERB',
#'   maxPoints = c(1e4, 1e4),  # limit the number of pixels in osc/spec
#'   output = 'original')
#' nrow(sp) * ncol(sp) / 5e3  # spec downsampled (~54 times fewer pixels)
#'
#' # Plot some arbitrary contour over the spectrogram (simply calling lines()
#' # will not work if the osc is shown because the plot layout is modified)
#' s = soundgen(sylLen = 1500, pitch = c(250, 350, 320, 220),
#'   jitterDep = c(0, 0, 3, 2, 0, 0))
#' an = analyze(s, 16000, plot = TRUE, extraContour = 'dom')
#' spectrogram(s, 16000, extraContour = an$detailed$dom,
#'   ylim = c(0, 2), yScale = 'bark')
#' spectrogram(s, 16000, extraContour = list(x = an$detailed$dom, col = 'green'),
#'   ylim = c(0, 2), yScale = 'bark')
#' # or simply add whatever you like to a spectrogram with points(), lines(),
#' # etc., (but only works without an oscillogram):
#' spectrogram(s, 16000, ylim = c(0, 2), yScale = 'bark', osc = 'none')
#' points(an$detailed$time/1000,  # time in s
#'        HzToOther(an$detailed$dom, 'bark'),  # values in barks
#'        lwd = 2, col = 'green', lty = 2)  # any graphic pars
#'
#' # For values that are not in Hz, normalize any way you like. NB: if yScale !=
#' # 'linear', the extra contour is by default warped to the same scale b/c it
#' # is assumed to be in Hz. Specify "warp = FALSE" to avoid this
#' spectrogram(s, 16000, yScale = 'ERB', ylim = c(0, 5), extraContour = list(
#'   x = an$detailed$loudness / max(an$detailed$loudness, na.rm = TRUE) * 5000,
#'   # because ylim[2] = 5000 Hz
#'   type = 'b', pch = 5, lwd = 2, lty = 2, col = 'blue', warp = FALSE))
#' # compare:
#' spectrogram(s, 16000, yScale = 'ERB', ylim = c(0, 5), extraContour = list(
#'   x = an$detailed$loudness / max(an$detailed$loudness, na.rm = TRUE) * 5000,
#'   # because ylim[2] = 5000 Hz
#'   type = 'b', pch = 5, lwd = 2, lty = 2, col = 'blue'))
#'
#' # Plot a spectrogram-like matrix paired with an osc
#' ms = modulationSpectrum(s, 16000, msType = '1D', amRes = 10)
#' spectrogram(s, 16000, specManual = ms$detailed$modulation_spectrogram,
#'   colorTheme = 'matlab', ylab = 'Modulation frequency, kHz',
#'   contrast = .25, blur = c(10, 10), yScale = 'log')
#' }
spectrogram = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    dynamicRange = 80,
    windowLength = 50,
    step = NULL,
    overlap = 50,
    specType = c('spectrum', 'reassigned', 'derivative'),
    logSpec = if (specType == 'derivative') FALSE else TRUE,
    rasterize = FALSE,
    wn = 'gaussian',
    zp = NULL,
    smoothFreq = 0,
    smoothTime = 0,
    qTime = 0,
    percentNoise = 10,
    noiseReduction = 0,
    output = c('original', 'processed', 'complex', 'all', 'none'),
    specManual = NULL,
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    osc = c('linear', 'dB', 'none'),
    heights = c(3, 1),
    ylim = NULL,
    yScale = c('linear', 'log', 'bark', 'mel', 'ERB'),
    contrast = 0,
    brightness = 0,
    blur = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = 'bw',
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # match args
  specType = match.arg(specType)
  output = match.arg(output)
  if (isTRUE(osc[1])) {
    osc = 'linear'  # for backward compatibility
  } else if (osc[1] == FALSE) {
    osc = 'none'
  } else {
    osc = match.arg(osc)
  }
  yScale = match.arg(yScale)

  if (is.null(step)) {
    if (is.null(overlap)) {
      stop('Need to specify either step or overlap')
    } else {
      # use the same overlap for all window lengths
      if (length(overlap) > 1 || !is.finite(overlap) ||
          any(overlap < 0) || any(overlap >= 100))
        stop('overlap must be >=0 and < 100%')
      overlap = rep(overlap[1], length(windowLength))
      step = windowLength * (1 - overlap / 100)
    }
  } else if (!length(windowLength) == length(step)) {
    stop('step must be of the same length as windowLength unless step = NULL')
  }
  if (logSpec && specType == 'derivative')
    message(paste('specType = "derivative" already log-transforms the spectrogram;',
                  'it is recommended to set logSpec = FALSE if using this method'))

  if (!is.finite(brightness) || !is.numeric(brightness) ||
      length(brightness) > 1 || brightness < -1 || brightness > 1) {
    warning('brightness must be a number between -1 and +1; resetting to 0')
    brightness = 0
  }

  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'reportEvery',
    'cores', 'savePlots', 'embed')]

  # call .spectrogram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.spectrogram',
    suffix = "spectrogram",
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, changesAudio = FALSE, suffix = "spectrogram",
                  width = paste0(width, units), embed = embed))
  }

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Spectrogram per sound
#' @noRd
.spectrogram = function(
    audio,
    dynamicRange = 80,
    windowLength = 50,
    step = NULL,
    overlap = 50,
    specType = 'spectrum',
    logSpec = if (specType == 'derivative') FALSE else TRUE,
    rasterize = FALSE,
    wn = 'gaussian',
    zp = NULL,
    smoothFreq = 0,
    smoothTime = 0,
    qTime = 0,
    percentNoise = 10,
    noiseReduction = 0,
    output = 'original',
    specManual = NULL,
    plot = FALSE,
    osc = 'linear',
    heights = c(3, 1),
    ylim = NULL,
    yScale = 'linear',
    contrast = 0,
    brightness = 0,
    blur = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = 'bw',
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    internal = NULL,
    ...
) {
  # check input, basic STFT settings
  if (any(!is.finite(audio$sound)))
    stop('the input cannot contain non-finite values')
  if (!any(audio$sound != 0))
    stop('nothing to do: the input is silent')
  if (is.null(audio$ls)) audio$ls = length(audio$sound)
  if (audio$ls < 3)
    stop('The sound is too short for plotting a spectrogram')
  if (is.null(audio$dur)) audio$dur = audio$ls / audio$samplingRate

  half_dur = audio$dur * 1000 / 2
  if (any(windowLength > half_dur)) {
    windowLength[windowLength > half_dur] = half_dur
    message(paste('windowLength > half the sound duration; resetting to',
                  windowLength, 'ms'))
  }
  wl = pmax(3, round(windowLength / 1000 * audio$samplingRate))
  if (is.null(step)) step = windowLength / 2
  step_points = pmax(1, round(step / 1000 * audio$samplingRate))

  nw = length(windowLength)
  if (is.null(zp)) zp = nextn(wl)
  if (length(zp) != nw)
    stop('zp must be of the same length as windowLength unless zp = NULL')
  contrast_exp = exp(3 * contrast)
  # br = seq(-1, 1, by = .01); plot(br, exp(3 * br), type = 'l')
  if (brightness >= 1) {
    brightness = 0.99
  } else if (brightness <= -1) {
    brightness = -0.99
  }
  reassigned_raw = NULL


  ## STFT
  reas = (specType == 'reassigned' & !rasterize)  # non-rasterized - a data.frame
  if (is.null(specManual)) {
    # the number of STFTs to perform (with different window lengths)
    nw = length(windowLength)
    if (nw > 1) specs = vector('list', nw)
    if (specType == 'derivative') {
      # --- helper: 1-D derivative, O(h^2) everywhere ---
      deriv1d = function(v) {
        n = length(v)
        if (n < 2) return(rep(0, n))
        if (n == 2) return(c(v[2] - v[1], v[2] - v[1]))
        d = numeric(n)
        d[1]       = (-3*v[1] + 4*v[2] - v[3])     / 2   # forward
        d[n]       = ( 3*v[n] - 4*v[n-1] + v[n-2]) / 2   # backward
        d[2:(n-1)] = (v[3:n] - v[1:(n-2)])          / 2   # central
        d
      }
    }

    for (w in 1:nw) {  # for each window length
      z_full = stft_simple(
        audio$sound,
        samplingRate = audio$samplingRate,
        wl = wl[w],
        step = step_points[w],
        wn = wn,
        zp = zp[w],
        padWithSilence = padWithSilence
      )
      z = z_full[1:(nrow(z_full) %/% 2 + 1), ]
      lx = ncol(z)
      X = as.numeric(colnames(z))
      if (!is.null(audio$timeShift)) {
        X = X + round(audio$timeShift * 1000)
        colnames(z) = X
      }
      ly = nrow(z)
      Y = as.numeric(rownames(z))  # frequency in kHz
      Z = Mod(z)  # freq (rows) x time (cols) — no transpose

      if (specType == 'derivative') {
        # Spectral derivative on a log-magnitude scale
        # Floor at the dynamic-range threshold so silence maps to a
        # well-defined level and the log range is bounded
        z_floor = max(Z) / 10^(dynamicRange / 20)
        silent = Z <= z_floor                       # per-bin silence mask
        Zlog = log(pmax(Z, z_floor))

        # Gradient of log-magnitude
        # Zlog is freq (rows) x time (cols)
        dZ_dt = t(apply(Zlog, 1, deriv1d))           # time derivative (along cols for each freq row)
        dZ_df = apply(Zlog, 2, deriv1d)              # frequency derivative (along rows for each time col)
        Z = sqrt(dZ_dt^2 + dZ_df^2)

        # Silent bins get exactly zero derivative (kills edge bumps)
        Z[silent] = 0

        # Clip onset / offset spikes so they don't dominate the color scale
        pos = Z > 0
        if (any(pos)) {
          clip = quantile(Z[pos], probs = 0.99, na.rm = TRUE)
          Z[Z > clip] = clip
        }
        colnames(Z) = colnames(z)  # disappear during differentiation
      } else if (specType == 'reassigned') {
        # Code adapted from librosa reassigned_spectrogram
        # Reassign frequencies (eq. 5.20 in Flandrin et al., 2002)
        seq_1_ly = 1:ly
        wl_w = wl[w]
        filter_h = winFun(wl_w, wn)
        # plot(filter_h)
        # derivative (second-order central difference)
        filter_dh = c(filter_h[2] - filter_h[1],
                      (filter_h[3:wl_w] - filter_h[1:(wl_w-2)]) / 2,
                      filter_h[wl_w] - filter_h[wl_w-1])
        # plot(filter_dh)
        z_dh = stft_simple(
          audio$sound,
          samplingRate = audio$samplingRate,
          wl = wl_w,
          step = step_points[w],
          wn = filter_dh,
          zp = zp[w],
          padWithSilence = padWithSilence
        )[seq_1_ly, , drop = FALSE]
        freqs_new = matrix(Y, nrow = nrow(z), ncol = ncol(z)) -
          Im(z_dh/z) * audio$samplingRate / (2000 * pi)

        # Reassign time stamps (eq. 5.23 in Flandrin et al., 2002)
        middle = (wl[w] + 1) / 2
        filter_th = filter_h * ((1:wl_w) - middle)
        # plot(filter_th)
        z_th = stft_simple(
          audio$sound,
          samplingRate = audio$samplingRate,
          wl = wl_w,
          step = step_points[w],
          wn = filter_th,
          zp = zp[w],
          padWithSilence = padWithSilence
        )[seq_1_ly, , drop = FALSE]
        times_new = matrix(X, nrow = ly, ncol = lx, byrow = TRUE) +
          Re(z_th / z) / audio$samplingRate * 1000

        # to long format, remove weird values
        reassigned_raw = na.omit(data.frame(
          time = as.numeric(times_new),
          freq = as.numeric(freqs_new),
          magn = as.numeric(Z))  # Z is freq x time; column-major order matches times_new/freqs_new
        )
        min_x = min(X); min_y = min(Y)
        max_x = max(X); max_y = max(Y)
        reassigned_raw = reassigned_raw[which(
          reassigned_raw$time > min_x &
            reassigned_raw$time < max_x &
            reassigned_raw$freq > min_y &
            reassigned_raw$freq < max_y), ]
        Z = reassigned_raw
      }
      if (nw > 1) specs[[w]] = Z
    }


    # combine several STFTs into a single multi-resolution spectrogram
    if (nw > 1) {
      if (specType != 'reassigned') {
        # linear interpolation to make all STFTs have the same dimensions
        # (namely, max freq and temp resolution)
        max_dims = apply(vapply(specs, dim, numeric(2)), 1, max)
        specs = lapply(specs, interpolMatrix, nr = max_dims[1], nc = max_dims[2])

        # take geometric mean of STFTs
        # Z = Reduce(function(x, y) exp((log(x) + log(y)) / 2), specs)
        Z = exp(Reduce(`+`, lapply(specs, function(x) log(x + 1e-12))) / nw)
        # avoid zeros: if one spec is log(0), the mean becomes non-finite

        # reset wl and step to the new effective freq/temp resolution
        windowLength = max(windowLength)
        step = min(step)
        X = as.numeric(colnames(Z)) # time
        Y = as.numeric(rownames(Z)) # freq
        lx = ncol(Z); ly = nrow(Z)
      } else {
        Z = reassigned_raw = do.call(rbind, specs)
      }
    }
  } else {
    # specManual is already freq (rows) x time (cols)
    Z = specManual
    X = as.numeric(colnames(Z)) # time
    Y = as.numeric(rownames(Z)) # freq
    z_full = NULL
    z = NULL
    lx = ncol(Z)
    ly = nrow(Z)
    if (length(X) > 1) step = (X[2] - X[1]) * 1000
  }
  # soundgen:::filled.contour.mod(X, Y, z = log(Z + 1e-6))


  # Rasterize the reassigned spectrogram
  if (specType == 'reassigned' && rasterize) {
    # An irregular time-frequency grid is hard to plot, so we rasterize it
    df = reassigned_raw
    time_bins = seq(min_x, max_x, length.out = lx + 1)
    # make ix & iy factors; otherwise, xtabs drops empty bins
    df$ix = factor(findInterval(df$time, time_bins, all.inside = TRUE), levels = 1:lx)
    freq_bins = seq(min_y, max_y, length.out = ly + 1)
    df$iy = factor(findInterval(df$freq, freq_bins, all.inside = TRUE), levels = 1:ly)
    Z = xtabs(magn ~ iy + ix, data = df)  # freq (rows) x time (cols)
    rownames(Z) = Y  # freq
    colnames(Z) = X  # time
  }


  ## Post-processing of STFT (for non-rasterized reassigned spectrograms, work
  ## just with the magnitudes)
  if (reas) {
    Z1 = Z$magn
  } else {
    Z1 = as.matrix(Z)
  }

  # removing noise. NB: the order of these operations is crucial,
  # don't change it!
  if (!reas) {
    # median smoothing
    if (smoothTime > 1) {
      smoothTime_points = (smoothTime / step) %/% 2 * 2 + 1  # make odd
      smoothTime_points = min(smoothTime_points, lx - (1 - lx %% 2))  # largest odd <= lx
      if (smoothTime_points > 1)
        Z1 = t(apply(Z1, 1, stats::runmed, k = smoothTime_points))
      colnames(Z1) = colnames(Z)  # runmed removed colnames
      # time domain (along cols for each freq row)
    }
    if (smoothFreq > 1) {
      if (!exists('bin_width')) bin_width = diff(as.numeric(rownames(Z1)[1:2])) * 1000
      smoothFreq_points = (smoothFreq / bin_width) %/% 2 * 2 + 1  # make odd
      smoothFreq_points = min(smoothFreq_points, ly - (1 - ly %% 2))  # largest odd <= ly
      if (smoothFreq_points > 1)
        Z1 = apply(Z1, 2, stats::runmed, k = smoothFreq_points)
      rownames(Z1) = rownames(Z)
      # freq domain (along rows for each time col)
    }
    if (qTime > 0) {
      # for each freq bin, subtract median or another quantile
      qx = apply(Z1, 1, quantile, probs = qTime)
      Z1 = sweep(Z1, 1, qx)
    }

    # noise subtraction
    if (noiseReduction > 0) {
      # silence frames with entropy above threshold
      entr = apply(Z1, 2, getEntropy) # Z1 >= 0; entropy per time frame (column)
      # don't count very quiet frames (eps)
      entr[colMeans(Z1) < 1e-6] = NA
      q = quantile(entr, probs = 1 - percentNoise/100, na.rm = TRUE)
      # na.rm b/c the entropy of silent frames is NA
      # plot(entr, type='l'); lines(x=seq_along(entr),y=rep(q,length(entr)), col='blue', lty=2)
      idx = as.numeric(which(entr >= q))
      if (length(idx) > 0) {
        noise_spectrum = as.numeric(apply(Z1[, idx, drop = FALSE], 1, mean))
        # plot(noise_spectrum, type = 'l')
        Z1 = sweep(Z1, 1, noiseReduction * noise_spectrum)
        Z1[Z1 < 0] = 0
      }
    }

    # Gaussian blur
    if (!is.null(blur) &&
        any(is.finite(blur)) &&
        any(blur != 0)) {
      if (!exists('bin_width')) bin_width = (Y[2] - Y[1]) * 1000
      if (length(blur) == 1) {
        # assume that this is in Hz
        filt_dim = c(round(blur / bin_width) * 2 + 1, 0)
      } else if (length(blur) == 2) {
        # the first number is Hz, the second ms
        filt_dim = c(
          round(blur[1] / bin_width) * 2 + 1,  # frequency (rows)
          round(blur[2] / step) * 2 + 1        # time (columns)
        )
      } else {
        stop('blur must be of length 1 or 2')
      }
      filt_dim[which(!is.finite(filt_dim))] = 0
      # Gaussian blur / unblur
      # filt_dim[d] > 0  -->  blur dimension d
      # filt_dim[d] < 0  -->  unblur (sharpen) dimension d
      # filt_dim[d] == 0 -->  no operation in dimension d
      # (NAs / Infs have already been replaced by 0 above)
      action_per_dim = ifelse(filt_dim > 0, "blur",
                              ifelse(filt_dim < 0, "unblur", "none"))
      if (all(action_per_dim == "none")) {
        # nothing to do
      } else if (action_per_dim[1] == action_per_dim[2]) {
        # both dimensions get the same operation --> single call
        Z1 = gaussianSmooth2D(Z1,
                              kernelSize = abs(filt_dim),
                              action     = action_per_dim[1])
      } else {
        # mixed: one dimension blurred, the other unblurred (or one is "none")
        # apply unblur first, then blur (matches original convention)
        for (d in order(action_per_dim, decreasing = TRUE)) {
          # "unblur" > "none" > "blur"
          if (action_per_dim[d] != "none") {
            ks = c(0, 0)
            ks[d] = abs(filt_dim[d])
            Z1 = gaussianSmooth2D(Z1,
                                  kernelSize = ks,
                                  action = action_per_dim[d])
          }
        }
      }
    }
  }

  # re-normalize, log-transform
  if (logSpec) {
    Z1 = floor_log(Z1, dynamicRange = dynamicRange)
  } else {
    Z1 = Z1 - min(Z1)
    threshold = max(Z1) / 10^(dynamicRange/20)
    Z1[Z1 < threshold] = 0
  }

  # contrast & brightness
  if (contrast_exp != 1) {
    Z1 = Z1 ^ contrast_exp
  }
  if (any(Z1 != 0)) Z1 = Z1 / max(Z1)  # now max(Z1) = 1
  if (brightness != 0) {
    # brightness: smooth sigmoid transfer curve
    k_max = 10  # steepness at |brightness| = 1; increase to enhance the effect
    k = abs(brightness) * k_max  # 0 --> identity
    m = 0.5 + brightness / 2  # inflection point

    sig = function(u) 1 / (1 + exp(-k * (u - m)))
    s0 = sig(0)
    s1 = sig(1)
    Z1 = (sig(Z1) - s0) / (s1 - s0)  # pinned: f(0) = 0, f(1) = 1
  }
  Z1[Z1 < 0] = 0

  if (reas) {
    # transform magnitudes back to the full reassigned spectrogram
    Z1 = data.frame(time = Z$time, freq = Z$freq, magn = Z1)
  }


  ## plot
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    # produce a spectrogram of the modified fft
    plotSpec(
      X = X,
      Y = Y,
      Z = Z1,
      audio = audio, internal = internal, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim, yScale = yScale,
      maxPoints = maxPoints, colorTheme = colorTheme, col = col,
      extraContour = extraContour,
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      ...
    )
  }

  if (output == 'none') return(invisible())
  if (output == 'original') {
    out = Z  # freq x time, before denoising/contrast etc., but after reassignment
  } else if (output == 'processed') {
    out = Z1  # freq x time, denoised
  } else if (output == 'complex') {
    out = z_full  # freq x time, with the imaginary part (before reassignment)
  } else if (output == 'all') {
    out = list(
      original = Z,
      processed = Z1,
      complex = z_full
    )
  }
  invisible(out)
}


#' Plot spectrogram
#' Helper function called by spectrogram() etc to plot a spectrogram.
#' @param X time stamps, ms
#' @param Y frequency stamps, kHz / mel / bark
#' @param Z frequency in rows, time in columns (the standard spectrogram
#'   orientation); or a data.frame with columns time, freq, magn for
#'   unrasterized reassigned spectrograms
#' @param audio a list returned by \code{readAudio}
#' @noRd
plotSpec = function(
    X, Y, Z,
    audio,
    internal = NULL,
    dynamicRange = 80,
    osc = 'linear',
    heights = c(3, 1),
    ylim = NULL,
    yScale = 'linear',
    maxPoints = c(1e5, 5e5),
    colorTheme = 'bw',
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    ...
) {
  # produce a spectrogram of the modified fft
  if (!is.null(col)) colorTheme = NULL
  if (!is.null(colorTheme)) {
    color.palette = switchColorTheme(colorTheme)
  } else {
    color.palette = switchColorTheme('bw')
  }
  if (is.null(col)) col = color.palette(30)
  if (osc == TRUE) osc = 'linear' else if (!is.character(osc)) osc = 'none'
  op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
  on.exit(par('mar' = op$mar, 'xaxt' = op$xaxt,
              'yaxt' = op$yaxt, 'mfrow' = op$mfrow))

  if (is.null(xlab)) xlab = ''
  if (!is.null(maxPoints)) {
    if (length(maxPoints) == 1) maxPoints = c(maxPoints, maxPoints)
  }
  if (is.null(ylim)) ylim = c(0, audio$samplingRate / 2 / 1000)
  if (is.null(main)) {
    if (audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
  }

  lx = length(X)
  # ly = length(Y)
  x_ms = X[lx] < 1    # need to convert x-scale

  if (any(c('linear', 'dB') == osc)) {
    # For long files, downsample before plotting
    if (!is.null(maxPoints) && maxPoints[1] < audio$ls) {
      myseq = seq(1, audio$ls, by = ceiling(audio$ls / maxPoints[1]))
      audio$sound = audio$sound[myseq]
      audio$ls = length(myseq)
    }

    if (osc == 'dB') {
      audio$sound = .osc(
        audio[names(audio) != 'savePlots'],
        dynamicRange = dynamicRange,
        dB = TRUE,
        plot = FALSE,
        returnWave = TRUE)
      ylim_osc = c(-2 * dynamicRange, 0)
    } else {
      ylim_osc = c(-audio$scale, audio$scale)
    }

    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
    par(mar = c(mar[1:2], 0, mar[4]), xaxt = 's', yaxt = 's')
    time_stamps = seq(0, audio$duration, length.out = audio$ls) + audio$timeShift
    plot(
      time_stamps,
      audio$sound,
      type = "l",
      ylim = ylim_osc,
      axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
      xlab = xlab, ylab = '', main = '', ...)
    box()
    time_location = axTicks(1, axp = xaxp)
    time_labels = convert_sec_to_hms(time_location, 3)
    axis(side = 1, at = time_location, labels = time_labels, ...)

    if (osc == 'dB') {
      axis(side = 4, at = seq(-dynamicRange, 0, by = 10), ...)
      abline(h = -dynamicRange, lty = 2, col = 'gray70')
      # mtext("dB", side = 2, line = 3, ...)
    } else {
      abline(h = 0, lty = 2, col = 'gray70')
    }
    par(mar = c(0, mar[2:4]), xaxt = 'n', yaxt = 's')
    xlab = ''
  } else {
    par(mar = mar)
  }

  if (x_ms) {
    xlim = c(0, audio$duration * 1000) + audio$timeShift * 1000
  } else {
    X = X / 1000
    if ('magn' %in% colnames(Z)) Z$time = Z$time / 1000
    xlim = c(0, audio$duration) + audio$timeShift
  }

  if (yScale == 'log' && ylim[1] < .01) ylim[1] = .01  # min 10 Hz
  y_Hz = ylim[2] < 1  # labels in Hz or kHz
  if (!exists('ylab') || is.null(ylab))
    if (y_Hz) ylab = 'Frequency, Hz' else ylab = 'Frequency, kHz'

  if ('magn' %in% colnames(Z)) {
    # unrasterized spectrogram
    plotUnrasterized(
      Z,
      col = col,
      ylim = ylim, main = main,
      xlab = xlab, ylab = ylab,
      xlim = xlim, xaxt = 'n',
      yScale = yScale,
      ...
    )
  } else {
    # rasterized spectrogram
    idx_y = which(Y >= (ylim[1] / 1.05) & Y <= (ylim[2] * 1.05))
    # 1.05 to avoid having a bit of white space
    Y = Y[idx_y]
    # ly = length(Y)
    Z = Z[idx_y, , drop = FALSE]  # subset frequency rows

    filled.contour.mod(
      x = X, y = Y, z = t(Z),  # transpose: filled.contour expects nrow = length(x), ncol = length(y)
      nlevels = length(col),
      col = col,
      ylim = ylim, main = main,
      xlab = xlab, ylab = ylab,
      xlim = xlim, xaxt = 'n',
      yScale = yScale,
      maxPoints = maxPoints[2],
      ...
    )
  }

  if (!(osc %in% c('linear', 'dB'))) {
    time_location = axTicks(1, axp = xaxp)
    time_labels = convert_sec_to_hms(time_location, 3)
    axis(side = 1, at = time_location, labels = time_labels, ...)
  }
  if (is.numeric(grid)) {
    n_grid_per_kHz = diff(range(ylim)) * grid
    if (Y[length(Y)] < 1) n_grid_per_kHz = n_grid_per_kHz / 1000
    # use the same number of grid lines for X and Y
    grid(nx = n_grid_per_kHz, ny = n_grid_per_kHz,
         col = rgb(0, 0, 0, .25, maxColorValue = 1), lty = 3)
  }
  if (!is.null(internal$pitch)) {
    do.call(addPitchCands, c(internal$pitch, list(y_Hz = y_Hz, yScale = yScale)))
  }

  # add an extra contour, if any
  if (!is.null(extraContour)) {
    extraContour_pars = list()
    extraContour_warp = TRUE
    if (is.list(extraContour)) {
      if (length(extraContour) > 1) {
        cnt = extraContour$x
        if (!is.null(extraContour$warp)) extraContour_warp = extraContour$warp
        extraContour_pars = extraContour[-which(names(extraContour) %in% c('x', 'warp'))]
      } else {
        cnt = extraContour[[1]]
      }
    } else {
      cnt = extraContour
    }
    # make sure the contour's length = ncol(spectrogram)
    lc = length(cnt)
    cnt = approx(x = 1:lc, y = cnt,
                 xout = seq(1, lc, length.out = length(X)),
                 na.rm = FALSE)$y  # see ex. in ?approx on handling NAs
    if (!extraContour_warp) {
      cnt = cnt / max(cnt, na.rm = TRUE) * HzToOther(ylim[2] * 1000, yScale)
    }
    do.call(addPitchCands, list(
      extraContour = cnt, extraContour_pars = extraContour_pars,
      extraContour_warp = extraContour_warp,
      y_Hz = y_Hz, timestamps = X, yScale = yScale,
      pitchCands = NA, pitchCert = NA, pitchSource = NA, pitch = NA))
  }
}


#' Modified filled.contour
#' A bare-bones version of \code{\link[graphics]{filled.contour}} that does not
#' plot a legend and accepts some additional graphical parameters like tick
#' marks.
#' @param x,y locations of grid lines (NB: x = time, y = frequency in kHz, not Hz!)
#' @param z numeric matrix of values to plot
#' @param xlim,ylim,zlim limits for the plot
#' @param levels levels for partitioning z (modified compared to default
#'   filled.contour to avoid artifacts)
#' @param nlevels numbers of levels for partitioning z
#' @param color.palette color palette function
#' @param col list of colors instead of color.palette
#' @param asp,xaxs,yaxs,... graphical parameters passed to plot.window() and
#'   axis()
#' @param axisX,axisY plot the axis or not (logical)
#' @param y_Hz Y-labels in Hz or kHz (rescales by *1000 if kHz and max < 1 kHz)
#' @noRd
#' @examples
#' data(speechEx, package = 'soundgen')
#' spec = spectrogram(speechEx, from = 0, to = 1, plot = FALSE)
#' soundgen:::filled.contour.mod(z = t(spec))
filled.contour.mod = function(
    x = seq(0, 1, len = nrow(z)),
    y = seq(0, 1, len = ncol(z)),
    z,
    xlim = range(x, finite = TRUE),
    ylim = range(y, finite = TRUE),
    zlim = range(z, finite = TRUE),
    levels = seq(min(z, na.rm = TRUE), max(z, na.rm = TRUE), length.out = nlevels + 1),
    nlevels = 30,
    color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE),
    col = color.palette(nlevels),
    legend = FALSE,
    asp = NA,
    xaxs = "i",
    yaxs = "i",
    las = 1,
    yScale = c('orig', 'bark', 'mel', 'ERB')[1],
    axisX = TRUE,
    axisY = TRUE,
    maxPoints = 5e5,
    y_Hz = NULL,
    ...
) {
  if (is.null(y_Hz)) y_Hz = (ylim[2] < 1)
  if (!missing(col) && !is.null(col)) {
    nlevels = length(col)
    ran_z = range(z, na.rm = TRUE, finite = TRUE)
    levels = seq(ran_z[1], ran_z[2], length.out = nlevels + 1)
  }
  if (ylim[2] > tail(y, 1)) ylim[2] = tail(y, 1)
  if (yScale == 'log' && ylim[1] < .01) ylim[1] = .01
  idx_keep = which(y >= ylim[1] & y <= ylim[2])
  y = y[idx_keep]
  z = z[, idx_keep]
  if (!yScale %in% c('orig', 'linear')) {
    y = HzToOther(y * 1000, yScale)
    ylim = HzToOther(ylim * 1000, yScale)
  } else {
    if (y_Hz) {
      y = y * 1000
      ylim = ylim * 1000
    }
  }

  if (legend) {
    mar.orig = (par.orig = par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w = (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar = mar.orig
    mar[4L] = mar[2L]
    mar[2L] = 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i",
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = NA)
    axis(4)
    mar = mar.orig
    mar[4L] = 1
    par(mar = mar)
  }

  suppressWarnings({
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, ...)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
      stop("no proper 'z' matrix specified")
    if (!is.double(z))  storage.mode(z) = "double"

    # for very large matrices, downsample before plotting to avoid delays
    if (!is.null(maxPoints)) {
      len_z = length(z)
      if (len_z > maxPoints) {
        message(paste('Plotting with reduced resolution;',
                      'increase maxPoints or set to NULL to override'))
        lx = length(x)
        seqx = round(seq(1, lx, length.out = ceiling(lx / (len_z / maxPoints))))
        x = x[seqx]
        z = z[seqx, ]
      }
    }
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
    title(...)
    if (axisX) axis(1, ...)
    # if (axisY) axis(2, ...)

    # could label frequency axis in Hz, but maybe it's a bit strange, and hard
    # to make sure ylab is correct (Hz or kHz, etc.)
    if (axisY)
      drawFreqAxis(y, yScale = yScale, nLbls = 5, y_Hz = y_Hz, ...)
  })
  invisible()
}


#' Draw frequency axis
#'
#' Internal soundgen function
#'
#' Helper function for adding a customized frequency axis to the spectrogram
#'
#' @param y  frequency values
#' @param ylim range of frequency values
#' @param yScale scale of frequency representation
#' @param nLbls number of frequency labels
#' @param y_Hz show frequency in Hz (TRUE) or kHz (FALSE)
#' @param ... other graphical parameters passed to axis()
#' @noRd
drawFreqAxis = function(y,
                        ylim = range(y),
                        yScale,
                        nLbls = 5,
                        y_Hz = TRUE,
                        ...) {
  if (yScale %in% c('orig', 'linear')) {
    axis(2, ...)
  } else {
    y_at = seq(ylim[1], ylim[2], length.out = nLbls) # pretty(c(y[1], tail(y, 1)))
    # round to pretty labels in Hz or kHz
    if (y_Hz) {
      y_lab = round(otherToHz(y_at, yScale))
      # and back to original yScale for precise position
      y_at = HzToOther(y_lab, yScale)
    } else {
      y_lab = round(otherToHz(y_at, yScale) / 1000, 1)
      y_at = HzToOther(y_lab * 1000, yScale)
    }
    axis(2, at = y_at, labels = y_lab, ...)
  }
}


#' Plot unrasterized spectrogram
#' @noRd
plotUnrasterized = function(
    df,
    xlim = range(df$time, finite = TRUE),
    ylim = range(df$freq, finite = TRUE),
    zlim = range(df$magn, finite = TRUE),
    levels = seq(min(df$magn, na.rm = TRUE), max(df$magn, na.rm = TRUE), length.out = nlevels + 1),
    nlevels = 30,
    pch = 16,
    cex = .25,
    color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE),
    col = color.palette(nlevels),
    legend = FALSE,
    asp = NA,
    xaxs = "i",
    yaxs = "i",
    las = 1,
    yScale = c('orig', 'bark', 'mel', 'ERB')[1],
    axisX = TRUE,
    axisY = TRUE,
    ...
) {
  if (!is.null(col)) {
    nlevels = length(col)
    levels = pretty(zlim, nlevels)
  } else if (!is.null(color.palette)) {
    col = color.palette(length(levels) - 1)
  }
  y_Hz = ylim[2] < 1  # labels in Hz or kHz
  mf = max(df$freq)
  if (ylim[2] > mf) ylim[2] = mf
  if (!yScale %in% c('orig', 'linear')) {
    df$freq = HzToOther(df$freq * 1000, yScale)
    ylim = HzToOther(ylim * 1000, yScale)
  } else {
    if (y_Hz) {
      df$freq = df$freq * 1000
      ylim = ylim * 1000
    }
  }

  idx_neg = which(df$magn <= 0)
  len_neg = length(idx_neg)
  if (len_neg == nrow(df)) {
    # nothing to plot
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, ...)
    invisible()
  }
  if (length(idx_neg) > 0)
    df$magn[idx_neg] = min(df$magn[-idx_neg])
  df$magn = zeroOne(df$magn) # zeroOne(log(df$magn))
  ord = findInterval(df$magn, seq(0, 1, length.out = length(col)))

  suppressWarnings({
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, ...)
    points(
      df$time, df$freq,
      col = col[ord],
      pch = pch, cex = cex, ...
    )
    title(...)
    if (axisX) axis(1, ...)
    if (axisY)
      drawFreqAxis(df$freq, ylim = ylim, yScale = yScale, nLbls = 5, y_Hz = y_Hz, ...)
  })
  invisible()
}
