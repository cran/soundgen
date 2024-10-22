### FUNCTIONS FOR PREPARING AND PLOTTING A SPECTROGRAM ###

#' Spectrogram
#'
#' Produces the spectrogram of a sound using short-time Fourier transform.
#' Inspired by \code{\link[seewave]{spectro}}, this function offers added
#' routines for reassignment, noise reduction, smoothing in time and frequency
#' domains, manual control of contrast and brightness, plotting the oscillogram
#' on a dB scale, grid, etc.
#'
#' Many soundgen functions call \code{spectrogram}, and you can pass along most
#' of its graphical parameters from functions like \code{\link{soundgen}},
#' \code{\link{analyze}}, etc. However, in some cases this will not work (eg for
#' "units") or may produce unexpected results. If in doubt, omit extra graphical
#' parameters or save your sound first, then call spectrogram() explicitly.
#'
#' @seealso \code{\link{osc}} \code{\link{modulationSpectrum}} \code{\link{ssm}}
#'
#' @param x path to a folder, one or more wav or mp3 files c('file1.wav',
#'   'file2.mp3'), Wave object, numeric vector, or a list of Wave objects or
#'   numeric vectors
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector)
#' @param scale maximum possible amplitude of input used for normalization of
#'   input vector (only needed if \code{x} is a numeric vector)
#' @param from,to if NULL (default), analyzes the whole sound, otherwise
#'   from...to (s)
#' @param dynamicRange dynamic range, dB. All values more than one dynamicRange
#'   under maximum are treated as zero
#' @param windowLength length of FFT window, ms
#' @param overlap overlap between successive FFT frames, \%
#' @param step you can override \code{overlap} by specifying FFT step, ms (NB:
#'   because digital audio is sampled at discrete time intervals of
#'   1/samplingRate, the actual step and thus the time stamps of STFT frames
#'   may be slightly different, eg 24.98866 instead of 25.0 ms)
#' @param specType plot the original FFT ('spectrum'), reassigned spectrogram
#'   ('reassigned'), or spectral derivative ('spectralDerivative')
#' @param logSpec if TRUE, log-transforms the spectrogram
#' @param rasterize (only applies if specType = 'reassigned') if TRUE, the
#'   reassigned spectrogram is plotted after rasterizing it: that is, showing
#'   density per time-frequency bins with the same resolution as an ordinary
#'   spectrogram
#' @param wn window type accepted by \code{\link[seewave]{ftwindow}}, currently
#'   gaussian, hanning, hamming, bartlett, blackman, flattop, rectangle
#' @param normalize if TRUE, scales input prior to FFT
#' @param zp window length after zero padding, points
#' @param smoothFreq,smoothTime length of the window for median smoothing in
#'   frequency and time domains, respectively, points
#' @param qTime the quantile to be subtracted for each frequency bin. For ex.,
#'   if qTime = 0.5, the median of each frequency bin (over the entire sound
#'   duration) will be calculated and subtracted from each frame (see examples)
#' @param percentNoise percentage of frames (0 to 100\%) used for calculating
#'   noise spectrum
#' @param noiseReduction how much noise to remove (non-negative number,
#'   recommended 0 to 2). 0 = no noise reduction, 2 = strong noise reduction:
#'   \eqn{spectrum - (noiseReduction * noiseSpectrum)}, where noiseSpectrum is
#'   the average spectrum of frames with entropy exceeding the quantile set by
#'   \code{percentNoise}
#' @param contrast a number, recommended range -1 to +1. The spectrogram is
#'   raised to the power of \code{exp(3 * contrast)}. Contrast >0 increases
#'   sharpness, <0 decreases sharpness
#' @param brightness how much to "lighten" the image (>0 = lighter, <0 = darker)
#' @param blur apply a Gaussian filter to blur or sharpen the image, two
#'   numbers: frequency (Hz), time (ms). A single number is interpreted as
#'   frequency, and a square filter is applied. NA / NULL / 0 means no blurring
#'   in that dimension. Negative numbers mean un-blurring (sharpening) the image
#'   by dividing instead of multiplying by the filter during convolution
#' @param maxPoints the maximum number of "pixels" in the oscillogram (if any)
#'   and spectrogram; good for quickly plotting long audio files; defaults to
#'   c(1e5, 5e5)
#' @param output specifies what to return: nothing ('none'), unmodified
#'   spectrogram ('original'), denoised and/or smoothed spectrogram
#'   ('processed'), or unmodified spectrogram with the imaginary part giving
#'   phase ('complex')
#' @param specManual manually calculated spectrogram-like representation in the
#'   same format as the output of spectrogram(): rows = frequency in kHz,
#'   columns = time in ms
#' @param reportEvery when processing multiple inputs, report estimated time
#'   left every ... iterations (NULL = default, NA = don't report)
#' @param cores number of cores for parallel processing
#' @param plot should a spectrogram be plotted? TRUE / FALSE
#' @param savePlots full path to the folder in which to save the plots (NULL =
#'   don't save, '' = same folder as audio)
#' @param osc "none" = no oscillogram; "linear" = on the original scale; "dB" =
#'   in decibels
#' @param ylim frequency range to plot, kHz (defaults to 0 to Nyquist
#'   frequency). NB: still in kHz, even if yScale = bark, mel, or ERB
#' @param yScale scale of the frequency axis: 'linear' = linear, 'log' =
#'   logarithmic (musical), 'bark' = bark with \code{\link[tuneR]{hz2bark}},
#'   'mel' = mel with \code{\link[tuneR]{hz2mel}}, 'ERB' = Equivalent
#'   Rectangular Bandwidths with \code{\link{HzToERB}}
#' @param heights a vector of length two specifying the relative height of the
#'   spectrogram and the oscillogram (including time axes labels)
#' @param padWithSilence if TRUE, pads the sound with just enough silence to
#'   resolve the edges properly (only the original region is plotted, so the
#'   apparent duration doesn't change)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   matlab-type palette ('matlab'), or any palette from
#'   \code{\link[grDevices]{palette}} such as 'heat.colors', 'cm.colors', etc
#' @param col actual colors, eg rev(rainbow(100)) - see ?hcl.colors for colors
#'   in base R (overrides colorTheme)
#' @param extraContour a vector of arbitrary length scaled in Hz (regardless of
#'   yScale!) that will be plotted over the spectrogram (eg pitch contour); can
#'   also be a list with extra graphical parameters such as lwd, col, etc. (see
#'   examples)
#' @param xlab,ylab,main,mar,xaxp graphical parameters for plotting
#' @param grid if numeric, adds n = \code{grid} dotted lines per kHz
#' @param width,height,units,res graphical parameters for saving plots passed to
#'   \code{\link[grDevices]{png}}
#' @param ... other graphical parameters
#' @export
#' @return Returns nothing if output = 'none', spectral magnitudes - not power!
#'   - if output = 'original', denoised and/or smoothed spectrum if output =
#'   'processed', or spectral derivatives if specType = 'spectralDerivative'.
#'   The output is a matrix of real numbers with time in columns (ms) and
#'   frequency in rows (kHz).
#' @examples
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
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   noiseReduction = 1.1,  # subtract the spectrum of noisy parts
#'   brightness = -1,  # reduce brightness
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
#' spectrogram('~/Downloads/temp', savePlots = '', cores = 2)
#'
#' # change dynamic range
#' spectrogram(sound, samplingRate = 16000, dynamicRange = 40)
#' spectrogram(sound, samplingRate = 16000, dynamicRange = 120)
#'
#' # remove the oscillogram
#' spectrogram(sound, samplingRate = 16000, osc = 'none')  # or NULL etc
#'
#' # frequencies on a logarithmic (musical) scale (mel/bark also available)
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
#' spectrogram(s, 22050, windowLength = 5, step = 1, ylim = c(0, 2))
#' spectrogram(s, 22050, specType = 'reassigned', windowLength = 5,
#'   step = 1, ylim = c(0, 2))
#' # ...or it can be rasterized, but that sacrifices frequency resolution:
#' sp = spectrogram(s, 22050, specType = 'reassigned', rasterize = TRUE,
#'                  windowLength = 5, step = 1, ylim = c(0, 2), output = 'all')
#' # The raw reassigned version is saved if output = 'all' for custom plotting
#' df = sp$reassigned
#' df$z1 = soundgen:::zeroOne(log(df$magn))
#' plot(df$time, df$freq, col = rgb(df$z1, df$z1, 1 - df$z1, 1),
#'   pch = 16, cex = 0.25, ylim = c(0, 2))
#'
#' # focus only on values in the upper 5% for each frequency bin
#' spectrogram(sound, samplingRate = 16000, qTime = 0.95)
#'
#' # detect 10% of the noisiest frames based on entropy and remove the pattern
#' # found in those frames (in this cases, breathing)
#' spectrogram(sound, samplingRate = 16000,  noiseReduction = 1.1,
#'   brightness = -2)  # white noise attenuated
#'
#' # increase contrast, reduce brightness
#' spectrogram(sound, samplingRate = 16000, contrast = .7, brightness = -.5)
#'
#' # apply median smoothing in both time and frequency domains
#' spectrogram(sound, samplingRate = 16000, smoothFreq = 5,
#'   smoothTime = 5)
#'
#' # Gaussian filter to blur or sharpen ("unblur") the image in time and/or
#' # frequency domains
#' spectrogram(sound, samplingRate = 16000, blur = c(100, 500))
#' # TIP: when unblurring, set the first (frequency) parameter to the
#' # frequency resolution of interest, eg ~500-1000 Hz for human formants
#' spectrogram(sound, samplingRate = 16000, windowLength = 10, blur = c(-500, 50))
#'
#' # specify location of tick marks etc - see ?par() for base graphics
#' spectrogram(sound, samplingRate = 16000,
#'             ylim = c(0, 3), yaxp = c(0, 3, 5), xaxp = c(0, .8, 10))
#'
#' # Plot long audio files with reduced resolution
#' data(sheep, package = 'seewave')
#' sp = spectrogram(sheep, overlap = 0,
#'   maxPoints = c(1e4, 5e3),  # limit the number of pixels in osc/spec
#'   output = 'original')
#' nrow(sp) * ncol(sp) / 5e3  # spec downsampled by a factor of ~2
#'
#' # Plot some arbitrary contour over the spectrogram (simply calling lines()
#' # will not work if osc = TRUE b/c the plot layout is modified)
#' s = soundgen(sylLen = 1500, pitch = c(250, 350, 320, 220),
#'   jitterDep = c(0, 0, 3, 2, 0, 0))
#' an = analyze(s, 16000, plot = FALSE)
#' spectrogram(s, 16000, extraContour = an$detailed$dom,
#'   ylim = c(0, 2), yScale = 'bark')
#' # For values that are not in Hz, normalize any way you like
#' spectrogram(s, 16000, ylim = c(0, 2), extraContour = list(
#'   x = an$detailed$loudness / max(an$detailed$loudness, na.rm = TRUE) * 2000,
#'   # ylim[2] = 2000 Hz
#'   type = 'b', pch = 5, lwd = 2, lty = 2, col = 'blue'))
#'
#' # Plot a spectrogram-like matrix paired with an osc
#' ms = modulationSpectrum(s, 16000, msType = '1D', amRes = 10)
#' spectrogram(s, 16000, specManual = ms$modulation_spectrogram,
#'   colorTheme = 'matlab', ylab = 'Modulation frequency, kHz',
#'   contrast = .25, blur = c(10, 10))
#' }
spectrogram = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    dynamicRange = 80,
    windowLength = 50,
    step = windowLength / 2,
    overlap = NULL,
    specType = c('spectrum', 'reassigned', 'spectralDerivative')[1],
    logSpec = TRUE,
    rasterize = FALSE,
    wn = 'gaussian',
    zp = 0,
    normalize = TRUE,
    smoothFreq = 0,
    smoothTime = 0,
    qTime = 0,
    percentNoise = 10,
    noiseReduction = 0,
    output = c('original', 'processed', 'complex', 'all')[1],
    specManual = NULL,
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = NULL,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    yScale = c('linear', 'log', 'bark', 'mel', 'ERB')[1],
    contrast = .2,
    brightness = 0,
    blur = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
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
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'reportEvery', 'cores', 'savePlots')]

  # call .spectrogram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.spectrogram',
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "spectrogram", width = paste0(width, units)))
  }

  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Spectrogram per sound
#'
#' Internal soundgen function called by \code{\link{spectrogram}} and
#' \code{\link{analyze}}.
#' @inheritParams spectrogram
#' @param internal a long list of stuff for plotting pitch contours passed by
#'   analyze()
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.spectrogram = function(
    audio,
    dynamicRange = 80,
    windowLength = 50,
    step = windowLength / 2,
    overlap = NULL,
    specType = c('spectrum', 'reassigned', 'spectralDerivative')[1],
    logSpec = TRUE,
    rasterize = FALSE,
    wn = 'gaussian',
    zp = 0,
    normalize = TRUE,
    smoothFreq = 0,
    smoothTime = 0,
    qTime = 0,
    percentNoise = 10,
    noiseReduction = 0,
    output = c('original', 'processed', 'complex', 'all')[1],
    specManual = NULL,
    plot = TRUE,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    yScale = 'linear',
    contrast = .2,
    brightness = 0,
    blur = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
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
  if (is.null(audio$ls)) audio$ls = length(audio$sound)
  if (is.null(audio$dur)) audio$dur = audio$ls / audio$samplingRate
  if (is.null(step)) {
    if (is.null(overlap)) {
      stop('Need to specify either step or overlap')
    } else {
      step = windowLength * (1 - overlap / 100)
    }
  }
  half_dur = audio$dur * 1000 / 2
  if (windowLength > half_dur) {
    windowLength = half_dur
    message(paste('windowLength > half the sound duration; resetting to', windowLength, 'ms'))
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  if (windowLength_points == 0) {
    stop('The sound and/or windowLength are too short for plotting a spectrogram')
  }
  rec_scales = c('linear', 'log', 'bark', 'mel', 'ERB')
  if (!any(rec_scales == yScale)) {
    yScale = 'linear'
    warning(paste0("Implemented yScale: ",
                   paste(rec_scales, collapse = ', '),
                   ". Defaulting to linear"))
  }
  if (!any(c('spectrum', 'reassigned', 'spectralDerivative') == specType)) {
    specType = 'spectrum'
    warning(paste("Implemented specType: 'spectrum', 'reassigned', 'spectralDerivative';",
                  "defaulting to 'spectrum'"))
  }
  contrast_exp = exp(3 * contrast)
  brightness_exp = exp(3 * brightness)
  # visualization: plot(exp(3 * seq(-1, 1, by = .01)), type = 'l')

  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_spectrogram.png"),
        width = width, height = height, units = units, res = res)
  }

  if (is.null(specManual)) {
    # Get a bank of windowed frames
    if (is.null(internal$frameBank)) {
      internal$frameBank = getFrameBank(
        sound = audio$sound,
        samplingRate = audio$samplingRate,
        windowLength_points = windowLength_points,
        step = step,
        zp = zp,
        normalize = normalize,
        wn = wn,
        filter = NULL,
        padWithSilence = padWithSilence,
        timeShift = audio$timeShift
      )
    }

    # time stamps
    X = as.numeric(colnames(internal$frameBank))
    lx = length(X)
    # adjust the timing of spectrogram to match the actual time stamps
    # in getFrameBank (~the middle of each fft frame)
    if (lx < 1) {
      message('The sound is too short for plotting a spectrogram')
      return(NA)
    }

    # frequency stamps
    zpExtra = max(0, floor((zp - windowLength_points) / 2) * 2)
    windowLength_points = windowLength_points + zpExtra
    n1 = floor(windowLength_points / 2)  # zpExtra
    bin_width = audio$samplingRate / windowLength_points
    Y = (0:(n1 - 1)) * bin_width / 1000
    ly = length(Y)
    if (ly < 2) {
      message('The sound and/or the windowLength is too short for obtaining a spectrogram')
      return(NA)
    }

    # fft of each frame
    z = apply(internal$frameBank, 2, function(x) stats::fft(x)[seq_len(n1)])
    if (!inherits(z, 'matrix')) z = matrix(z, ncol = 1)
    rownames(z) = Y
    colnames(z) = X
    Z = t(Mod(z))
    # image(Z)

    reassigned_raw = NULL
    if (specType == 'spectralDerivative') {
      # first derivative of spectrum by time
      dZ_dt = cbind(rep(0, lx), t(apply(Z, 1, diff)))
      # first derivative of spectrum by frequency
      dZ_df = rbind(rep(0, ly), apply(Z, 2, diff))
      Z = sqrt(dZ_dt ^ 2 + dZ_df ^ 2)  # length of gradient vector
    } else if (specType == 'reassigned') {
      # Code adapted from librosa reassigned_spectrogram
      # Reassign frequencies (eq. 5.20 in Flandrin et al., 2002)
      filter_h = seewave::ftwindow(wl = windowLength_points, wn = wn)
      # plot(filter_h)
      filter_dh = diff(filter_h)
      filter_dh = c(filter_dh[1] - (filter_dh[2] - filter_dh[1]), filter_dh)
      # plot(filter_dh)
      internal$frameBank_dh = getFrameBank(
        sound = audio$sound,
        samplingRate = audio$samplingRate,
        windowLength_points = windowLength_points,
        step = step,
        zp = zp,
        normalize = normalize,
        filter = filter_dh,
        padWithSilence = padWithSilence,
        timeShift = audio$timeShift
      )
      z_dh = apply(internal$frameBank_dh, 2, function(x) stats::fft(x)[seq_len(n1)])
      freqs_new = matrix(Y, nrow = nrow(z), ncol = ncol(z)) -
        Im(z_dh/z) * audio$samplingRate / (2000 * pi)

      # Reassign time stamps (eq. 5.23 in Flandrin et al., 2002)
      middle = (windowLength_points + 1) / 2
      filter_th = filter_h * (middle - (seq_len(windowLength_points)))
      # plot(filter_th)
      internal$frameBank_th = getFrameBank(
        sound = audio$sound,
        samplingRate = audio$samplingRate,
        windowLength_points = windowLength_points,
        step = step,
        zp = zp,
        normalize = normalize,
        filter = filter_th,
        padWithSilence = padWithSilence,
        timeShift = audio$timeShift
      )
      z_th = apply(internal$frameBank_th, 2, function(x) stats::fft(x)[seq_len(n1)])
      times_new = matrix(X, nrow = ly, ncol = lx, byrow = TRUE) +
        Re(z_th / z) / audio$samplingRate * 1000

      # to long format, remove weird values
      reassigned_raw = na.omit(data.frame(
        time = as.numeric(times_new),
        freq = as.numeric(freqs_new),
        magn = as.numeric(t(Z)))
      )
      min_x = min(X); min_y = min(Y)
      max_x = max(X); max_y = max(Y)
      reassigned_raw = reassigned_raw[which(
        reassigned_raw$time > min_x &
          reassigned_raw$time < max_x &
          reassigned_raw$freq > min_y &
          reassigned_raw$freq < max_y), ]

      if (!rasterize & plot) {
        # plot without rasterizing
        plotSpec(
          X = X, Y = Y, Z = reassigned_raw,
          audio = audio, internal = internal, dynamicRange = dynamicRange,
          osc = osc, heights = heights, ylim = ylim, yScale = yScale,
          maxPoints = maxPoints, colorTheme = colorTheme, col = col,
          extraContour = extraContour,
          xlab = xlab, ylab = ylab, xaxp = xaxp,
          mar = mar, main = main, grid = grid,
          width = width, height = height,
          units = units, res = res,
          ...
        )
        if (is.character(audio$savePlots)) dev.off()
        return(invisible(list(
          original = t(Z),
          processed = t(Z),
          reassigned = reassigned_raw,
          complex = z
        )))
      }

      # An irregular time-frequency grid is hard to plot, so we rasterize it
      df = reassigned_raw
      df$ix = findInterval(df$time, seq(min_x, max_x, length.out = lx + 1),
                           all.inside = TRUE)
      df$iy = findInterval(df$freq, seq(min_y, max_y, length.out = ly + 1),
                           all.inside = TRUE)
      Z = matrix(min(df$magn), nrow = lx, ncol = ly)
      for (i in seq_len(nrow(df)))
        Z[df$ix[i], df$iy[i]] = Z[df$ix[i], df$iy[i]] + df$magn[i]


      if (FALSE) {
        # alternative (marginally faster): use library(raster)
        # e = extent(df[, 1:2])
        # r = raster(e, ncol = lx, nrow = ly)
        # r_new = rasterize(df[, 1:2], r, df[, 3], fun = mean)
        # # raster::filledContour(r_new)  # need freq in Hz
        #
        # # convert from raster to df and plot with filled.contour
        # sam = sampleRegular(r_new, lx * ly, asRaster = TRUE, useGDAL = TRUE)
        # Z1 = t(matrix(getValues(sam), ncol = sam@ncols, byrow = TRUE)[nrow(sam):1, ])
        # Z1[is.na(Z1)] = min(Z1, na.rm = T)
      }
      rownames(Z) = X
      colnames(Z) = Y
    }
  } else {
    Z = t(specManual)
    X = as.numeric(rownames(Z)) # time
    Y = as.numeric(colnames(Z)) # freq
  }
  # soundgen:::filled.contour.mod(X, Y, z = log(Z))

  # set to zero under dynamic range
  Z1 = Z
  threshold = max(Z1) / 10^(dynamicRange/20)
  Z1[Z1 < threshold] = 0

  # removing noise. NB: the order of these operations is crucial,
  # don't change it!
  if (smoothTime > 1) {
    Z1 = t(apply(Z1, 1, function(x) {
      zoo::rollmedian(x, k = smoothTime, fill = 0)
    }))  # time domain
  }
  if (smoothFreq > 1) {
    Z1 = apply(Z1, 2, function(x) {
      zoo::rollmedian(x, k = smoothFreq, fill = 0)
    }) # freq domain
  }
  if (qTime > 0) {
    Z1 = t(apply(Z1, 1, function(x) {
      x - quantile(x, probs = qTime)
    }))  # for each freq bin, subtract median or another quantile
  }

  # re-normalize, log-transform
  if (logSpec) {
    positives = which(Z1 > 0)
    nonpositives = which(Z1 <= 0)
    Z1[positives] = log(Z1[positives])
    if (length(positives) > 0 & length(nonpositives) > 0) {
      Z1[nonpositives] = min(Z1[positives])
    }
  }
  Z1 = Z1 - min(Z1)

  if (noiseReduction > 0) {
    # silence frames with entropy above threshold
    entr = apply(Z1, 1, getEntropy) # Z1 >= 0
    q = quantile(entr, probs = 1 - percentNoise/100, na.rm = TRUE)
    # na.rm b/c the entropy of silent frames is NA
    # plot(entr, type='l'); lines(x=seq_along(entr),y=rep(q,length(entr)), col='blue', lty=2)
    idx = as.numeric(which(entr >= q))
    if (length(idx) > 0) {
      noise_spectrum = as.numeric(apply (Z1[idx, , drop = FALSE], 2, mean))
      # plot(noise_spectrum, type = 'l')
      Z1 = t(apply(Z1, 1, function(x) x - noiseReduction * noise_spectrum))
    }
    # re-normalize
    Z1[Z1 <= 0] = 0
  }

  # contrast & brightness
  if (contrast_exp != 1) {
    Z1 = Z1 ^ contrast_exp
  }
  if (any(Z1 != 0)) Z1 = Z1 / max(Z1)
  # if (inherits(tr, 'try-error')) browser()
  if (brightness_exp != 1) {
    Z1 = Z1 / brightness_exp
  }
  if (brightness_exp < 1) {
    Z1[Z1 > 1] = 1 # otherwise values >1 are shown as white instead of black
  }
  # Gaussian filter
  if (!is.null(blur) &&
      any(is.finite(blur)) &&
      any(blur != 0)) {
    if (!is.null(specManual)) step = (Y[2] - Y[1]) * 1000
    if (!exists('bin_width')) bin_width = (X[2] - X[1]) / length(X)
    if (length(blur) == 1) {
      # assume that this is in Hz and make a square Gaussian filter
      filt_dim = rep(round(blur / bin_width * 2) + 1, 2)
    } else if (length(blur) == 2) {
      # the first number is Hz, the second ms
      filt_dim = c(
        round(blur[2] / step) * 2 + 1,  # time
        round(blur[1] / bin_width) * 2 + 1     # frequency
      )
      # NB: rows/columns reversed because Z1 is t(spectrogram)!
    } else {
      stop('blur must be of length 1 or 2')
    }
    filt_dim[which(!is.finite(filt_dim))] = 0
    old_max = max(Z1)
    if (sign(prod(filt_dim)) < 0) {
      # first unblur in one dimension, then blur in the other
      if (filt_dim[1] < 0) {
        # unblur frequency, then blur time
        Z1 = gaussianSmooth2D(
          Z1,
          kernelSize = c(Mod(filt_dim[1]), 0),
          action = 'unblur')
        Z1 = gaussianSmooth2D(
          Z1,
          kernelSize = c(0, filt_dim[1]),
          action = 'blur')
      } else if (filt_dim[2] < 0) {
        # unblur time, then blur frequency
        Z1 = gaussianSmooth2D(
          Z1,
          kernelSize = c(0, Mod(filt_dim[2])),
          action = 'unblur')
        Z1 = gaussianSmooth2D(
          Z1,
          kernelSize = c(filt_dim[1], 0),
          action = 'blur')
      }
    } else {
      # a single blur/unblur operation
      Z1 = gaussianSmooth2D(
        Z1,
        kernelSize = Mod(filt_dim),
        action = if (filt_dim[1] >= 0) 'blur' else 'unblur')
    }

    idx_pos = which(Z1 > 0)
    Z1[-idx_pos] = 0
    Z1[idx_pos] = Z1[idx_pos] / max(Z1[idx_pos]) * old_max
  }


  ## plot
  if (plot) {
    # produce a spectrogram of the modified fft
    plotSpec(
      X = X, Y = Y, Z = Z1,
      audio = audio, internal = internal, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim, yScale = yScale,
      maxPoints = maxPoints, colorTheme = colorTheme, col = col,
      extraContour = extraContour,
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      width = width, height = height,
      units = units, res = res,
      ...
    )
  }
  if (is.character(audio$savePlots)) dev.off()

  if (output == 'original') {
    out = t(Z)  # before denoising, contrast, etc., but after reassignment
  } else if (output == 'processed') {
    out = t(Z1)  # denoised
  } else if (output == 'complex') {
    out = z  # with the imaginary part (before reassignment)
  } else {
    out = list(
      original = t(Z),
      processed = t(Z1),
      reassigned = reassigned_raw,
      complex = z
    )
  }
  invisible(out)
}


#' Frame bank
#'
#' Internal soundgen function.
#'
#' A subroutine of \code{\link{spec}} that saves windowed (and optionally
#' zero-padded) frames, i.e. chunks of the sound file of the right size and
#' spacing. Handy for further processing.
#' @param sound numeric vector
#' @inheritParams spectrogram
#' @param windowLength_points length of fft window (points)
#' @param filter fft window filter (defaults to NULL)
#' @param timeShift time (s) added to timestamps
#' @return A matrix with \code{nrow = windowLength_points/2} and \code{ncol}
#'   depending on \code{length(sound)} and \code{step}
#' @keywords internal
#' @examples
#' a = soundgen:::getFrameBank(sin(1:1000), 16000, 512, 'gaussian', 15, 0)
#' str(a)
getFrameBank = function(sound,
                        samplingRate,
                        windowLength_points,
                        wn,
                        step,
                        zp,
                        normalize = TRUE,
                        filter = NULL,
                        padWithSilence = FALSE,
                        timeShift = NULL) {
  # normalize to range from no less than -1 to no more than +1
  if (!is.numeric(sound)) return(NA)
  sound[is.na(sound)] = 0
  if (normalize && any(sound != 0)) {
    sound = sound - mean(sound)
    sound = sound / max(abs(max(sound)), abs(min(sound)))
  }
  step_points = round(step / 1000 * samplingRate)

  if (padWithSilence) {
    # pad with silence to make sure edges are properly analyzed
    sound = c(rep(0, windowLength_points / 2),
              sound,
              rep(0, (windowLength_points + step_points)))
  }
  myseq = seq(1, max(1, (length(sound) - windowLength_points)),
              by = step_points)
  if (padWithSilence) {
    time_stamps = (myseq - 1) *
      1000 / samplingRate
  } else {
    time_stamps = (myseq - 1 + windowLength_points / 2) *
      1000 / samplingRate
  }
  if (!is.null(timeShift)) time_stamps = time_stamps + round(timeShift * 1000)

  if (is.null(filter)) {
    filter = seewave::ftwindow(wl = windowLength_points, wn = wn)
  }

  # zero padding
  zpExtra = max(0, floor((zp - windowLength_points) / 2) * 2)
  if (zpExtra > 0) {
    frameBank = sapply(myseq, function(x) {
      c(rep(0, zpExtra / 2),
        sound[x:(windowLength_points + x - 1)] * filter,
        rep(0, zpExtra / 2))
    })
  } else {
    frameBank = sapply(myseq, function(x) {
      sound[x:(windowLength_points + x - 1)] * filter
    })
  }
  colnames(frameBank) = time_stamps
  frameBank
}


#' Plot spectrogram
#'
#' Internal soundgen function
#'
#' Helper function called by spectrogram() etc to plot a spectrogram.
#' @param X time stamps, ms
#' @param Y frequency stamps, kHz / mel / bark
#' @param Z time in rows, frequency in columns (NB: this is the transpose of the
#'   exported spectrogram!)
#' @param audio a list returned by \code{readAudio}
#' @inheritParams spectrogram
#' @keywords internal
plotSpec = function(
    X, Y, Z,
    audio = NULL,
    internal = NULL,
    dynamicRange = 80,
    osc = c('none', 'linear', 'dB')[2],
    heights = c(3, 1),
    ylim = NULL,
    yScale = 'linear',
    contrast = .2,
    brightness = 0,
    maxPoints = c(1e5, 5e5),
    padWithSilence = TRUE,
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
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
  # produce a spectrogram of the modified fft
  if (!is.null(col)) colorTheme = NULL
  if (!is.null(colorTheme)) {
    color.palette = switchColorTheme(colorTheme)
  } else {
    color.palette = NULL
  }
  if (osc == TRUE) osc = 'linear' else if (!is.character(osc)) osc = 'none'
  op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
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
  ly = length(Y)
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

  if (yScale == 'log' & ylim[1] < .01) ylim[1] = .01  # min 10 Hz
  y_Hz = ylim[2] < 1  # labels in Hz or kHz
  if (!exists('ylab') || is.null(ylab))
    if (y_Hz) ylab = 'Frequency, Hz' else ylab = 'Frequency, kHz'

  if ('magn' %in% colnames(Z)) {
    # unrasterized spectrogram
    plotUnrasterized(
      Z,
      color.palette = color.palette,
      col = col,
      ylim = ylim, main = main,
      xlab = xlab, ylab = ylab,
      xlim = xlim, xaxt = 'n',
      log = ifelse(yScale == 'log', 'y', ''),
      yScale = yScale,
      maxPoints = maxPoints[2],
      ...
    )
  } else {
    # rasterized spectrogram
    idx_y = which(Y >= (ylim[1] / 1.05) & Y <= (ylim[2] * 1.05))
    # 1.05 to avoid having a bit of white space
    Y = Y[idx_y]
    ly = length(Y)
    Z = Z[, idx_y]

    filled.contour.mod(
      x = X, y = Y, z = Z,
      levels = seq(0, 1, length = 30),
      color.palette = color.palette,
      col = col,
      ylim = ylim, main = main,
      xlab = xlab, ylab = ylab,
      xlim = xlim, xaxt = 'n',
      log = ifelse(yScale == 'log', 'y', ''),
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
    grid(nx = n_grid_per_kHz, ny = n_grid_per_kHz,
         col = rgb(0, 0, 0, .25, maxColorValue = 1), lty = 3)
    # grid(nx = NULL, ny = NULL,
    #      col = rgb(0, 0, 0, .25, maxColorValue = 1), lty = 3,
    #      equilogs = TRUE)
  }
  if (!is.null(internal$pitch)) {
    do.call(addPitchCands, c(internal$pitch, list(y_Hz = y_Hz, yScale = yScale)))
  }

  # add an extra contour, if any
  if (!is.null(extraContour)) {
    extraContour_pars = list()
    if (is.list(extraContour)) {
      if (length(extraContour) > 1)
        extraContour_pars = extraContour[2:length(extraContour)]
      cnt = extraContour[[1]]
    } else {
      cnt = extraContour
    }
    # make sure the contour's length = ncol(spectrogram)
    lc = length(cnt)
    cnt = approx(x = seq_len(lc), y = cnt,
                 xout = seq(1, lc, length.out = length(X)),
                 na.rm = FALSE)$y  # see ex. in ?approx on handling NAs
    do.call(addPitchCands, list(
      extraContour = cnt, extraContour_pars = extraContour_pars,
      y_Hz = y_Hz, timestamps = X, yScale = yScale,
      pitchCands = NA, pitchCert = NA, pitchSource = NA, pitch = NA))
  }
  # restore original pars
  par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
}


#' Modified filled.contour
#'
#' Internal soundgen function
#'
#' A bare-bones version of \code{\link[graphics]{filled.contour}} that does not
#' plot a legend and accepts some additional graphical parameters like tick
#' marks.
#' @param x,y locations of grid lines (NB: x = time, y = frequency in kHz, not Hz!)
#' @param z numeric matrix of values to plot
#' @param xlim,ylim,zlim limits for the plot
#' @param levels levels for partitioning z
#' @param nlevels numbers of levels for partitioning z
#' @param color.palette color palette function
#' @param col list of colors instead of color.palette
#' @param asp,xaxs,yaxs,... graphical parameters passed to plot.window() and
#'   axis()
#' @param axisX,axisY plot the axis or not (logical)
#' @param log log = 'y' log-transforms the y axis
#' @param y_Hz Y-labels in Hz or kHz (rescales by *1000 if kHz and max < 1 kHz)
#' @keywords internal
#' @examples
#' data(sheep, package = 'seewave')
#' spec = spectrogram(sheep, from = 0.3, to = 0.6, plot = FALSE)
#' soundgen:::filled.contour.mod(z = t(spec))
filled.contour.mod = function(
    x = seq(0, 1, len = nrow(z)),
    y = seq(0, 1, len = ncol(z)),
    z,
    xlim = range(x, finite = TRUE),
    ylim = range(y, finite = TRUE),
    zlim = range(z, finite = TRUE),
    levels = pretty(zlim, nlevels),
    nlevels = 30,
    color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE),
    col = color.palette(length(levels) - 1),
    legend = FALSE,
    asp = NA,
    xaxs = "i",
    yaxs = "i",
    las = 1,
    log = '',
    yScale = c('orig', 'bark', 'mel', 'ERB')[1],
    axisX = TRUE,
    axisY = TRUE,
    maxPoints = 5e5,
    y_Hz = NULL,
    ...
) {
  if (is.null(y_Hz)) y_Hz = (ylim[2] < 1)
  if (!is.null(col)) {
    nlevels = length(col)
    levels = pretty(zlim, nlevels)
  } else if (!is.null(color.palette)) {
    col = color.palette(length(levels) - 1)
  }
  if (ylim[2] > tail(y, 1)) ylim[2] = tail(y, 1)
  if (yScale == 'bark') {
    y = tuneR::hz2bark(y * 1000)
    ylim = tuneR::hz2bark(ylim * 1000)
  } else if (yScale == 'mel') {
    y = hz2mel(y * 1000)
    ylim = hz2mel(ylim * 1000)
  } else if (yScale == 'ERB') {
    y = HzToERB(y * 1000)
    ylim = HzToERB(ylim * 1000)
  } else {
    if (y_Hz) {
      y = y * 1000
      ylim = ylim * 1000
    }
    if (log == 'y' & ylim[1] < .01) ylim[1] = .01
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
                asp = asp, log = log, ...)
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
        seqx = seq(1, lx, length.out = ceiling(lx / (len_z / maxPoints)))
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
#'
#' @keywords internal
drawFreqAxis = function(y,
                        ylim = range(y),
                        yScale,
                        nLbls = 5,
                        y_Hz = TRUE,
                        ...) {
  if (!any(c('bark', 'mel', 'ERB') == yScale)) {
    axis(2, ...)
    return()
  }
  y_at = seq(ylim[1], ylim[2], length.out = nLbls) # pretty(c(y[1], tail(y, 1)))
  if (yScale == 'bark') {
    # round to pretty labels in Hz or kHz
    if (y_Hz) {
      y_lab = round(tuneR::bark2hz(y_at))
      # and back to bark for precise position
      y_at = tuneR::hz2bark(y_lab)
    } else {
      y_lab = round(tuneR::bark2hz(y_at) / 1000, 1)
      y_at = tuneR::hz2bark(y_lab * 1000)
    }
  } else if (yScale == 'mel') {
    # round to pretty labels in Hz or kHz
    if (y_Hz) {
      y_lab = round(tuneR::mel2hz(y_at))
      # and back to mel for precise position
      y_at = hz2mel(y_lab)
    } else {
      y_lab = round(tuneR::mel2hz(y_at) / 1000, 1)
      y_at = hz2mel(y_lab * 1000)
    }
  } else if (yScale == 'ERB') {
    # round to pretty labels in Hz or kHz
    if (y_Hz) {
      y_lab = round(ERBToHz(y_at))
      # and back to bark for precise position
      y_at = HzToERB(y_lab)
    } else {
      y_lab = round(ERBToHz(y_at) / 1000, 1)
      y_at = HzToERB(y_lab * 1000)
    }
  }
  axis(2, at = y_at, labels = y_lab, ...)
}


#' Plot unrasterized spetrogram
#'
#' Internal soundgen function
#'
#' Helper function for adding a customized frequency axis to the spectrogram
#'
#' @param df data to plot
#' @param xlim,ylim,zlim range of values
#' @param yScale scale of frequency representation
#' @param
#'   levels,nlevels,pch,cex,color.palette,col,legend,asp,xaxs,yaxs,las,log,axisX,axisY
#'   graphical parameters passed to plot()
#' @param maxPoints downsample if too big for plotting
#'
#' @keywords internal
plotUnrasterized = function(
    df,
    xlim = range(df$time, finite = TRUE),
    ylim = range(df$freq, finite = TRUE),
    zlim = range(df$magn, finite = TRUE),
    levels = pretty(df$magn, nlevels),
    nlevels = 30,
    pch = 16,
    cex = .25,
    color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE),
    col = color.palette(length(levels) - 1),
    legend = FALSE,
    asp = NA,
    xaxs = "i",
    yaxs = "i",
    las = 1,
    log = '',
    yScale = c('orig', 'bark', 'mel', 'ERB')[1],
    axisX = TRUE,
    axisY = TRUE,
    maxPoints = 5e5,
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
  if (yScale == 'bark') {
    df$freq = tuneR::hz2bark(df$freq * 1000)
    ylim = tuneR::hz2bark(ylim * 1000)
  } else if (yScale == 'mel') {
    df$freq = hz2mel(df$freq * 1000)
    ylim = hz2mel(ylim * 1000)
  } else if (yScale == 'ERB') {
    df$freq = HzToERB(df$freq * 1000)
    ylim = HzToERB(ylim * 1000)
  } else {
    if (y_Hz) {
      df$freq = df$freq * 1000
      ylim = ylim * 1000
    }
    if (log == 'y' & ylim[1] < .01) ylim[1] = .01
  }

  idx_neg = which(df$magn <= 0)
  if (length(idx_neg) > 0)
    df$magn[idx_neg] = min(df$magn[-idx_neg])
  df$magn = zeroOne(log(df$magn))
  ord = findInterval(df$magn, seq(0, 1, length.out = length(col)))

  # for very large matrices, downsample before plotting to avoid delays
  if (!is.null(maxPoints)) {
    nr = nrow(df)
    if (nr > maxPoints) {
      message(paste('Plotting with reduced resolution;',
                    'increase maxPoints or set to NULL to override'))
      idx = seq(1, nr, length.out = maxPoints)
      df = df[idx, ]
    }
  }

  suppressWarnings({
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, log = log, ...)
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
