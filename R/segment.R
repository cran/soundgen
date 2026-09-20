## FINDING SYLLABLES AND VOCAL BURSTS ##

#' Segment a sound
#'
#' Finds syllables and bursts / beats separated by background noise in long
#' recordings (up to 1-2 hours of audio per file). Syllables are defined as
#' continuous segments that differ from background noise based on amplitude
#' and/or spectral contrast. Bursts are defined as local maxima in the detection
#' contour that are high enough relative to the surrounding region. A note on
#' long recordings: an hour of audio takes ~1 min to process, but watch your
#' memory usage and perhaps decrease \code{maxDur} to avoid running out of RAM.
#' This has little effect on processing speed, but dramatically cuts memory
#' requirements. Another advantage of using shorter \code{maxDur} is that
#' signal-noise separation may improve because noise amounts and profiles can be
#' estimated in each chunk, adjusting for variable recording conditions over
#' time. The downside is that small segments at chunk boundaries may be missed.
#'
#' Algorithm: the sound is analyzed in chunks of at most \code{maxDur} seconds.
#' In each chunk, the quietest and most stable regions are located, and a noise
#' threshold is derived either from a user-specified proportion of noise
#' (\code{propNoise}) or, if \code{propNoise = NULL}, automatically from the
#' distribution of a weighted product of amplitude and stability. The detection
#' contour is then compared against the estimated noise. If \code{input = 'env'},
#' the contour is a smoothed log RMS amplitude envelope. If
#' \code{input = 'spec'} or \code{'mel'}, the contour is computed by comparing
#' the spectrum of each frame with the estimated noise spectrum using
#' \code{specDiffMeasure} and \code{amplWeight}.
#'
#' Syllables are detected as continuous regions of the contour that exceed the
#' noise threshold by approximately \code{SNR} dB. Pauses shorter than
#' \code{shortestPause} are merged. Syllable start and end times correspond to
#' the edges of envelope bins, and \code{pauseLen} is the time between the end
#' of the previous syllable and the start of the current syllable.
#'
#' Bursts are detected as local maxima of the contour above the syllable
#' detection threshold. The minimum spacing/scale for burst detection is
#' controlled by \code{interburst}, and the required peak prominence is
#' controlled by \code{peakToTrough}.
#'
#' @seealso \code{\link{analyze}}  \code{\link{ssm}} \code{\link{segment_ann}}
#'
#' @inheritParams .roxygen_defaults
#' @param shortestSyl minimum acceptable length of syllables, ms
#' @param shortestPause minimum acceptable break between syllables, ms
#'   (syllables separated by shorter pauses are merged)
#' @param input the contour used to search for syllables: 'env' = smoothed RMS
#'   amplitude envelope; 'spec' = power spectrum returned by
#'   \code{tuneR::melfcc}; 'mel' = mel-filterbank amplitude spectrum from
#'   \code{tuneR::melfcc}
#' @param propNoise the proportion of analysis frames assumed to represent
#'   background noise, 0 to 1. If NULL, this proportion is estimated
#'   automatically in each chunk. If 0, correction for background noise is
#'   skipped, in which case \code{SNR} must be specified
#' @param SNR expected signal-to-noise ratio (dB above noise), which determines
#'   the threshold for syllable detection. If NULL, SNR is estimated
#'   automatically in each chunk unless \code{propNoise = 0}. The meaning of
#'   "dB" here is approximate because the detection contour may not be sound
#'   intensity, depending on \code{input}
#' @param specDiffMeasure similarity measure used to compare each spectrum with
#'   the estimated noise spectrum (ignored when \code{input = 'env'}): \describe{
#'   \item{cosine}{cosine distance without centering}
#'   \item{logSpecDist}{root-mean-square log-spectral distance}
#'   \item{specDiff}{mean absolute log-spectral difference}}
#' @param amplWeight amplitude weighting applied to the spectral contrast
#'   (ignored when \code{input = 'env'}): \describe{
#'   \item{add}{combines spectral distinctiveness with amplitude elevation; the
#'   default behavior in soundgen v2.x when \code{specDiffMeasure = 'cosine'}}
#'   \item{gate}{amplitude-gated contrast: spectral contrast contributes mainly
#'   when the frame is also above the estimated noise amplitude}
#'   \item{scale}{spectral contrast scaled to the dynamic range of the amplitude
#'   contour}
#'   \item{none}{unnormalized spectral contrast, mainly useful for diagnostics}}
#' @param noiseLevelStabWeight a vector of length 2 specifying the relative
#'   weights of the overall signal level vs. stability (time derivative) when
#'   attempting to automatically locate the regions that represent noise.
#'   Increasing the weight of stability prioritizes sudden changes as marking
#'   the beginning and end of a syllable
#' @param reverb_pars parameters passed on to \code{\link{reverb}} to attempt to
#'   cancel the effects of reverberation or echo, which otherwise tend to merge
#'   short and loud segments like rapid barks. This is particularly useful if
#'   you have a recording of an impulse response in the same environment to pass
#'   on to \code{\link{reverb}}. The default disables echo by setting
#'   \code{echoLevel = -Inf}. Set to NULL to use a static detection threshold
#'   without reverb compensation
#' @param interburst minimum time between two consecutive bursts, ms. Also
#'   determines the analysis window used for detecting burst peaks. Defaults to
#'   the median detected \code{sylLen + pauseLen} in each chunk, falling
#'   back to \code{shortestSyl} if no syllables are detected
#' @param peakToTrough to qualify as a burst, a local maximum has to be at least
#'   \code{peakToTrough} dB above the surrounding contour. Defaults to
#'   \code{SNR + 3} dB in each chunk when \code{peakToTrough = NULL}
#' @param summaryFun functions used to summarize each acoustic characteristic;
#'   see \code{\link{analyze}}
#' @param maxDur long files are split into chunks \code{maxDur} s in duration to
#'   avoid running out of RAM. The outputs for all fragments are glued together,
#'   but plotting is switched off for chunked files. Note that the noise profile
#'   is estimated in each chunk separately, so set it low if the background
#'   noise is highly variable
#' @param ptvStep,ptvTime,ptvFreq the instantaneous proportion of time
#'   vocalizing (PTV) is calculated by producing a binary (sound on/off) contour
#'   with a step of \code{ptvStep} ms and convolving it with a half-Gaussian
#'   filter with SD = \code{ptvTime} seconds (\code{$ptv_conv}), and by
#'   low-pass filtering it over \code{ptvFreq} Hz (\code{$ptv_lowpass}).
#'   \code{ptvStep} defaults to the same value as \code{step}
#' @param addSilence if syllables are saved as separate audio files, they are
#'   padded with this much silence before and after, ms
#' @param specPlot a list of graphical parameters for displaying the spectrogram
#'   (if \code{input = 'spec' or 'mel'}); set to NULL to hide the spectrogram
#' @param contourPlot a list of graphical parameters for displaying the signal
#'   contour used to detect syllables
#' @param sylPlot a list of graphical parameters for displaying the syllable
#'   threshold
#' @param burstPlot a list of graphical parameters for displaying the bursts
#' @param xlab,ylab,main main plotting parameters
#' @param maxPoints maximum number of points to plot for the waveform and the
#'   segmentation contour. Longer contours are downsampled to avoid slow
#'   plotting
#' @param showLegend if TRUE, shows a legend for thresholds
#' @param saveAudio if TRUE, saves the extracted syllables in a "/segment"
#'   subdirectory created in the input directory (if input is a file or folder)
#'   or in the working directory
#' @param ... other graphical parameters passed to \code{graphics::plot}
#'
#' @return A list with the following components:
#'   \describe{
#'   \item{syllables}{data frame with columns \code{syllable}, \code{start},
#'   \code{end}, \code{pauseLen}, \code{sylLen}, \code{sylRate}, and
#'   \code{ptv}. Times are in ms. \code{start} and \code{end} correspond to
#'   syllable edges; \code{pauseLen} is the time between the end of the previous
#'   syllable and the start of the current syllable. $ptv gives the proportion
#'   of time vocalizing for each syllable-pause pair.}
#'   \item{bursts}{data frame with columns \code{time}, \code{ampl}, and
#'   \code{interburst}. \code{time} and \code{interburst} are in ms}
#'   \item{summary}{data frame summarizing temporal descriptives per file,
#'   including global PTV = sum of all syllable durations / total duration of
#'   audio. NULL if \code{summaryFun = NULL}}
#'   \item{ptv}{contour of instantaneous proportion of time vocalizing, with
#'   columns \code{time} (ms), \code{on} (1 if this frame is part of a syllable,
#'   0 otherwise), \code{ptv_lowpass}, and \code{ptv_conv}. If PTV cannot be
#'   computed (e.g., if there are <2 syllables), this may be NA or 0}
#'   }
#'   If more than one file is analyzed, \code{syllables}, \code{bursts}, and
#'   \code{ptv} are lists with one element per file, while \code{summary} remains
#'   a single data frame.
#'
#' @export
#' @examples
#' sound = soundgen(nSyl = 4, sylLen = 100, pauseLen = 70,
#'                  attackLen = 20, amplGlobal = c(0, -20),
#'                  pitch = c(368, 284), temperature = .001)
#' # add noise so SNR decreases from 20 to 0 dB from syl1 to syl4
#' sound = sound + runif(length(sound), -10 ^ (-20 / 20), 10 ^ (-20 / 20))
#' # osc(sound, samplingRate = 16000, dB = TRUE)
#' # spectrogram(sound, samplingRate = 16000)
#' # playme(sound, samplingRate = 16000)
#'
#' s = segment(sound, samplingRate = 16000, plot = TRUE)
#' str(s)
#'
#' # customizing the plot
#' segment(sound, samplingRate = 16000, plot = TRUE,
#'         sylPlot = list(lty = 2, col = 'gray20'),
#'         burstPlot = list(pch = 16, col = 'blue'),
#'         specPlot = list(col = rev(heat.colors(50))),
#'         xlab = 'Some custom label', cex.lab = 1.2,
#'         showLegend = TRUE,
#'         main = 'My awesome plot')
#'
#' # set SNR manually to control detection threshold
#' s = segment(sound, samplingRate = 16000, SNR = 1, plot = TRUE)
#'
#' # simple intensity threshold (anything >5 dB is signal)
#' segment(sound, 16000, input = 'env', SNR = 5, plot = TRUE,
#'   # less smoothing gives more precise timing
#'   windowLength = 10, step = 5,
#'   # don't correct SNR based on estimated background noise
#'   propNoise = 0,
#'   # don't use dynamic thresholds to cancel reverb
#'   reverb_pars = NULL
#' )
#'
#' # same with automatic threshold setting
#' segment(sound, 16000, input = 'env', plot = TRUE,
#'   windowLength = 10, step = 5, reverb_pars = NULL)
#'
#' \dontrun{
#' # plot the PTV contour (proportion of time vocalizing)
#' plot(s$ptv$time, s$ptv$on, type = 'l', xlab = 'Time, ms',
#'   ylab = 'Prop. time voc.')
#' points(s$ptv$time, s$ptv$ptv_conv, type = 'l', col = 'blue')
#' points(s$ptv$time, s$ptv$ptv_lowpass, type = 'l', col = 'red')
#' s$summary$ptv; mean(s$ptv$ptv_conv); mean(s$ptv$ptv_lowpass) # similar
#'
#' # different ways to calculate instantaneous PTV
#' s2 = segment(sound, 16000, ptvTime = 2, ptvFreq = 5)
#' s3 = segment(sound, 16000, ptvTime = 0.05, ptvFreq = 0.5)
#' plot(s2$ptv$time, s2$ptv$on, type = 'l', xlab = 'Time, ms',
#'   ylab = 'Prop. time voc.')
#' points(s2$ptv$time, s2$ptv$ptv_conv, type = 'l', col = 'blue')
#' points(s2$ptv$time, s2$ptv$ptv_lowpass, type = 'l', col = 'yellow')
#' points(s3$ptv$time, s3$ptv$ptv_conv, type = 'l', col = 'purple')
#' points(s3$ptv$time, s3$ptv$ptv_lowpass, type = 'l', col = 'orange')
#'
#' # segment all files in a folder and save the segments as separate files
#' segment('~/Downloads/temp',
#'   saveAudio = TRUE, savePlots = TRUE)
#' }
segment = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    shortestSyl = 40,
    shortestPause = 40,
    input = c('mel', 'env', 'spec'),
    propNoise = NULL,
    SNR = NULL,
    specDiffMeasure = c('cosine', 'logSpecDist', 'specDiff'),
    amplWeight = c('add', 'gate', 'scale', 'none'),
    noiseLevelStabWeight = c(1, .25),
    windowLength = 40,
    step = NULL,
    overlap = 80,
    reverb_pars = list(reverbDelay = 70, reverbSpread = 130, reverbLevel = -35,
                      reverbDensity = 50, echoLevel = -Inf),
    interburst = NULL,
    peakToTrough = NULL,
    summaryFun = c('median', 'sd'),
    maxDur = 120,
    ptvStep = NULL,
    ptvTime = 0.5,
    ptvFreq = 1,
    reportEvery = NULL,
    cores = 1,
    plot = FALSE,
    savePlots = FALSE,
    embed = FALSE,
    saveAudio = FALSE,
    addSilence = 50,
    main = NULL,
    xlab = '',
    ylab = NULL,
    showLegend = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    maxPoints = c(1e5, 5e5),
    specPlot = list(colorTheme = 'bw'),
    contourPlot = list(lty = 1, lwd = 2, col = 'green'),
    sylPlot = list(lty = 1, lwd = 2, col = 'blue'),
    burstPlot = list(pch = 8, cex = 3, col = 'red'),
    ...
) {
  input = match.arg(input)
  specDiffMeasure = match.arg(specDiffMeasure)
  amplWeight = match.arg(amplWeight)

  ## Check the arguments
  if (windowLength < 10) {
    warning('windowLength < 10 ms is slow and usually not very useful')
  }
  if (is.null(SNR) && isTRUE(propNoise == 0))
    stop(paste('if SNR is null, propNoise cannot be 0;',
               'either set propNoise = NULL or specify SNR'))
  if (is.null(ylab)) ylab = if (input != 'env' && specDiffMeasure == 'cosine' &&
                                amplWeight == 'none') 'Signal' else 'Signal, dB'
  if (!is.null(propNoise)) {
    propNoise = propNoise[1]
    if (is.na(propNoise)) propNoise = NULL
    else if (!is.finite(propNoise) || propNoise < 0 || propNoise > 1)
      stop("propNoise must be NULL or a single number between 0 and 1")
  }
  if (!is.null(SNR)) {
    SNR = SNR[1]
    if (!is.finite(SNR))
      stop("SNR must be NULL or a finite number")
  }
  if (!is.null(interburst)) {
    interburst = interburst[1]
    if (!is.finite(interburst) || interburst <= 0) interburst = NULL
  }
  if (!is.null(peakToTrough)) {
    peakToTrough = peakToTrough[1]
    if (!is.finite(peakToTrough)) peakToTrough = NULL
  }
  if (is.null(maxDur) || !is.finite(maxDur) || maxDur <= 0) {
    maxDur = Inf
  }
  noiseLevelStabWeight = rep_len(noiseLevelStabWeight, 2)
  if (!is.finite(addSilence) || addSilence < 0) addSilence = 0

  ## Prepare a list of arguments to pass to .segment()
  myPars = c(as.list(environment()), list(...))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'reportEvery', 'cores',
    'summaryFun', 'reverb_pars', 'savePlots', 'saveAudio', 'embed')]
  myPars$reverb_pars = if (is.null(reverb_pars)) NA else reverb_pars

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.segment',
    suffix = 'segment',
    savePlots = savePlots,
    saveAudio = saveAudio,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  if (pa$input$n == 0) stop('Failed to analyze any input')

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units),
                  changesAudio = FALSE,
                  # b/c we refer to originals, not extracted syls
                  embed = embed))

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
      if (!pa$input$failed[i]) {
        seg = pa$result[[i]]
        temp[[i]] = try({
          sum_syl = summarizeAnalyze(
            seg$syllables[, c('sylLen', 'pauseLen'), drop = FALSE],
            summaryFun = summaryFun,
            var_noSummary = NULL)
          sum_bursts = summarizeAnalyze(
            seg$bursts[, 'interburst', drop = FALSE],
            summaryFun = summaryFun,
            var_noSummary = NULL)
          res = as.data.frame(c(
            list(nSyl = sum(!is.na(seg$syllables$start)),
                 ptv = seg$ptv),
            sum_syl,
            list(nBursts = sum(!is.na(seg$bursts$time))),
            sum_bursts
          ))
          res[apply(res, c(1, 2), is.nan)] = NA
          res
        }, silent = TRUE)
        if (inherits(temp[[i]], 'try-error')) temp[[i]] = NULL
      }
    }

    # Identify failed files (either processAudio failed or summarizeAnalyze failed/returned NULL)
    idx_failed = which(pa$input$failed)
    for (i in seq_len(pa$input$n)) {
      if (is.null(temp[[i]])) idx_failed = c(idx_failed, i)
    }
    idx_failed = unique(idx_failed)
    idx_ok = which(!seq_len(pa$input$n) %in% idx_failed)

    if (length(idx_failed) > 0) {
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]][1, , drop = FALSE]
        filler[1, ] = NA
      } else {
        stop('Failed to analyze any input')
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = try(cbind(data.frame(file = pa$input$filenames_base),
                          rbind_fill_list(temp)))
    if (inherits(mysum_all, 'try-error')) mysum_all = NULL
  } else {
    mysum_all = NULL
  }

  if (pa$input$n == 1) {
    if (pa$input$failed[1]) {
      syllables = NA
      bursts = NA
      ptv = NA
    } else {
      syllables = pa$result[[1]]$syllables
      bursts = pa$result[[1]]$bursts
      ptv = pa$result[[1]]$ptv_cont
    }
  } else {
    syllables = vector('list', pa$input$n)
    bursts = vector('list', pa$input$n)
    ptv = vector('list', pa$input$n)
    names(syllables) = names(bursts) = names(ptv) = pa$input$filenames_base

    for (i in seq_len(pa$input$n)) {
      if (pa$input$failed[i]) {
        syllables[[i]] = NA
        bursts[[i]] = NA
        ptv[[i]] = NA
      } else {
        s = pa$result[[i]]$syllables
        b = pa$result[[i]]$bursts
        p = pa$result[[i]]$ptv_cont
        # Protect against NULL extraction, which would drop the list element
        syllables[[i]] = if (is.null(s)) NA else s
        bursts[[i]] = if (is.null(b)) NA else b
        ptv[[i]] = if (is.null(p)) NA else p
      }
    }
  }

  output = list(
    syllables = syllables,
    bursts = bursts,
    summary = mysum_all,
    ptv = ptv
  )
  invisible(output)
}


#' Segment per sound
#' @noRd
.segment = function(
    audio,
    shortestSyl = 40,
    shortestPause = 40,
    input = 'mel',
    propNoise = NULL,
    SNR = NULL,
    specDiffMeasure = 'cosine',
    amplWeight = 'add',
    noiseLevelStabWeight = c(1, .25),
    windowLength = 40,
    step = windowLength / 5,
    overlap = 80,
    reverb_pars = list(reverbDelay = 70, reverbSpread = 130, reverbLevel = -35,
                      reverbDensity = 50, echoLevel = -Inf),
    interburst = NULL,
    peakToTrough = NULL,
    maxDur = 120,
    ptvStep = step,
    ptvTime = 0.5,
    ptvFreq = 1,
    addSilence = 50,
    plot = FALSE,
    plotname = '',
    main = NULL,
    xlab = '',
    ylab = NULL,
    showLegend = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    maxPoints = c(1e5, 5e5),
    specPlot = NULL,
    contourPlot = list(lty = 1, lwd = 2, col = 'green'),
    sylPlot = list(lty = 1, lwd = 2, col = 'blue'),
    burstPlot = list(pch = 8, cex = 3, col = 'red'),
    ...) {
  if (audio$failed) stop('failed to open the input')
  if (any(!is.finite(audio$sound))) stop('non-finite values in input')
  audio$sound = audio$sound - mean(audio$sound)  # center around 0
  if (!any(audio$sound != 0)) stop('silent or constant input: nothing to do')
  ms = max(abs(audio$sound))

  val = validateWlOvlp(audio, windowLength, step, overlap)
  wl = val$wl; step = val$step; step_points = val$step_points
  dur_total_ms = audio$duration * 1000
  if (is.null(ptvStep)) ptvStep = step

  # normalize
  audio$sound = audio$sound / max(abs(audio$sound))  # range approx. -1 to 1
  audio$timeShift_ms = audio$timeShift * 1000

  analyze_from = 0
  analyze_to = min(maxDur * 1000, dur_total_ms)
  stopNextTime = analyze_to >= dur_total_ms
  syllables = bursts = NULL
  if (isTRUE(audio$savePlots)) plot = TRUE

  canPlot = TRUE
  while (analyze_to <= dur_total_ms) {
    if (plot && analyze_to < dur_total_ms) {
      plot = FALSE
      canPlot = FALSE
      message(paste(
        'This long sound will be analyzed piecewise and cannot be plotted.',
        'Increase maxDur or use from/to to analyze and plot a part.'))
    }
    from_points = max(1, round(analyze_from * audio$samplingRate / 1000))
    to_points = min(audio$ls, round(analyze_to * audio$samplingRate / 1000))
    sound_part = audio$sound[from_points:to_points]
    if (!is.null(peakToTrough)) {
      peakToTrough_chunk = peakToTrough
    } else {
      peakToTrough_chunk = SNR + 3
    }

    if (input == 'env') {
      ## work with smoothed amplitude envelope
      ampl = getEnv(sound_part,
                    method = 'rms',
                    wl = wl,
                    step = step_points,
                    upsample = FALSE)
      ampl = 20 * log10(ampl + 1e-6)  # + 1e-6 in case of complete silence giving log(0)
      ampl = ampl - min(ampl)
      nc = length(ampl)
      # plot(ampl, type = 'l')

      # attempt to estimate propNoise from data
      d_ampl = c(0, abs(diff(ampl)))
      # ampl_dampl = ampl * d_ampl  # plot(ampl_dampl, type = 'l')
      # weighted contributions from ampl (overall level) and d_ampl (level stability)
      ampl_dampl = exp(log(ampl + 1e-6) * noiseLevelStabWeight[1] +
                         log(d_ampl + 1e-6) * noiseLevelStabWeight[2])
      nonZero_ampl_dampl = which(ampl_dampl > 0)
      col_noise = integer(0)
      if (is.null(propNoise) || is.na(propNoise)) {
        if (length(nonZero_ampl_dampl) < 2) {
          propNoise_chunk = 0.1
          thres_difNoise = as.numeric(quantile(ampl, probs = propNoise_chunk))
          col_noise = seq_len(min(nc, max(1L, floor(nc * propNoise_chunk))))
        } else {
          dens_ampl_dampl = density(ampl_dampl[nonZero_ampl_dampl])
          idx = findPeaks(dens_ampl_dampl$y, wl = 3)
          if (length(idx) > 0) {
            thres_noise = dens_ampl_dampl$x[idx[1]]
            col_noise = which(ampl_dampl <= thres_noise)
            propNoise_chunk = max(.01, round(length(col_noise) / nc, 3))
          } else {
            propNoise_chunk = 0.1
            thres_noise = as.numeric(
              quantile(ampl_dampl[nonZero_ampl_dampl], probs = propNoise_chunk)
            )
            col_noise = which(ampl_dampl <= thres_noise)
          }
          message(paste0('propNoise set to ', propNoise_chunk, '; reset manually if needed'))
          thres_difNoise = as.numeric(
            quantile(ampl[nonZero_ampl_dampl], probs = propNoise_chunk)
          )
        }
      } else if (propNoise == 0) {
        # ignore noise
        propNoise_chunk = 0
        thres_difNoise = 0
        thres_noise = 0
        col_noise = integer(0)
      } else {
        # use the global user-supplied propNoise in all chunks
        propNoise_chunk = propNoise
        col_noise = which(ampl_dampl <= quantile(ampl_dampl[nonZero_ampl_dampl],
                                                 probs = propNoise))
        thres_noise = as.numeric(median(ampl[col_noise]))
        thres_difNoise = as.numeric(quantile(ampl[nonZero_ampl_dampl], probs = propNoise))
      }

      # attempt to estimate SNR from data
      if (is.null(SNR) && propNoise_chunk > 0) {
        nonNoise = if (length(col_noise) < length(ampl)) ampl[-col_noise] else numeric(0)
        SNR_chunk = if (length(nonNoise)) {
          round((median(nonNoise) - thres_difNoise) / 2, 1)
        } else {
          3
        }
        message(paste0('SNR set to ', SNR_chunk, '; reset manually if needed'))
      } else {
        SNR_chunk = SNR
      }
      if (is.null(SNR_chunk) || !is.finite(SNR_chunk)) SNR_chunk = 3

      # adaptive thresholds may help to control for reverb
      if (!is.null(reverb_pars) &&
          (length(reverb_pars) > 0 && is.list(reverb_pars))) {
        # dynamic threshold
        rvb_list = do.call('.reverb', c(
          list(audio = list(
            sound = ampl,
            samplingRate = 1000 / step,
            ls = nc,
            scale_used = 1), # scale not really needed
            output = 'detailed',
            seed = 137  # set.seed() for reproducibility
          ),
          reverb_pars
        ))
        rvb = rvb_list$rvb[1:nc]
        threshold = thres_difNoise + SNR_chunk + rvb
        # plot(difNoise, type = 'l')
        # abline(h = thres_difNoise + SNR_chunk, lty = 2)
        # points(threshold, type = 'l', col = 'blue')
      } else {
        # static threshold
        threshold = rep(thres_difNoise + SNR_chunk, nc)
      }

      # find syllables
      syllables_part = findSyllables(
        ampl = ampl,
        threshold = threshold,
        shortestSyl = shortestSyl,
        shortestPause = shortestPause,
        step = step,
        windowLength = windowLength
      )
    } else if (input %in% c('mel', 'spec')) {
      ## work with some form of spectrogram
      myspec = tuneR::melfcc(
        tuneR::Wave(sound_part, samp.rate = audio$samplingRate, bit = 16),
        wintime = windowLength / 1000,
        hoptime = step / 1000,
        lifterexp = 0,
        preemph = 0,
        numcep = 1,
        spec_out = TRUE
      )
      if (input == 'mel') {
        sp = t(myspec$aspectrum)
      } else if (input == 'spec') {
        sp = t(myspec$pspectrum)
      }
      sp = 20 * log10(sp / max(max(sp), 1e-6))  # in case of pure silence --> log(0)
      nc = ncol(sp)
      cs = colMeans(sp)
      cs = cs - min(cs)  # make non-negative for ease of further processing
      # image(t(sp))

      # estimate the spectrum of background noise
      d_cs = c(0, abs(diff(cs)))
      # weighted contributions from cs (overall level) and d_cs (level stability)
      cs_dcs = exp(log(cs + 1e-6) * noiseLevelStabWeight[1] +
                     log(d_cs + 1e-6) * noiseLevelStabWeight[2])
      nonZero_cs_dcs = which(cs_dcs > 0)
      if (is.null(propNoise) || is.na(propNoise)) {
        # estimate propNoise_chunk
        dens_cs_dcs = density(cs_dcs[nonZero_cs_dcs])
        hb = 1
        idx = which(vapply(
          (hb + 1):(length(dens_cs_dcs$y) - hb), function(x) {
            frx = dens_cs_dcs$y[(x - hb):(x + hb)]
            dens_cs_dcs$y[x] == max(frx)
          }, logical(1))) + hb
        if (length(idx) > 0) {
          thres_noise = max(min(cs_dcs), dens_cs_dcs$x[idx[1]])
        } else {
          thres_noise = min(cs_dcs)
        }
        col_noise = which(cs_dcs <= thres_noise)
        propNoise_chunk = max(.01, round(length(col_noise) / nc, 3))
        message(paste0('propNoise set to ', propNoise_chunk, '; reset manually if needed'))
      } else if (propNoise == 0) {
        # assume the quietest possible noise (0th quantile)
        propNoise_chunk = 0
        col_noise = numeric(0)
        thres_noise = min(cs)
      } else {
        # use the global user-supplied propNoise in all chunks
        propNoise_chunk = propNoise
        col_noise = which(cs_dcs <= quantile(cs_dcs[nonZero_cs_dcs], probs = propNoise_chunk))
        if (length(col_noise) == 0) {
          thres_noise = min(cs)
        } else {
          thres_noise = as.numeric(median(cs[col_noise]))
        }
      }

      if (length(col_noise) > 0) {
        noise = rowMeans(sp[, col_noise, drop = FALSE])
      } else {
        # assume white noise
        noise = rep(1, nrow(sp))
      }
      # plot(noise, type = 'l')  # the spectrum of background noise, presumably

      # compare the spectrum of each STFT frame to the spectrum of noise
      difNoise = .spectralNoiseContrast(
        sp = sp,
        noise = noise,
        cs = cs,
        thres_noise = thres_noise,
        col_noise = col_noise,
        propNoise = propNoise,
        specDiffMeasure = specDiffMeasure,
        amplWeight = amplWeight
      )
      # plot(difNoise, type = 'l')
      thres_difNoise = as.numeric(quantile(difNoise, probs = propNoise_chunk))
      # plot(difNoise, type = 'l')
      # abline(h = thres_difNoise + SNR, lty = 3, col = 'blue')

      # attempt to estimate SNR from data
      if (is.null(SNR)) {
        SNR_chunk = round((median(difNoise[-col_noise]) - thres_difNoise) / 2, 3)
        message(paste0('SNR set to ', SNR_chunk, '; reset manually if needed'))
      } else {
        SNR_chunk = SNR
      }

      # adaptive thresholds may help to control for reverb
      if (length(reverb_pars) > 0 && is.list(reverb_pars)) {
        # dynamic thresholdf
        rvb_list = do.call('.reverb', c(
          list(audio = list(
            sound = cs,
            samplingRate = 1000 / step,
            scale_used = 1,
            ls = nc),
            output = 'detailed',
            seed = 137),
          reverb_pars
        ))
        rvb = rvb_list$rvb[1:nc]
        threshold = thres_difNoise + SNR_chunk + rvb
      } else {
        # static threshold
        threshold = rep(thres_difNoise + SNR_chunk, nc) # * 10 ^ (SNR / 20)
      }

      # find syllables
      ampl = difNoise
      syllables_part = findSyllables(
        ampl = ampl,
        threshold = threshold,
        shortestSyl = shortestSyl,
        shortestPause = shortestPause,
        step = step,
        windowLength = windowLength
      )
    }
    ## find bursts and get descriptives
    # calculate the window for analyzing bursts based on syllables
    # (if no syllables are detected, just use shortestSyl)
    if (is.null(interburst)) {
      median_scaled = median(syllables_part$sylLen, na.rm = TRUE)
      if (any(!is.na(syllables_part$pauseLen))) {
        median_scaled = median_scaled + median(syllables_part$pauseLen, na.rm = TRUE)
      }
      interburst_chunk = if (!is.na(median_scaled) && length(median_scaled) > 0)
        median_scaled else shortestSyl
    } else {
      interburst_chunk = interburst
    }
    if (is.null(peakToTrough)) {
      peakToTrough_chunk = SNR_chunk + 3
    } else {
      peakToTrough_chunk = peakToTrough
    }

    idx_burst = findPeaks(
      ampl - threshold,
      wl = floor(interburst_chunk / step / 4) * 2 + 1,
      thres = 0, # ampl > threshold
      thresRel = peakToTrough_chunk  # in dB
    )
    if (length(idx_burst) > 0) {
      bursts_part = data.frame(
        time = (idx_burst - 1) * step + windowLength / 2,
        ampl = ampl[idx_burst])
      bursts_part$interburst = c(NA, diff(bursts_part$time))
    } else {
      bursts_part = data.frame(time = NA, ampl = NA, interburst = NA)
    }

    # update times from within-chunk to global
    syllables_part[, c('start', 'end')] = syllables_part[, c('start', 'end')] +
      analyze_from
    bursts_part$time = bursts_part$time + analyze_from

    # start next part just before the last detected syllable if it looks like
    # this syllable might be incomplete (ends within 100 ms of the end of sound_part)
    if (!is.na(syllables_part$start[1]) &&
        analyze_to < dur_total_ms &&
        (analyze_to - (syllables_part$end[nrow(syllables_part)]) < 100)) {
      analyze_from = max(analyze_from, syllables_part$start[nrow(syllables_part)] - shortestPause)
      # remove syllables and bursts from the overlapping part
      syllables_part = syllables_part[-nrow(syllables_part), ]
      bursts_part = bursts_part[bursts_part$time < analyze_from, ]
    } else {
      analyze_from = analyze_to
    }
    analyze_to = min(analyze_from + maxDur * 1000, dur_total_ms)
    if (analyze_to >= dur_total_ms) stopNextTime = TRUE

    # add to growing syllables/bursts for the entire file
    syllables = rbind(syllables, syllables_part)
    bursts = rbind(bursts, bursts_part)
    if (stopNextTime) break
  }
  # end of WHILE loop for processing long sounds

  # in case of long files, interbursts for the first burst in a new analyzed
  # fragment become NAs - recalculate
  syllables = na.omit(syllables)
  nrs = nrow(syllables)

  if (nrs > 0) {
    # update pauseLen (in case of multiple chunks with timing issues)
    if (nrs > 1) {
      syllables$pauseLen = c(syllables$start[1], syllables$start[-1] - syllables$end[-nrs])
      syllables$syllable = 1:nrow(syllables)
    } else {
      # a single syllable
      syllables$pauseLen = syllables$start
    }

    # calculate proportion of time vocalizing
    ptv = sum(syllables$sylLen) / 1000 / audio$duration

    ## calculate ptv for each syllable
    ptv_syl = getPTV(syllables,
                     duration = audio$duration,
                     ptvStep = ptvStep,
                     ptvTime = ptvTime,
                     ptvFreq = ptvFreq)
    syllables = ptv_syl$syllables
    ptv_cont = ptv_syl$ptv_cont
  } else {
    syllables = data.frame(syllable = NA, start = NA, end = NA,
                           pauseLen = NA, sylLen = NA,
                           sylRate = NA, ptv = NA)
    ptv = ptv_cont = 0
  }

  bursts = bursts[which(!is.na(bursts$time)), ]
  if (nrow(bursts) < 1) {
    bursts = data.frame(time = NA, ampl = NA, interburst = NA)
  } else if (nrow(bursts) > 1) {
    for (i in 2:nrow(bursts)) {
      if (is.na(bursts$interburst[i]))
        bursts$interburst[i] = bursts$time[i] - bursts$time[i - 1]
    }
  }

  ## plotting
  if (isTRUE(audio$savePlots) && canPlot) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }

  ## save all extracted syllables as separate audio files for easy examination
  if (isTRUE(audio$saveAudio) && !is.na(syllables$sylLen[1])) {
    addSil = rep(0, round(addSilence * audio$samplingRate / 1000))
    sound_out = audio$sound * audio$scale_used  # back to original scale from [-1, 1]
    for (i in 1:nrow(syllables)) {
      from = max(1, round(audio$samplingRate * ((syllables$start[i]) / 1000)))
      #  - windowLength / 2
      to = min(audio$ls, round(audio$samplingRate * ((syllables$end[i]) / 1000)))
      temp = c(addSil, sound_out[from:to], addSil)
      times_shifted = round(syllables[i, c('start', 'end')] + audio$timeShift_ms)
      filename_i = file.path( audio$path_output, paste0(
        audio$filename_base, '_', times_shifted[1], '-', times_shifted[2], '.wav'))
      writeAudio(temp, audio = audio, filename = filename_i,
                 scale_used = max(abs(temp)))
    }
  }

  ## back to absolute time
  if (!is.null(audio$timeShift) && audio$timeShift > 0) {
    syllables$start = syllables$start + audio$timeShift_ms
    syllables$end = syllables$end + audio$timeShift_ms
    bursts$time = bursts$time + audio$timeShift_ms
  }

  if (plot) {
    # defaults
    if (is.null(sylPlot$lty)) sylPlot$lty = 1
    if (is.null(sylPlot$lwd)) sylPlot$lwd = 2
    if (is.null(sylPlot$col)) sylPlot$col = 'blue'
    if (is.null(burstPlot$pch)) burstPlot$pch = 8
    if (is.null(burstPlot$cex)) burstPlot$cex = 3
    if (is.null(burstPlot$col)) burstPlot$col = 'red'

    if (is.null(main)) {
      if (audio$filename_base == 'sound') {
        main = ''
      } else {
        main = audio$filename_base
      }
    }

    if (!is.null(maxPoints)) {
      if (length(maxPoints) == 1) maxPoints = c(maxPoints, maxPoints)
    }

    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    on.exit(par(op), add = TRUE)
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(2, 1))
    par(mar = c(op$mar[1:2], 0, op$mar[4]), xaxt = 's', yaxt = 's')
    xlim = c(0, audio$ls / audio$samplingRate) + audio$timeShift

    # downsample long sounds to avoid delays when plotting
    if (audio$ls > maxPoints[1]) {
      idx_sound = seq(1, audio$ls, by = ceiling(audio$ls / maxPoints[1]))
    } else {
      idx_sound = 1:audio$ls
    }

    # plot osc
    plot(x = idx_sound / audio$samplingRate + audio$timeShift,
         y = audio$sound[idx_sound], type = 'l', xlim = xlim,
         axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
         xlab = xlab, ylab = '', main = '', ...)
    box()
    time_location = axTicks(1)
    time_labels = convert_sec_to_hms(time_location, 3)
    axis(side = 1, at = time_location, labels = time_labels, ...)
    abline(h = 0, lty = 2)
    par(mar = c(0, op$mar[2:4]), xaxt = 'n', yaxt = 's')


    # downsample long sounds to avoid delays when plotting
    if (audio$ls > maxPoints[1]) {
      idx_sound = seq(1, audio$ls, by = ceiling(audio$ls / maxPoints[1]))
    } else {
      idx_sound = 1:audio$ls
    }

    # plot envelope - downsample long envelopes
    envelope = data.frame(
      time = (seq_along(ampl) - 1) * step + windowLength / 2 + audio$timeShift_ms,
      value = ampl
    )
    if (length(envelope$time) > maxPoints[1]) {
      idx_env = seq(1, length(envelope$time), length.out = maxPoints[1])
    } else {
      idx_env = seq_along(envelope$time)
    }
    if (input %in% c('spec', 'mel') && !is.null(specPlot)) {
      pal = if (!is.null(specPlot$color.palette)) {
        specPlot$color.palette
      } else if (!is.null(specPlot$colorTheme)) {
        specPlot$colorTheme
      } else {
        'bw'
      }
      specPlot$color.palette = switchColorTheme(pal)
      specPlot$colorTheme = NULL  # to avoid warnings
      do.call('filled.contour.mod', c(list(
        x = seq(envelope$time[1], tail(envelope$time, 1), length.out = ncol(sp)),
        y = seq(min(envelope$value), max(envelope$value) * 1.05, length.out = nrow(sp)),
        z = t(zeroOne(sp)),
        levels = seq(0, 1, length = 30),
        xaxs = "i", xlab = '',
        ylab = ylab, main = main,
        maxPoints = maxPoints[2],
        y_Hz = FALSE,
        ...),
        specPlot))
      do.call('points', c(list(x = envelope$time,
                               y = envelope$value,
                               type = 'l'),
                          contourPlot))
    } else {
      do.call('plot', c(list(x = envelope$time[idx_env],
                             y = envelope$value[idx_env],
                             type = 'l',
                             xaxs = "i", xlab = '',
                             ylab = ylab, main = main, ...),
                        contourPlot))
    }
    abline(h = thres_difNoise, lty = 3, col = 'black')
    # plot bursts
    do.call('points', c(list(x = bursts$time, y = bursts$ampl),
                        burstPlot))
    if (length(threshold) == 1) threshold = rep(threshold, nc)
    if (any(!is.na(syllables$start))) {
      thres_contour = rep(NA, nc)
      for (i in 1:nrow(syllables)) {
        idx_i = syllables$start_idx[i]:syllables$end_idx[i]
        thres_contour[idx_i] = threshold[idx_i]
      }
      do.call('points', c(list(x = envelope$time[idx_env],
                               y = thres_contour[idx_env],
                               type = 'l'),
                          sylPlot))
    }

    if (showLegend)
      legend('topright',
             legend = c('Syllable threshold', 'Noise threshold'),
             lty = c(1, 3), col = c(sylPlot$col, 'gray50'))
  }

  on.exit(gc(), add = TRUE)
  list(
    syllables = syllables[, c('syllable', 'start', 'end', 'pauseLen',
                              'sylLen', 'sylRate', 'ptv')],
    bursts = bursts,
    ptv = ptv, ptv_cont = ptv_cont
  )
}
