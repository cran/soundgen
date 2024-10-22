## FIND SOUNDGEN SETTINGS TO REPRODUCE AN EXISTING SOUND ##

#' Match soundgen pars (experimental)
#'
#' Attempts to find settings for \code{\link{soundgen}} that will reproduce an
#' existing sound. The principle is to mutate control parameters, trying to
#' improve fit to target. The currently implemented optimization algorithm is
#' simple hill climbing. Disclaimer: this function is experimental and may or
#' may not work for particular tasks. It is intended as a supplement to - not
#' replacement of - manual optimization. See vignette('sound_generation',
#' package = 'soundgen') for more information.
#'
#' @return Returns a list of length 2: \code{$history} contains the tried
#'   parameter values together with their fit to target (\code{$history$sim}),
#'   and \code{$pars} contains a list of the final - hopefully the best -
#'   parameter settings.
#' @param target the sound we want to reproduce using soundgen: path to a .wav
#'   file or numeric vector
#' @param samplingRate sampling rate of \code{target} (only needed if target is
#'   a numeric vector, rather than a .wav file)
#' @param pars arguments to \code{\link{soundgen}} that we are attempting to
#'   optimize
#' @param init a list of initial values for the optimized parameters \code{pars}
#'   and the values of other arguments to soundgen that are fixed at non-default
#'   values (if any)
#' @param probMutation the probability of a parameter mutating per iteration
#' @param stepVariance scale factor for calculating the size of mutations
#' @param maxIter maximum number of mutated sounds produced without improving
#'   the fit to target
#' @param minExpectedDelta minimum improvement in fit to target required to
#'   accept the new sound candidate
#' @param compareSoundsPars a list of control parameters passed to
#'   \code{\link{compareSounds}}
#' @param verbose if TRUE, plays back the accepted candidate at each iteration
#'   and reports the outcome
#' @export
#' @examples
#' \dontrun{
#' target = soundgen(sylLen = 600, pitch = c(300, 200),
#'                   rolloff = -15, play = TRUE, plot = TRUE)
#' # we hope to reproduce this sound
#'
#' # Match pars based on acoustic analysis alone, without any optimization.
#' # This *MAY* match temporal structure, pitch, and stationary formants
#' m1 = matchPars(target = target,
#'                samplingRate = 16000,
#'                maxIter = 0,  # no optimization, only acoustic analysis
#'                verbose = TRUE)
#' cand1 = do.call(soundgen, c(m1$pars, list(
#'   temperature = 0.001, play = TRUE, plot = TRUE)))
#'
#' # Try to improve the match by optimizing rolloff
#' # (this may take a few minutes to run, and the results may vary)
#' m2 = matchPars(target = target,
#'                samplingRate = 16000,
#'                pars = 'rolloff',
#'                maxIter = 100,
#'                verbose = TRUE)
#' # rolloff should be moving from default (-9) to target (-15):
#' sapply(m2$history, function(x) x$pars$rolloff)
#' cand2 = do.call(soundgen, c(m2$pars, list(play = TRUE, plot = TRUE)))
#' }
matchPars = function(target,
                     samplingRate = NULL,
                     pars = NULL,
                     init = NULL,
                     probMutation = .25,
                     stepVariance = 0.1,
                     maxIter = 50,
                     minExpectedDelta = 0.001,
                     compareSoundsPars = list(),
                     verbose = TRUE) {
  parsToRound = c('repeatBout', 'nSyl', 'rolloffParabHarm')
  tgt_audio = readAudio(target, input = checkInputType(target), samplingRate = samplingRate)
  pars_melSpec = c('windowLength', 'overlap', 'step', 'dynamicRange', 'maxFreq')
  passPars = which(names(compareSoundsPars) %in% pars_melSpec)
  if (length(passPars) > 0) {
    passPars_list = compareSoundsPars[[passPars]]
  } else {
    passPars_list = list()
  }
  spec1 = do.call(getMelSpec, c(list(
    tgt_audio$sound,
    samplingRate = tgt_audio$samplingRate,
    specPars = compareSoundsPars$specPars,
    plot = FALSE),
    passPars_list
  ))

  ## initialize
  # start with default par values, unless initial values are provided
  parDefault = defaults[pars]
  parDefault[names(parDefault) %in% names(init)] = init[names(parDefault)]
  parDefault[['samplingRate']] = samplingRate

  # analyse the target and update the default pars
  aa = analyze(target, samplingRate = samplingRate, plot = FALSE,
               nFormants = 0, roughness = NULL, novelty = NULL, loudness = NULL)
  af = phonTools::findformants(tgt_audio$sound,
                               fs = tgt_audio$samplingRate, verify = FALSE)
  if (FALSE) {
    # syllable analysis
    as = suppressMessages(segment(target,
                                  samplingRate = samplingRate, plot = FALSE))
    parDefault$nSyl = max(as$summary$nSyl, 1)
    parDefault$sylLen = as$summary$sylLen_median
    medianPause = as$summary$pauseLen_median
  } else {
    # just a single pitch contour with NAs
    parDefault$sylLen = aa$summary$duration_noSilence * 1000
  }
  parDefault$pitch = aa$detailed$pitch
  # downsample F0 measures to 5 Hz
  # parDefault$pitch = resample(p, samplingRate_new = 5, samplingRate = 1 / 20 * 1000)
  if (is.list(af)) {
    if (nrow(af) > 0) {
      for (f in 1:min(3, nrow(af))) {  # add max 3 formants
        parDefault$formants[[paste0('f', f)]] = list(
          freq = round(af$formant[f]),
          width = round(af$bandwidth[f])
        )
      }
    }
  }

  # replace defaults with user-provided values, if any
  if (is.list(init)) {
    for (i in seq_along(init)) {
      if (!names(init)[i] %in% names(defaults)) {
        stop(paste('init parameter not recognized:', init[i]))
      }
      parDefault[[names(init)[i]]] = init[[i]]
    }
  }

  # calculate method of initial par values
  output = list(list(pars = parDefault, sim = NA))
  parLoop = parDefault
  cand = try(do.call(soundgen, parLoop), silent = FALSE)
  if (inherits(cand, 'try-error')) {
    stop ('Invalid initial pars')
  }
  output[[1]]$sim = mean(do.call(compareSounds, c(list(
    x = spec1,
    y = cand,
    samplingRate = samplingRate
  ), compareSoundsPars))$sim)

  # iteratively mutate pars and save par values that improve fit to target ('sim')
  i = 1
  while (i < maxIter) {
    if (!is.numeric(length(pars)) | length(pars) < 1) {
      stop(paste("No parameters for optimization are specified!",
                 "Either list them in 'pars' or set 'maxIter = 0'"))
    }
    # mutate pars
    parMut = parLoop
    parMut = wigglePars(parMut,
                        parsToWiggle = pars,
                        probMutation = probMutation,
                        stepVariance = stepVariance)
    # generate a sound based on mutated pars
    cand = try(do.call(soundgen, parMut), silent = FALSE)
    if (inherits(cand, 'try-error')) {
      sim_new = -Inf
      delta = -Inf
      warning(paste('soundgen crashed with settings',
                    paste(parMut, collapse = ', ')))
    } else {
      # compare to target
      sim_new = mean(do.call(compareSounds, c(list(
        x = spec1,
        y = cand,
        samplingRate = samplingRate
      ), compareSoundsPars))$sim)
      delta = sim_new - output[[length(output)]]$sim  # want to maximize similarity
    }

    condition = (delta > minExpectedDelta)
    if (condition) {
      i = 1  # reset the count of iterations
      output = c(output, list(list(pars = parMut,
                                   sim = sim_new)))
      parLoop = parMut
      if (verbose) {
        print(paste('Best similarity: ', round(output[[length(output)]]$sim, 4)))
        playme(cand, samplingRate)
      }
    } else {
      parMut = parLoop # back to previous step
    }
    i = i + 1
  }

  if (verbose) {
    if (length(output) == 1) {
      print ('Failed to improve fit to target! Try increasing maxIter.')
    } else {
      print ('Improved fit to target')
    }
  }

  list(history = output, pars = output[[length(output)]]$pars)
}
