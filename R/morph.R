#' Morph sounds
#'
#' Takes two formulas for synthesizing two target sounds with
#' \code{\link{soundgen}} and produces a number of intermediate forms (morphs),
#' attempting to go from one target sound to the other in a specified number of
#' equal steps. Normally you will want to set \code{temperature} very low; the
#' \code{tempEffects} argument is not supported. Undefined values are set to
#' reasonably intelligent defaults, but it is safer to specify the same
#' parameters in both formulas. For instance, if formula1 includes "noise" and
#' "mouth", set these parameters explicitly in formula2 as well.
#'
#' @seealso \code{\link{soundgen}}
#'
#' @param formula1,formula2 lists of parameters for calling
#'   \code{\link{soundgen}} that produce the two target sounds between which
#'   morphing will occur. Character strings containing the full call to soundgen
#'   are also accepted (see examples)
#' @param nMorphs the number of morphs to produce, including target sounds
#' @param play if TRUE or a character referring to a valid player, the morphs
#'   will be played as they are produced
#' @param savePath if it is the path to an existing directory, morphs will be
#'   saved there as individual .wav files (defaults to NA)
#' @param samplingRate sampling rate of output, Hz. NB: overrides the values in
#'   \code{formula1} and \code{formula2}
#' @return A list containing two lists, (\code{$formulas} and \code{$sounds}),
#'   each of length \code{nMorphs}. For ex., the formula for the second hybrid
#'   is \code{m$formulas[[2]]}, and the waveform is \code{m$sounds[[2]]}
#' @export
#' @examples
#' \dontrun{
#' # write two formulas or copy-paste them from soundgen_app() or presets:
#' playback = c(TRUE, FALSE)[1]
#' # [a] to barking
#' m = morph(formula1 = list(repeatBout = 2),
#'           # equivalently: formula1 = 'soundgen(repeatBout = 2)',
#'           formula2 = presets$Misc$Dog_bark,
#'           nMorphs = 5, play = playback)
#'  # use $formulas to access formulas for each morph, $sounds for waveforms
#'  # m$formulas[[4]]
#'  # playme(m$sounds[[3]])
#'
#' # morph intonation and vowel quality
#' m = morph(
#'   'soundgen(pitch = c(300, 250, 400),
#'             formants = c(350, 2900, 3600, 4700))',
#'   'soundgen(pitch = c(300, 700, 500, 300),
#'             formants = c(800, 1250, 3100, 4500))',
#'   nMorphs = 5, play = playback
#' )
#'
#' # from a grunt of disgust to a moan of pleasure
#' m = morph(
#'   formula1 = 'soundgen(sylLen = 180, pitch = c(160, 160, 120), rolloff = -12,
#'     nonlinBalance = 70, subDep = 15, jitterDep = 2,
#'     formants = c(550, 1200, 2100, 4300, 4700, 6500, 7300),
#'     noise = data.frame(time = c(0, 180, 270), value = c(-20, -20, -30)),
#'     rolloffNoise = 0)',
#'   formula2 = 'soundgen(sylLen = 320, pitch = c(340, 330, 300),
#'     rolloff = c(-18, -16, -30), ampl = c(0, -10), formants = c(950, 1700, 3700),
#'     noise = data.frame(time = c(0, 300, 440), value = c(-25, -15, -35)),
#'     mouth = c(.4, .5), rolloffNoise = -5, attackLen = 30)',
#'   nMorphs = 8, play = playback
#' )
#'
#' # from scream_010 to moan_515b
#' # (see online demos at http://cogsci.se/soundgen/humans/humans.html)
#' m = morph(
#'   formula1 = "soundgen(
#'     sylLen = 490,
#'     pitch = list(time = c(0, 80, 250, 370, 490),
#'     value = c(1000, 2900, 3200, 2900, 1000)),
#'     rolloff = c(-5, 0, -25), rolloffKHz = 0,
#'     temperature = 0.001,
#'     jitterDep = c(.5, 1, 0), shimmerDep = c(5, 15, 0),
#'     formants = c(1100, 2300, 3100, 4000, 5300, 6200),
#'     mouth = c(.3, .5, .6, .5, .3))",
#'   formula2 = "soundgen(sylLen = 520,
#'     pitch = c(300, 310, 300),
#'     ampl = c(0, -30),
#'     temperature = 0.001, rolloff = c(-18, -25),
#'     jitterDep = .05, shimmerDep = 2,
#'     formants = list(f1 = c(700, 900),
#'       f2 = c(1600, 1400),
#'       f3 = c(3600, 3500), f4 = c(4300, 4200)),
#'     mouth = c(.5, .3),
#'     noise = data.frame(time = c(0, 400, 660),
#'                        value = c(-40, -20, -70)),
#'     rolloffNoise = c(-5, -15))",
#'  nMorphs = 5, play = playback
#' )
#' }
morph = function(formula1,
                 formula2,
                 nMorphs,
                 play = FALSE,
                 savePath = NA,
                 samplingRate = 16000) {
  # which pars are different from the defaults of soundgen()?
  arg_sng = as.list(args(soundgen))
  # exclude booleans / non-morphable args
  arg_sng = arg_sng[which(!names(arg_sng) %in% c('', '...', 'plot', 'play',
                                                 'savePath', 'invalidArgAction'))]
  defaults = lapply(arg_sng, eval, envir = environment(soundgen))

  # convert formulas to lists
  if (is.character(formula1)) {
    formula1 = paste0('list', substr(formula1, 9, nchar(formula1)))
    formula1 = eval(parse(text = formula1))
  }
  if (!is.list(formula1)) {
    stop('Formula1 must be either a list of pars like "list(sylLen = 500)"
         or a character string like "soundgen(sylLen = 500)"')
  }

  if (is.character(formula2)) {
    formula2 = paste0('list', substr(formula2, 9, nchar(formula2)))
    formula2 = eval(parse(text = formula2))
  }
  if (!is.list(formula2)) {
    stop('Formula2 must be either a list of pars like "list(sylLen = 500)"
         or a character string like "soundgen(sylLen = 500)"')
  }

  # expand all abbreviations to full format
  if (!is.null(formula1$formants)) {  # otherwise drops NULL completely
    formula1$formants = reformatFormants(formula1$formants)
  }
  if (!is.null(formula2$formants)) {
    formula2$formants = reformatFormants(formula2$formants)
  }

  if (!is.null(formula1$formantsNoise)) {
    formula1$formantsNoise = reformatFormants(formula1$formantsNoise)
  }
  if (!is.null(formula2$formantsNoise)) {
    formula2$formantsNoise = reformatFormants(formula2$formantsNoise)
  }

  if (is.numeric(formula1$noise)) {
    if (length(formula1$noise) > 0) {
      formula1$noise = data.frame(
        time = seq(0,
                   ifelse(is.numeric(formula1$sylLen),
                          formula1$sylLen,
                          defaults$sylLen),
                   length.out = max(2, length(formula1$noise))),
        value = formula1$noise)
    }
  }
  if (is.numeric(formula2$noise)) {
    if (length(formula2$noise) > 0) {
      formula2$noise = data.frame(
        time = seq(0,
                   ifelse(is.numeric(formula2$sylLen),
                          formula2$sylLen,
                          defaults$sylLen),
                   length.out = max(2, length(formula2$noise))),
        value = formula2$noise)
    }
  }

  # expand attackLen
  if (length(formula1$attackLen) == 2 && length(formula2$attackLen) < 2) {
    formula2$attackLen = rep(formula2$attackLen, 2)
  }
  if (length(formula2$attackLen) == 2 && length(formula1$attackLen) < 2) {
    formula1$attackLen = rep(formula1$attackLen, 2)
  }

  # set pitchSamplingRate to the higher value
  formula1$samplingRate = formula2$samplingRate = samplingRate
  if (is.numeric(formula1$pitchSamplingRate) ||
      is.numeric(formula2$pitchSamplingRate)) {
    mps = max(formula1$pitchSamplingRate, formula2$pitchSamplingRate)
    formula1$pitchSamplingRate = formula2$pitchSamplingRate = mps
  }

  # ascertain that the formulas are not identical
  if (identical(formula1[order(names(formula1))], formula2[order(names(formula2))])) {
    stop('Nothing to morph!')
  }

  notDefaultIdx_formula1 = notDefaultIdx_formula2 = integer(0)
  if (length(formula1) > 0) {
    notDefaultIdx_formula1 = which(!mapply(identical, formula1, defaults[names(formula1)]))
  }
  if (length(formula2) > 0) {
    notDefaultIdx_formula2 = which(!mapply(identical, formula2, defaults[names(formula2)]))
  }

  # these pars have to be morphed:
  notDefaultNames = unique(c(names(formula1)[notDefaultIdx_formula1],
                             names(formula2)[notDefaultIdx_formula2]))
  notDefaultNames1 = names(formula1) [names(formula1) %in% notDefaultNames]
  notDefaultNames2 = names(formula2) [names(formula2) %in% notDefaultNames]

  # set up two formulas that contain the same number of pars, fill in with
  # specified values or, for values that are not specified, with defaults
  f1 = f2 = defaults[notDefaultNames]
  f1[notDefaultNames1] = formula1[match(notDefaultNames1, names(formula1))]
  f2[notDefaultNames2] = formula2[match(notDefaultNames2, names(formula2))]
  f2 = f2[match(names(f1), names(f2))]

  suppressWarnings({
    # fill in formant stuff if it is missing for one sound but defined for the other
    # b/c defaults contain formants in c() form, repeat:
    if (!is.null(f1$formants)) {  # otherwise drops NULL completely
      f1$formants = reformatFormants(f1$formants)
    }
    if (!is.null(f2$formants)) {
      f2$formants = reformatFormants(f2$formants)
    }

    if ((!is.list(f1$formants) & is.list(f2$formants)) |
        (is.list(f1$formants) & !is.list(f2$formants))) {
      stop(paste('Specify formants either for both formulas or for neither'))
    }

    if ((!is.list(f1$formantsNoise) & is.list(f2$formantsNoise)) |
        (is.list(f1$formantsNoise) & !is.list(f2$formantsNoise))) {
      stop(paste('Specify formantsNoise either for both formulas or for neither'))
    }

    if ((any(is.na(unlist(f1$formants))) & !any(is.na(unlist(f2$formants)))) |
        (!any(is.na(unlist(f1$formants))) & any(is.na(unlist(f2$formants))))) {
      stop(paste('All formant amplitudes and bandwidths should be either left default',
                 'or fully specified in both formulas, not a mix'))
    }

    if ((any(is.na(unlist(f1$formantsNoise))) & !any(is.na(unlist(f2$formantsNoise)))) |
        (!any(is.na(unlist(f1$formantsNoise))) & any(is.na(unlist(f2$formantsNoise))))) {
      stop(paste('All formantsNoise amplitudes and bandwidths should be either left default',
                 'or fully specified in both formulas, not a mix'))
    }
  })


  # Standardize all anchor parameters to dataframes
  # This matches the exact list of arguments processed by reformatAnchors() inside soundgen()
  anchor_pars = c('pitch', 'pitchGlobal', 'glottis',
                  'ampl', 'amplGlobal', 'noise', 'rolloffNoise', 'rolloffNoiseExp',
                  'mouth', 'vocalTract', 'formantLocking', 'glottis',
                  'vibratoFreq', 'vibratoDep',
                  'subRatio', 'subDep', 'jitterLen', 'jitterDep', 'shimmerLen', 'shimmerDep',
                  'vibratoFreq', 'vibratoDep', 'subDep',
                  'rolloff', 'rolloffOct', 'rolloffKHz',
                  'amDep', 'amFreq', 'amShape')

  for (anchor in anchor_pars) {
    if (anchor %in% names(f1)) {
      if (is.numeric(f1[[anchor]]) || is.list(f1[[anchor]])) {
        f1[[anchor]] = reformatAnchors(f1[[anchor]], normalizeTime = (anchor != 'noise'))
      }
    }
    if (anchor %in% names(f2)) {
      if (is.numeric(f2[[anchor]]) || is.list(f2[[anchor]])) {
        f2[[anchor]] = reformatAnchors(f2[[anchor]], normalizeTime = (anchor != 'noise'))
      }
    }
  }

  # Intelligently fill missing / NA values for specific parameters
  # to ensure morphing produces acoustically meaningful results rather than zeros.
  is_missing = function(x) {
    if (is.null(x)) return(TRUE)
    tryCatch({
      return(all(is.na(unlist(x))))
    }, error = function(e) return(FALSE))
  }

  # 1. ampl, amplGlobal, pitchGlobal: NA or NULL --> 0 (as anchors)
  for (par in c('ampl', 'amplGlobal', 'pitchGlobal')) {
    if (par %in% names(f1) && is_missing(f1[[par]])) f1[[par]] = reformatAnchors(0)
    if (par %in% names(f2) && is_missing(f2[[par]])) f2[[par]] = reformatAnchors(0)
  }

  # 2. noise: NA or NULL --> -dynamicRange
  dr1 = if (!is.null(f1$dynamicRange)) f1$dynamicRange else defaults$dynamicRange
  dr2 = if (!is.null(f2$dynamicRange)) f2$dynamicRange else defaults$dynamicRange
  if ('noise' %in% names(f1) && is_missing(f1$noise)) f1$noise = reformatAnchors(-dr1)
  if ('noise' %in% names(f2) && is_missing(f2$noise)) f2$noise = reformatAnchors(-dr2)

  # 3. vocalTract: NA or NULL --> estimateVTL(formants)
  if ('vocalTract' %in% names(f1) && is_missing(f1$vocalTract)) {
    vtl_est = if (!is_missing(f1$formants)) estimateVTL(f1$formants) else
      16.5 # 16.5 cm is a safe human default
    f1$vocalTract = reformatAnchors(vtl_est)
  }
  if ('vocalTract' %in% names(f2) && is_missing(f2$vocalTract)) {
    vtl_est = if (!is_missing(f2$formants)) estimateVTL(f2$formants) else 16.5
    f2$vocalTract = reformatAnchors(vtl_est)
  }

  # 4. mouth: NA or NULL --> 0.5 (neutral)
  if ('mouth' %in% names(f1) && is_missing(f1$mouth)) f1$mouth = reformatAnchors(0.5)
  if ('mouth' %in% names(f2) && is_missing(f2$mouth)) f2$mouth = reformatAnchors(0.5)

  # Guard against any remaining undefined parameters that cannot be intelligently defaulted
  for (p in seq_along(f1)) {
    if (is_missing(f1[[p]]) || is_missing(f2[[p]])) {
      # Format values safely for the error message to avoid crashing on lists/dataframes
      val1 = if (is.atomic(f1[[p]])) paste(f1[[p]], collapse = ',') else class(f1[[p]])[1]
      val2 = if (is.atomic(f2[[p]])) paste(f2[[p]], collapse = ',') else class(f2[[p]])[1]
      stop(paste0('Failed to morph ', names(f1)[p], ': "', val1, '" into "', val2,
                  '". Both values must be defined for morphing.'))
    }
  }


  # log-transform pitch and formant frequencies before morphing
  if ('pitch' %in% names(f1)) {
    if (is.data.frame(f1$pitch) && is.numeric(f1$pitch$value)) f1$pitch$value = log(f1$pitch$value)
    if (is.data.frame(f2$pitch) && is.numeric(f2$pitch$value)) f2$pitch$value = log(f2$pitch$value)
  }
  if ('formants' %in% names(f1)) {
    for (l in seq_along(f1$formants)) {
      if (is.data.frame(f1$formants[[l]]) && is.numeric(f1$formants[[l]]$freq)) {
        f1$formants[[l]]$freq = log(f1$formants[[l]]$freq)
      }
    }
    for (l in seq_along(f2$formants)) {
      if (is.data.frame(f2$formants[[l]]) && is.numeric(f2$formants[[l]]$freq)) {
        f2$formants[[l]]$freq = log(f2$formants[[l]]$freq)
      }
    }
  }

  if ('formantsNoise' %in% names(f1)) {
    for (l in seq_along(f1$formantsNoise)) {
      if (is.data.frame(f1$formantsNoise[[l]]) && is.numeric(f1$formantsNoise[[l]]$freq)) {
        f1$formantsNoise[[l]]$freq = log(f1$formantsNoise[[l]]$freq)
      }
    }
    for (l in seq_along(f2$formantsNoise)) {
      if (is.data.frame(f2$formantsNoise[[l]]) && is.numeric(f2$formantsNoise[[l]]$freq)) {
        f2$formantsNoise[[l]]$freq = log(f2$formantsNoise[[l]]$freq)
      }
    }
  }
  # f1 and f2 are now fully prepared: two target formulas,
  # both of the same length, including the same pars, in the same order

  # MORPH THE FORMULAS
  m = f1
  formulas = rep(list(f1), nMorphs)
  sounds = list()
  for (p in seq_along(f1)) {
    # morph each element of the formula list according to its type
    if (is.numeric(f1[[p]]) && is.numeric(f2[[p]])) {
      if (length(f1[[p]]) == 1 && length(f2[[p]]) == 1) {
        m[[p]] = seq(f1[[p]], f2[[p]], length.out = nMorphs)
      } else {
        len = max(length(f1[[p]]), length(f2[[p]]))
        v1 = rep_len(f1[[p]], len)
        v2 = rep_len(f2[[p]], len)
        m[[p]] = lapply(1:nMorphs, function(i) {
          v1 + (i - 1) / max(1, nMorphs - 1) * (v2 - v1)
        })
      }
    } else if (names(f1[p]) %in% c('formants', 'formantsNoise')) {
      m[[p]] = morphList(f1[[p]], f2[[p]], nMorphs = nMorphs)
    } else if (is.data.frame(f1[[p]]) || is.data.frame(f2[[p]])) {
      m[[p]] = morphDF(f1[[p]], f2[[p]], nMorphs = nMorphs)
    } else {
      # For non-numeric, non-anchor parameters (e.g., character strings, lists like tempEffects),
      # we cannot smoothly morph them. Just replicate f1's value for all morphs.
      m[[p]] = rep(list(f1[[p]]), nMorphs)
    }

    # save the output in appropriate slots in the output list
    for (h in 1:nMorphs) {
      formulas[[h]][[p]] = m[[p]][[h]]
    }
  }
  # END OF MORPHING THE FORMULAS

  # exponentiate pitch and formant frequencies after morphing
  if ('pitch' %in% names(f1)) {
    for (h in 1:nMorphs) {
      if (is.data.frame(formulas[[h]]$pitch) && is.numeric(formulas[[h]]$pitch$value)) {
        formulas[[h]]$pitch$value = exp(formulas[[h]]$pitch$value)
      }
    }
  }
  if ('formants' %in% names(f1)) {
    for (h in 1:nMorphs) {
      for (l in seq_along(f1$formants)) {
        if (is.data.frame(formulas[[h]]$formants[[l]]) && is.numeric(formulas[[h]]$formants[[l]]$freq)) {
          formulas[[h]]$formants[[l]]$freq = exp(formulas[[h]]$formants[[l]]$freq)
        }
      }
    }
  }
  if ('formantsNoise' %in% names(f1)) {
    for (h in 1:nMorphs) {
      for (l in seq_along(f1$formantsNoise)) {
        if (is.data.frame(formulas[[h]]$formantsNoise[[l]]) && is.numeric(formulas[[h]]$formantsNoise[[l]]$freq)) {
          formulas[[h]]$formantsNoise[[l]]$freq = exp(formulas[[h]]$formantsNoise[[l]]$freq)
        }
      }
    }
  }

  # generate the morphs based on the formulas
  for (h in 1:nMorphs) {
    sounds[[h]] = do.call(soundgen, formulas[[h]])
    if (isTRUE(play)) {
      playme(sounds[[h]], samplingRate)
    } else if (is.character(play)) {
      playme(sounds[[h]], samplingRate, player = play)
    }

    if (!is.na(savePath)){
      filename = file.path(savePath, paste0('morph_', h, '.wav'))
      writeAudio(sounds[[h]],
                 audio = readAudio(sounds[[h]], samplingRate = samplingRate),
                 filename = filename,
                 scale_used = max(abs(sounds[[h]])))
    }
  }
  return(list(formulas = formulas, sounds = sounds))
}
