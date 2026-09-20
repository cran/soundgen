# Functions for generating and modifying smooth contours from a few anchors

#' Smooth contour from anchors
#'
#' Returns a smooth contour based on an arbitrary number of anchors - points
#' through which we wish to draw a curve, and which may or may not be
#' equidistant along the x (time) dimension. \code{getSmoothContour} is
#' used extensively by \link{soundgen} for generating intonation contour, mouth
#' opening, etc. Pitch contours are treated as a special case: values are
#' log-transformed prior to smoothing, so that with 2 anchors we get a linear
#' transition on a log scale (as if we were operating with musical notes rather
#' than frequencies in Hz).
#'
#' @param anchors a numeric vector of values or a list/dataframe with one column
#'   (value) or two columns (time and value). \code{anchors$time} can be in ms
#'   or in arbitrary units, eg 0 to 1. \code{anchors$value} can be on any scale.
#' @param len the required length of the output contour. If NULL, it will be
#'   calculated based on the maximum time value (in ms) and \code{samplingRate}.
#'   If \code{len == 1}, returns the mean of anchors
#' @param thisIsPitch (boolean) is this a pitch contour? If TRUE, log-transforms
#'   before smoothing and plots in both Hz and musical notation
#' @param normalizeTime if TRUE, normalizes anchors$time values to range from 0 to 1
#' @param interpol the method of interpolation (only used for upsampling,
#'   not downsampling); see \link{interpolate} for details
#' @param discontThres if two anchors are closer in time than
#'   \code{discontThres} (on a 0-1 scale, ie specified as proportion of total
#'   length), the contour is broken into segments with a linear transition
#'   between these segments; set to 0 to avoid ever splitting into segments
#' @param jumpThres if anchors are closer than \code{jumpThres}, a new section
#'   starts with no transition at all (e.g. for adding pitch jumps); normally,
#'   \code{jumpThres <= discontThres}
#' @param valueFloor,valueCeiling lower/upper bounds for the contour
#' @param plot (boolean) produce a plot?
#' @param samplingRate sampling rate used to convert time values to points (Hz)
#' @param voiced,contourLabel graphical parameters for plotting breathing
#'   contours (see examples below)
#' @param NA_to_zero if TRUE, all NAs are replaced with zero; if FALSE, NAs
#'   propagate and can be upsampled
#' @param xlim,ylim,xlab,ylab,main plotting options
#' @param ... other plotting options passed to \code{plot()}
#'
#' @return A numeric vector of length \code{len}.
#' @keywords internal
#' @examples
#' # long format: anchors are a dataframe
#' a = soundgen:::getSmoothContour(anchors = data.frame(
#'   time = c(50, 137, 300), value = c(0.03, 0.78, 0.5)),
#'   normalizeTime = FALSE,
#'   voiced = 200, valueFloor = 0, plot = TRUE, main = '',
#'   samplingRate = 16000) # breathing
#'
#' # short format: anchors are a vector (equal time steps assumed)
#' a = soundgen:::getSmoothContour(anchors = c(350, 800, 600),
#'   len = 5500, thisIsPitch = TRUE, plot = TRUE,
#'   samplingRate = 3500) # pitch
#'
#' # a single anchor gives constant value
#' a = soundgen:::getSmoothContour(anchors = 800,
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
#'
#' # two pitch anchors give loglinear F0 change
#' a = soundgen:::getSmoothContour(anchors = c(220, 440),
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
#'
#' ## Two closely spaced anchors produce a pitch jump
#' # one curve through all anchors
#' a1 = soundgen:::getSmoothContour(anchors = list(time = c(0, .15, .2, .7, 1),
#'     value = c(360, 116, 550, 700, 610)), len = 500, thisIsPitch = TRUE,
#'     plot = TRUE, samplingRate = 500)
#' # two segments with a linear transition
#' a2 = soundgen:::getSmoothContour(anchors = list(time = c(0, .15, .17, .7, 1),
#'     value = c(360, 116, 550, 700, 610)), len = 500, thisIsPitch = TRUE,
#'     plot = TRUE, samplingRate = 500)
#' # two segments with an abrupt jump
#' a3 = soundgen:::getSmoothContour(anchors = list(time = c(0, .15, .155, .7, 1),
#'     value = c(360, 116, 550, 700, 610)), len = 500, thisIsPitch = TRUE,
#'     plot = TRUE, samplingRate = 500)
#' # compare:
#' plot(a2)
#' plot(a3)  # NB: the segment before the jump is upsampled to compensate
#'
#' ## Control the amount of smoothing
#' # linear - no smoothing
#' soundgen:::getSmoothContour(c(1, 3, 9, 10, 9, 9, 2), len = 100, plot = TRUE,
#'   interpol = 'linear')
#' # spline (smooth, but may overshoot slightly)
#' soundgen:::getSmoothContour(c(1, 3, 9, 10, 9, 9, 2), len = 100, plot = TRUE,
#'   interpol = 'splineFC')
#' # cosine - goes strictly through anchors, no overshoot, but possible ripples
#' soundgen:::getSmoothContour(c(1, 3, 9, 10, 9, 9, 2), len = 100, plot = TRUE,
#'   interpol = 'cosine')
#' # see ?interpolate for more options
#'
#' ## Upsample preserving leading and trailing NAs
#' anchors = data.frame(time =  c(1,  4,  5,  7,  10, 20, 23, 25, 30),
#'                      value = c(NA, NA, 10, 15, 12, NA, 17, 15, NA))
#' plot(anchors, type = 'b')
#' anchors_ups = soundgen:::getSmoothContour(
#'   anchors, len = 200,
#'   interpol = 'linear',
#'   NA_to_zero = FALSE,   # preserve NAs
#'   discontThres = 0)     # don't break into sub-contours
#' plot(anchors_ups, type = 'b')
getSmoothContour = function(
    anchors = data.frame(time = c(0, 1), value = c(0, 1)),
    len = NULL,
    thisIsPitch = FALSE,
    normalizeTime = TRUE,
    interpol = 'splineFC',
    discontThres = .05,
    jumpThres = .01,
    valueFloor = NULL,
    valueCeiling = NULL,
    plot = FALSE,
    xlim = NULL,
    ylim = NULL,
    xlab = 'Time, ms',
    ylab = ifelse(thisIsPitch, 'Frequency, Hz', 'Amplitude'),
    main = ifelse(thisIsPitch, 'Pitch contour', ''),
    samplingRate = 16000,
    voiced = NULL,
    contourLabel = NULL,
    NA_to_zero = TRUE,
    ...) {
  anchors = reformatAnchors(anchors, normalizeTime = normalizeTime)
  if (!is.null(len) && len == 1) return(mean(anchors$value))
  # each segment must have >=3 points
  discontThres = min(discontThres, 3 / length(anchors$value))

  if (!is.null(valueFloor)) {
    anchors$value[anchors$value < valueFloor] = valueFloor
  }
  if (!is.null(valueCeiling)) {
    anchors$value[anchors$value > valueCeiling] = valueCeiling
  }
  if (thisIsPitch) {
    anchors$value = HzToSemitones(anchors$value)
    if (!is.null(valueFloor) && is.numeric(valueFloor))
      if (valueFloor < 0)
        stop('valueFloor must be positive if thisIsPitch = TRUE')
    valueFloor = HzToSemitones(valueFloor)
    if (!is.null(valueCeiling) && is.numeric(valueCeiling))
      if (valueCeiling < 0)
        stop('valueCeiling must be positive if thisIsPitch = TRUE')
    valueCeiling = HzToSemitones(valueCeiling)
  }

  if (is.null(len)) {
    # if len is null, we expect that anchors$time encoded
    # the desired duration, in ms
    duration_ms = diff(range(anchors$time))
    len = floor(duration_ms * samplingRate / 1000)
  } else {
    duration_ms = len / samplingRate * 1000
  }
  anchors_bckp = anchors  # original time scale for plotting
  anchors$time = anchors$time - min(anchors$time)
  mt = max(anchors$time)
  if (mt != 0)
    anchors$time = anchors$time / mt # strictly 0 to 1

  if (!is.numeric(duration_ms) || !is.numeric(len)) {
    return(NA)
  } else if (duration_ms == 0 || len == 0) {
    return(NA)
  }
  if (NA_to_zero) anchors$value[is.na(anchors$value)] = 0

  # get smooth contours
  nr = nrow(anchors)
  if (nr == len) {
    # nothing to do
    smoothContour = anchors$value
  } else if (nr > len) {
    # downsample
    smoothContour = suppressWarnings(.resample(list(
      sound = anchors$value),len = len, lowPass = TRUE, interpol = interpol))
  } else if (discontThres <= 0 || nr < 3) {
    # upsample in one go
    if (nr == 1) {
      smoothContour = rep(anchors$value, len)
    } else {
      smoothContour = drawContour(len = len,
                                  anchors = anchors,
                                  interpol = interpol)
    }
  } else {
    # some anchors might be too close, so we split the contour into segments
    # to avoid weird behavior of loess etc
    sections = splitContour(anchors = anchors,
                            discontThres = discontThres,
                            jumpThres = jumpThres)
    smoothContour = vector()
    for (i in 1:nrow(sections)) {
      segm_len = max(1, round((anchors$time[sections$end[i]] -
                                 anchors$time[sections$start[i]]) * len))
      cont = drawContour(len = segm_len,
                         anchors = anchors[sections$start[i]:sections$end[i], ],
                         interpol = interpol)
      transition = vector()
      if (i < nrow(sections)) {
        trans_len = round((anchors$time[sections$start[i + 1]] -
                             anchors$time[sections$end[i]]) * len)
        if (sections$jump[i] && length(cont) > 0) {
          # upsample the segment before the jump to make up for skipped transition
          cont = approx(cont, n = length(cont) + trans_len)$y
        } else {
          # make a linear transition preserving spacing between anchors
          transition = seq(anchors$value[sections$end[i]],
                           anchors$value[sections$start[i + 1]],
                           length.out = trans_len)
        }
      }
      smoothContour = c(smoothContour, cont, transition)
    }
    if (length(smoothContour) != len) {
      # shouldn't happen, but you never know
      smoothContour = spline(smoothContour, n = len)$y
    }
  }
  # plot(smoothContour, type='p')

  if (plot && any(!is.na(smoothContour))) {
    op = par("mar") # save user's original margin settings
    on.exit(par(mar = op))

    # presuming that len was specified and anchors$time are on a
    # relative scale, we transform to ms for plotting
    if (max(anchors_bckp$time) <= 1) {
      anchors_bckp$time = anchors_bckp$time * duration_ms
      time = seq(0, len / samplingRate * 1000, length.out = len)
    } else {  # time is already in ms
      time = seq(anchors_bckp$time[1], anchors_bckp$time[nrow(anchors)],
                 length.out = len)
    }
    ran = range(smoothContour, na.rm = TRUE)

    if (thisIsPitch) {
      # pitch - log-transformed
      if (!is.numeric(ylim)) {
        ylim = c(
          ran[1] / 1.1,  # can't be negative
          ran[2] * 1.1
        )
      } else {
        ylim = HzToSemitones(ylim)
        if (ran[1] < ylim[1])
          ylim[1] = ran[1] / 1.1
        if (ran[2] > ylim[2])
          ylim[2] = ran[2] * 1.1
      }
      lbls_semitones = unique(seq(ylim[1], ylim[2], length.out = 5))
      # unique to remove duplicates, max 5 labels
      lbls_notes = notesDict$note[round(lbls_semitones) + 1]
      lbls_Hz = round(semitonesToHz(lbls_semitones))

      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      plot(time, smoothContour,
           type = 'l', yaxt = "n", ylab = ylab, xlab = xlab,
           ylim = ylim, main = main, ...)
      axis(2, at = lbls_semitones, labels = lbls_Hz, las = 1)
      axis(4, at = lbls_semitones, labels = lbls_notes, las = 1)
      points(anchors_bckp$time, anchors_bckp$value, col = 'blue', cex = 3)
    } else {
      # not pitch - not log-transformed
      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      if (is.null(xlim)) {
        xlim = c(min(0, anchors_bckp$time[1]),
                 anchors_bckp$time[length(anchors$time)])
      }
      if (is.null(ylim)) {
        m1 = min(c(ran, anchors_bckp$value))
        m1 = ifelse(m1 > 0, m1 / 1.1, m1 * 1.1)
        m2 = max(c(ran, anchors_bckp$value))
        m2 = ifelse(m2 > 0, m2 * 1.1, m2 / 1.1)
        ylim = c(m1, m2)
      }

      plot(time, smoothContour, type = 'l', ylab = ylab,
           xlab = xlab, xlim = xlim, ylim = ylim, main = main, ...)
      points(anchors_bckp$time, anchors_bckp$value, col = 'blue', cex = 3)
      if (is.numeric(voiced)) {
        lines(x = c(0, voiced), y = c(0, 0), col = 'blue', lwd = 10)
        text(x = voiced / 2, y = abs(ylim[2] - ylim[1]) / 25,
             adj = 0.5, labels = 'voiced part', col = 'blue')
        text(x = anchors_bckp$time[nrow(anchors_bckp)],
             y = anchors_bckp$value[nrow(anchors_bckp)] - (ylim[2] - ylim[1]) / 25,
             adj = 1, labels = contourLabel, col = 'red')
      }
    }
  }
  # NAs may arise if the first anchor time > 0 or if input vector contains NAs
  if (NA_to_zero) smoothContour[is.na(smoothContour)] = 0

  # floor / ceiling
  if (!is.null(valueFloor)) smoothContour[smoothContour < valueFloor] = valueFloor
  if (!is.null(valueCeiling)) smoothContour[smoothContour > valueCeiling] = valueCeiling
  if (thisIsPitch) {
    return(semitonesToHz(smoothContour))
  } else {
    return(smoothContour)
  }
}


#' Draw contour
#'
#' NAs are propagated.
#' @noRd
#' @param len length of output
#' @param anchors preformatted anchors with time between [0, 1]
#' @param interpol the method of interpolation
drawContour = function(len,
                       anchors,
                       interpol) {
  time = 1:len
  nr = nrow(anchors)
  if (nr == len) return(anchors$value)
  if (nr == 1) {
    # flat
    smoothContour = rep(anchors$value[1], len)
  } else if (nr == 2) {
    # just a line between two values
    smoothContour = seq(anchors$value[1],
                        anchors$value[2],
                        length.out = len)
  } else {
    # many anchors. Any NAs?
    idx_notNA = which(!is.na(anchors$value))
    if (!any(is.na(anchors$value))) {
      # there are no NAs - just call interpolate(), and we are done
      smoothContour = interpolate(x = anchors$time, y = anchors$value,
                                  xout = seq(anchors$time[1], anchors$time[nr], length.out = len),
                                  method = interpol)
    } else {
      # there are NAs: we'll have to go through .resample()
      # STEP 1: use approx to make anchors equidistant
      time_steps = diff(anchors$time)
      # NB: time is strictly [0, 1], so time_step gives us the time resolution we need
      len_interm = min(nrow(anchors) * 100, round(1 / min(time_steps)) + 1)
      if (max(diff(time_steps)) > 0.01) {
        # anchors not equidistant in time - need to standardize before calling `resample`
        # first, let's tackle NAs
        # if there are leading and/or trailing NAs, we have to upsample them separately
        # replace trailing NA with the first / last non-NA values
        idx_notNA = which(!is.na(anchors$value))
        n_leading_na = round(len_interm * anchors$time[idx_notNA[1]])
        n_trailing_na = round(len_interm * (1 - anchors$time[tail(idx_notNA, 1)]))
        n_mid = len_interm - n_leading_na - n_trailing_na
        mid_new = approx(x = anchors$time, y = anchors$value, n = n_mid, na.rm = FALSE)$y
        # NB: approx drops leading and trailing NAs, so this just upsamples the middle part
        curve_interm = c(
          rep(NA, n_leading_na),
          mid_new,
          rep(NA, n_trailing_na)
        )
      } else {
        curve_interm = anchors$value
      }

      # STEP 2: call `resample` to down- or upsample `curve_interm` to the right length
      # (`resample` respects NAs, and now the time steps between points are all identical)
      smoothContour = .resample(
        list(sound = curve_interm, samplingRate = len_interm),
        len = len, lowPass = FALSE, interpol = interpol)
    }
  }
  smoothContour
}


#' Split contour
#'
#' Splits a smooth contour into several segments. A new segment is started if
#' the time step between two anchors is smaller than discontThres.
#' @param anchors a dataframe with two columns: time and value (time on any scale)
#' @inheritParams getSmoothContour
#' @return A dataframe containing the index of anchor rows for start and
#'   end of each segment and whether we want a transition or a jump between
#'   segments.
#' @noRd
#' @examples
#' soundgen:::splitContour(data.frame(time = c(0, 370, 650, 655, 1050, 1400),
#'   value = c(360, 316, 345, 550, 610, 590)))
#' soundgen:::splitContour(data.frame(time = c(0, .2, .205, .8, .81, 1),
#'   value = c(360, 316, 345, 550, 610, 590)))
#' soundgen:::splitContour(data.frame(time = c(0, .4, .45, .6, .8, 1),
#'   value = c(360, 316, 345, 550, 610, 590)))
#' soundgen:::splitContour(data.frame(time = c(0, .4, .45, .6, .8, 1),
#'   value = c(360, 316, 345, 550, 610, 590)),
#'   discontThres = .1)
#' soundgen:::splitContour(data.frame(time = c(0, 1),
#'   value = c(360, 590)))
splitContour = function(anchors,
                        discontThres = .05,
                        jumpThres = .01,
                        nSections_max = 5) {
  discont = which(diff(anchors$time) / diff(range(anchors$time)) < discontThres)
  # limit max number of sections
  if (length(discont) > nSections_max)
    discont = sample(discont, nSections_max)
  jumps = which(diff(anchors$time) / diff(range(anchors$time)) < jumpThres)
  if (length(discont) > 0) {
    sections = data.frame(start = 1,
                          end = rep(nrow(anchors), length(discont) + 1),
                          jump = FALSE)
    for (j in seq_along(discont)) {
      sections$end[j] = discont[j]
      sections$start[j + 1] = discont[j] + 1
      sections$jump[j] = any(jumps == discont[j])
    }
  } else {
    sections = data.frame(start = 1, end = nrow(anchors), jump = FALSE)
  }
  sections
}


#' Discrete smooth contour from anchors
#' A discrete version of \code{\link{getSmoothContour}} with modified plotting.
#' Intended for plotting variation in parameters across syllables. The time in
#' `anchors` should be between [0, 1].
#' @param len the number of syllables (equivalently, the length of generated
#'   contour)
#' @inheritParams getSmoothContour
#' @param ylim ylim for plotting
#' @return A numeric vector.
#' @noRd
#' @examples
#' # for a bout consisting of 10 syllables
#' soundgen:::getDiscreteContour(len = 10, interpol = 'spline', plot = TRUE,
#'   ylab = 'Semitones', anchors = data.frame(time = c(0, .2, .6, 1),
#'   value = c(0, -3, 1, 0)))
getDiscreteContour = function(
    len,
    anchors,
    interpol = 'splineFC',
    valueFloor = NULL,
    valueCeiling = NULL,
    ylim = NULL,
    plot = FALSE,
    ...) {
  contour = getSmoothContour(
    len = len,
    anchors = anchors,
    interpol = interpol,
    valueFloor = valueFloor,
    valueCeiling = valueCeiling
  )
  if (plot) {
    if (is.null(ylim)) {
      ylim = c(min(min(contour), min(anchors$value)),
               max(max(contour), max(anchors$value)))
    }
    plot(contour, type = 'b', xlab = 'Syllable', col = 'red', bg = 'red',
         cex = 1, pch = 23, ylim = ylim, ...)
    points(x = anchors$time * (len - 1) + 1, y = anchors$value, col = 'blue',
           cex = 3)
  }
  contour
}
