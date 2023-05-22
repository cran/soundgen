#' Nonlinear prediction
#'
#' Predicts new points in a time series. The functionality is provided by
#' \code{\link[nonlinearTseries]{nonLinearPrediction}}. This function is just a
#' simple wrapper "for dummies" that reconstructs the phase space under the
#' hood, including the choice of time lag, embedding dimensions, etc. It can
#' also predict not one but many points in a single step.
#'
#' @param x numeric vector
#' @param nPoints number of points to predict, ideally not more than length(x) /
#'   2 (the function is called recursively to predict longer sequences, but
#'   don't expect miracles)
#' @param time.lag time lag for constructing Takens vectors. Defaults to the
#'   time to the first exponential decay of mutual information. See
#'   \code{\link[nonlinearTseries]{timeLag}}
#' @param embedding.dim the number of dimensions of the phase space. Defaults to
#'   an estimate based on
#'   \code{\link[nonlinearTseries]{estimateEmbeddingDim}}
#' @param max.embedding.dim,threshold,max.relative.change parameters used to
#'   estimate the optimal number of embedding dimensions - see
#'   \code{\link[nonlinearTseries]{estimateEmbeddingDim}}
#' @param radius,radius.increment the radius used for detecting neighbors in the
#'   phase space and its increment in case no neighbors are found - see
#'   \code{\link[nonlinearTseries]{nonLinearPrediction}}
#' @param plot if TRUE, plots the original time series and the predictions
#'
#' @return Returns a numeric vector on the same scale as input \code{x}.
#'
#' @export
#' @examples
#' x = c(rep(1, 3), rep(0, 4), rep(1, 3), rep(0, 4), rep(1, 3), 0, 0)
#' nonlinPred(x, 5, plot = TRUE)
#'
#' nonlinPred(sin(1:25), 22, plot = TRUE)
#'
#' x = soundgen(sylLen = 50, addSilence = 0)[250:450]
#' nonlinPred(x, 100, plot = TRUE)
#'
#' nonlinPred(c(rnorm(5), NA, rnorm(3)))
#' nonlinPred(1:4)
#' nonlinPred(1:6)
#'
#' \dontrun{
#' s1 = soundgen(sylLen = 500, pitch = rnorm(5, 200, 20),
#'               addSilence = 0, plot = TRUE)
#' playme(s1)
#' length(s1)
#' # we can predict output that is longer than the original time series by
#' # predicting a bit at a time and using the output as the new input
#' s2 = nonlinPred(s1, 16000)
#' spectrogram(c(s1, s2))
#' playme(c(s1, s2))
#' }
nonlinPred = function(x,
                      nPoints = 1,
                      time.lag = NULL,
                      embedding.dim = NULL,
                      max.embedding.dim = 15,
                      threshold = 0.95,
                      max.relative.change = 0.1,
                      radius = NULL,
                      radius.increment = NULL,
                      plot = FALSE) {
  myPars = as.list(environment())
  myPars = myPars[!names(myPars) %in% c('x', 'nPoints')]
  len = length(x)
  if (len < 5) return(NA)
  if (any(!is.finite(x))) x = intplNA(x)
  ran_x = diff(range(x))
  if (ran_x == 0) return(rep(x[1], nPoints))
  if (nPoints > (len / 2)) {
    # prediction breaks down if we want about as many points as the original input, but we can avoid this problem by calling nonlinPred recursively, increasing the length of input incrementally
    pr = numeric(0)
    len_out = 0
    len_sum = len + len_out
    while (len_out < nPoints) {
      temp = do.call(nonlinPred, c(list(x = c(x, pr), nPoints = floor(len_sum / 2)), myPars))
      pr = c(pr, temp)
      len_out = length(pr)
      len_sum = len + len_out
    }
    pr = pr[1:nPoints]
    return(pr)
  }

  ## create a good embedding
  # pick a good time lag
  if (is.null(time.lag)) time.lag = try(nonlinearTseries::timeLag(
    x, technique = 'ami', selection.method = 'first.e.decay', do.plot = FALSE),
    silent = TRUE)
  if (inherits(time.lag, 'try-error') || time.lag > len / 2) time.lag = 1

  # pick a good number of embedding dimensions
  if (is.null(embedding.dim)) {
    # fn = try(tseriesChaos::false.nearest(x, m = max.embedding.dim,
    # d = time.lag, t = theiler.win), silent = TRUE)
    # d = data.frame(x = 1:ncol(fn), y = fn[1, ])
    # embedding.dim = findElbow(d)
    embedding.dim = try(nonlinearTseries::estimateEmbeddingDim(
      x, time.lag = time.lag,
      max.embedding.dim = max.embedding.dim,
      threshold = threshold,
      max.relative.change = max.relative.change,
      do.plot = FALSE), silent = TRUE)
    # plot(fn)
    if (inherits(embedding.dim, 'try-error')) {
      embedding.dim = 2
    } else if (!is.finite(embedding.dim)) {
      embedding.dim = 2
    }
    if (!is.finite(embedding.dim) ||
        embedding.dim > max.embedding.dim) embedding.dim = 2
  }

  # pick a radius for nonlinear prediction
  if (is.null(radius)) radius = ran_x / 100
  if (is.null(radius.increment)) radius.increment = radius * 2

  ## perform nonlinear prediction
  pr = try(nonLinearPrediction_mod(
    x, embedding.dim = embedding.dim, time.lag = time.lag, nPoints = nPoints,
    radius = radius, radius.increment = radius.increment), silent = TRUE)
  if (inherits(pr, 'try-error')) pr = NA

  if (plot) {
    plot(x, xlim = c(1, len + nPoints), type = 'l')
    if (any(!is.na(pr)))
      points((1:nPoints) + len, pr, col = 'blue', type = 'l')
  }
  return(pr)
}


#' Nonlinear prediction modified
#'
#' A slightly modified version of
#' \code{\link[nonlinearTseries]{nonLinearPrediction}} that can return multiple
#' new points in one go, without having to call the function repeatedly.
#'
#' @param time.series,embedding.dim,time.lag,radius,radius.increment see \code{\link[nonlinearTseries]{nonLinearPrediction}}
#' @param nPoints number of points to return
#'
#' @keywords internal
nonLinearPrediction_mod = function(time.series,
                                   embedding.dim,
                                   time.lag,
                                   nPoints,
                                   radius,
                                   radius.increment) {
  nfound = 0
  av = 0
  l = length(time.series)
  prediction.step = 1:nPoints
  jumpsvect = seq((embedding.dim - 1) * time.lag, 0, -time.lag)
  takensVector = time.series[l - jumpsvect]
  beg = (embedding.dim - 1) * time.lag + 1
  en = l - nPoints
  while (nfound == 0) {
    for (i in beg:en) {
      if (isNeighbour_mod(takensVector, time.series[i - jumpsvect],
                          embedding.dim, radius)) {
        av = av + time.series[i + prediction.step]
        nfound = nfound + 1
      }
    }
    radius = radius + radius.increment
  }
  av/nfound
}


#' Is neighbor modified
#'
#' Just a copy of nonlinearTseries:::isNeighbour to avoid a note in
#' CMD check when calling an unexported function from another package
#'
#' @param v1,v2,embedding.dim,radius see nonlinearTseries:::isNeighbour
#' @keywords internal
isNeighbour_mod = function(v1, v2, embedding.dim, radius) {
  for (i in 1:embedding.dim) {
    if (abs(v1[[i]] - v2[[i]]) >= radius) {
      return(FALSE)
    }
  }
  TRUE
}
