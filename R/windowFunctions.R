#' Windowing functions for the Fourier transform
#'
#' Generates an n-point window vector for the discrete Fourier transform and
#' short-time Fourier analysis. Windows control spectral leakage by tapering the
#' input signal, trading off main lobe width against sidelobe level. See the
#' \code{\link[signal]{signal-package}} for more windows and/or different
#' implementations. External and user-defined windowing functions can also be
#' passed to all soundgen functions that call \code{winFun} - see examples.
#'
#' @param n window length in samples (integer >= 3)
#' @param wn window type (strings and functions can be passed to other soundgen
#'   functions that have a "wn" argument):
#'   \describe{
#'   \item{character string}{name of a predefined window}
#'   \item{function}{a function that takes \code{n} and returns a window
#'   vector, e.g. a constructor such as \code{winFun_gauss(0.3)} or a
#'   user-defined function}
#'   \item{numeric vector}{a user-supplied window of length n}}
#' @param normalize "none" = no normalization; "amplitude" = divide by sum of
#'   weights; "energy" = divide by sum of squared weights
#' @param ... extra arguments specific to parameterized windows, e.g.
#'   \code{sigma = 0.3} for \code{"gaussian"} (see "Predefined windows" for
#'   per-window arguments). Used only when \code{wn} is a character string;
#'   when \code{wn} is a constructor function, parameters are passed to the
#'   constructor directly. Do not pass graphical arguments here.
#'
#' @return A numeric vector of length n with values typically between 0 and 1
#'   for \code{normalize = "none"} (or user-supplied values).
#'
#' @section Predefined windows:
#'
#' \describe{
#'   \item{"rectangle"}{Rectangular (uniform) window – no tapering. Minimizes
#'   mean square error, narrowest main lobe (-13 dB sidelobe). Good for
#'   transients or when leakage is not critical.}
#'
#'   \item{"hann" / "hanning"}{Hann window (raised cosine). Smooth ends,
#'   moderate sidelobe roll-off (-31.5 dB first sidelobe). Standard for general
#'   audio work.}
#'
#'   \item{"hamming"}{Hamming window. Almost cancels the first sidelobe of the
#'   Hann window (-43 dB). Popular in speech processing.}
#'
#'   \item{"blackman"}{Blackman window (3-term cosine sum). Lower sidelobes
#'   (-58 dB) than Hann/Hamming, slightly wider main lobe.}
#'
#'   \item{"flattop"}{Flat-top window (5-term cosine sum). Very wide main lobe
#'   but low sidelobes and excellent amplitude accuracy. Useful when precise
#'   amplitude measurement matters more than frequency resolution.}
#'
#'   \item{"bartlett" / "triangular"}{Bartlett (triangular) window –
#'   linearly tapering to zero. Simple, reasonably low sidelobes (-25 dB).}
#'
#'   \item{"welch"}{Welch (parabolic) window. Tapers to zero smoothly,
#'   sidelobe fall-off is asymptotic, close to sine window.}
#'
#'   \item{"parzen"}{Parzen window (cubic spline). Very smooth, continuous
#'   second derivative, excellent sidelobe roll-off. Common in non-parametric
#'   spectral density estimation.}
#'
#'   \item{"bspline"}{Cubic B-spline window (order 4). Extremely smooth; good
#'   when sidelobe structure must be suppressed heavily.}
#'
#'   \item{"bohman"}{Bohman window (time-domain convolution of two
#'   half-cosines). Sidelobes decay as \eqn{1/f^3}, flat at zero, very low
#'   leakage.}
#'
#'   \item{"blackmanharris"}{Blackman–Harris 4-term window (minimum sidelobe
#'   design). Very low sidelobes (-92 dB), suitable when dynamic range is
#'   important.}
#'
#'   \item{"nuttall"}{Nuttall 4-term window (symmetric, continuous first
#'   derivative). Slightly better sidelobes than Blackman–Harris in some metrics
#'   (-93 dB).}
#'
#'   \item{"barthannwin"}{Bartlett–Hann window (linear + cosine taper).
#'   Zero at edges, good compromise between Bartlett and Hann.}
#'
#'   \item{"sine" or "cosine"}{Half-cycle sine window
#'   \eqn{\sin(\pi k/(n-1))}. Simple, zero at edges, often used in audio coding
#'   (e.g., MP3).}
#'
#'   \item{"lanczos"}{Lanczos window (main lobe of \eqn{\mathrm{sinc}}).
#'   Smooth, zero at edges, used in interpolation and image resampling.}
#'
#'   \item{"gaussian"}{Gaussian window. Minimizes time–frequency uncertainty;
#'   never quite reaches zero at the edges. The default \code{sigma =
#'   1/sqrt(24)} (~= 0.204) reproduces the Praat/seewave window.
#'   Constructor: \code{winFun_gauss(sigma)}.}
#'
#'   \item{"tukey"}{Tukey (tapered cosine) window. "fraction" = fraction of
#'   the window devoted to cosine tapering (0 = rectangular, 1 = Hann).
#'   Constructor: \code{winFun_tukey(fraction)}.}
#'
#'   \item{"poisson"}{Poisson (exponential) window:
#'   \eqn{\exp(-\alpha\,|k - N/2|\,/\,(N/2))}. Sharp central peak, quick decay.
#'   Good for isolating fast transients. Constructor:
#'   \code{winFun_poisson(alpha)}.}
#'
#'   \item{"cauchy"}{Cauchy (Lorentzian) window: \eqn{1 / (1 +
#'   (\alpha\,(k-N/2)/(N/2))^2)}. Smooth and heavy-tailed, with a parameter
#'   controlling the width. Constructor: \code{winFun_cauchy(alpha)}.}
#'
#'   \item{"kaiser"}{Kaiser window based on the modified Bessel function of
#'   the first kind. "beta" controls the trade-off between main lobe width and
#'   sidelobe level (0 = rectangular, ~5 ~= Hamming, ~8.6 ~= Blackman).
#'   Constructor: \code{winFun_kaiser(beta)}.}
#' }
#'
#' @examples
#' # "wn" as a character string
#' wns = c('rectangle', 'hann', 'hamming', 'blackman', 'flattop', 'bartlett',
#' 'welch', 'parzen', 'bspline', 'bohman', 'blackmanharris', 'nuttall',
#' 'barthannwin', 'sine', 'lanczos', 'gaussian', 'tukey', 'poisson',
#' 'cauchy', 'kaiser')
#' op = par(c('mfrow', 'mar')); par(mfrow = c(5, 4), mar = c(0, 0, 3, 0))
#' for (w in wns)
#'   plot(winFun(256, w), xlab='', ylab='', bty='n', xaxt='n', yaxt='n', main=w)
#' par(op)
#'
#' # Passing window-specific parameters via ...
#' plot(winFun(256, 'gaussian', sigma = 0.2), main = 'Gaussian, sigma = 0.2')
#' plot(winFun(256, 'gaussian', sigma = 0.5), main = 'Gaussian, sigma = 0.5')
#' plot(winFun(256, 'tukey', fraction = 0.8), main = 'Tukey, fraction = 0.8')
#' plot(winFun(256, 'kaiser', beta = 8.6), main = 'Kaiser, beta = 8.6')
#'
#' # Equivalent: passing a constructor (useful when forwarding through
#' # higher-level functions like meanSpectrum(), spectrogram(), etc.)
#' plot(winFun(256, winFun_gauss(0.2)), main = 'Gaussian, sigma = 0.2')
#' plot(winFun(256, winFun_tukey(0.8)), main = 'Tukey, fraction = 0.8')
#' plot(winFun(256, winFun_kaiser(8.6)), main = 'Kaiser, beta = 8.6')
#'
#' # use "wn" in other soundgen functions:
#' s = cos(2 * pi * 440 * (1:2000) / 2000) + 0.3 +
#'     cos(2 * pi * 880 * (1:2000) / 2000)
#' meanSpectrum(s, 2000, wn = 'blackman', yScale = 'dB')
#' meanSpectrum(s, 2000, wn = winFun_cauchy(1.2), yScale = 'dB')
#' meanSpectrum(s, 2000, wn = winFun_kaiser(8), yScale = 'dB')
#'
#' # "wn" as a user-supplied function
#' plot(signal::kaiser(100, 4))
#' meanSpectrum(s, 2000, wn = function(n) signal::kaiser(n, 4), yScale = 'dB')
#' meanSpectrum(s, 2000, wn = function(n) signal::kaiser(n, 16), yScale = 'dB')
#'
#' plot(winFun(256, signal::chebwin(256, 80)),
#'      main = 'Dolph-Chebyshev, -80 dB sidelobes')
#' meanSpectrum(s, 2000, wn = function(n) signal::chebwin(n, 80), yScale = 'dB')
#'
#' halfsine = function(n) sin(pi * (0:(n-1)) / (n-1))
#' plot(winFun(25, halfsine), main = 'Half-sine')
#' meanSpectrum(s, 2000, wn = halfsine, yScale = 'dB')
#'
#' # "wn" as a user-supplied numeric vector
#' custom = sin(pi * (0:255) / 255)   # half sine again
#' plot(winFun(256, custom), main = 'Half-sine')
#' @export
winFun = function(n, wn, normalize = c('none', 'amplitude', 'energy'), ...) {
  normalize = match.arg(normalize)
  if (!is.numeric(n) || length(n) != 1 || n < 3)
    stop('window length n must be a positive integer >= 3')
  n = round(n)

  # wn is a numeric vector
  if (is.numeric(wn)) {
    if (length(wn) != n) stop('If wn is a numeric vector, it must be of length n')
    if (!all(is.finite(wn))) stop('wn must not contain NA/Inf values')
    w = wn
    if (normalize == "amplitude") w = w / sum(w)
    if (normalize == "energy")    w = w / sum(w^2)
    return(w)
  }

  # wn is a function (constructor or user-supplied)
  if (is.function(wn)) {
    w = wn(n)
    if (normalize == "amplitude") w = w / sum(w)
    if (normalize == "energy")    w = w / sum(w^2)
    return(w)
  }

  # wn is a character string
  if (!is.character(wn) || length(wn) != 1) {
    stop("'wn' must be a character string, a numeric vector of length n, or a function")
  }

  # Extract method-specific parameters from ...
  all_args = list(...)
  all_arg_names = names(all_args)

  if (wn == "gaussian") {
    sigma = if ('sigma' %in% all_arg_names) all_args$sigma else 1 / sqrt(24)
  } else if (wn == "tukey") {
    fraction = if ('fraction' %in% all_arg_names) all_args$fraction else 0.5
  } else if (wn == "poisson") {
    alpha = if ('alpha' %in% all_arg_names) all_args$alpha else 1
  } else if (wn == "cauchy") {
    alpha = if ('alpha' %in% all_arg_names) all_args$alpha else 1
  } else if (wn == "kaiser") {
    beta = if ('beta' %in% all_arg_names) all_args$beta else 5
  }

  # Dispatch
  if (wn == "gaussian") {
    w = winFun_gauss(sigma = sigma)(n)
  } else if (wn == "tukey") {
    w = winFun_tukey(fraction = fraction)(n)
  } else if (wn == "poisson") {
    w = winFun_poisson(alpha = alpha)(n)
  } else if (wn == "cauchy") {
    w = winFun_cauchy(alpha = alpha)(n)
  } else if (wn == "kaiser") {
    w = winFun_kaiser(beta = beta)(n)
  } else if (wn %in% c("bartlett", "triangular")) {
    N = n - 1
    N2 = N / 2
    w = 1 - abs(0:N - N2) / N2
  } else if (wn == "blackman") {
    N = n - 1
    w = 0.42 - 0.5 * cos(2 * pi * (0:N) / N) +
      0.08 * cos(4 * pi * (0:N) / N)
  } else if (wn == "flattop") {
    N = n - 1
    w = 0.21557895 -
      0.41663158 * cos(2 * pi * (0:N) / N) +
      0.277263158 * cos(4 * pi * (0:N) / N) -
      0.083578947 * cos(6 * pi * (0:N) / N) +
      0.006947368 * cos(8 * pi * (0:N) / N)
  } else if (wn == "hamming") {
    N = n - 1
    w = 0.54 - 0.46 * cos(2 * pi * (0:N) / N)
  } else if (wn %in% c("hanning", "hann")) {
    N = n - 1
    w = 0.5 - 0.5 * cos(2 * pi * (0:N) / N)
  } else if (wn == "rectangle") {
    w = rep(1, n)
  } else if (wn == "welch") {
    N = n - 1
    N2 = N / 2
    x = (0:N - N2) / N2
    w = 1 - x^2
  } else if (wn == "parzen") {
    N = n - 1
    N2 = N / 2
    u = abs((0:N - N2) / N2)
    w = numeric(n)
    idx = u <= 0.5
    w[idx] = 1 - 6 * u[idx]^2 + 6 * u[idx]^3
    w[!idx] = 2 * (1 - u[!idx])^3
  } else if (wn == "bspline") {
    N = n - 1
    N2 = N / 2
    x = 2 * (0:N - N2) / N2
    ax = abs(x)
    w = numeric(n)
    inner = ax <= 1
    w[inner] = ((2 - ax[inner])^3 - 4 * (1 - ax[inner])^3) / 4
    outer = ax > 1 & ax <= 2
    w[outer] = (2 - ax[outer])^3 / 4
  } else if (wn == "bohman") {
    N = n - 1
    N2 = N / 2
    u = abs((0:N - N2) / N2)
    w = (1 - u) * cos(pi * u) + (1/pi) * sin(pi * u)
  } else if (wn == "blackmanharris") {
    N = n - 1
    a0 = 0.35875; a1 = 0.48829; a2 = 0.14128; a3 = 0.01168
    w = a0 - a1 * cos(2 * pi * (0:N) / N) +
      a2 * cos(4 * pi * (0:N) / N) -
      a3 * cos(6 * pi * (0:N) / N)
  } else if (wn == "nuttall") {
    N = n - 1
    a0 = 0.355768; a1 = 0.487396; a2 = 0.144232; a3 = 0.012604
    w = a0 - a1 * cos(2 * pi * (0:N) / N) +
      a2 * cos(4 * pi * (0:N) / N) -
      a3 * cos(6 * pi * (0:N) / N)
  } else if (wn == "barthannwin") {
    N = n - 1
    w = 0.62 - 0.48 * abs((0:N) / N - 0.5) -
      0.38 * cos(2 * pi * (0:N) / N)
  } else if (wn %in% c("sine", "cosine")) {
    N = n - 1
    w = sin(pi * (0:N) / N)
  } else if (wn == "lanczos") {
    N = n - 1
    x = 2 * (0:N) / N - 1
    w = sin(pi * x) / (pi * x)
    w[x == 0] = 1
  } else {
    stop("Window type '", wn, "' not recognised. See ?winFun for supported types.")
  }

  # normalize
  if (normalize == "amplitude")
    w = w / sum(w)
  if (normalize == "energy")
    w = w / sum(w^2)
  w
}

#' @rdname winFun
#' @param fraction (winFun_tukey) fraction of the window that is tapered (0 =
#'   rectangular, 1 = Hann). Defaults to 0.5.
#' @export
winFun_tukey = function(fraction = 0.5) {
  if (!is.numeric(fraction) || fraction < 0 || fraction > 1)
    stop('fraction must be between 0 and 1')
  function(n) {
    if (fraction <= 0) return(rep(1, n))
    if (fraction >= 1) return(winFun(n, "hann"))
    N = n - 1
    w = rep(1, n)
    half_taper = floor(fraction * N / 2)
    taper_indices = 0:half_taper
    w[taper_indices + 1] = 0.5 * (1 - cos(pi * taper_indices / (fraction * N / 2)))
    # Symmetry
    w[(N - taper_indices + 1)] = w[taper_indices + 1]
    w
  }
}

#' @rdname winFun
#' @param sigma (winFun_gauss) standard deviation in units of half-window
#'   (positive). Defaults to \eqn{1/\sqrt{24} \approx 0.204}, matching the
#'   Praat/seewave Gaussian window.
#' @export
winFun_gauss = function(sigma = 1/sqrt(24)) {
  if (!is.numeric(sigma) || sigma <= 0)
    stop('sigma must be positive')
  function(n) {
    N = n - 1
    x = (0:N) / N - 0.5   # -0.5 to 0.5
    w = exp(-0.5 * (x / sigma)^2)
    w / max(w)            # centre = 1
  }
}

#' @rdname winFun
#' @param alpha (winFun_poisson, winFun_cauchy) shape parameter controlling the
#'   width. Defaults to 1.
#' @export
winFun_poisson = function(alpha = 1) {
  if (!is.numeric(alpha) || alpha < 0)
    stop('alpha must be non-negative')
  function(n) {
    N = n - 1
    x = abs((0:N) - N/2) / (N/2)   # 0 to 1
    w = exp(-alpha * x)
    w / max(w)
  }
}

#' @rdname winFun
#' @param alpha (winFun_poisson, winFun_cauchy) shape parameter controlling the
#'   width. Defaults to 1.
#' @export
winFun_cauchy = function(alpha = 1) {
  if (!is.numeric(alpha) || alpha < 0)
    stop('alpha must be non-negative')
  function(n) {
    N = n - 1
    x = ((0:N) - N/2) / (N/2)       # -1 to 1
    w = 1 / (1 + (alpha * x)^2)
    w / max(w)
  }
}

#' @rdname winFun
#' @param beta (winFun_kaiser) shape parameter controlling the trade-off
#'   between main lobe width and sidelobe level. \code{beta = 0} gives a
#'   rectangular window; \code{beta = 5} is similar to Hamming; \code{beta =
#'   8.6} is similar to Blackman. Defaults to 5.
#' @export
winFun_kaiser = function(beta = 5) {
  if (!is.numeric(beta) || beta < 0)
    stop('beta must be non-negative')
  function(n) {
    N = n - 1
    x = (0:N) / N * 2 - 1   # -1 to 1
    s = sqrt(pmax(0, 1 - x^2))
    w = exp(beta * (s - 1)) *
      besselI(beta * s, 0, expon.scaled = TRUE) /
      besselI(beta, 0, expon.scaled = TRUE)
    w
  }
}
