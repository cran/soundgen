### INTERPOLATION ###

#' Interpolation
#'
#' Interpolates unevenly spaced points into a relatively smooth curve. If the
#' points are evenly spaced but there are missing values and/or aliasing should
#' be avoided, use \link{resample} instead.
#'
#' @seealso \link{interpolateNA} \link{resample}
#'
#' @param x,y numeric vectors giving the coordinates of the points to be
#'   interpolated (no NAs)
#' @param xout numeric vector of target x‑coordinates where interpolation is to
#'   take place
#' @param method interpolation method to use. Accepts either a character string
#'   naming an inbuilt method (see "Interpolation methods" below) or a
#'   constructor function such as \code{interpol_loess(span = 0.3)} or a custom
#'   function (see examples). The default is \code{"splineFC"}. When calling
#'   \code{interpolate()} directly, a string plus method‑specific arguments in
#'   \code{...} is the simplest option. When passing \code{method} through a
#'   higher‑level function (e.g. \code{resample()}), use a constructor because
#'   it packages all method‑specific arguments into a single object
#' @param ... extra arguments specific to the chosen interpolation method, e.g.
#'   \code{span = 0.7} for \code{"loess"} (see "Interpolation methods" for
#'   per‑method arguments). Used only when \code{method} is a character string;
#'   when \code{method} is a constructor function, parameters are passed to the
#'   constructor directly. Do not pass graphical arguments here
#' @param plot logical; if TRUE, a quick diagnostic plot is drawn
#'
#' @return A numeric vector of interpolated y‑values at the requested \code{xout}
#'   locations.
#' @export
#'
#' @section Interpolation methods:
#' \describe{
#'   \item{constant}{Constant interpolation via \code{\link[stats]{approx}}. Fast,
#'   but no smoothing.}
#'   \item{linear aka approx}{Linear interpolation via \code{\link[stats]{approx}}.
#'   Fast, but not smooth.}
#'   \item{spline}{Cubic spline interpolation (FMM method) via
#'   \code{\link[stats]{spline}}. Fast, but overshoots.}
#'   \item{splineFC}{Monotone cubic interpolation using the Fritsch‑Carlson
#'   method (see \code{\link[stats]{splinefun}}). Moderately fast, less
#'   overshooting than the FMM spline.}
#'   \item{approxLowPass}{Linear interpolation followed by low‑pass filtering.
#'   Fast, smooth, but reduced range compared to original y. Constructor:
#'   \code{interpol_approxLowPass(bandwidth)}.}
#'   \item{sgolay}{Linear interpolation followed by Savitzky‑Golay smoothing
#'   (see \code{\link[signal]{sgolayfilt}}). Fairly similar to approxLowPass,
#'   but much slower. Constructor: \code{interpol_sgolay(p, n)}.}
#'   \item{pchip}{Piecewise Cubic Hermite Interpolating Polynomial (preserves
#'   monotonicity). Calls \code{\link[signal]{interp1}} with \code{method = "pchip"}.}
#'   \item{cosine}{Cosine‑eased (smoothstep) interpolation. Eases between
#'   anchor points using a cosine curve. Fast, passes exactly through input
#'   points.}
#'   \item{cardinal}{Cardinal spline interpolation (a generalization of
#'   Catmull‑Rom). Fast, can be forced to pass exactly through input points.
#'   Constructor: \code{interpol_cardinal(tension)}.}
#'   \item{hermite}{Hermite spline interpolation with forced zero slope at
#'   local extrema to prevent overshoot. Fast, passes exactly through input
#'   points.}
#'   \item{loess}{Locally estimated scatterplot smoothing (LOESS) via
#'   \code{\link[stats]{loess}}. Smooth, but slow; may overshoot. Constructor:
#'   \code{interpol_loess(span)}.}
#' }
#'
#' @examples
#' x  = c(0, .15, .2, .3, .7, 1)
#' y  = c(360, 116, 550, 350, 700, 610)
#' xout = seq(0, 1, length.out = 100)
#'
#' # Compare inbuilt interpolation methods
#' ms = c('constant', 'linear', 'spline', 'splineFC', 'approxLowPass',
#'         'sgolay', 'cosine', 'cardinal', 'hermite', 'loess')
#' op = par(c('mfrow', 'mar')); par(mfrow = c(4, 3), mar = c(2, 2, 3, 1))
#' for (m in ms) {interpolate(x, y, xout, method = m, plot = TRUE); title(m)}
#' par(op)
#'
#' # Passing method‑specific parameters via ...
#' interpolate(x, y, xout, method = 'cardinal', tension = 0, plot = TRUE)
#' interpolate(x, y, xout, method = 'loess', span = 0.2, plot = TRUE)
#' interpolate(x, y, xout, method = 'loess', span = 0.9, plot = TRUE)
#'
#' # Equivalent: passing a constructor (useful when forwarding through
#' # higher‑level functions like resample())
#' interpolate(x, y, xout, method = interpol_cardinal(tension = 0), plot = TRUE)
#' interpolate(x, y, xout, method = interpol_loess(span = 0.2), plot = TRUE)
#'
#' # Passing a fully custom function
#' interpolate(x, y, xout, plot = TRUE,
#'   method = function(...) spline(x, y, xout = xout, method = 'natural')$y)
interpolate = function(
    x, y, xout,
    method = c('splineFC', 'spline', 'constant', 'linear', 'approxLowPass',
               'sgolay', 'pchip', 'cosine', 'cardinal', 'hermite', 'loess'),
    plot = FALSE,
    ...) {
  len_x = length(x)
  if (len_x != length(y)) stop('x and y must have the same length')
  len_out = length(xout)
  if (len_out == 0) return(numeric(0))

  if (is.function(method)) {
    # method is a function
    out = try(method(x, y, xout))
    if (inherits(out, 'try-error')) return(NA)
  } else if (is.character(method)) {
    if (method == 'approx') method = 'linear'
    method = match.arg(method)

    # defaults
    all_args = list(...) #  match.call() doesn't evaluate the args, eg tension = my_var doesn't work
    all_arg_names = names(all_args)
    if (method == 'approxLowPass') {
      bandwidth = if ('bandwidth' %in% all_arg_names) all_args$bandwidth else len_x / len_out
    } else if (method == 'sgolay') {
      p = if ('p' %in% all_arg_names) all_args$p else 3
      n = if ('n' %in% all_arg_names) all_args$n else floor(len_out / len_x / 2) * 2 + 1
    } else if (method == 'cardinal') {
      tension = if ('tension' %in% all_arg_names) all_args$tension else 0.5
    } else if (method == 'loess') {
      span = if ('span' %in% all_arg_names) all_args$span else 0.5
    }

    # list inbuilt methods
    method_fun = switch(
      method,
      "constant" = function(x, y, xout) approx(x, y, xout, method = 'constant')$y,
      "linear" = function(x, y, xout) approx(x, y, xout = xout, method = 'linear')$y,
      "spline" = function(x, y, xout) spline(x, y, xout = xout, method = 'fmm')$y,
      "splineFC" = function(x, y, xout) splinefun(x, y, method = "monoH.FC")(xout),
      "approxLowPass" = interpol_approxLowPass(bandwidth = bandwidth),
      "sgolay" = interpol_sgolay(p = p, n = n),
      "pchip" = function(x, y, xout) signal::interp1(x, y, xout, method = 'pchip'),
      "cosine" = interpol_cosine(),
      "cardinal" = interpol_cardinal(tension = tension),
      "hermite" = interpol_hermite(),
      "loess" = interpol_loess(span = span)
    )

    # Pass fixed arguments plus any user‑supplied ... arguments
    # out = try(do.call(methods[[method]], c(list(x = x, y = y, xout = xout), list(...))))
    out = try(do.call(method_fun, c(list(x = x, y = y, xout = xout))))
    if (inherits(out, 'try-error')) return(NA)
  }

  if (plot) {
    plot(xout, out, type = 'l', ylim = range(c(out, y)), xlab = 'x', ylab = 'y')
    points(x, y, col = 'blue')
  }

  invisible(out)
}

#' @rdname interpolate
#' @param bandwidth (interpol_approxLowPass) the amount of smoothing, a number
#'   between 0 and 1: close to 0 = more smoothing, close to 1 = less smoothing.
#'   Defaults to \code{len_x / len_out} when called via the string dispatch of
#'   \code{interpolate()}.
#' @export
interpol_approxLowPass = function(bandwidth = 0.1) {
  if (!is.numeric(bandwidth) || bandwidth < 0 || bandwidth > 1)
    stop('bandwidth must be between [0, 1]')
  function(x, y, xout) {
    a = approx(x, y, xout = xout)$y
    pitchSmoothPraat(a, samplingRate = 1, bandwidth = bandwidth)
  }
}

#' @rdname interpolate
#' @param p (interpol_sgolay) polynomial order for Savitzky‑Golay smoothing
#'   (positive integer, defaults to 3).
#' @param n (interpol_sgolay) filter length for Savitzky‑Golay smoothing (odd
#'   positive integer, defaults to \code{floor(len_out / len_x / 2) * 2 + 1}
#'   when called via the string dispatch of \code{interpolate()}).
#' @export
interpol_sgolay = function(p = 3, n = 15) {
  function(x, y, xout) {
    a = approx(x, y, xout = xout)$y
    signal::sgolayfilt(a, p = p, n = n)
  }
}

#' @noRd
interpol_cosine = function() {
  function(x, y, xout) {
    # Find which segment each xout falls in
    idx = pmax(1, findInterval(xout, x, rightmost.closed = TRUE))
    idx = pmin(idx, length(x) - 1) # clamp at boundaries
    tt = (xout - x[idx]) / (x[idx + 1] - x[idx])
    # Cosine ease-in-out: (1 - cos(pi*t)) / 2
    mu = (1 - cos(pi * tt)) / 2
    y[idx] * (1 - mu) + y[idx + 1] * mu
  }
}

#' @rdname interpolate
#' @param tension (interpol_cardinal) a number between 0 and 1 controlling the
#'   tightness of the cardinal spline: \code{tension = 0} gives Catmull‑Rom
#'   (smooth but may overshoot), \code{tension = 0.5} is a good balance, and
#'   \code{tension = 1} makes the curve pass through midpoints between the
#'   input points. Defaults to 0.5.
#' @export
interpol_cardinal = function(tension = 0.5) {
  if (!is.numeric(tension) || tension < 0 || tension > 1)
    stop('tension must be between [0, 1]')
  function(x, y, xout) {
    n = length(x)
    c = tension
    # Estimate tangent at each anchor using central differences
    # Endpoints use one-sided differences
    m = numeric(n)
    m[1] = (1 - c) * (y[2] - y[1]) / (x[2] - x[1])
    m[n] = (1 - c) * (y[n] - y[n-1]) / (x[n] - x[n-1])
    if (n > 2) {
      for (i in 2:(n-1)) {
        m[i] = (1 - c) * (y[i+1] - y[i-1]) / (x[i+1] - x[i-1])
      }
    }
    idx = pmax(1, findInterval(xout, x, rightmost.closed = TRUE))
    idx = pmin(idx, n - 1)
    # Normalize t to [0, 1] within each segment
    t = (xout - x[idx]) / (x[idx + 1] - x[idx])
    t2 = t * t
    t3 = t2 * t
    # Scale tangents by segment width
    dx = x[idx + 1] - x[idx]
    # Cubic Hermite basis
    h00 = 2*t3 - 3*t2 + 1
    h10 = t3 - 2*t2 + t
    h01 = -2*t3 + 3*t2
    h11 = t3 - t2
    h00 * y[idx] + h10 * dx * m[idx] + h01 * y[idx + 1] + h11 * dx * m[idx + 1]
  }
}

#' @noRd
interpol_hermite = function() {
  function(x, y, xout) {
    n = length(x)
    m = numeric(n)
    # Calculate slopes (m)
    if (n > 2) {
      for (i in 2:(n-1)) {
        # If the point is a local extremum, force the derivative to 0
        if ((y[i] - y[i-1]) * (y[i+1] - y[i]) <= 0) {
          m[i] = 0
        } else {
          # Otherwise, use central difference for smoothness
          m[i] = (y[i+1] - y[i-1]) / (x[i+1] - x[i-1])
        }
      }
    }

    # Simple end conditions
    if (n >= 2) {
      m[1] = (y[2] - y[1]) / (x[2] - x[1])
      m[n] = (y[n] - y[n-1]) / (x[n] - x[n-1])
    }

    # Cubic Hermite Evaluation
    out = numeric(length(xout))
    for (i in 1:(n-1)) {
      idx = which(xout >= x[i] & xout < x[i+1])
      if (length(idx) > 0) {
        dx = x[i+1] - x[i]
        t = (xout[idx] - x[i]) / dx
        # Hermite basis functions
        h00 = 2*t^3 - 3*t^2 + 1
        h10 = t^3 - 2*t^2 + t
        h01 = -2*t^3 + 3*t^2
        h11 = t^3 - t^2
        out[idx] = h00 * y[i] + h10 * dx * m[i] + h01 * y[i+1] + h11 * dx * m[i+1]
      }
    }
    # Handle the final point
    if (max(xout) >= x[n]) out[xout >= x[n]] = y[n]
    out
  }
}

#' @rdname interpolate
#' @param span (interpol_loess) the amount of LOESS smoothing, a number between
#'   0 and 1: a larger \code{span} gives more smoothing, a smaller \code{span}
#'   captures finer detail but may overfit. Defaults to 0.5.
#' @export
interpol_loess = function(span = 0.5) {
  if (!is.numeric(span) || span < 0 || span > 1)
    stop('span must be between [0, 1]')
  function(x, y, xout) {
    if (length(x) < 2) return(rep(y[1], length(xout)))
    l = suppressWarnings(loess(y ~ x, span = span))
    smoothContour = try(predict(l, xout), silent = TRUE)
    while(inherits(smoothContour, 'try-error') && span < 1) {
      span = span + 0.1
      l = suppressWarnings(loess(y ~ x, span = span))
      smoothContour = try(predict(l, xout), silent = TRUE)
    }
    if (inherits(smoothContour, 'try-error')) return(rep(y, length.out = length(xout)))
    smoothContour
  }
}


#' Interpolate NAs
#'
#' Takes a numeric vector, such as a pitch contour, and fills in the NAs, first
#' by linear interpolation in the middle and then by constant or linear
#' interpolation at the ends.
#' @param x numeric vector
#' @param idx_na which(is.na(x))
#' @param nPoints the number of points to use for interpolating leading and
#'   trailing NAs: 1 = constant interpolation, 2 = use the first two non-NAs at
#'   the beginning and the last two non-NAs at the end (possibly after
#'   interpolating NAs in the middle), etc.
#' @return The input numeric vector with NAs filled in by interpolation.
#' @export
#' @examples
#' a = c(NA, 405, NA, 460, NA, NA, NA, 480, 490, NA, NA)
#' interpolateNA(a)
#' interpolateNA(a, nPoints = 3)
#' plot(interpolateNA(a), type = 'l', col = 'blue'); points(a)
#' plot(interpolateNA(a, nPoints = 3), type = 'l', col = 'blue'); points(a)
#' # Compare to approx - terminal NAs are simply trimmed
#' approx(a, na.rm = TRUE, n = length(a))$y
interpolateNA = function(x, idx_na = NULL, nPoints = 1) {
  len = length(x)
  if (is.null(idx_na)) idx_na = which(is.na(x))
  n_na = length(idx_na)
  if (n_na == len || n_na == 0) return(x)
  idx_notNA = (1:len)[-idx_na]
  len_notNA = length(idx_notNA)
  nPoints = min(nPoints, len_notNA)

  # fill in NAs in the middle by linear interpolation
  if (n_na > 0) {
    idx_center = idx_notNA[1] : idx_notNA[len_notNA]
    if (length(idx_center) > 1) {
      xc = try(approx(x[idx_center], n = length(idx_center), na.rm = TRUE)$y,
               silent = TRUE)
      if (!inherits(xc, 'try-error')) x[idx_center] = xc
    }
    first_nonNA = idx_notNA[1]
    last_nonNA = idx_notNA[len_notNA]

    # fill in NAs at the ends
    if (nPoints == 1) {
      # constant interpolation
      if (idx_notNA[1] > 1) {
        x[1:(idx_notNA[1] - 1)] = x[first_nonNA]
      }

      if (last_nonNA < len) {
        x[(last_nonNA + 1):len] = x[last_nonNA]
      }
    } else {
      # linear interpolation of NAs at the beg & end
      if (first_nonNA > 1) {
        a = first_nonNA : min(len, (first_nonNA + nPoints - 1))
        b = x[a]
        slope = cov(a, b) / var(a)  # like lm(), but >10 times faster
        intercept = mean(b) - slope * mean(a)
        idx_fna = 1:(first_nonNA - 1)
        x[idx_fna] = intercept + slope * idx_fna
      }
      if (last_nonNA < len) {
        a = max(1, (last_nonNA - nPoints + 1)) : last_nonNA
        b = x[a]
        slope = cov(a, b) / var(a)
        intercept = mean(b) - slope * mean(a)
        idx_lnn = (last_nonNA + 1):len
        x[idx_lnn] = intercept + slope * idx_lnn
      }
    }
  }
  x
}


#' Interpolate matrix
#'
#' Performs a chosen type of separable interpolation across both rows and
#' columns of a matrix, in effect up- or downsampling a matrix to required
#' dimensions. Rownames and colnames are also interpolated as needed. Make sure
#' there are no NAs in the input to avoid unpredictable behavior (dropped by
#' approx and spline, propagated by resample).
#' @param m input matrix of numeric values
#' @param nr,nc target dimensions
#' @param interpol interpolation method ('approx' for linear, 'spline' for
#'   spline, 'resample' for more nuanced resampling with low-pass filtering).
#'   NB: only linear interpolation is implemented for matrices of complex
#'   numbers
#' @keywords internal
#' @examples
#' m = matrix(1:12 + rnorm(12, 0, .2), nrow = 3)
#' rownames(m) = 1:3; colnames(m) = 1:4
#' soundgen:::interpolMatrix(m)  # just returns the original
#' soundgen:::interpolMatrix(m, nr = 10, nc = 7)
#' soundgen:::interpolMatrix(m, nr = 10, nc = 7, interpol = 'spline')
#' soundgen:::interpolMatrix(m, nr = 2, nc = 7)
#' soundgen:::interpolMatrix(m, nr = 2, nc = 3)
#'
#' # input matrices can have a single row/column
#' soundgen:::interpolMatrix(matrix(1:5, nrow = 1), nc = 9)
#' soundgen:::interpolMatrix(matrix(1:5, ncol = 1), nr = 5, nc = 3)
#'
#' # use resample() with a low-pass filter to avoid artifacts
#' a = matrix(c(rep(0, 10), 1, rep(0, 10)))
#' soundgen:::interpolMatrix(a, nr = 5)  # ok
#' soundgen:::interpolMatrix(a, nr = 6)  # spike lost
#' soundgen:::interpolMatrix(a, nr = 6, interpol = 'resample')  # ok
#'
#' # complex values
#' cm = matrix(complex(real = 1:9, imaginary = 1:9), nrow = 3)
#' soundgen:::interpolMatrix(cm, nr = 5)
interpolMatrix = function(m,
                          nr = NULL,
                          nc = NULL,
                          interpol = c("approx", "spline", "resample")) {
  interpol = match.arg(interpol)
  if (!is.matrix(m)) {
    m = as.matrix(m)
    warning('non-matrix input m: converting to matrix')
  }
  nr0 = nrow(m)
  nc0 = ncol(m)
  if (is.null(nr)) nr = nr0
  if (is.null(nc)) nc = nc0
  if (nr == nr0 && nc == nc0) return(m)
  # if (nr < 2) stop('nr must be >1')
  # if (nc < 2) stop('nc must be >1')
  isComplex = is.complex(m)

  # Interpolate rows if necessary
  if (nr0 != nr) {
    if (nr0 == 1) {
      temp = matrix(rep(m, nr), nrow = nr, byrow = TRUE)
    } else {
      temp = matrix(1, nrow = nr, ncol = nc0)
      for (c in seq_len(nc0)) {
        col_c = m[, c]
        if (!any(!is.na(col_c))) {
          temp[, c] = rep(NA, nr)
          next
        }
        if (isComplex) {
          # approx doesn't work with complex numbers properly, so we treat the
          # Re and Im parts separately
          temp_re = approx(Re(col_c), n = nr)$y
          temp_im = approx(Im(col_c), n = nr)$y
          temp[, c] = complex(real = temp_re, imaginary = temp_im)
        } else {
          if (interpol == 'resample') {
            temp[, c] = .resample(list(sound = col_c), len = nr)
          } else {
            temp[, c] = do.call(interpol, list(x = col_c, n = nr))$y
          }
        }
      }
    }
    if (!is.null(rownames(m))) {
      rnms = as.numeric(rownames(m))
      if (nr0 == 1) {
        rownames(temp) = rep(rnms, nr)
      } else {
        try_rownames = try(approx(rnms, n = nr)$y, silent = TRUE)
        if (!inherits(try_rownames, 'try-error')) {
          rownames(temp) = try_rownames
        }
      }
    }
  } else {
    temp = m
    rownames(temp) = rownames(m)
  }
  colnames(temp) = colnames(m)

  # Interpolate columns if necessary
  if (nc0 != nc) {
    if (nc0 == 1) {
      out = matrix(rep(temp[, 1], nc), ncol = nc, byrow = FALSE)
    } else {
      out = matrix(1, nrow = nr, ncol = nc)
      for (r in seq_len(nr)) {
        row_r = temp[r, ]
        if (!any(!is.na(row_r))) {
          out[r, ] = rep(NA, nc)
          next
        }
        if (isComplex) {
          temp_re = approx(Re(row_r), n = nc)$y
          temp_im = approx(Im(row_r), n = nc)$y
          out[r, ] = complex(real = temp_re, imaginary = temp_im)
        } else {
          if (interpol == 'resample') {
            out[r, ] = .resample(list(sound = row_r), len = nc)
          } else {
            out[r, ] = do.call(interpol, list(x = row_r, n = nc))$y
          }
        }
      }
    }
    if (!is.null(colnames(m))) {
      cnms = as.numeric(colnames(m))
      if (nc0 == 1) {
        colnames(out) = rep(cnms, nc)
      } else {
        try_colnames = try(approx(cnms, n = nc)$y, silent = TRUE)
        if (!inherits(try_colnames, 'try-error')) {
          colnames(out) = try_colnames
        }
      }

    }
  } else {
    out = temp
    colnames(out) = colnames(temp)
  }
  rownames(out) = rownames(temp)
  out
}
