### MATRIX MATH ###

#' Gaussian smoothing in 2D
#'
#' Takes a matrix of numeric values and smooths it by convolution with a
#' symmetric Gaussian window function. Values outside the matrix are either
#' treated as zero, attenuating the edges, or assumed to continue beyond the
#' edges.
#'
#' @seealso \code{\link{modulationSpectrum}} \code{\link{spectrogram}}
#'
#' @param m input matrix (numeric, on any scale, doesn't have to be square)
#' @param kernelSize vector of size 1 or 2: the size of the Gaussian kernel, in
#'   points (forced to odd values). kernelSize = 0 means no smoothing along that
#'   dimension.
#' @param kernelSD the SD of the Gaussian kernel evaluated over [-1, 1]: for
#'   ex., if kernelSD = 0.5, the kernel spans approximately ±2 SDs
#' @param action 'blur' = kernel-weighted average, 'unblur' = unsharp masking
#' @param amount the amount of residual to mix with the original when
#'   unblurring: \eqn{result = orig + amount * (orig - blurred)}
#' @param padWith how to treat the edges of the matrix: 'repeat' = the edge
#'   rows/columns are assumed to continue beyond the matrix; 'zero' = values
#'   outside the matrix are assumed to be zero, attenuating the smoothed
#'   edges
#' @param plotKernel if TRUE, plots the kernel
#' @return A numeric matrix of the same dimensions as input.
#' @export
#' @examples
#' data('speechEx', package = 'soundgen')
#' s = spectrogram(speechEx, from = 0, to = 1, windowLength = 10,
#'   output = 'original', plot = FALSE)
#' s = log(s + .001)
#' image(t(s))
#' s1 = gaussianSmooth2D(s, kernelSize = 5, plotKernel = TRUE)
#' image(t(s1))
#'
#' # more smoothing in time than in frequency
#' s2 = gaussianSmooth2D(s, kernelSize = c(5, 15))
#' image(t(s2))
#'
#' # vice versa - more smoothing in frequency
#' s3 = gaussianSmooth2D(s, kernelSize = c(25, 3))
#' image(t(s3))
#'
#' # smoothing only in one dimension
#' s4 = gaussianSmooth2D(s, kernelSize = c(25, 0))
#' image(t(s4))
#' s5 = gaussianSmooth2D(s, kernelSize = c(0, 15))
#' image(t(s5))
#'
#' # sharpen the image
#' s6 = gaussianSmooth2D(s, kernelSize = 5, action = 'unblur', amount = .5)
#' image(t(s6))
gaussianSmooth2D = function(m,
                            kernelSize = 5,
                            kernelSD = .5,
                            action = c('blur', 'unblur'),
                            amount = 0.5,
                            padWith = c('repeat', 'zero'),
                            plotKernel = FALSE) {
  # check inputs
  action = match.arg(action)
  padWith = match.arg(padWith)
  if (!is.matrix(m)) stop("'m' must be a matrix")
  if (!is.numeric(m)) stop("'m' must be numeric")
  if (!all(is.finite(m))) stop("'m' must contain only finite values")
  if (length(amount) != 1 || !is.numeric(amount) || !is.finite(amount))
    stop("'amount' must be a finite numeric scalar")
  if (length(kernelSD) != 1 ||
      !is.numeric(kernelSD) ||
      !is.finite(kernelSD) ||
      kernelSD <= 0) {
    stop("'kernelSD' must be a finite positive scalar")
  }
  if (length(kernelSize) == 1) kernelSize = c(kernelSize, kernelSize)
  if (length(kernelSize) != 2) stop("'kernelSize' must have length 1 or 2")

  nr = nrow(m)
  nc = ncol(m)
  if (nrow(m) < 2) return(m)
  if (max(kernelSize) < 2) return(m)
  if (kernelSize[1] >= (nr / 2)) {
    kernelSize[1] = ceiling(nr / 2) - 1
    warning("kernelSize was reduced to fit matrix dimensions")
  }
  if (kernelSize[2] >= (nc / 2)) {
    kernelSize[2] = ceiling(nc / 2) - 1
    warning("kernelSize was reduced to fit matrix dimensions")
  }
  if (kernelSize[1] %% 2 == 0) kernelSize[1] = kernelSize[1] + 1  # make odd
  if (kernelSize[2] %% 2 == 0) kernelSize[2] = kernelSize[2] + 1  # make odd
  kernelSize[1] = min(kernelSize[1], nr)
  kernelSize[2] = min(kernelSize[2], nc)
  if (max(kernelSize) < 2) return(m)

  # set up 2D Gaussian filter
  kernel = getCheckerboardKernel(
    size = kernelSize,
    kernelSD = kernelSD,
    checker = FALSE,
    plot = plotKernel)
  sum_kernel = sum(kernel)
  if (!is.finite(sum_kernel) || sum_kernel == 0)
    stop("Gaussian kernel is degenerate; check 'kernelSD' and 'kernelSize'")
  kernel = kernel / sum_kernel  # convert to probability density function

  # padWith margins: half the (odd) kernel size along each dimension
  ph = (kernelSize[1] - 1) %/% 2
  pw = (kernelSize[2] - 1) %/% 2

  # FFT array size: original matrix + margins on all sides
  out_dim = nextn(c(nr + 2 * ph, nc + 2 * pw))

  # embed the matrix in the center of the FFT array, filling the margins
  # with zeros or with replicated edge rows/columns
  m_pad = matrix(0, out_dim[1], out_dim[2])
  k_pad = matrix(0, out_dim[1], out_dim[2])
  if (padWith == 'repeat') {
    row_idx = c(rep(1, ph), seq_len(nr), rep(nr, ph))
    col_idx = c(rep(1, pw), seq_len(nc), rep(nc, pw))
    m_pad[1:(nr + 2 * ph), 1:(nc + 2 * pw)] = m[row_idx, col_idx, drop = FALSE]
  } else {
    m_pad[(ph + 1):(ph + nr), (pw + 1):(pw + nc)] = m
  }
  k_pad[1:kernelSize[1], 1:kernelSize[2]] = kernel

  # 2D FFT convolution (NB: divide by n as prod(out_dim); the kernel is
  # symmetric, so convolution and correlation coincide)
  full_conv = Re(fft(fft(m_pad) * fft(k_pad), inverse = TRUE)) / prod(out_dim)

  # extract the region in which the kernel overlaps only the matrix and its
  # margins: with the matrix centered, this starts at the kernel size
  r_start = kernelSize[1]
  c_start = kernelSize[2]
  blurred = full_conv[r_start:(r_start + nr - 1), c_start:(c_start + nc - 1)]

  # kernel smoothing / deconvolution
  if (action == 'blur') {
    out = blurred
  } else if (action == 'unblur') {
    # out = m - blurred  # just the residuals
    out = m + amount * (m - blurred)
  }

  if (!is.null(rownames(m))) rownames(out) = rownames(m)
  if (!is.null(colnames(m))) colnames(out) = colnames(m)
  out
}


#' Checkerboard kernel
#'
#' Prepares a matrix \code{size[1] x size[2]} specifying a Gaussian kernel for
#' measuring novelty of self-similarity matrices (always square) or for Gaussian
#' blur. Called by getNovelty() and gaussianSmooth2D().
#' @param size kernel size (points), one or two numbers >= 2; forced to be even if
#'   \code{checker = TRUE}
#' @param kernelMean,kernelSD mean and SD of the Gaussian kernel
#' @param plot if TRUE, shows a perspective plot of the kernel
#' @param checker if TRUE, inverts two quadrants. NB: don't use odd kernel sizes
#'   if checker = TRUE
#' @return A matrix with size[1] rows and size[2] columns. If size has length 1,
#'   a square size by size matrix is returned.
#' @noRd
#' @examples
#' kernel = soundgen:::getCheckerboardKernel(size = 64, kernelSD = 0.1, plot = TRUE)
#' dim(kernel)
#' image(kernel)
#' kernel = soundgen:::getCheckerboardKernel(size = 19, kernelSD = .5,
#'   checker = FALSE, plot = TRUE)
#' kernel = soundgen:::getCheckerboardKernel(size = c(9, 50), kernelSD = .5,
#'   checker = FALSE, plot = TRUE)
getCheckerboardKernel = function(size,
                                 kernelMean = 0,
                                 kernelSD = 0.5,
                                 plot = FALSE,
                                 checker = TRUE) {
  if (checker && any(size %% 2 != 0)) {
    size = ifelse(size %% 2 != 0, size + 1, size)
    warning(paste('kernel size must be even if checker = TRUE; resetting to', size))
  }
  if (length(size) == 1) {
    # a square matrix
    x = seq(-1, 1, length.out = size)
    dx = dnorm(x, mean = kernelMean, sd = kernelSD)
    kernel = outer(dx, dx)

    if (checker) {
      fl_row = floor(size / 2)
      cl_row = cl_col = ceiling(size / 2)
      # quadrant 0 to 3 o'clock
      kernel[seq_len(fl_row), (cl_col + 1):size] =
        -kernel[seq_len(fl_row), (cl_col + 1):size]
      # quadrant 6 to 9 o'clock
      kernel[(cl_row + 1):size, seq_len(cl_col)] =
        -kernel[(cl_row + 1):size, seq_len(cl_col)]
    }
  } else if (length(size) == 2) {
    # not square
    x = seq(-1, 1, length.out = size[1])
    dx = dnorm(x, mean = kernelMean, sd = kernelSD)
    y = seq(-1, 1, length.out = size[2])
    dy = dnorm(y, mean = kernelMean, sd = kernelSD)
    kernel = outer(dx, dy)

    if (checker) {
      fl_row = floor(size[1]/2)
      fl_col = floor(size[2]/2)
      cl_row = ceiling(size[1]/2)
      cl_col = ceiling(size[2]/2)
      kernel[seq_len(fl_row), (cl_col + 1):size[2]] = -kernel[seq_len(fl_row),
                                                              (cl_col + 1):size[2]]
      kernel[(cl_row + 1):size[1], seq_len(cl_col)] = -kernel[(cl_row +
                                                                 1):size[1], seq_len(cl_col)]
    }
  }
  kernel = kernel / max(kernel)

  if (plot) {
    persp(
      kernel,
      theta = -20,
      phi = 25,
      # zlim = c(-1, 4),
      ticktype = 'detailed'
    )
  }
  kernel
}


#' Match number of columns
#' Adds or removes columns of new values (eg zeros or NAs) to a matrix, so that
#' the new number of columns = \code{len}. Rows are left unchanged.
#' @param matrix_short input matrix
#' @param nCol the required number of columns
#' @param padWith the value to pad with, normally \code{0} or \code{NA}
#' @noRd
#' @examples
#' a = matrix(1:9, nrow = 3)
#' soundgen:::matchColumns(a, nCol = 6, padWith = NA, padDir = 'central')
#' soundgen:::matchColumns(a, nCol = 6, padWith = 0, padDir = 'central')
#' soundgen:::matchColumns(a, nCol = 6, padWith = NA, padDir = 'left')
#' soundgen:::matchColumns(a, nCol = 6, padWith = 'a', padDir = 'right')
#' soundgen:::matchColumns(a, nCol = 2)
matchColumns = function (matrix_short,
                         nCol,
                         padWith = 0,
                         padDir = 'central',
                         interpol = c("approx", "spline")) {
  interpol = match.arg(interpol)
  if (!inherits(matrix_short, 'matrix')) stop('input must be a matrix')
  if (ncol(matrix_short) > nCol) {
    # downsample
    new = interpolMatrix(matrix_short, nc = nCol, interpol = interpol)
  } else {
    # pad with 0 / NA / etc
    if (is.null(colnames(matrix_short))) {
      col_short = seq_len(ncol(matrix_short))
    } else {
      col_short = colnames(matrix_short)
    }
    col_long = matchLengths(col_short, nCol,
                            padDir = padDir, padWith = NA)
    new = matrix(padWith,
                 nrow = nrow(matrix_short),
                 ncol = length(col_long))
    colnames(new) = col_long
    # paste the old matrix where it belongs
    new[, !is.na(colnames(new))] = matrix_short
  }
  new
}


#' Log-warp matrix
#'
#' Log-warps a matrix, as if log-transforming plot axes.
#' @param m a matrix of numeric values of any dimensions (not necessarily
#'   square)
#' @param base the base of logarithm
#' @noRd
#' @examples
#' m = matrix(1:90, nrow = 10)
#' colnames(m) = 1:9
#' soundgen:::logMatrix(m, base = 2)
#' soundgen:::logMatrix(m, base = 10)
#'
#' soundgen:::logMatrix(m = matrix(1:9, nrow = 1), base = 2)
#'
#' \dontrun{
#' s = spectrogram(soundgen(), 16000, output = 'original')
#' image(log(soundgen:::logMatrix(s, base = 2))))
#' }
logMatrix = function(m, base = 2) {
  # the key is to make a sequence of time locations within each row/column for
  # interpolation: (1:nrow(m)) ^ base (followed by normalization, so this index
  # will range from 1 to nrow(m) / ncol(m))
  if (ncol(m) > 1) {
    idx_row = (1:ncol(m)) ^ base - 1
    idx_row = idx_row / max(idx_row)
    idx_row = idx_row * (ncol(m) - 1) + 1
    # interpolate rows at these time points
    m1 = t(apply(m, 1, function(x) approx(x, xout = idx_row)$y))
  } else {
    m1 = m
  }

  # same for columns
  if (nrow(m) > 1) {
    idx_col = (1:nrow(m)) ^ base - 1
    idx_col = idx_col / max(idx_col)
    idx_col = idx_col * (nrow(m) - 1) + 1
    # interpolate columns at these time points
    m2 = apply(m1, 2, function(x) approx(x, xout = idx_col)$y)
  } else {
    m2 = m1
  }

  # interpolate row and column names
  # (assuming numeric values, as when called from modulationSpectrum())
  if (!is.null(colnames(m)) && ncol(m) > 1) {
    colnames(m2) = approx(as.numeric(colnames(m)), xout = idx_row)$y
  }
  if (!is.null(rownames(m)) && nrow(m) > 1) {
    rownames(m2) = approx(as.numeric(rownames(m)), xout = idx_col)$y
  }
  m2
}


#' Warp matrix
#' Warps or scales each column of a matrix (normally a spectrogram).
#' @noRd
#' @param m matrix (rows = frequency bins, columns = time)
#' @param scaleFactor 1 = no change, >1 = raise formants
#' @param interpol interpolation method
warpMatrix = function(m, scaleFactor, interpol = 'splineFC') {
  scaleFactor = getSmoothContour(scaleFactor, len = ncol(m))
  if (any(!is.finite(scaleFactor) | scaleFactor <= 0))
    stop('scaleFactor (multFormants) must be positive')
  if (all(scaleFactor == 1)) return(m)
  log_m = log(pmax(m, 1e-10))  # work on a log-scale throughout
  n1 = nrow(m)
  nc = ncol(m)
  m_warped = log_m
  if (any(scaleFactor < 1)) {
    # save some objects here to speed up the loop over columns of m
    seq_len_n1 = seq_len(n1)
  }
  for (i in seq_len(nc)) {
    if (scaleFactor[i] > 1) {
      # "stretch" the vector (eg spectrum of a frame)
      n2 = max(2, round(n1 / scaleFactor[i]))
      seq_len_n2 = seq_len(n2)
      m_warped[, i] = interpolate(x = seq_len_n2, y = log_m[seq_len_n2, i],
                                  xout = seq(1, n2, length.out = n1),
                                  method = interpol)
      # plot(m_warped[, i], type = 'l')
    } else if (scaleFactor[i] < 1) {
      # "shrink" the vector, then extrapolate the log-envelope over the gap
      # left at high frequencies, so that the spectrum keeps decaying at a
      # natural rate instead of being zeroed out
      n2 = max(2, round(n1 * scaleFactor[i]))
      compressed = interpolate(x = seq_len_n1, y = log_m[, i],
                               xout = seq(1, n1, length.out = n2),
                               method = interpol)
      g = n1 - n2
      if (g > 0) {
        # fit a regression to the log-envelope over a window as wide as the
        # gap, immediately below it
        lo = max(1, n2 - g + 1)
        x_fit = lo:n2
        y_fit = compressed[x_fit]
        if (length(x_fit) >= 2) {
          xm = mean(x_fit)
          ym = mean(y_fit)
          slope = min(0, cov(x_fit, y_fit) / var(x_fit))
          # spectra decay toward Nyquist; never extrapolate upward
        } else {
          slope = 0
        }
        # anchor the line at the last value before Nyquist to avoid discontinuities
        padWith = compressed[n2] + slope * seq_len(g)
      } else {
        padWith = numeric(0)
      }
      m_warped[, i] = c(compressed, padWith)
    }
    # plot(abs(log_m[, i]), type = 'l', xlim = c(1, n1))
    # lines(m_warped[, i], col = 'blue')
  }
  # the envelope is a magnitude filter: keep it finite and non-negative
  m_warped = exp(m_warped)
  m_warped[!is.finite(m_warped)] = 0
  m_warped
}


#' rbind_fill
#'
#' Fills missing columns with NAs, then rbinds - handy in case one has extra
#' columns. Used in formant_app(), pitch_app()
#' @param df1,df2 two dataframes with partly matching columns
#' @param x a list of dataframes
#' @noRd
#' @examples
#' df1 = data.frame(a = 1:2, b = letters[1:2])
#' df2 = data.frame(b = "z", c = 3)
#' soundgen:::rbind_fill(df1, df2)
rbind_fill = function(df1, df2) {
  if (!is.data.frame(df1) || nrow(df1) == 0) return(df2)
  if (!is.data.frame(df2) || nrow(df2) == 0) return(df1)
  df1[setdiff(names(df2), names(df1))] = NA
  df2[setdiff(names(df1), names(df2))] = NA
  rbind(df1, df2)
}


#' rbind_fill_list
#' @noRd
#' @examples
#' df1 = data.frame(a = 1:2, b = letters[1:2])
#' df2 = data.frame(b = "z", c = 3)
#' df3 = data.frame(a = 4, d = TRUE)
#' soundgen:::rbind_fill_list(list(df1, df2, df3))
rbind_fill_list = function(x) {
  # empty list, a single dataframe, etc.
  if (length(x) == 0) return(data.frame())
  if (is.data.frame(x)) return(x)
  if (!all(vapply(x, is.data.frame, logical(1)))) {
    stop("All elements in the list must be dataframes.")
  }

  # Extract unique column names in order of their first appearance
  all_cols = unique(unlist(lapply(x, names)))

  # add missing columns and standardize order in each dataframe
  x_filled = lapply(x, function(df) {
    missing_cols = setdiff(all_cols, names(df))
    if (length(missing_cols) > 0) {
      # use rep(NA, nrow(df)) to ensure the new NA columns have the same length
      # as the dataframe (or we might run into trouble with 0-row dataframes)
      na_list = rep(list(rep(NA, nrow(df))), length(missing_cols))
      df[missing_cols] = na_list
    }

    # reorder the columns to match the standard order
    df[all_cols]
  })

  # bind all standardized dataframes together
  do.call(rbind, x_filled)
}


#' Interpolate and combine matrices
#'
#' Takes a list of matrices (normally, modulation spectra), aligns them by their
#' physical axis labels (rownames and colnames), interpolates them to a common
#' target grid, and then reduces them (e.g., takes the sum).
#'
#' The target grid along each axis is regenerated as a proper DFT grid: the
#' physical "sampling rate" of the axis is inferred from each input matrix as
#' (label spacing * n) - the same for all chunks, regardless of their
#' dimensions - and the target labels are then recalculated for the chosen
#' target dimensions using the same convention as \code{centerMS()}. This
#' guarantees that a zero bin is always present and that bin widths are
#' physically meaningful. If the input labels are all non-negative (uncentered
#' DFT order), the target grid is likewise uncentered; if the inputs do not
#' look like a DFT grid, the labels fall back to a linear grid between the
#' extremes.
#'
#' To avoid "zero-bin leaking" (smearing of the DC component during
#' interpolation), any axis that crosses zero is split into negative and
#' positive halves, which are interpolated separately and then recombined.
#' Values outside the original range of a matrix are treated as zero (i.e.,
#' padded with 0).
#'
#' @param mat_list a list of matrices to aggregate (e.g., spectrograms or
#'   modulation spectra). All matrices must have numeric rownames and colnames.
#' @param rFun,cFun functions used to determine the number of rows and columns
#'   in the target grid (e.g., 'max', 'median').
#' @param reduceFun function used to aggregate the aligned matrices (e.g., '+').
#' @noRd
#' @examples
#' # Two matrices with different physical ranges and resolutions
#' m1 = matrix(1:15, nrow = 3)
#' rownames(m1) = c(-1, 0, 1); colnames(m1) = c(-10, 0, 10, 20, 30)
#'
#' m2 = matrix(101:125, nrow = 5)
#' rownames(m2) = c(-2, -1, 0, 1, 2); colnames(m2) = c(-20, -10, 0, 10, 20)
#'
#' # sum them on a common grid
#' res = soundgen:::reduceMatrices(list(m1, m2), rFun = 'max', cFun = 'max')
#' image(t(res))
#'
#' # note how the zero bin is preserved and values outside the original
#' # ranges are padded with 0
#'
#' # single-row case
#' reduceMatrices(list(m1[1, , drop = FALSE], m2[1, , drop = FALSE]))
#'
#' # other reduceFun's. NB: reduceFun(max, ...) just returns global max!
#' reduceFun = function(x, y) pmax(x, y, na.rm = TRUE)
#' soundgen:::reduceMatrices(list(m1, m2), reduceFun = reduceFun)
#' reduceFun = function(x, y) pmin(x, y, na.rm = TRUE)  # same for min
reduceMatrices = function(mat_list,
                          rFun = 'max',
                          cFun = 'median',
                          reduceFun = '+') {
  if (length(mat_list) == 0) return(NULL)
  mat_list = mat_list[!sapply(mat_list, is.null)]
  if (length(mat_list) == 0) return(NULL)

  # basic checks for valid numeric dimnames
  valid_dims = sapply(mat_list, function(m) {
    is.matrix(m) && !is.null(rownames(m)) && !is.null(colnames(m)) &&
      !any(is.na(as.numeric(rownames(m)))) && !any(is.na(as.numeric(colnames(m))))
  })
  if (!all(valid_dims)) {
    idx_drop = which(!valid_dims)
    mat_list = mat_list[-idx_drop]
    warning(paste("Matrices must have valid numeric rownames and colnames for physical alignment.",
                  "Dropping matrices number:", idx_drop))
    if (length(mat_list) == 0) return(NULL)
  }

  # calculate target dimensions
  nr = max(1, round(do.call(rFun, list(unlist(lapply(mat_list, nrow))))))
  nc = max(1, round(do.call(cFun, list(unlist(lapply(mat_list, ncol))))))

  # Regenerate physically correct target labels. For a DFT grid (centered or
  # not), label spacing * n = the sampling rate of the axis, which is the same
  # for all chunks regardless of their dimensions. The target labels are then
  # recalculated for the target n with the centerMS() convention.
  targetLabels = function(label_list, dim_list, n) {
    all_labels = unlist(label_list)

    if (n == 1) {
      # Degenerate one-bin grid. Keep zero if present; otherwise use the
      # ordinary fallback. This also avoids the 2:1 sequence problem below.
      if (any(all_labels == 0)) return(0)
      return(seq(min(all_labels), max(all_labels), length.out = 1))
    }

    sr = suppressWarnings(median(sapply(seq_along(label_list), function(k) {
      lab = sort(unique(label_list[[k]]))
      if (length(lab) < 2) return(NA)
      w = median(diff(lab))
      if (!is.finite(w) || w <= 0) return(NA)
      w * dim_list[k]
    }), na.rm = TRUE))

    # Fallback for non-DFT grids: linear grid between the extremes
    if (!is.finite(sr) || sr <= 0)
      return(seq(min(all_labels), max(all_labels), length.out = n))

    x = (0:(n - 1)) * (sr / n)
    centered = any(all_labels < 0) && any(all_labels > 0)

    if (!centered) {  # uncentered DFT order
      return(x)
    } else if (n %% 2 == 0) {  # centered, even: Nyquist labeled negative
      i = n %/% 2
      neg = if (i >= 2) rev(-x[2:i]) else numeric(0)
      return(c(-x[i + 1], neg, x[1:i]))
    } else {  # centered, odd: symmetric
      i = n %/% 2
      return(c(rev(-x[2:(i + 1)]), x[1:(i + 1)]))
    }
  }

  r_list = lapply(mat_list, function(m) as.numeric(rownames(m)))
  c_list = lapply(mat_list, function(m) as.numeric(colnames(m)))
  R_target = targetLabels(r_list, unlist(lapply(mat_list, nrow)), nr)
  C_target = targetLabels(c_list, unlist(lapply(mat_list, ncol)), nc)

  # combine matrices
  mat_list_aligned = lapply(mat_list, function(m) {
    r = as.numeric(rownames(m))
    c = as.numeric(colnames(m))

    # interpolate rows
    m1 = interp_axis(r, R_target, m, axis = 1)
    rownames(m1) = R_target
    colnames(m1) = c

    # interpolate cols
    m2 = interp_axis(c, C_target, m1, axis = 2)
    rownames(m2) = R_target
    colnames(m2) = C_target

    # replace NAs (values outside original range) with 0
    m2[is.na(m2)] = 0
    m2
  })

  Reduce(reduceFun, mat_list_aligned)
}


#' Interpolate axis
#' Internal helper function for reduceMatrices(). Interpolate one axis,
#' splitting at 0 if both original and target cross it.
#' @noRd
interp_axis = function(vals, target_vals, mat, axis = 1) {
  if (is.unsorted(vals)) {
    ord = order(vals)
    vals = vals[ord]
    if (axis == 1) mat = mat[ord, , drop = FALSE] else mat = mat[, ord, drop = FALSE]
  }
  crosses_zero = (min(vals) < 0 && max(vals) > 0) &&
    (min(target_vals) < 0 && max(target_vals) > 0)
  if (crosses_zero) {
    idx0 = which.min(abs(vals))
    idx0_t = which.min(abs(target_vals))

    v_neg = vals[1:idx0]
    v_pos = vals[idx0:length(vals)]
    t_neg = target_vals[1:idx0_t]
    t_pos = target_vals[idx0_t:length(target_vals)]

    if (axis == 1) {
      m_neg = mat[1:idx0, , drop = FALSE]
      m_pos = mat[idx0:nrow(mat), , drop = FALSE]

      res_neg = matrix(NA, nrow = length(t_neg), ncol = ncol(mat))
      res_pos = matrix(NA, nrow = length(t_pos), ncol = ncol(mat))

      for (j in seq_len(ncol(mat))) {
        res_neg[, j] = interp_1d(v_neg, t_neg, m_neg[, j])
        res_pos[, j] = interp_1d(v_pos, t_pos, m_pos[, j])
      }
      res = rbind(res_neg, res_pos[-1, , drop = FALSE])
    } else {
      m_neg = mat[, 1:idx0, drop = FALSE]
      m_pos = mat[, idx0:ncol(mat), drop = FALSE]

      res_neg = matrix(NA, nrow = nrow(mat), ncol = length(t_neg))
      res_pos = matrix(NA, nrow = nrow(mat), ncol = length(t_pos))

      for (i in seq_len(nrow(mat))) {
        res_neg[i, ] = interp_1d(v_neg, t_neg, m_neg[i, ])
        res_pos[i, ] = interp_1d(v_pos, t_pos, m_pos[i, ])
      }
      res = cbind(res_neg, res_pos[, -1, drop = FALSE])
    }
  } else {
    if (axis == 1) {
      res = matrix(NA, nrow = length(target_vals), ncol = ncol(mat))
      for (j in seq_len(ncol(mat))) {
        res[, j] = interp_1d(vals, target_vals, mat[, j])
      }
    } else {
      res = matrix(NA, nrow = nrow(mat), ncol = length(target_vals))
      for (i in seq_len(nrow(mat))) {
        res[i, ] = interp_1d(vals, target_vals, mat[i, ])
      }
    }
  }
  res
}


#' Interpolate 1d
#' Internal helper function for reduceMatrices(). NA-safe, complex-aware 1-D
#' interpolation. Points outside the range of v are returned as NA (they are
#' later replaced by 0 in reduceMatrices()); if fewer than two usable (v, y)
#' pairs are available, the whole result is NA.
#' @noRd
interp_1d = function(v, t, y) {
  isComplex = is.complex(y)
  ok = is.finite(v) &
    if (isComplex) is.finite(Re(y)) & is.finite(Im(y)) else is.finite(y)
  n_ok = sum(ok)

  # No usable points (e.g., an all-NA row left over from the previous
  # interpolation pass, or a degenerate axis): return NAs
  if (n_ok < 1) {
    return(if (isComplex) {
      complex(real = rep(NA, length(t)), imaginary = rep(NA, length(t)))
    } else {
      rep(NA, length(t))
    })
  } else if (n_ok == 1) {
    # A single usable point: just repeat it
    out = rep(y[ok], length(t))
    return(out)
  }

  if (isComplex) {
    # approx() cannot handle complex numbers: interpolate Re and Im separately
    re = approx(v[ok], Re(y)[ok], xout = t, rule = 1)$y
    im = approx(v[ok], Im(y)[ok], xout = t, rule = 1)$y
    complex(real = re, imaginary = im)
  } else {
    approx(v[ok], y[ok], xout = t, rule = 1)$y
  }
}
