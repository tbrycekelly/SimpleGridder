#' @title Build Grid
#' @author Thomas Bryce Kelly
#' @keywords Gridding
#' @export

buildGrid = function(xlim,
                     ylim,
                     nx = NULL,
                     ny = NULL,
                     x.scale = NULL,
                     y.scale = NULL,
                     x.factor = NULL,
                     y.factor = NULL) {

  ## Determine grid to use:
  meta = list()

  meta$xlim = xlim
  meta$ylim = ylim

  if (is.null(x.scale) & !is.null(nx)) {
    x.scale = (xlim[2] - xlim[1]) / (nx-1)
  } else {
    stop('Error, either nx or x.scale need to be provided.')
  }

  if (is.null(y.scale) & !is.null(ny)) {
    y.scale = (ylim[2] - ylim[1]) / (ny-1)
  } else {
    stop('Error, either ny or y.scale need to be provided.')
  }

  meta$x.scale = x.scale
  meta$y.scale = y.scale

  ## Rescale x and y based on x.factor and y.factor
  if (is.null(x.factor)) { x.factor = (y.scale/x.scale + 1/2) }
  if (is.null(y.factor)) { y.factor = (x.scale/y.scale + 1/2) }
  meta$x.factor = x.factor
  meta$y.factor = y.factor

  y.new = seq(ylim[1], ylim[2], by = y.scale)
  x.new = seq(xlim[1], xlim[2], by = x.scale)
  nx = length(x.new)
  ny = length(y.new)
  meta$nx = nx
  meta$ny = ny

  ## Make grid and fill in
  grid = expand.grid(x = x.new, y = y.new)

  ## Construct return object
  grid = list(x = x.new,
              y = y.new,
              data = list(),
              interp = list(),
              grid = list(x = matrix(x.new, nrow = nx, ncol = ny),
                          y = t(matrix(y.new, nrow = ny, ncol = nx))),
              meta = meta
  )

  ## Return
  grid
}
