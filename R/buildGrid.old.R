#' @title Build Grid
#' @author Thomas Bryce Kelly
#' @keywords Gridding

buildGrid.old = function(x,
                     y,
                     z,
                     nx = 50,
                     ny = 50,
                     x.scale = NULL,
                     y.scale = NULL,
                     gridder = gridWeighted,
                     grid = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     x.factor = NULL,
                     y.factor = NULL,
                     neighborhood = 20) {

  ## Get Data ready
  # Remove NAs (they poison everything)
  l = !is.na(x) & !is.na(y) & !is.na(z)
  x = x[l]
  y = y[l]
  z = z[l]

  neighborhood = min(neighborhood, length(x))

  ## Determine grid to use:
  meta = list()

  if (is.null(xlim)) { xlim = range(pretty(x)) }
  if (is.null(ylim)) { ylim = range(pretty(y)) }
  meta$xlim = xlim
  meta$ylim = ylim

  if (is.null(x.scale)) { x.scale = (xlim[2] - xlim[1]) / (nx-1)} ## Default to nx or ny steps
  if (is.null(y.scale)) { y.scale = (ylim[2] - ylim[1]) / (ny-1)}
  meta$x.scale = x.scale
  meta$y.sclae = y.scale

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

  ##setup kdtree
  tree = less::KDTree$new(X = data.frame(x = x, y = y))

  grid = gridWeighted(tree = tree, z = z, gx = grid$x, gy = grid$y, neighborhood = neighborhood)
  meta$neighborhood = neighborhood

  ## Construct return object
  grid = list(x = x.new,
              y = y.new,
              z = matrix(grid$z, nrow = nx, ncol = ny),
              meta = meta
  )

  ## Return
  grid
}


