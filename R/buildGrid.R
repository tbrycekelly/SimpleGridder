#' @title Build Grid
#' @author Thomas Bryce Kelly
#' @keywords Gridding
#' @export
#' @param x dimensions (e.g. lat, lon, depth, section distance, time, etc)
#' @param y dimensions (e.g. lat, lon, depth, section distance, time, etc)
#' @param z signal to be gridded (e.g. T, S, ...)
#' @param xlim Limits of the gridding. These are the bounds of the new x-y grid. Default: NULL will set it based on the data + 10%.
#' @param ylim Limits of the gridding. These are the bounds of the new x-y grid. Default: NULL will set it based on the data + 10%.
#' @param x.factor The relative scale difference between x and y, used to calculate distances. Take into account actual scale AND the relevent scaling of the system (vertical distance tends to be more important than horizontal distance).
#' @param y.factor The relative scale difference between x and y, used to calculate distances. Take into account actual scale AND the relevent scaling of the system (vertical distance tends to be more important than horizontal distance).
#' @param x.scale The step size in the new x-y grid. By default the scale is set to generate a grid that is 50x50.
#' @param y.scale The step size in the new x-y grid. By default the scale is set to generate a grid that is 50x50.
#' @param uncertainty = 0: Scaling applied to the distance from the cener of a grid cell to a vertex, used to add a base-line distance to all measurements. 0 = no minimum, 1 = minimum = to half a grid cell.
#' @param field.name Sets the name of the new interpolated field. By default the name is 'z1'
#' @param gridder A function to perform gridding, options gridIDW (default: inverse distance), gridNN (nearest neighbor), gridNNI (natural neighbor) or gridKrige (Krigging)
#' @param nx The number of splits to make in the x direction (defaults to 50). Used only if x.scale is not set.
#' @param ny The number of splits to make in the y direction (defaults to 50). Used only if y.scale is not set.
#' @param proj A gdal projection string such as from make.proj() function. Used to redistribute grid over non-euclidean surfaces like maps.

buildGrid = function(x,
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
                     neighborhood = 20,
                     verbose = T) {

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


