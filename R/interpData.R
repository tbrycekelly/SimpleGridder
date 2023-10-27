#' Apply interpolation to dataset
#' @export
#' @import less
interpData = function(grid) {

  neighborhood = min(grid$gridder$neighborhood, length(x))

  x = grid$data$x / grid$meta$x.scale / grid$meta$x.factor
  y = grid$data$y / grid$meta$y.scale / grid$meta$y.factor
  tree = less::KDTree$new(X = data.frame(x = x, y = y))

  tmp = expand.grid(x = grid$x / grid$meta$x.scale / grid$meta$x.factor,
                    y = grid$y / grid$meta$y.scale / grid$meta$y.factor)

  for (label in names(grid$data)[-c(1:2)]) {
    z = grid$data[[label]]
    z.grid = gridWeighted(tree = tree, z = z, gx = tmp$x, gy = tmp$y, neighborhood = neighborhood)

    grid$interp[[label]] = array(z.grid$z, dim = c(grid$meta$nx, grid$meta$ny))
  }

  grid
}
