#' Apply interpolation to dataset
#' @export
#' @import less
interpData = function(grid) {

  neighborhood = min(grid$gridder$neighborhood, nrow(grid$data))

  x = grid$data$x / grid$meta$x.scale / grid$meta$x.factor
  y = grid$data$y / grid$meta$y.scale / grid$meta$y.factor
  tree = less::KDTree$new(X = data.frame(x = x, y = y))

  tmp = expand.grid(x = grid$x / grid$meta$x.scale / grid$meta$x.factor,
                    y = grid$y / grid$meta$y.scale / grid$meta$y.factor)

  for (label in names(grid$data)[-c(1:2)]) {
    z = grid$data[[label]]
    z.grid = grid$gridder$gridder(x = x,
                                  y = y,
                                  tree = tree,
                                  z = z,
                                  gx = tmp$x,
                                  gy = tmp$y,
                                  dx = grid$meta$x.scale / grid$meta$x.factor,
                                  dy = grid$meta$y.scale / grid$meta$y.factor,
                                  neighborhood = neighborhood,
                                  func = grid$gridder$func)

    grid$interp[[label]] = array(z.grid$z, dim = c(grid$meta$nx, grid$meta$ny))
  }

  grid
}
