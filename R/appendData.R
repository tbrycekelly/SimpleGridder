#' @export
appendData = function(grid,
                      x,
                      y,
                      z
                      ) {

  ## Get Data ready
  # Remove NAs (they poison everything)
  l = !is.na(x) & !is.na(y) & !is.na(z)
  x = x[l]
  y = y[l]
  z = z[l]

  neighborhood = min(grid$gridder$neighborhood, length(x))

  x = x / grid$meta$x.scale / grid$meta$x.factor
  y = y / grid$meta$y.scale / grid$meta$y.factor
  tree = less::KDTree$new(X = data.frame(x = x, y = y))

  tmp = expand.grid(x = grid$x / grid$meta$x.scale / grid$meta$x.factor,
                    y = grid$y / grid$meta$y.scale / grid$meta$y.factor)


  z.grid = gridWeighted(tree = tree, z = z, gx = tmp$x, gy = tmp$y, neighborhood = neighborhood)

  array(z.grid$z, dim = c(grid$meta$nx, grid$meta$ny))
}
