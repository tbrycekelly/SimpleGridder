#' @export
interpData = function(grid,
                      label
) {

  if (!label %in% names(grid$data)) {
    message('No dataset with label ', label, ' found in the grid object. Skipping.')
    return(grid)
  }
  l = !is.na(grid$data[[label]])
  x = grid$grid$x[l]
  y = grid$grid$y[l]
  z = grid$data[[label]][l]

  neighborhood = min(grid$gridder$neighborhood, length(x))

  x = x / grid$meta$x.scale / grid$meta$x.factor
  y = y / grid$meta$y.scale / grid$meta$y.factor
  tree = less::KDTree$new(X = data.frame(x = x, y = y))

  tmp = expand.grid(x = grid$x / grid$meta$x.scale / grid$meta$x.factor,
                    y = grid$y / grid$meta$y.scale / grid$meta$y.factor)


  z.grid = gridWeighted(tree = tree, z = z, gx = tmp$x, gy = tmp$y, neighborhood = neighborhood)

  grid$interp[[label]] = array(z.grid$z, dim = c(grid$meta$nx, grid$meta$ny))

  grid
}
