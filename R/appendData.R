#' @export
appendData = function(grid,
                      x,
                      y,
                      z,
                      label
                      ) {

  if (length(x) != length(y)) {
    message('X and Y vectors given are different lengths. Error.')
    return(grid)
  }

  if (length(x) != length(z)) {
    message('X (and Y) and Z  vectors given are different lengths. Error.')
    return(grid)
  }


  if (is.null(colnames(grid$data))) {
    grid$data$x = x
    grid$data$y = y
  }

  grid$data[[label]] = z

  grid
}
