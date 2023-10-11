#' @export
appendData = function(grid,
                      x,
                      y,
                      z,
                      label
                      ) {
  if (is.null(colnames(grid$data))) {
    grid$data$x = x
    grid$data$y = y
  }

  grid$data[[label]] = z

  grid
}
