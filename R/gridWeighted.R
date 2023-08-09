#' @title Grid via Inverse Distance Weighting Interpolation
#' @author Thomas Bryce Kelly
#' @param gx Grid x values to interpolate onto
#' @param gy Grid y values to interpolate onto
#' @param x Observations, x values
#' @param y Observations, y values
#' @param z Observations, z values
#' @param p Exponent on the distance function
#' @export
gridWeighted = function(tree, z, gx, gy, neighborhood = 25, weight.func = function(x) {1 / x^2}) {

  grid = data.frame(x = gx, y = gy)
  tmp = tree$query(grid, neighborhood)

  grid$z = NA

  for (i in 1:nrow(grid)) {
    w = weight.func(tmp$nn.dist[i,])
    grid$z[i] = sum(z[tmp$nn.idx[i,]] * w) / sum(w)
  }

  grid
}
