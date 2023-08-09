#' @title Grid via Inverse Distance Weighting Interpolation
#' @author Thomas Bryce Kelly
#' @param gx Grid x values to interpolate onto
#' @param gy Grid y values to interpolate onto
#' @param x Observations, x values
#' @param y Observations, y values
#' @param z Observations, z values
#' @param p Exponent on the distance function
#' @export
gridWeighted = function(tree, z, gx, gy, neighborhood = 25) {

  grid = data.frame(x = gx, y = gy)
  tmp = tree$query(grid, neighborhood)

  grid$z = NA

  for (i in 1:nrow(grid)) {
    w = 1 / tmp$nn.dist[i,]^2
    grid$z[i] = sum(z[tmp$nn.idx[i,]] * w) / sum(w)
  }

  grid
}