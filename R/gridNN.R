#' @title Grid via Nearest Neighbor
#' @description Bins data into reguarly spaced grid
#' @param gx Grid x values to interpolate onto
#' @param gy Grid y values to interpolate onto
#' @param x Observations, x values
#' @param y Observations, y values
#' @param z Observations, z values
#' @param p Unused
#' @param xscale The spacing of the x grid
#' @param yscale The spacing of the y grid
#' @param uncertainty Unused
#' @author Thomas Bryce Kelly
#' @export
gridNN = function(x,
                   y,
                   z,
                   tree,
                   gx,
                   gy,
                   dx,
                   dy,
                   neighborhood = NULL,
                   func = NULL) {

  grid = data.frame(x = gx, y = gy)
  tmp = tree$query(grid, 1)

  grid$z = NA

  for (i in 1:nrow(grid)) {
    grid$z[i] = z[tmp$nn.idx[i,]]
  }

  grid ## Return
}

