#' @title Grid via Binning
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
gridBin = function(x,
                   y,
                   z,
                   tree,
                   gx,
                   gy,
                   dx,
                   dy,
                   neighborhood = NULL,
                   func = function(x){mean(x, na.rm = T)}) {

  grid = data.frame(x = gx, y = gy)
  grid$z = NA

  for (i in 1:length(gx)) {
    grid$z[i] = func(z[abs(gx[i] - x + .Machine$double.eps) <= dx/2 & abs(gy[i] - y + .Machine$double.eps) <= dy/2])
  }

  grid ## Return
}

