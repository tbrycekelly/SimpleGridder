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
gridBin = function(gx, gy, x, y, z, p = 2, xscale = 1, yscale = 1, uncertainty = 0.1, neighborhood = NULL) {
  gz = rep(NA, length(gx))

  if (xscale == 0) { xscale = Inf }
  if (yscale == 0) { yscale = Inf }

  for (i in 1:length(gz)) {
    gz[i] = mean(z[abs(gx[i] - x) < xscale/2 & abs(gy[i] - y) < yscale/2], na.rm = T)
  }

  gz ## Return
}

