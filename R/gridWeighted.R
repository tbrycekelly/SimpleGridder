#' @title Grid via Inverse Distance Weighting Interpolation
#' @author Thomas Bryce Kelly
#' @param gx Grid x values to interpolate onto
#' @param gy Grid y values to interpolate onto
#' @param x Observations, x values
#' @param y Observations, y values
#' @param z Observations, z values
#' @param p Exponent on the distance function
#' @export
gridWeighted = function(gx, gy, x, y, z, p = 2, xscale = 1, yscale = 1, uncertainty = 0.1, neighborhood = NULL, x.factor = 1, y.factor = 1, verbose = F) {

  if (is.null(neighborhood)) {
    neighborhood = min(25, length(x))
    if (verbose) { message(' GRIDIDW: No neighborhood given, set to ', neighborhood)}
  } else if (neighborhood > length(x)) {
    neighborhood = length(x)
    if (verbose) { message(' GRIDIDW: Neighborhood longer than valid points, decreasing to ', neighborhood)}
  }

  deltamin = sqrt((x.factor * xscale/2.0)^2 + (y.factor * yscale/2.0)^2) * uncertainty
  out = rep(NA, length(gx))

  for (i in 1:length(gx)) {
    w = abs((x.factor * (x - gx[i]))^2 + (y.factor * (y - gy[i]))^2 + deltamin)^-p

    k = order(w, decreasing = T)[1:neighborhood]
    out[i] = sum(z[k] * w[k]) / sum(w[k])
  }

  out
}
