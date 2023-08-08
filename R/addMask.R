#' @title Add Section mask
#' @author Thomas Bryce Kelly
#' @param section a section object from build.section()
#' @param bathy a bathymetric object such as "bathy.global" (the default)
#' @param binning a smoothing paramter; must be an odd integer
#' @param bathy.col the color of the bathymetry
#' @export
addMask = function(section, bathy = bathy.global, binning = 1, bathy.col = 'darkgrey') {

  if (length(section$lon) == 1) {
    message(' Bathymetry can only be drawn for sections where lon/lat are provided.')
    return()
  }



  ## Project lon/lat points
  p = make.proj('nsper', lat = median(section$lat, na.rm = T), lon = median(section$lon, na.rm = T))
  bathy$grid = expand.grid(lon = bathy$Lon, lat = bathy$Lat)
  bathy$xy = rgdal::project(cbind(bathy$grid$lon, bathy$grid$lat), proj = p) # Project bathymetric values
  section$xy = rgdal::project(cbind(section$lon, section$lat), proj = p) # project section values.

  ## Calculate depth via bilinear interpolation
  depth = interp.bilinear(x = section$xy[,1],
                          y = section$xy[,2],
                          gx = as.numeric(bathy$xy[,1]),
                          gy = as.numeric(bathy$xy[,2]),
                          z = as.numeric(-bathy$Z))

  ## Filter
  depth = runmed(depth, binning)

  ## Draw polygon
  polygon(x = c(section$x, rev(section$x)), y = c(depth, rep(1e8, length(section$x))), col = bathy.col)
}
