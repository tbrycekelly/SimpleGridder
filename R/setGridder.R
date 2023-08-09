#' @export
setGridder = function(grid,
                      gridder = gridWeighted,
                      weight.func = function(x) {1 / x^2},
                      neighborhood = 20) {

  grid$gridder = list(neighborhood = neighborhood,
                      gridder = gridder,
                      weight.func = weight.func)

  grid
}







