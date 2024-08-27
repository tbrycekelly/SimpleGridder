#' @export
setGridder = function(grid,
                      gridder = gridWeighted,
                      func = function(x) {1 / x^2},
                      neighborhood = 20) {

  grid$gridder = list(neighborhood = neighborhood,
                      gridder = gridder,
                      func = func)

  grid
}







