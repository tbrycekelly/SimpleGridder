## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(SimpleGridder)

## Make test data (x, y, z)
n = 10
x = runif(n)
y = runif(n)
z = x + y

## Use SimpleGridder to make grid
grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
grid = setGridder(grid, neighborhood = 20)
grid = appendData(grid, x, y, z, 'salinity')
grid = interpData(grid)
plotGrid(grid, 'salinity')

## ----echo=TRUE----------------------------------------------------------------
grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
str(grid)

## ----eval=FALSE---------------------------------------------------------------
#  buildGrid(xlim, ylim, nx, ny, x.scale, y.scale, x.factor, y.factor)

## ----eval=FALSE---------------------------------------------------------------
#  setGridder(grid,
#             gridder = gridWeighted,
#             weight.func = function(x) {1 / x^2},
#             neighborhood = 20)

## ----eval=FALSE---------------------------------------------------------------
#  grid = appendData(grid, x, y, z, label)

## ----eval=FALSE---------------------------------------------------------------
#  grid = interpData(grid)

## ----plotGrid, eval=FALSE-----------------------------------------------------
#  plotGrid(section, label, xlim, ylim, xlab, ylab, zlim, pal, ...)

## ----example-gridPlots--------------------------------------------------------
grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
grid = setGridder(grid, neighborhood = 20)
grid = appendData(grid, x, y, z, 'salinity')
grid = interpData(grid)

#par(mfrow=c(2,2))
plotGrid(grid, 'salinity', pal = pals::inferno(8))
plotGrid(grid, 'salinity', pal = pals::inferno(128))
plotGrid(grid, 'salinity', pal = pals::ocean.dense(8))
plotGrid(grid, 'salinity', pal = pals::ocean.dense(128))


## -----------------------------------------------------------------------------
par(plt = c(0.1, 0.8, 0.1, 0.9)) # Changing plot aspect ratio to make room for colorbar
plotGrid(grid, 'salinity', pal = pals::ocean.dense(16), zlim = c(0,2))
colorbar(pal = pals::ocean.dense(16), zlim = c(0, 2))

