#library(TheSource)
library(SimpleGridder)
library(pals)
library(less)


n = 10
x = runif(n)
y = runif(n)
z = x + y


## Setup a blank grid
grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
grid = setGridder(grid, neighborhood = 20)

## Append Data
grid = appendData(grid, x, y, z, 'salinity')


## Interpolate
grid = interpData(grid)
plotGrid(grid, 'salinity')

grid

plot.image(z = grid$z, pal = 'ocean.deep')



val = c(10, 30, 100, 300, 1000, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6, 3e6)
delay = c()
for (n in val) {
  x = runif(n)
  y = runif(n)
  z = x + y

  a = Sys.time()
  ## Setup a blank grid
  grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
  grid = setGridder(grid, neighborhood = 20)
  grid = appendData(grid, x, y, z, 'salinity')
  grid = interpData(grid)
  b = Sys.time()

  delay = c(delay, as.numeric(b) - as.numeric(a))
}
plot(log10(val), delay)

