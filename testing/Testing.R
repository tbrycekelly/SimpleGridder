library(TheSource)
library(SimpleGridder)
library(pals)


n = 1000
x = runif(n)
y = runif(n)
z = runif(n)


## Setup a blank grid
grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
grid = setGridder(grid, neighborhood = 20)

## Append Data
grid = appendData(grid, x, y, z, label = 'z')
grid = appendData(grid, y, x, z)
grid = appendData(grid, y, z, x)

## Interpolate

grid

plot.image(z = grid$z, pal = 'ocean.deep')
