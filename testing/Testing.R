library(TheSource)
library(SimpleGridder)
library(pals)


n = 400000
x = runif(n)
y = runif(n)
z = runif(n)


grid = buildGrid(x, y, z, gridder = gridWeighted, nx = 100, ny = 100, xlim = c(0,1), ylim = c(0,1), neighborhood = 20)

plotGrid(grid, pal = inferno, N = 5)
