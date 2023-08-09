library(TheSource)
library(SimpleGridder)
library(pals)


n = 1000
x = runif(n)
y = runif(n)
z = runif(n)



grid = buildGrid(xlim = c(0,1), ylim = c(0,1), nx = 100, ny = 100)
grid = setGridder(grid, neighborhood = 20)

grid$z = appendData(grid, x, y, z)
grid$z = appendData(grid, y, x, z)
grid$z = appendData(grid, y, z, x)

plot.image(z = grid$z, pal = 'ocean.deep')
