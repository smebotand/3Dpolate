library(gstat)
library(lattice)
library(sf)
library(sp)
library(plotly)

# Create Data Points (Random)
n <- 50
data3D <- data.frame(x = runif(n), y = runif(n), z = runif(n), v = rnorm(n))
data3D <- data.frame(x = 1:n, y = 1:n, z = 1:n, v = 1:n)
data3Draw=data3D
coordinates(data3D) = ~x+y+z

size=10

plot_ly() %>%
  add_markers(x = data3Draw$x, y = data3Draw$y, z = data3Draw$z,
              marker = list(size=size,color = data3Draw$v, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))  %>%
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'z')))


# Create empty grid to krige
range1D <- seq(from = 0, to = 50, length = 11)
grid3D <- expand.grid(x = range1D, y = range1D, z = range1D)
data3Draw = grid3D
gridded(grid3D) = ~x+y+z

size=1

plot_ly() %>%
  add_markers(x = data3Draw$x, y = data3Draw$y, z = data3Draw$z,
              marker = list(size=size,#color = data3Draw$v,
                            colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))  %>%
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'z')))


# Perform CGS with 10 realizations; maxdist & nmax important for speed of calculation.

variogram()

meuse
coordinates(meuse) <- ~ x + y
dataVar = variogram(data3D~x+y-z, data3D)
dataVgm = variogram(v~1, data3D)
dataFit = fit.variogram(dataVgm, model =vgm(NA,"Exp",40,NA))
plot(dataVgm, dataFit)

vgm()


graphics.off()
plot(1:10)
plot(var,map=F)
plot(var,map=T)
res3D <- krige(formula = v ~ 1, data3D, grid3D, model = vgm(NA, "Lin", .2),nsim=10,maxdist=10,nmax=9)

# Plot Results
levelplot(sim1 ~ x + y | z, as.data.frame(res3D))

levelplot(v ~ x + y | z, as.data.frame(data3D))

data3Draw=as.data.frame(res3D)
data3Draw$v=data3Draw$sim1
data3Draw$size=data3Draw$sim1
#data3Draw$size[data3Draw$size<11]=1

data3Draw$size=abs(data3Draw$size-mean(data3Draw$size))*2

plot_ly(data3Draw,
        x = ~x,
        y = ~y,
        z = ~z,
        type="scatter3d",
        mode="markers",
        color = ~v
        ,marker = list(size=data3Draw$size)
        )  %>%
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'z')))


