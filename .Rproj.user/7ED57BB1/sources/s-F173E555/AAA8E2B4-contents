library(rgl)

open3d()

# create and plot a box at (x,y,z) of size (x1,y1,z1)
printBox <- function(x, y, z, x1, y1, z1) {
  mycube <- cube3d()                      # create a cube as mesh object   
  mycube <- scale3d(mycube, x1, y1, z1)   # now scale that object by x1,y1,z1
  mycube <- translate3d(mycube, x, y, z)  # now move it to x,y,z
  wire3d(mycube)                          # now plot it to rgl as a wireframe
}

# Display 5 boxes along a diagonal line
n <- 5
for (i in 1:n) {
  x <- i/n 
  y <- i/n
  z <- i/n
  sz <- 1/(1.2*n)
  printBox(x, y, z, sz,sz,sz )
}
