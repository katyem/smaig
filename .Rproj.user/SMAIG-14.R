# Easy Bordered Cubes in R: https://www.r-bloggers.com/fun-with-bordered-cubes/
#install.packages('magrittr')
#install.packages("magick", verbose=TRUE)
setwd("D:/R stuff/AIG") #My AIG files
#library('tidyverse')
library('magick')
library('rgl') 
library('magrittr')

#install.packages("xlsx")
library("xlsx")

SMAIGTable <- read.xlsx("SMAIG.xlsx", 1)  # read first sheet
head(SMAIGTable[, 1:5])
SMAIGTable[1] <- NULL # deletes the column that write.xlsx inserts into the table.
# the table has coordinates for each cube in stacks 1-n; the last row (11th) is the rotation coordinates
# SMAIGTable <- SMAIGTable[-c(31:50), ] ## delete rows 
# attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == 1), ]; detach(SMAIGTable)
##***********************************************************************

cube <- function(x=0,y=0,z=0, bordered=FALSE, 
                 filled = TRUE, lwd=2, scale=1,
                 fillcol = 'blue',
                 bordercol ='gray', ...) {
   mycube <- cube3d(color = fillcol, alpha = 1, shininess=1)
  bg3d('black')
  #material3d(shininess=1)
  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2
  
  for (i in 1:length(x)) {
    # Add cube border
    if (bordered) {
      bcube <- mycube
      bcube$material$lwd <- lwd
      bcube$material$front <- 'line'
      bcube$material$back <- 'line'
      bcube %>% translate3d(x[i], y[i], z[i]) %>% shade3d
    }
    # Add cube fill
    if (filled) {
      fcube <- mycube
      #fcube$vb[4,] <- fcube$vb[4,]*1.01
      fcube$material$col <- fillcol
      fcube %>% translate3d(x[i], y[i], z[i]) %>% shade3d
    }
  }
}

# Build a matrix (2 dime array) for cube coordinates 1-10 x 3 (x,y,z)
upLeg = 4 # upper bound number of cubes allowed in a leg
stackSize = 10 # This could be changed to vary within a range and change upLeg accordingly

blankMatrix <- function(stackSize=10) {
  temp = (stackSize+3)*4
  return(matrix(
  replicate(temp, 0),
  nrow = stackSize+3,
  dimnames = list(
    c("one", "two", "three", "four","five","six","seven","eight","nine","ten","mtrx1","mtrx2","mtrx3"),
    c("leg", "xx", "yy", "zz" ) # Damn thing replace "x" with cubeCord!! WTF? Works if "xx"
  )
)
)
}

#---------------------------------------------------------------------------------
#build the stack in cubeCo using the SMAIGTable data frame which can have multiple stacks via stackID
tableStack <- function(stackID=1, stackSize=10)  {
  cubeCo = blankMatrix
  print(cubeCo)
  # clearStack 
  for (i in 1:stackSize+1) { 
    for (j in 2:4) {
      cubeCo[i,j] = 0
    }
  }  # Build Legs ------------------------------------------------------------------
  attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == stackID), ]; detach(SMAIGTable)
  for (i in 1:stackSize+1) {
    for (j in 1:4) {
      cubeCo[i,j] <- newSMAIGTable[i,j+1]
    }
  }
  #print(cubeCo)
  return(cubeCo)
}

#---------------------------------------------------------------------------------
## Build stack randomly - tried to use this as a function by passing upLeg and stackSize but couldn't get it to work i.e., no change in matrix
## update: used -- cubeCoord = buildStack() -- and it worked
buildStack <- function(upLeg = 4, stackSize=10)  {
  # clearStack 
  cubeCoo = blankMatrix()
  # 2-4 cubes in a leg; if legs 1 and 2 == 4 then 3 and 4 = 2; if 1 and 2 == 2 then 3 and 4 = 4
  legCubes <- c(0,0,0,0)
  legCubes[1] = sample(2:4, 1) #the number of cubes in the first leg 
  if (legCubes[1] == 4) {
    legCubes[1] <- sample(2:4, 1)  ## trim the odds of getting 4 on first leg
  }
  if (legCubes[1] > 3) {
    upLeg = 3
  }
  legCubes[2] <- sample(2:upLeg, 1) #the number of cubes in the second leg 
  if (legCubes[1]+legCubes[2] > 5) {
    upLeg = 3
  }
  if (legCubes[1]+legCubes[2] > 6) {
    upLeg = 2
  }
  
  if (upLeg > 2) { 
    legCubes[3] <- sample(2:upLeg, 1)
  } else {
    legCubes[3] = 2
  }
  
  legCubes[4] <- stackSize-(legCubes[1]+legCubes[2]+legCubes[3])
  
  prevDirection = 0
  legCubeCount = 1 #count up of cubes in current leg (of 4). Controls turn 
  leg = 1 # 4 legs in a stack  | 10 cubes
  
  # Initial face and direction of first leg
  face <- 2 #sample(2:4, 1) # select face/axis 2=x 3=y 4=z
  prevFace <- array(c(2,0,0)) #track the number of times a face/axis is selected; allow no more than 2
  direction <- 1 #sample(1:2, 1) # 1=pos 2=neg  #if (direction == 2) {  direction <- -1 }    
  # Copy direction to the face/axis of each cube > 1. We'll add (+1 or -1) as we loop through each cube.
  for (i in 2:10) { 
    cubeCoo[i,face] <- direction
  }
  
  # Build Legs ------------------------------------------------------------------------------------------
  for (i in 1:stackSize) { # build four legs using 'stackSize' number of cubes
    cubeCoo[i,1] <- leg # store leg 
    ## note to self: think of the first cube as your first turn decision with all faces available.
    ## another note to self: screw that first note! set the face and direction to always be xx = 1 or cubeCoord[1,2] = 1 (moving away from 0,0,0)
    ## also, set the face and direction of the second leg - seems logical given random turns don't have an impact until the 3rd turn; up to that point rotation is the only difference you would see looking at three dimensional perspectives
    ## so, cubeCoord[1,2] = 1  ; if (leg==2) {cubeCoord[i,4] = 1}
    ## Set prevFace 1-3 to count the number times a direction is selected; allow no more than two selection and never twice in a row
    if (legCubeCount == 1 && i > 1) {  # first cube is preset
      checkFace <- face # don't allow a face twice in a row
      while (checkFace == face || prevFace[face-1] > 1) {
        face <- sample(2:4, 1) # select face/axis 2=x 3=y 4=z
      }
      
      prevFace[face-1] <- prevFace[face-1]+1
      
      direction <- sample(1:2, 1) # 1=pos 2=neg
      if (direction == 2) {
        direction <- -1
      } 
      
      if (leg==2) {  #set leg 2; see explanation above
        face = 4 #z axis
        direction = 1 #away from x axis in centered perspective with -z coming at you.
      }
    } 
    
    cubeCoo[i,face] <- cubeCoo[i,face]+direction #increment the axis
    
    if (i < stackSize) {  # replace all following with current 
      temp = i+1
      for (j in temp:stackSize) {  
        cubeCoo[j,face] = cubeCoo[i,face] 
      }
    }
    
    legCubeCount = legCubeCount + 1
    
    if (legCubeCount > legCubes[leg]) {  
      leg = leg+1
      legCubeCount = 1
    }
    
  }
  # Final 3 rows: generate and store userMatrix for par3d function: 3x3 matrix
  cubeCoo[11:13,1] <- 99
  
  dim(diag(3))  # lets create a 3x3 matrix with 1s in the diagnal
  newMatrix = diag(1,3,3) # diag = 1
  deg = pi*(180/sample(10:350, 1)) # pi = 180 degrees rotation in rotate3d
  x = sample(1:2,1) # direction 1 or -1
  if (x == 2) {x=-1}
  y = sample(1:2,1) # direction 1 or -1
  if (y == 2) {y=-1}
  z = sample(1:2,1) # direction 1 or -1
  if (z == 2) {z=-1}
  
  newMatrix <- round(rotate3d(newMatrix, deg, x, y, z),3)
  
  cubeCoo[11:13,2:4] <- newMatrix[1:3,1:3]
  #print(cubeCoo)
  return(cubeCoo)
}


#-----------------------------------------------------------------------------------

displayStack <- function(stackSize=10){
  clear3d() #initiate/clear RGL scrreen
  rgl.viewpoint(); 
  for (i in 1:stackSize) { # build four legs using 'stackSize' number of cubes
    if (i ==1) {
      cubeColor <- 'blue'
    } else if (i == stackSize) {
      cubeColor <- 'red'
    } else {
      cubeColor <- 'gray'
    }
    cube(cubeCoord[i,2],cubeCoord[i,3],cubeCoord[i,4], filled=T, fillcol = cubeColor)
  }
  dim(diag(4))
  newMatrix <- diag(1,4,4) # diag = 1
  newMatrix[1:3,1:3] = cubeCoord[11:13,2:4]
  par3d(userMatrix = newMatrix)
}

mirrorStack <- function(clear = TRUE){
  if (clear) {clear3d()} #initiate/clear RGL screen
  x <- cubeCoord
  addSpace <- array(c(0,0,0))
  addEval <- array(c(0,0))
  for (i in 2:4) {
    addSpace[i-1] = 0
    for (j in 1:10) {
      addEval[1] = addSpace[i-1]
      addEval[2] = x[j,i]-1
      if (i == 2) { # first addSpace is the smallest x value
        if (addEval[1] > addEval[2]){  #looking for the smallest number for x
          addSpace[i-1] <- x[j,i]-1
        }        
      } else {
        if (abs(addEval[1]) < abs(addEval[2])){  #looking for the largest number for y,z
          addSpace[i-1] <- x[j,i]
        }
      }
    }
  }
  #print(addSpace)
  #clear3d() #initiate/clear RGL scrreen
  for (i in 1:stackSize) { # build four legs
    cubeX = (x[i,2]*-1) #the first x in the mirror is always -1
    #addSpace[1] can be pos, if it is neg and less than -1, we have an issue
    if (addSpace[1] < 1) {
      cubeX = cubeX + addSpace[1]
    }
    #print(cubeX)
    cubeY = (x[i,3]*-1)+addSpace[2]
    cubeZ = (x[i,4]*-1)+addSpace[3]
    if (i ==1) {
      cubeColor <- 'blue'
    } else if (i == stackSize) {
      cubeColor <- 'red'
    } else {
      cubeColor <- 'gray'
    }
    cube(cubeX,cubeY,cubeZ, filled=T, fillcol = cubeColor)
  }
}

## store userMatrix to cubeCoord
storeMatrix <- function(){
  cubeCo <- cubeCoord
  newMatrix <- round(par3d("userMatrix"),3)
  print(newMatrix)
  cubeCo[11:13,2:4] <- newMatrix[1:3,1:3]
  return(cubeCo)
}

# copy cubeCoord to SMAIGTable  -----------------------------------------
saveStack <- function()  {
  SMAIGnew = SMAIGTable
  #  get the last ID number from the last row and first column

  lastID = nrow(SMAIGnew)
  stackID = SMAIGnew[lastID,1]+1
  print(cubeCoord)
  x = nrow(cubeCoord) # should be 13; stackSize plus 3x3 userMatrix
  for (i in 1:x) {
    j = nrow(SMAIGnew)+1
    SMAIGnew[j,1:5] = c(stackID,cubeCoord[i,1:4]) 
    print(SMAIGnew[j,1:5])
    print(list(stackID,cubeCoord[i,1:4]) )
  }
  #print(SMAIGnew)
  return(SMAIGnew)
}

#-----------------------------------------------------------------------------------
#FUNCTION call!!  Build the Stack!!
#-----------------------------------------------------------------------------------
 # This "randomly" repopulates the cubeCoord matrix variable
cubeCoord = buildStack()

displayStack() 

#rgl.viewpoint(); 
# either manipulate the stack with mouse or use the mirrorStack function to change rotation
# use storeMatrix function to save the changed coordinates to cubeCoord
cubeCoord = storeMatrix()

# appends cubeCoord (the current stack) to SMAIGTable (which you can save to excel)
SMAIGTable <- saveStack() 
warnings()
#Repopulates the cubeCoord matrix with SMAIGTable data for the stackID passed
cubeCoord <- tableStack(stackID=8) 

mirrorStack(clear = TRUE) # clear = clear viewer

write.xlsx(SMAIGTable, file = "SMAIG.xlsx")
##movie3d(spin3d(axis=c(20,-10,-60), rpm=2), duration=2) 
##spin3d()
## write the table to an Excel file



#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------



# rotate model according to model's center
Ustart <- par3d("userMatrix")
U <- par3d("userMatrix")
pause <- 0.02
for (theta in seq(0, pi, len=100)) {
  U = rotate3d(U, theta, 0,0,1) # Rotate about model's z axis
  #Sys.sleep(pause)  
  #U2 <- par3d("userMatrix")
  U = rotate3d(U, theta, 0,1,0) # Rotate about model's y axis 
  #Sys.sleep(pause)  
  #U3 <- par3d("userMatrix")
  U = rotate3d(U, theta, 1,0,0) # Rotate about model's x axis
  par3d(userMatrix = U)
  Sys.sleep(pause)
  #print(theta)
}


par3d(userMatrix = Ustart) # reset
## 
U <- par3d("userMatrix")
cubeCoord[11,2] = 0
for (i in 2:4) {
  if (cubeCoord[11,i] > 0) { # x axis
      while (cubeCoord[11,i] > 360) {
        cubeCoord[11,i] <- cubeCoord[11,i]-360
      }
    delta <- pi*(cubeCoord[11,i]/180)
    if (i==2) {
      U = rotate3d(U, delta, 1,0,0) # Rotate about model's x axis
    } else if (i==3) {
      U = rotate3d(U, delta, 0,1,0) # Rotate about model's y axis
    } else {
      U = rotate3d(U, delta, 0,0,1) # Rotate about model's z axis
    }
  }
} 
par3d(userMatrix = U)


# experiment 22342349  --------------------------------------------
par3d(userMatrix = Ustart) # reset
#U = rotate3d(Ustart, pi/2, 0,1,0) # Rotate about model's y axis 90 deg
U = rotate3d(Ustart, pi/.6, 1,0,0) 
par3d(userMatrix = U)
Uback = rotate3d(U, -pi/.6, 1,0,0) 
par3d(userMatrix = Uback)

U


# --------------------------------------------------------------------------
# center worldview (almost)
rgl.viewpoint(-.001, .001,); 
# rotate model according to model's center

dim(diag(4))

Ustart = diag(1,4,4) # diag = 1
par3d(userMatrix = Ustart)
#U = rotate3d(Ustart, pi/2, 0,1,0) # Rotate about model's y axis 90 deg
U = rotate3d(Ustart, pi/4, 0,1,0) # Rotate about model's y axis 45 deg
par3d(userMatrix = U)
U
#Ustart <- par3d("userMatrix")

for (i in 1:4) {
  for (j in 1:4) {
    U[i,j] <- round(U[i,j],2)
  }
}

digits = -2

par3d(userMatrix = Ustart) # reset
Ustart
newU = Ustart
temp = U
for (i in 1:4) {
  for (j in 1:4) {
    temp[i,j] <- U[i,j]-Ustart[i,j]
    newU[i,j] = newU[i,j] + temp[i,j]
  }
}

temp
newU

par3d(userMatrix = newU)
par3d(userMatrix = U)
U
newU
Ustart

2.666893e-04

# --------------------------------------------------------------------
# rotate viewer acording to viewer's worldview center
for (theta in seq(0, pi, len=100)) {
  par3d(userMatrix = rotationMatrix(theta, 0,1,0) %*%U ) # Rotate about viewer's y axis
   Sys.sleep(0.1)
}

rgl.light( theta = 0, phi = 0, viewpoint.rel = TRUE, ambient = "#FFFFFF", 
           diffuse = "#FFFFFF", specular = "#FFFFFF", x = NULL, y = NULL, z = NULL)


cubesTibble <- as_tibble(cubeCoord)

nrow(cubeCoord)


rgl.open()
shade3d(oh3d(), color = "red")

start <- proc.time()[3]
while ((i <- 36*(proc.time()[3] - start)) < 360) {
  rgl.viewpoint(i, i/4); 
}

rgl.viewpoint(1, 1); 


cubeCoord[1,2]-cubeCoord[10,2]
cubeCoord[1,3]-cubeCoord[10,3]
cubeCoord[1,4]-cubeCoord[10,4]

for (i in 2:4) {
  print(sum(cubeCoord[,i]))
}


movie3d(spin3d(axis=c(0,-280,0), rpm=4), duration=6) 

movie3d(spin3d(axis=c(1,0,1), rpm=20), duration=2.95)



## Snippets from web

# ----------------------------------------------------------------
# Cool sample from: https://www.rdocumentation.org/packages/rgl/versions/0.100.54/topics/light
data(volcano)
z <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- terrain.colors(zlen) # height color lookup table
col <- colorlut[ z - zlim[1] + 1 ] # assign colors to heights for each point

open3d()
bg3d("gray50")
surface3d(x, y, z, color = col, back = "lines")
r <- max(y) - mean(y)
lightid <- spheres3d(1, 1, 1, alpha = 0)
frame <- function(time) {
  a <- pi*(time - 1)
  save <- par3d(skipRedraw = TRUE)
  clear3d(type = "lights")
  rgl.pop(id = lightid)
  xyz <- matrix(c(r*sin(a) + mean(x), r*cos(a) + mean(y), max(z)), ncol = 3)
  light3d(x = xyz, diffuse = "gray75", 
          specular = "gray75", viewpoint.rel = FALSE) 
  light3d(diffuse = "gray10", specular = "gray25")
  lightid <<- spheres3d(xyz, emission = "white", radius = 4)
  par3d(save)
  Sys.sleep(0.02)
  NULL
}
play3d(frame, duration = 2)

# ----------------------------------------------------------------
# Cool sample from: https://www.rdocumentation.org/packages/rgl/versions/0.100.54/topics/scene
x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
p <- plot3d(x, y, z, type = 's')
rgl.ids()
lines3d(x, y, z)
rgl.ids()
if (interactive() && !rgl.useNULL()) {
  readline("Hit enter to change spheres")
  rgl.pop(id = p["data"])
  spheres3d(x, y, z, col = "red", radius = 1/10)
  box3d()
}
