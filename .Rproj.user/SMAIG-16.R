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



#-----------------------------------------------------------------------------------
#FUNCTION call!!  Build the Stack!!
#-----------------------------------------------------------------------------------
# This "randomly" repopulates the cubeCoord matrix variable
cubeCoord = buildStack()

displayStack(cMarker = T) 

#rgl.viewpoint(); 
# either manipulate the stack with mouse or use the mirrorStack function to change rotation
# use storeMatrix function to save the changed coordinates to cubeCoord
cubeCoord = storeMatrix()

# appends cubeCoord (the current stack) to SMAIGTable (which you can save to excel)
SMAIGTable <- saveStack() 

#Repopulates the cubeCoord matrix with SMAIGTable data for the stackID passed
cubeCoord <- tableStack(stackID=8) 

mirrorStack(clear = TRUE) # clear = clear viewer

## write the table to an Excel file  
write.xlsx(SMAIGTable, file = "SMAIG.xlsx")

#---------------------------------------------------------------------------------
###---------------------------------------------------------------------------------
###---------------------------------------------------------------------------------
###---------------------------------------------------------------------------------
## Load all functions below (highlight and run)


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

#-----------------------------------------------------------------------------------

blankMatrix <- function(stackSize=10) {
  temp = (stackSize+1)*4
  return(matrix(
  replicate(temp, 0),
  nrow = stackSize+1,
  dimnames = list(
    c("one", "two", "three", "four","five","six","seven","eight","nine","ten","coord"),
    c("xx", "yy", "zz","legZ" ) # Damn thing replace "x" with cubeCord!! WTF? Works if "xx"
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
    for (j in 1:3) {
      cubeCo[i,j] = 0
    }
  }  # Build Legs ------------------------------------------------------------------
  attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == stackID), ]; detach(SMAIGTable)
  for (i in 1:stackSize+1) {
    for (j in 1:4) {
      cubeCo[i,j] <- newSMAIGTable[i,j+1] #first column in table is row count
    }
  }
  #print(cubeCo)
  return(cubeCo)
}

#---------------------------------------------------------------------------------

## Build stack randomly - tried to use this as a function by passing upLeg and stackSize but couldn't get it to work i.e., no change in matrix
## update: used -- cubeCoord = buildStack() -- and it worked
buildStack <- function(upLeg = 4, stackSize=10)  {
  #upLeg: limit of legs in stack
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
  face <- 1 #Set it to 1; it won't vary 3d until the second turn. sample(1:3, 1) # select face/axis 1=x 2=y 3=z
  prevFace <- array(c(2,0,0)) #used to track the number of times a face/axis is selected; allow no more than 2
  direction <- 1 #sample(1:2, 1) # 1=pos 2=neg  #if (direction == 2) {  direction <- -1 }    
  # Copy direction to the face/axis of each cube > 1. We'll add (+1 or -1) as we loop through each cube.
  for (i in 2:10) { 
    cubeCoo[i,face] <- direction
  }
  
  # Build Legs ------------------------------------------------------------------------------------------
  for (i in 1:stackSize) { # build four legs using 'stackSize' number of cubes
    cubeCoo[i,4] <- leg # store leg 
    ## note to self: think of the first cube as your first turn decision with all faces available.
    ## another note to self: screw that first note! set the face and direction to always be xx = 1 or cubeCoord[1,2] = 1 (moving away from 0,0,0)
    ## also, set the face and direction of the second leg - seems logical given random turns don't have an impact until the 3rd turn; up to that point rotation is the only difference you would see looking at three dimensional perspectives
    ## so, cubeCoord[1,1] = 1  ; if (leg==2) {cubeCoord[i,3] = 1}
    ## Set prevFace 1-3 to count the number times a direction is selected; allow no more than two selection and never twice in a row
    if (legCubeCount == 1 && i > 1) {  # first cube is preset
      checkFace <- face # don't allow a face twice in a row
      while (checkFace == face || prevFace[face] > 1 || face == 0) {
        face <- sample(1:3, 1) # select face/axis 1=x 2=y 3=z
        direction < sample(1:2)
        if (direction == 2) {direction = -1}
      }
      
      prevFace[abs(face)] <- prevFace[abs(face)]+1
      
      if (leg==2) {  #set leg 2; see explanation above
        face = 3 #z axis
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
  # Final row: generate and store coordinates xyz and zoom for par3d function: 3x3 matrix
  # This provides rotation of the matrix (with one axis held constant). 
  # degree of rotation is base on an origin of a right angle 
  # e.g., 3x3 matrix with diagonal of ones. 
  # Euler angles for x,y,z are used to calculate (multiply using +/-cosine across 
  # the diagonals) the degree of rotation (percentage [degrees/180] of pi) for each axis.
  # rglToLattice(rotm = matrixName) will be used to extract x,y,z from userMatrix in storeMatrix function
  newRotation = c(0,0,0) 
  newRotation = sample(seq(-175,180,by=5), 3, replace = FALSE) # pi = 180 degrees rotation; greatest degree = 359 360 = 0 (look up cosine)
  cubeCoo[11,1:3] <- newRotation[1:3]
  cubeCoo[11,4] <- 1 #default zoom # par3d("zoom") set to 1 as default
  #print(cubeCoo)
  return(cubeCoo)
}

#-----------------------------------------------------------------------------------

mirrorStack <- function(clear = TRUE, cMarker=FALSE){
  # cMarker: first cube=blue, second cube=red
  if (clear) {clear3d()} #initiate/clear RGL screen
  x <- cubeCoord
  addSpace <- array(c(0,0,0))
  addEval <- array(c(0,0))
  for (i in 1:3) {
    addSpace[i] = 0
    for (j in 1:10) {
      addEval[1] = addSpace[i]
      addEval[2] = x[j,i]-1
      if (i == 2) { # first addSpace is the smallest x value
        if (addEval[1] > addEval[2]){  #looking for the smallest number for x
          addSpace[i-1] <- x[j,i]-1
        }        
      } else {
        if (abs(addEval[1]) < abs(addEval[2])){  #looking for the largest number for y,z
          addSpace[i] <- x[j,i]
        }
      }
    }
  }
  #print(addSpace)
  #clear3d() #initiate/clear RGL scrreen
  cubeColor <- 'gray'
  
  for (i in 1:stackSize) { # build four legs
    cubeX = (x[i,1]*-1) #the first x in the mirror is always -1
    #addSpace[1] can be pos, if it is neg and less than -1, we have an issue
    if (addSpace[1] < 1) {
      cubeX = cubeX + addSpace[1]
    }
    #print(cubeX)
    cubeY = (x[i,2]*-1)+addSpace[2]
    cubeZ = (x[i,3]*-1)+addSpace[3]
    
    if (cMarker) {    
      if (i == 1) {
        cubeColor <- 'blue'
      } else if (i == stackSize) {
        cubeColor <- 'red'
      } else {
        cubeColor <- 'gray'
      }
    }
    cube(cubeX,cubeY,cubeZ, filled=T, fillcol = cubeColor)
  }
}


#-----------------------------------------------------------------------------------


## store current userMatrix (x,y,z) to cubeCoord
storeMatrix <- function(){
  cubeCo <- cubeCoord
  degLatt = rglToLattice(rotm = par3d("userMatrix"))
  print(degLatt)
  
    cubeCo[11,1] = round(degLatt[[3]][1],5) # FYI, pi == 180 degrees
    cubeCo[11,2] = round(degLatt[[2]][1],5) # FYI, pi == 180 degrees
    cubeCo[11,3] = round(degLatt[[1]][1],5) # FYI, pi == 180 degrees
   cubeCo[11,4] = par3d("zoom") 

  return(cubeCo)
}

#-----------------------------------------------------------------------------------


# copy cubeCoord to SMAIGTable  -----------------------------------------
saveStack <- function()  {
  SMAIGnew = SMAIGTable
  #  get the last ID number from the last row and first column

  lastID = nrow(SMAIGnew)
  stackID = SMAIGnew[lastID,1]+1
  print(cubeCoord)
  x = nrow(cubeCoord) # should be 11; stackSize plus row for xyz
  for (i in 1:x) {
    j = nrow(SMAIGnew)+1
    SMAIGnew[j,1:5] = c(stackID,cubeCoord[i,1:4]) 
    print(SMAIGnew[j,1:5])
    #print(list(stackID,cubeCoord[i,1:4]) )
  }
  #print(SMAIGnew)
  return(SMAIGnew)
}

#-----------------------------------------------------------------------------------


displayStack <- function(stackSize=10, cMarker=FALSE){
  clear3d() #initiate/clear RGL scrreen
  cubeColor <- 'gray'
  for (i in 1:stackSize) { # build four legs using 'stackSize' number of cubes
    if (cMarker) {    
      if (i == 1) {
        cubeColor <- 'blue'
      } else if (i == stackSize) {
        cubeColor <- 'red'
      } else {
        cubeColor <- 'gray'
      }
    }
    cube(cubeCoord[i,1],cubeCoord[i,2],cubeCoord[i,3], filled=T, fillcol = cubeColor)
  }

  dim(diag(4))
  newMatrix = diag(1,4,4) # diag = 1

  newMatrix = rotate3d(newMatrix, pi*(cubeCoord[11,1]/180), 1,0,0) # Rotate about model's x axis
  newMatrix = rotate3d(newMatrix, pi*(cubeCoord[11,2]/180), 0,1,0) # Rotate about model's y axis
  newMatrix = rotate3d(newMatrix, pi*(cubeCoord[11,3]/180), 0,0,1) # Rotate about model's z axis
  
  par3d(userMatrix = newMatrix, zoom = cubeCoord[11,4])
 
}
displayStack(cMarker = T) 
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
##movie3d(spin3d(axis=c(20,-10,-60), rpm=4), duration=10) 
##spin3d()
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
