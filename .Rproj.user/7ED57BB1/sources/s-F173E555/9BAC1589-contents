## Load all functions below

#-----------------------------------------------------------------------------------
notafunction <- function() {
open3d()
open3d(params = getr3dDefaults())
select3d()
x = c(-800, 150, -75, 900)
par3d(windowRect = x)

par3d("windowRect")
rgl.cur() # current device screen - focus
rgl.set(which = 40)

rgl.dev.list() # returns all device IDs
rgl.snapshot("smaig001.png")
par3d(zoom = 1)
rgl.set(which = 3)
rgl.bbox(color=c("#333377","black"), emission="#333377",
         specular="#3333FF", shininess=5, alpha=0.3, nticks = 3, expand = 1.5 ) 
par3d("viewport")
    # A vector giving the dimensions of the window in pixels. The entries are taken to be c(x, y, width, height) where c(x, y) are the coordinates in pixels of the lower left corner within the window.

rgl.pop("lights")
light3d(diffuse = "gray75", specular = "gray75")

?light3d()
?play3d()
####
}

#-----------------------------------------------------------------------------------
changeFocus <- function(upFocus = FALSE)
{
  listScreens = rgl.dev.list()
  focusScreen = rgl.cur()
  focusScreen[[1]]
  moveScreen = 0  
  
  if (length(listScreens) > 1) {
    if (upFocus == TRUE) {  
      moveScreen = 1
    } else if (upFocus == FALSE) {
      moveScreen = -1
    }
    
    if (focusScreen[[1]] == listScreens[[length(listScreens)]] && upFocus == TRUE) {  # last value/screen available # nope, can't do that
        moveScreen = 0
      } else if (focusScreen[[1]] == listScreens[1] && upFocus == FALSE){
        moveScreen = 0
      }
  } 
  newScreen = focusScreen[[1]] + moveScreen
  rgl.set(which = newScreen) # move focus
  
}


#-----------------------------------------------------------------------------------

evalTest <- function()
{
  listScreens = rgl.dev.list()
  
  if (length(listScreens) == 2) {
    
    rgl.set(which = listScreens[1]) # set focus to first screen

    degLatt1 = rglToLattice(rotm = par3d("userMatrix"))
    str(degLatt1)
    rgl.set(which = listScreens[2]) # set focus to second screen
    degLatt2 = rglToLattice(rotm = par3d("userMatrix"))
    str(degLatt2)
    degLattDiff = array(c(0,0,0), 3)

    for (i in 1:3) {
      if(degLatt1[i] > 0 && degLatt2[i] > 0) {
        degLattDiff[i] =  abs(degLatt1[[i]] - degLatt2[[i]])
      }
      else if (degLatt1[i] < 0 && degLatt2[i] < 0) {
        degLattDiff[i] =  abs(degLatt1[[i]] - degLatt2[[i]])
      } else {
        degLattDiff[i] =  abs(degLatt1[[i]] + degLatt2[[i]])
      }
      cat(names(degLatt1)[i], "1 - ", names(degLatt2)[i], "2 = ", degLattDiff[i], "\n")
    }

    totalDiff = sum(degLattDiff[1:3])  
    cat("Total Difference = ", totalDiff, "\n")

    
  }
  
}  

#-----------------------------------------------------------------------------------

rotateTest <- function(cMarker=FALSE )
{
  focusScreen = rgl.cur()
  if (focusScreen[1] == 0) { # there is no current device screen
    displayStack(newScreen = TRUE) # new device screen
  }
    displayStack(newScreen = TRUE) # new device screen
    newMatrix = par3d("userMatrix")
    degRotate = 45 # Staying with degrees as the common theme for rotations
    piRotate = pi*(degRotate/180)    
    
    newMatrix = rotate3d(newMatrix, piRotate, 1,1,1) # Rotate about model's z axis
    i = cubeCoord[nrow(cubeCoord),4]
    par3d(userMatrix = newMatrix, zoom = i)
    
  
}  

#-----------------------------------------------------------------------------------
displayStack <- function(cMarker=FALSE, newScreen = FALSE){
  x = c(20, 20, 420, 420) # size of new screen if first one
  countScreens = rgl.dev.list()
  if (length(countScreens) == 0) {
    newScreen = TRUE
  }
  if (newScreen == TRUE) {
      x = c(20, 20, 420, 420)

    if (length(countScreens) == 1) {
      x = c(20, 420, 420, 820)
    } else if (length(countScreens) > 1) {
      x = c(20, 20+(length(countScreens)*10), 420, 470+(length(countScreens)*10))
    }
  
      open3d()    
      par3d(windowRect = x)  #initiate/clear RGL screen
    bg3d('black')
  } else {
    clear3d() #initiate/clear RGL screen
  }

  rgl.bringtotop(stay = FALSE)
  stackSize=nrow(cubeCoord)-1
  cubeColor <- 'gray'
  if (exists("cubeCoord") == F) {
    cubeCoord <<- buildStack() # assign to envir = .GlobalEnv)
  }
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
  
  newMatrix = diag(1,4,4) # diag = 1
  cRow = stackSize+1
  newMatrix = rotate3d(newMatrix, pi*(cubeCoord[stackSize+1,1]/180), 1,0,0) # Rotate about model's x axis
  newMatrix = rotate3d(newMatrix, pi*(cubeCoord[stackSize+1,2]/180), 0,1,0) # Rotate about model's y axis
  newMatrix = rotate3d(newMatrix, pi*(cubeCoord[stackSize+1,3]/180), 0,0,1) # Rotate about model's z axis
  par3d(userMatrix = newMatrix, zoom = cubeCoord[stackSize+1,4])

  rgl.pop("lights")
  light3d(diffuse = "gray75", specular = "gray75")
  }

#-----------------------------------------------------------------------------------

mirrorStack <- function(cMarker=FALSE, saveStack = FALSE){
  #cMarker: first cube blue,second red; 
  # saveStack: save mirror to cubeCord

  clear3d() #initiate/clear RGL screen
  x <- cubeCoord
  stackSize = nrow(x)-1
  cubeColor <- 'gray'
  
  for (i in 1:stackSize) { # build four legs
    x[i,1] = (x[i,1]*-1) #the first x in the mirror is always -1
    x[i,2] = (x[i,2]*-1)
    x[i,3] = (x[i,3]*-1)
    
    if (cMarker) {    
    if (i == 1) {
      cubeColor <- 'blue'
    } else if (i == stackSize) {
      cubeColor <- 'red'
    } else {
      cubeColor <- 'gray'
    }
    }
    cube(x[i,1],x[i,2],x[i,3], filled=T, fillcol = cubeColor)

  }
  bg3d('black')
  #bg3d('tomato') # change this to an argument 
  return(x)
}

#-----------------------------------------------------------------------------------

twinStack <- function(cMarker=FALSE){
  #cMarker: first cube blue,second red; # saveStack: save mirror to cubeCord
  stackSize = 10
  # cMarker: first cube=blue, second cube=red
  x <- cubeCoord
  addSpace <- array(c(0,0,0))
  addEval <- array(c(0,0))
  temp = min(x[1:10,1])
  if (temp < 1){
    subCoord = temp-1
  } else {
    subCoord = -1
  }
  #print(addSpace)
  #clear3d() #initiate/clear RGL scrreen
  cubeColor <- 'gray'
  
  for (i in 1:stackSize) { # build four legs
    cubeX = (x[i,1]*-1)-subCoord 
    #addSpace[1] can be pos, if it is neg and less than -1, we have an issue
    if (addSpace[1] < 1) {
      cubeX = cubeX + subCoord
    }
    #print(cubeX)
    cubeY = (x[i,2]*-1)+addSpace[2]
    cubeZ = (x[i,3]*-1)+addSpace[3]
    
    #if (cMarker) {    
    if (i == 1) {
      cubeColor <- 'blue'
    } else if (i == stackSize) {
      cubeColor <- 'red'
    } else {
      cubeColor <- 'gray'
    }
    #}
    cube(cubeX,cubeY,cubeZ, filled=T, fillcol = cubeColor)
    x[i,1] = cubeX
    x[i,2] = cubeY
    x[i,3] = cubeZ
  }

}

## -------------------------------------------------------------------------------------
# cube function is from https://gist.github.com/EconometricsBySimulation/5c00a9e91abebd889fb7
cube <- function(x=0,y=0,z=0, bordered=FALSE, 
                 filled = TRUE, lwd=2, scale=1,
                 fillcol = 'blue',
                 bordercol ='gray', ...) {
   mycube <- cube3d(color = fillcol, alpha = 1, shininess=1)
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
  temp = (stackSize)*4
  return(matrix(
    replicate(temp, 0),
    nrow = stackSize,
    dimnames = list(
      c("one", "two", "three", "four","five","six","seven","eight","nine","ten","coord"),
      c("xx", "yy", "zz","legZ" ) # Damn thing replace "x" with cubeCord!! WTF? Works if "xx"
    )
  ))
  }

#---------------------------------------------------------------------------------

#build the stack in cubeCo using the SMAIGTable data frame which can have multiple stacks via stackID
tableStack <- function(stackID=0)  {
  
  newSMAIGtable <- subset(SMAIGtable, Stack_ID == stackID) 
  stackSize = nrow(newSMAIGtable)  
  
  if (stackSize > 1) {
    cubeCo = blankMatrix(stackSize = stackSize)  #blankMatrix() sets up new cubeCoord formated matrix
    # Build Legs ------------------------------------------------------------------
    cubeCo[1:stackSize,1] <- newSMAIGtable[1:stackSize,2]        
    cubeCo[1:stackSize,2] <- newSMAIGtable[1:stackSize,3]
    cubeCo[1:stackSize,3] <- newSMAIGtable[1:stackSize,4]
    cubeCo[1:stackSize,4] <- newSMAIGtable[1:stackSize,5]
    return(cubeCo)
  } else {
    str("stackSize is too small or zero")
  }
}

#---------------------------------------------------------------------------------

## Build stack randomly - tried to use this as a function by passing upLeg and stackSize but couldn't get it to work i.e., no change in matrix
## update: used -- cubeCoord = buildStack() -- and it worked
buildStack <- function(upLeg = 4, stackSize=10)  {
  #upLeg: limit of legs in stack
  # clearStack 
  cubeCoo = blankMatrix(stackSize = stackSize+1)
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
  legCubeCount = 1 #counter for cubes in current leg (up to 4). Controls turn 
  leg = 1 # counter for legs; 4 legs in a stack  | 10 cubes
  
  # Initial face and direction of first leg
  face <- 1 #Set it to 1; it won't vary 3d until the second turn. sample(1:3, 1) # select face/axis 1=x 2=y 3=z
  prevFace <- array(c(1,0,0)) #used to track the number of times a face/axis is selected; allow no more than 2
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
        direction <- sample(1:2, 1)
        if (direction == 2) {
          direction = -1
        }
      }
      
      prevFace[abs(face)] <- prevFace[abs(face)]+direction
      
      if (leg==2) {  #set leg 2; see explanation above
        face = 3 #z axis
        direction = 1 #away from x axis in centered perspective with -z coming at you.
      }
      if (leg==4) {
        face = 1 #x axis; this matches Vinay's images
      }
      if (leg==3){
        face = 2 #x axis; this matches Vinay's images
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
  return(cubeCoo)
}

#-----------------------------------------------------------------------------------

## store current userMatrix (x,y,z) to cubeCoord
store3d <- function(){
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
  SMAIGnew = SMAIGtable
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
##movie3d(spin3d(axis=c(20,-10,-60), rpm=4), duration=10) 
##spin3d()
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
