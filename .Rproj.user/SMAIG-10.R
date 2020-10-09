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
# SMAIGTable <- SMAIGTable[-c(31:50), ] ## delete rows 
# attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == 1), ]; detach(SMAIGTable)
##***********************************************************************

cube <- function(x=0,y=0,z=0, bordered=FALSE, 
                 filled = TRUE, lwd=2, scale=1,
                 fillcol = gray(1),
                 bordercol ='gray', ...) {
      mycube <- cube3d(color = 'gray', alpha = 1, shininess=1)
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
temp = stackSize*4
upLeg = 4 # upper bound number of cubes allowed in a leg
stackSize = 10 # This could be changed to vary within a range and change upLeg accordingly

cubeCoord <- matrix(
  replicate(temp, 0),
  nrow = stackSize,
  dimnames = list(
    c("one", "two", "three", "four","five","six","seven","eight","nine","ten"),
    c("leg", "xx", "yy", "zz" ) # Damn thing replace "x" with cubeCord!! WTF? Works if "xx"
  )
)

# copy cubeCoord to SMAIGTable  -----------------------------------------
saveStack <- function(cubeCoo = cubeCoord)  {
  SMAIGnew = SMAIGTable
  smRow = nrow(SMAIGnew)
  #  get the last ID number from the last row and first column
  print(SMAIGnew[smRow,1])
  stackID = SMAIGnew[smRow,1]
  
  x = nrow(cubeCoo) # should be 10, same as stackSize
  for (i in 1:x) {
    smRow = nrow(SMAIGnew)+1
    SMAIGnew[smRow,] = list(stackID+1,cubeCoo[i,1],cubeCoo[i,2],cubeCoo[i,3],cubeCoo[i,4]) 
  }
  return(SMAIGnew)
}


#---------------------------------------------------------------------------------
#build the stack in cubeCoo using the SMAIGTable data frame which can have multiple stacks via stackID
tableStack <- function(stackID=1, stackSize=10)  {
  cubeC = cubeCoord
  print(cubeC)
  # clearStack 
  for (i in 1:stackSize) { 
    for (j in 2:4) {
      cubeC[i,j] = 0
    }
  }  # Build Legs ------------------------------------------------------------------
  attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == stackID), ]; detach(SMAIGTable)
    for (i in 1:stackSize) {
      for (j in 1:4) {
        cubeC[i,j] <- newSMAIGTable[i,j+1]
      }
    }
  print(cubeC)
  return(cubeC)
}

#---------------------------------------------------------------------------------
## Build stack randomly - tried to use this as a function by passing upLeg and stackSize but couldn't get it to work i.e., no change in matrix
## update: used -- cubeCoord = buildStack() -- and it worked
buildStack <- function(upLeg = 4, stackSize=10, cubeCoo = cubeCoord)  {
  # clearStack 
  for (i in 1:stackSize) { 
    for (j in 2:4) {
      cubeCoo[i,j] = 0
    }
  }
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
    
    legCubes[4] <- 10-(legCubes[1]+legCubes[2]+legCubes[3])
    
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
  return(cubeCoo)
}
#-----------------------------------------------------------------------------------

displayStack <- function(stackSize){
  clear3d() #initiate/clear RGL scrreen
  for (i in 1:stackSize) { # build four legs using 'stackSize' number of cubes
    cube(cubeCoord[i,2],cubeCoord[i,3],cubeCoord[i,4], filled=T)
  }
}

mirrorStack <- function(x = cubeCoord, stackSize){
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
    cube(cubeX,cubeY,cubeZ, filled=T)
  }
  
}


#-----------------------------------------------------------------------------------
                   #Build the Stack!!
#-----------------------------------------------------------------------------------
cubeCoord = buildStack() # This "randomly" repopulates the cubeCoord matrix variable

SMAIGTable <- saveStack() # appends the current stack to SMAIGTable
cubeCoord <- tableStack(stackID=3) #Repopulates the cubeCoord matrix with the ID passed

displayStack(stackSize)
#clear3d() #initiate/clear RGL scrreen
mirrorStack(x=cubeCoord,stackSize) 

movie3d(spin3d(axis=c(0,-280,200), rpm=4), duration=6) 
movie3d(spin3d(axis=c(0,280,200), rpm=4), duration=6) 

write.xlsx(SMAIGTable, file = "SMAIG.xlsx")
#-----------------------------------------------------------------------------------

cubesTibble <- as_tibble(cubeCoord)

nrow(cubeCoord)



cubeCoord[1,2]-cubeCoord[10,2]
cubeCoord[1,3]-cubeCoord[10,3]
cubeCoord[1,4]-cubeCoord[10,4]

for (i in 2:4) {
  print(sum(cubeCoord[,i]))
}


movie3d(spin3d(axis=c(0,-280,0), rpm=4), duration=6) 

movie3d(spin3d(axis=c(1,0,1), rpm=20), duration=2.95)
