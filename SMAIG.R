#install.packages('magrittr')
#install.packages("magick", verbose=TRUE)
setwd("D:/R stuff/smaig") #My AIG files
#library('tidyverse') #tibble vs table
library('magick')
library('rgl') 
library('magrittr')
# library ("orientlib") #needed?? V had to have this.

#install.packages("openxlsx")
library("openxlsx")

source("SMAIGpkg.R")

SMAIGtable <- read.xlsx("SMAIG.xlsx")  # read first sheet of your deck of stacks
#head(SMAIGtable[, 1:5])
SMAIGtable[1] <- NULL # deletes the column that write.xlsx inserts into the table.

# the table has coordinates for each cube in stacks 1-n; the last row (11th) is the rotation coordinates
# SMAIGTable <- SMAIGTable[-c(31:50), ] ## delete rows 
# attach(SMAIGTable); newSMAIGTable<- SMAIGTable[which(Stack_ID == 1), ]; detach(SMAIGTable)
#rm(SMAIG)

#-----------------------------------------------------------------------------------
#FUNCTION calls!!  Build the Stack!!
#-----------------------------------------------------------------------------------
# This "randomly" repopulates the cubeCoord matrix variable
  cubeCoord = buildStack() 

##***********************************************************************
#Repopulates the cubeCoord matrix with SMAIGTable data for the stackID passed
   cubeCoord <- tableStack(stackID =19) # if stackID==0, choose first stack in SMAIGtable

   #cubeCoord = buildStack() 
  displayStack(cMarker = F) #cMarker: blue for first cube and red for last cube
  displayStack(cMarker = T)
  #OR
  displayStack(newScreen = TRUE) # new device screen
  
# move focus between two or more device screens; up or down
  changeFocus() # default is upFocus = FALSE  
  changeFocus(upFocus = TRUE)

#build mirror in the same screen
  twinStack(cMarker = TRUE) #

#build the same model in two screens, rotate the 2nd
  rotateTest() 

#Check disparity between the two screens
  evalTest()
   
#build mirror in cleared screen  (saveStack must be TRUE if you want to save the mirror)
  cubeCoord = mirrorStack(cMarker = F, saveStack = T)
# arguments:
#saveStack=T saves the mirror to cubeCoord; you can then store it and save it to table/xls
#cMarker=T blue for first cube and red for last cube

#rgl.viewpoint(); 
# either manipulate the stack with mouse or use the mirrorStack function to change rotation
# use store3d function to save the changed coordinates to cubeCoord
  cubeCoord = store3d()

# appends cubeCoord (the current stack) to SMAIGTable (which you can save to excel)
  SMAIGtable <- saveStack() 

# save picture as a png to your working directory. WARNING: overwrites
  setwd("D:/R stuff/AIG/smaigPics") # Change to reflect your working directory
  i = 999 # should use the stackID in the SMAIGtable to match pictures to data

    snapName <- paste0("smaig ", formatC(i), "L.png")
  snapName
  rgl.snapshot(snapName)
  ##
  stophere
  ##
  snapName <- paste0("smaig ", formatC(i), "R.png")
  snapName
  rgl.snapshot(snapName)  
  
  
  setwd("D:/R stuff/AIG") # Change to reflect your working directory


## write the table to an Excel file  
  write.xlsx(SMAIGtable, file = "SMAIG.xlsx")

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
##movie3d(spin3d(axis=c(20,-10,-60), rpm=4), duration=10) 
#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
