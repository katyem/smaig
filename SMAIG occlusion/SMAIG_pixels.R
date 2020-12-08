##install.packages('imager')
library('imager')
library(xlsx) 
getwd()
setwd("D:/R stuff/smaig/SMAIG occlusion/")
im_names <- list.files (path = "AIGpics/"); #folder with pictures in your working directory

cnt = 1
colorCount <- matrix(ncol = NROW(im_names)+1)  # set up matrix 

for (imName in im_names) {
  temp <- paste("AIG/", imName, sep='');
  im <- load.image(temp);  #sets the name of the image in the loop que
  cnt = cnt+1; # first row = 2 because row 1 is all N/A
  cntColors = 0; #track the number of colors in this image
  
  for (x in 1:400) {  # width = 400 pixels
    for (y in 1:400) {  # height = 400 pixels
      thisColor <- round(as.numeric(im[x,y,1,1]), 1) # The color (rounded) of the pixel at location x,y
      if ( thisColor != 0) {  # if the color is not black
        #search colorCount for thisColor
        thisMatch = match(thisColor, colorCount);
        if (!is.na(thisMatch) & thisMatch > nrow(colorCount)) {  # match wasn't in first column
          thisMatch <- NA;
        }
        if (is.na(thisMatch)) { # there was no match AND it wasn't black so add it to colorCount array
          colorCount <- rbind(colorCount, c(replicate(NROW(im_names)+1, 0))); # add a row and store 0 to each cell
          colorCount[nrow(colorCount),1] <- thisColor; # save the color to the first column of the new row
          colorCount[nrow(colorCount), cnt] <- 1; # start the count of the new color in the column for this image/cnt

          #print(thisColor);    
          #Sys.sleep(5);
         } else { 
          colorCount[thisMatch,cnt] = colorCount[thisMatch,cnt]+1;
        }
      }
    }
  }  
  print(imName);  
}


colorCount <- colorCount[-1,-1]; #Delete the empty first row

write.xlsx(colorCount, "colorCount.xlsx") # each column = image; rows = colors (.1-1)

##### STOP HERE  #############
# How to process/report output?
# percentage of change (JND)? absolute value of amount of change/highest value

# the following assumes the data are in order of paired stacks.
im_cnt = 1;
a = ncol(colorCount)/2; #loop once for each pair
loop_cnt = sign(a) * floor(abs(a)) # if there were an odd number of images, round down so the code won't crash

for(i in 1:loop_cnt) {
  temp = blah #count of first stack minus count of second stack
  im_cnt = im_cnt+2;
  }


