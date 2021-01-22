##install.packages('imager')
library('imager')
library(xlsx) 
getwd()
setwd("D:/R stuff/smaig/SMAIG occlusion/")
picPath <- "AIG-6/"  ## CHANGE to appropriate image sub-folder
im_names <- list.files (path = picPath); #folder with pictures in your working directory

cnt = 1
colorCount <- matrix(ncol = NROW(im_names)+1)  # set up matrix 

for (imName in im_names) {
  temp <- paste(picPath, imName, sep='');  
  im <- load.image(temp);  #sets the name of the image in the loop que
  cnt = cnt+1; # column cnt: first column is used for color categories/codes
  cntColors = 0; #track the number of colors in this image
  
  for (x in 1:400) {  # width = 400 pixels
    for (y in 1:400) {  # height = 400 pixels
      thisColor <- as.numeric(im[x,y,1,1]) # The color (rounded) of the pixel at location x,y
      #  if ( thisColor != 0) {  # if the color is not black
          #search colorCount for thisColor
          thisMatch = as.numeric(match(thisColor, colorCount));
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
          #}
      }
  }  
  print(imName);  print(cnt);
}


colorCount[1,1] <- 0

colorCount <- colorCount[order(colorCount[, 1], decreasing=FALSE),]

  ## save image names to first row of colorCount matrix
  colorCount[1,1] <- "color code"
  cnt = 1
for (imName in im_names) {
  cnt = cnt+1
  colorCount[1,cnt] <- imName
}

write.xlsx(colorCount, "colorCount.xlsx", col.names=FALSE, row.names=FALSE) # each column = image; rows = non-black color categories (.1-1); black == 0, white = 1



