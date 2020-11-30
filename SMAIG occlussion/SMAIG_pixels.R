##install.packages('imager')
library('imager')
getwd()
im_names <- list.files (path = "smaig_pics/"); #folder in your working directory

cnt = 1
im_plus_cnt <- cbind(im_names, 0);
#im_plus_cnt <- cbind(im_plus_cnt, '');


colorCount <- matrix(ncol = NROW(im_names)+1)

cntColors = 0;

for (imName in im_names) {
  temp <- paste("smaig_pics/", imName, sep='');
  im <- load.image(temp);
  cnt = cnt+1;

  for (x in 1:400) {  # width = 400 pixels
    for (y in 1:400) {  # height = 400 pixels
      thisColor <- round(as.numeric(im[x,y,1,1]), 1)
      if ( thisColor != 0) {
        #search colorCount for thisColor
        thisMatch = match(thisColor, colorCount);
        if (!is.na(thisMatch) & thisMatch > nrow(colorCount)) {  # match wasn't in first column
          thisMatch <- NA;
        }
        if (is.na(thisMatch)) {
          colorCount <- rbind(colorCount, c(replicate(NROW(im_names)+1, 0))); 
          colorCount[nrow(colorCount),1] <- thisColor;
          colorCount[nrow(colorCount), cnt] <- 1;
          cntColors = cntColors+1;
          print(thisColor);    
          #Sys.sleep(5);
         } else { 
          colorCount[thisMatch,cnt] = colorCount[thisMatch,cnt]+1;
        }
      }
    }
  }  
  print(cntColors);  
}

nrow(colorCount)

library(xlsx) 
  
  write.xlsx(colorCount, "colorCount.xlsx")



# How to process/report output?
# percentage of change (JND)? absolute value of amount of change/highest value

# the following assumes the data are in order of paired stacks.
im_cnt = 1;
loop_cnt = nrow(im_plus_cnt)/2;

for(i in 1:loop_cnt) {
  temp = as.numeric(im_plus_cnt[im_cnt,2])-as.numeric(im_plus_cnt[im_cnt+1,2]);  #count of first stack minus count of second stack
  if (temp < 0) { # determine largest
    diff = as.numeric(im_plus_cnt[im_cnt,2]);
  } else {
    diff = as.numeric(im_plus_cnt[im_cnt+1,2]);  
  }
    diff = round(abs(temp)/diff, 3); 

  print(paste(im_plus_cnt[im_cnt,1], im_plus_cnt[im_cnt,2], im_plus_cnt[im_cnt+1,1], im_plus_cnt[im_cnt+1,2],diff, sep=' : '));
  im_cnt = im_cnt+2;
  }


