# Shepard-Metzler Automated Item Generation (SMAIG) project
* R functions using the rgl graphics package to build 3D models. 

* Working our way to a package that generates Shepard-Metzler test images.

* The current code will genderate images and allow the user to manipulate images and save the coordiates to a spreadsheet.
* The [SMAIG occlusion project](https://github.com/katyem/SMAIG_occlusion) is now in its own project repository.

## Getting Started

* Using [RStudio](https://rstudio.com/), open the SMAIG.R file.
* Install the libraries if/when needed.
* Run code as needed.
* Note: It is sometime helpful to reboot RStudio when the program fails to work properly.

### Prerequisites

A working knowledge of R and RStudio.

## Running the code

Step through the [SMAIG.R](https://github.com/katyem/smaig) code, carefully reading the comments before you run each line/function.

### Example: creating a random cube stack, rotating it, and saving it to a file.
* Run: cubeCoord = buildStack() 
* Rotate the stack in the RGL window using mouse/tracking device.
* Save the rotated version of the stack data to the cubeCoord variable: cubeCoord = store3d()
** cubeCoord: a matrix variable that holds the data associated with your current stack build - if you want to keep the orignal data, use the instructions below to save it to SMAIGtable before saving your rotated stack.
* Append the rotated stack image's data to your current SMAIGtable: SMAIGtable <- saveStack() 
* Save your SMAIGtable to an Excel file: write.xlsx(SMAIGtable, file = "SMAIG.xlsx")


## Built With the following R packages

* library('magick')
* library('rgl') 
* library('magrittr')
* library("openxlsx")


## Authors

* **Tilman Sheets** - *Initial work* - [SMAIG](https://katyem.netlify.app/project/smaig/)


## License

This work is licensed under CC BY SA 4.0 - see the [LICENSE](https://creativecommons.org/licenses/by-sa/4.0/) file for details

## Acknowledgments

* Hat tip to anyone whose code was used! e.g., [EconometricsBySimulation](https://gist.github.com/EconometricsBySimulation/5c00a9e91abebd889fb7)
* Inspiration
* etc
