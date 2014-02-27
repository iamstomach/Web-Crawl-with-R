#################################################
# Disabled Selected Text      Value      
# [1,] FALSE    FALSE    "2009-11" "2009-2011"
# [2,] FALSE    TRUE     "2008-10" "2008-2010"
# [3,] FALSE    FALSE    "2007-09" "2007-2009"
# [4,] FALSE    FALSE    "2006-08" "2006-2008"
# [5,] FALSE    FALSE    "2005-07" "2005-2007"
# [6,] FALSE    FALSE    "2004-06" "2004-2006"


setwd("M:/Data")
library("xlsx")
library(RCurl)
library(rjson)

# change the modelyears value (modelYears = "2009-2011") one by one, and create
# an excel file named 2009-2011.xlsx in the local directory "M:/Data" before run the following code

# step1: get aviable size list
opts = list(verbose=T )
GG <- postForm('http://www.iihs.org/research/hldi/composite/getviewmodel', .opts=opts,
               modelYears = "2009-2011",
               style="POST")
TT.modelyear <- fromJSON(GG)
yearlist <- do.call(rbind, TT.modelyear$yearRanges)
year.select <- as.character(yearlist[yearlist[,"Selected"] == T, "Value"])
sizelist <- do.call(rbind, TT.modelyear$sizes)
sizelist1 <- sizelist[sizelist[,"Disabled"] == F,]

for (sizelist.i in 1:dim(sizelist1)[1]){
# step2: get aviable class list
  size <- sizelist1[,"Value"][[sizelist.i]];
  getdata.size <- postForm('http://www.iihs.org/research/hldi/composite/getviewmodel', .opts=opts,
                           modelYears = year.select, vehicleSizeId = size,
                           style="POST")
  TT.size <- fromJSON(getdata.size)
  classlist <- do.call(rbind, TT.size$classes)
  classist1 <- classlist[classlist[,"Disabled"] == F,]
  
  for (classist.i in 1:dim(classist1)[1]){
# step3: get data
  class <- classist1[,"Value"][[classist.i]];
  getdata <- postForm('http://www.iihs.org/research/hldi/composite/getviewmodel', .opts=opts,
                      modelYears = year.select, vehicleClassId = class, vehicleSizeId = size,
                      style="POST")
  TT <- fromJSON(getdata)
  
  n.veh <- dim(summary(TT$vehiclesByClass))[1]
  tt.all.header <- as.data.frame(do.call(rbind, TT$vehiclesByClass[[1]]));
  tt.names <- rownames(tt.all.header)
  tt.all.numeric <- t(as.character(do.call(rbind, TT$vehiclesByClass[[1]])[,"numericValue"]))
  if (n.veh == 1) {
    tt.all.numeric <- as.data.frame(tt.all.numeric);
  } else {
      for (i in 2:n.veh){
      tt.all.numeric0 <- t(as.character(do.call(rbind, TT$vehiclesByClass[[i]])[,"numericValue"]))
      tt.all.numeric <- as.data.frame(rbind(tt.all.numeric, tt.all.numeric0)) 
      }
  }
  names(tt.all.numeric) = tt.names
# step4: export data to excel file
  file.name <- paste(year.select,".xlsx", sep = "")
  sheet.name  <- paste(TT$selectedSizeText, TT$selectedClassText)
  sheet.name <- sub('/', '_', sheet.name);
  write.xlsx(tt.all.numeric, file = file.name, sheetName = sheet.name, append=T)
  }
}
