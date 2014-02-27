rootfile <- "M:/Car.com/MDM Segment/Add/Spec2"
setwd(rootfile)

################# Part2: get specification ----
setwd(rootfile)
dir.create("Specification")
setwd(paste(rootfile, "/Specification", sep = ""))
StartTime <- Sys.time()
NoRecords.List <- character(0)
Style.NoRecords.List <- character(0)
list30 <- character(0)
onlyCommonStyleList <- character(0)
overview_spec_record <- character(0)

for (segid in 1:dim(MDM.segment)[1]){
#   segid=14
  segname <- MDM.segment[segid,]
  dirname <- paste(segname)
#   if (!file.exists(paste(rootfile,'/Specification/',dirname, sep = ''))){
#     dir.create(paste(rootfile,'/Specification/',dirname, sep = ''))
#   }
#   setwd(paste(rootfile,'/Specification/',dirname, sep = ''))
  
  MDM.models.select <- MDM.models[MDM.models[,"segment"] == MDM.segment[segid,],"ModelID"]
  MDM.models.select <- unique(MDM.models.select[!is.na(MDM.models.select)])  
  for (veh.n in MDM.models.select){
#     veh.n <- 322
    my <- strsplit(as.character(ModelList.all[veh.n,5]),",")[[1]]
    my <- my[my>=2000]
    makename <- as.character(ModelList.all[veh.n,2])
    modelname <- as.character(ModelList.all[veh.n,4])
    veh <- paste(tolower(makename), '/', tolower(modelname), '/', sep = '')
    overview.URLname.List00 <- paste("http://www.cars.com/", veh, my, "/", sep = '')
    spec.URLname.List0 <- paste("http://www.cars.com/", veh, my, '/specifications/', sep = '')
    
#     dirname1 <- paste(makename, modelname)
#     if (!file.exists(paste(rootfile,'/Specification/', dirname, "/", dirname1, sep = ''))){
#       dir.create(paste(rootfile,'/Specification/',dirname, "/",dirname1, sep = ''))
#     }
#     setwd(paste(rootfile,'/Specification/',dirname, "/",dirname1, sep = ''))
    
    for (Sty.n0 in 1:length(spec.URLname.List0)){
#       Sty.n0 <- 4
      spec.URLname0 <- spec.URLname.List0[Sty.n0];
      spec.URLname0 <- gsub(" +", "-", spec.URLname0)
      spec.URLname0 <- gsub("&", "and", spec.URLname0)
      spec <- getURL(spec.URLname0) 
      spec1 = htmlParse(spec, asText = TRUE, encoding = 'utf-8')
      StyleList <- getNodeSet(doc = spec1, path = '//div/ul[@id="trimpop"]/li')
      if (is.null(StyleList)){
        print(paste('No records',segname, spec.URLname0))
        NoRecords.List <- rbind(NoRecords.List, paste(segname, spec.URLname0))
#       for the spec in overview
        overview.URLname <- overview.URLname.List00[Sty.n0];
        overview.URLname <- gsub(" +", "-", overview.URLname)
        overview.URLname <- gsub("&", "and", overview.URLname)
        overview <- getURL(overview.URLname)
        overview1 = htmlParse(overview, asText = TRUE, encoding = 'utf-8')
        
        overview_spec <- getNodeSet(doc = overview1, path = '//div[@class="module-body"]/table//a')
        if (!is.null(overview_spec)) {
          
          overview_specid <- sapply(seq(1, length(overview_spec)-1, 2),
                                function(i){xmlGetAttr((overview_spec[[i]]), 'href')})
          overview_specname <- sapply(seq(1, length(overview_spec)-1, 2),
                                    function(i){xmlValue((overview_spec[[i]]))})
          overview_spec_record <- rbind(overview_spec_record, cbind(segname,overview_specid, overview_specname))
          
        }
      } else if (length(StyleList)==1) {
        onlyCommonStyleList <- rbind(onlyCommonStyleList, paste(segname, spec.URLname0))
      }
      print(paste(segname, spec.URLname0))
    }
  }
}
(UseTime <- Sys.time() - StartTime)
write.table(NoRecords.List, "Spec-NoRecords-List.txt")
write.table(Style.NoRecords.List, "Style-NoRecords-List.txt")
write.table(list30, "list30.txt") 
