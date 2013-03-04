setwd("M:/Car.com/MDM Segment")
library(RCurl)
library(rjson)
require(XML)
library(XLConnect)
options( java.parameters = "-Xmx4g" )
library( "RWeka" )

# get ModelList ----
ModelYear <- getURL('http://www.cars.com/core/js/templates/crp/mmyCrp.json')
TT.modelyear1 <- fromJSON(ModelYear)
hh <- as.data.frame(do.call(rbind, TT.modelyear1$crpData));

ModelList.all <- character(0)
for (nn in 1:dim(hh)[1]){
gg <- TT.modelyear1$crpData[[nn]]
Make <- as.data.frame(do.call(cbind, gg$mk));
Model.ModelYear <- as.data.frame(do.call(rbind, gg$mds));
ModelList <- as.data.frame(cbind(Make, Model.ModelYear))
ModelList.all <- rbind(ModelList.all, ModelList)
}
FordList <- ModelList.all[(ModelList.all$id==14),]
file.create("AllModelList.xlsx")
write.xlsx(ModelList.all, "AllModelList.xlsx", sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)
file.info("AllModelList.xlsx")

wb <- loadWorkbook("MDM_segmentation_select.xlsx")
MDM.models <- readWorksheet(wb, sheet = "Segment-MDM")
MDM.segment <- unique(MDM.models["segment"])


# get Standard-equipment ----
dir.create("Equipment")
setwd("M:/Car.com/MDM Segment/Equipment")

StartTime <- Sys.time()
Equip.NoRecords.List <- character(0)
for (segid in 1:dim(MDM.segment)[1]){

for (segid in 1:1){
  segname <- MDM.segment[segid,]
  MDM.models.select <- MDM.models[MDM.models[,"segment"] == MDM.segment[segid,],"ModelID"]
  MDM.models.select <- unique(MDM.models.select[!is.na(MDM.models.select)])  
  for (veh.n in MDM.models.select){
    
    # for (veh.n in c(13,23)){
    # veh.n <- 24
    my <- strsplit(as.character(ModelList.all[veh.n,5]),",")[[1]]
    my <- my[my>=2000]
    makename <- as.character(ModelList.all[veh.n,2])
    modelname <- as.character(ModelList.all[veh.n,4])
    veh <- paste(tolower(makename), '/', tolower(modelname), '/', sep = '')
    equip.URLname.List0 <- paste("http://www.cars.com/", veh, my, '/standard-equipment/', sep = '')
    for (my.n in 1:length(equip.URLname.List0)){
      #    my.n <- 24
      URLname <- equip.URLname.List0[my.n]
      URLname <- gsub(" +", "-", URLname)
      URLname <- gsub("&", "and", URLname)
      b0 <- getURL(URLname) 
      b1 = htmlParse(b0, asText = TRUE, encoding = 'UTF-8')
      
      equipList <- getNodeSet(doc = b1, path = '//table/tbody/tr/td/h2|//td[@class="rowHeader"]')
      equip.name <- as.character(sapply(equipList, xmlValue))
      if (length(equipList)==0){
        print(paste('No records',segname, URLname))
        Equip.NoRecords.List <- rbind(Equip.NoRecords.List, paste(segname, URLname))
      } else {
        SRSList <- getNodeSet(doc = b1, path = '//table/thead/tr//div[@class="trimHeaderName"]/h2')
        SRSList.value <- getNodeSet(doc = b1, path = '//table/thead/tr//div[@class="trimHeaderName"]/span')        
        trim.value.base <- character(0)
        for (trim.n in 1:length(SRSList)+1){
          trim.value <- getNodeSet(doc = b1, path = paste('//table/tbody/tr/td/h2|//table/tbody/tr[position()>1]/td[', trim.n, ']', sep=''))        
          trim.value1 <- sapply(seq_len(length(trim.value)),
                                function(i){ifelse(is.null(xmlChildren(trim.value[[i]])$img), 
                                                   xmlValue(trim.value[[i]]), xmlGetAttr(xmlChildren(trim.value[[i]])$img, 'src'))})
          trim.value1 <- gsub('\\r|\\t', "", trim.value1)
          trim.value1[trim.value1==c("http://www.cars.com/go/configurator/images/standard.gif")]='Std'
          trim.value1[trim.value1==c("http://www.cars.com/go/configurator/images/available.gif")]='Opt'
          trim.value1[trim.value1==c("http://www.cars.com/go/configurator/images/notavailable.gif")]='Not'
          trim.value.base <- cbind(trim.value.base, trim.value1)
        }
        trim.name <- sapply(SRSList, xmlValue)
        MSRP <- sapply(SRSList.value, xmlValue)
        
        equip.value <- rbind(cbind("MSRP", t(MSRP)), cbind(equip.name,trim.value.base))
        
        colnames(equip.value) <- c("trim", trim.name)
        # step4: export data to excel file
        file.name <- paste(paste("Equip", segname, makename, modelname), ".xlsx", sep = "")
        sheet.name  <- my[my.n]
        xls <- loadWorkbook(file.name, create=TRUE)
        createSheet(xls, name=sheet.name)
        writeWorksheet(xls, equip.value, sheet.name, header=TRUE)
        setColumnWidth(xls, sheet.name, column = 1:(length(SRSList)+2), width = -1)
        saveWorkbook(xls)
        print(paste(segname, URLname))
      }
    }
  }
}

(UseTime <- Sys.time() - StartTime)
Equip.NoRecords.List <- as.data.frame(Equip.NoRecords.List)
write.table(Equip.NoRecords.List, "Equip-NoRecords-List.txt")

################ get specification ----
setwd("M:/Car.com/MDM Segment")
dir.create("Specification")
setwd("M:/Car.com/MDM Segment/Specification")
StartTime <- Sys.time()
NoRecords.List <- character(0)
Style.NoRecords.List <- character(0)
list30 <- character(0)
for (segid in 1:dim(MDM.segment)[1]){
  
# for (segid in 8:8){
#  segid <- 19
  segname <- MDM.segment[segid,]
  dirname <- paste(segname)
  if (!file.exists(paste('M:/Car.com/MDM Segment/Specification/',dirname, sep = ''))){
    dir.create(paste('M:/Car.com/MDM Segment/Specification/',dirname, sep = ''))
  }
  setwd(paste('M:/Car.com/MDM Segment/Specification/',dirname, sep = ''))
  
  MDM.models.select <- MDM.models[MDM.models[,"segment"] == MDM.segment[segid,],"ModelID"]
  MDM.models.select <- unique(MDM.models.select[!is.na(MDM.models.select)])  
  for (veh.n in MDM.models.select){
#  for (veh.n in 338){
#    veh.n <- 338
    my <- strsplit(as.character(ModelList.all[veh.n,5]),",")[[1]]
    my <- my[my>=2000]
    makename <- as.character(ModelList.all[veh.n,2])
    modelname <- as.character(ModelList.all[veh.n,4])
    veh <- paste(tolower(makename), '/', tolower(modelname), '/', sep = '')
    spec.URLname.List0 <- paste("http://www.cars.com/", veh, my, '/specifications/', sep = '')
    
    dirname1 <- paste(makename, modelname)
    if (!file.exists(paste('M:/Car.com/MDM Segment/Specification/', dirname, "/", dirname1, sep = ''))){
      dir.create(paste('M:/Car.com/MDM Segment/Specification/',dirname, "/",dirname1, sep = ''))
    }
    setwd(paste('M:/Car.com/MDM Segment/Specification/',dirname, "/",dirname1, sep = ''))
    
    for (Sty.n0 in 1:length(spec.URLname.List0)){
#    for (Sty.n0 in 13:15){
      spec.URLname0 <- spec.URLname.List0[Sty.n0];
      spec.URLname0 <- gsub(" +", "-", spec.URLname0)
      spec.URLname0 <- gsub("&", "and", spec.URLname0)
      spec <- getURL(spec.URLname0) 
      spec1 = htmlParse(spec, asText = TRUE, encoding = 'utf-8')
      StyleList <- getNodeSet(doc = spec1, path = '//div/ul[@id="trimpop"]/li')
      if (is.null(StyleList)){
        print(paste('No records',segname, spec.URLname0))
        NoRecords.List <- rbind(NoRecords.List, paste(segname, spec.URLname0))
      } else {
        
        StyleList1 = sapply(StyleList, xmlValue)[1:length(StyleList)]
        StyleList1.final <- StyleList1[-1]
        StyleList2 <- getNodeSet(doc = spec1, path = '//div/ul[@id="trimpop"]')
        
#        try.test <- try(StyleID <- sapply(seq(2, length(StyleList))*2-1,
#                              function(i){xmlAttrs(xmlChildren(StyleList2[[1]])[[i]])[['id']]}),
#            silent = T)
        StyleID <- sapply(seq(2, length(StyleList))*2-1,
                          function(i){xmlAttrs(xmlChildren(StyleList2[[1]])[[i]])[['id']]})
        #save style list
        StyleList3 <- cbind(StyleID, StyleList1.final)
        colnames(StyleList3) = c("StyleID", "Style")
        file.name <- paste(paste("Spec", segname, makename, modelname, my[Sty.n0]), ".xlsx", sep = "")
        xls <- loadWorkbook(file.name, create=TRUE)
        createSheet(xls, name="Style List")
        writeWorksheet(xls, StyleList3, "Style List", header=TRUE)
        setColumnWidth(xls, "Style List", column = 1:2, width = -1)
        saveWorkbook(xls)
        if (length(StyleID)>25) {
          file.name <- paste(paste("Spec", segname, makename, modelname, my[Sty.n0], "-2"), ".xlsx", sep = "")
          xls <- loadWorkbook(file.name, create=TRUE)
          createSheet(xls, name="Style List")
          writeWorksheet(xls, StyleList3, "Style List", header=TRUE)
          setColumnWidth(xls, "Style List", column = 1:2, width = -1)
          saveWorkbook(xls)
          list30 <- rbind(list30, paste(segname, spec.URLname0))
        }
        
        
        spec.URLname.List <- paste(spec.URLname0,"?acode=", substr(StyleID, 7, 20), sep = '')

        
        for (Sty.n in 1:length(spec.URLname.List)){
#        for (Sty.n in 22:28){
          spec.URLname <- spec.URLname.List[Sty.n]
          spec.URLname <- gsub(" +", "-", spec.URLname)
          spec.URLname <- gsub("&", "and", spec.URLname)
          spec <- getURL(spec.URLname)
          spec = iconv(spec, 'gbk', 'utf-8')
          spec1 = htmlParse(spec, asText = TRUE, encoding = 'UTF-8')
          # more than one style
          Engine.specname <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[2]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[2]//span[@class="spec-name"]')
          Transm.specname <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[3]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[3]//span[@class="spec-name"]')
          Other.specname <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[4]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[4]//span[@class="spec-name"]')
          if (is.null(Engine.specname) & is.null(Transm.specname) & is.null(Other.specname)){
            print(paste('No records',segname, spec.URLname))
            Style.NoRecords.List <- rbind(Style.NoRecords.List, paste(segname, spec.URLname))
          } else {
            spec.Engine <- sapply(Engine.specname,xmlValue)
            spec.Transm <- sapply(Transm.specname,xmlValue)
            spec.result1 <- cbind(rbind(t(t(spec.Engine)), t(t(spec.Transm))), "")
            spec.result1[spec.result1[,1] == "Engines", 2] = "Engines"
            spec.result1[spec.result1[,1] == "Transmissions", 2] = "Transmissions"
            
            Other.specdetail <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/div[4]/h2|//div[@id="specifications"]//div[@class="module-body"]/div[4]//span[@class="spec-detail float-right"]')
            spec.Other.name <- sapply(Other.specname,xmlValue)
            spec.Other.detail <- sapply(Other.specdetail,xmlValue)
            kk_final <- rbind(spec.result1, cbind(spec.Other.name, spec.Other.detail))
            kk_final <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",kk_final)
            # get Style name
            stylename <- getNodeSet(doc = spec1, path = '//div[@id="specifications"]//div[@class="module-body"]/p/strong')
            kk_final2 <- rbind(c("Style Name", sapply(stylename,xmlValue)), kk_final)

            colnames(kk_final2) <- c("SpecName", "SpecDetail")
            
            # export data to excel file
            if (Sty.n>25) {
              file.name <- paste(paste("Spec",segname, makename, modelname, my[Sty.n0], "-2"), ".xlsx", sep = "")
              xls <- loadWorkbook(file.name, create=TRUE)
              sheet.name  <- paste(StyleID[Sty.n])
              createSheet(xls, name=sheet.name)
              writeWorksheet(xls, kk_final2, sheet.name, header=TRUE)
              setColumnWidth(xls, sheet.name, column = 1:2, width = -1)
              saveWorkbook(xls)
            } else {
              file.name <- paste(paste("Spec",segname, makename, modelname, my[Sty.n0]), ".xlsx", sep = "")
              xls <- loadWorkbook(file.name, create=TRUE)
              sheet.name  <- paste(StyleID[Sty.n])
              createSheet(xls, name=sheet.name)
              writeWorksheet(xls, kk_final2, sheet.name, header=TRUE)
              setColumnWidth(xls, sheet.name, column = 1:2, width = -1)
              saveWorkbook(xls)
            }

            print(paste(segname, veh, my[Sty.n0], sapply(stylename, xmlValue)))
          }
        }
      }
    }
  }
}
(UseTime <- Sys.time() - StartTime)
write.table(NoRecords.List, "Spec-NoRecords-List4.txt")
write.table(Style.NoRecords.List, "Style-NoRecords-List4.txt")
write.table(list30, "list304.txt") 
################# get warranty ----
setwd("M:/Car.com/MDM Segment")
dir.create("Warranty")
setwd("M:/Car.com/MDM Segment/Warranty")
StartTime <- Sys.time()
Warr.NoRecords.List <- character(0)

for (segid in 1:dim(MDM.segment)[1]){
  
for (segid in 1:1){
  segname <- MDM.segment[segid,]
  MDM.models.select <- MDM.models[MDM.models[,"segment"] == MDM.segment[segid,],"ModelID"]
  MDM.models.select <- unique(MDM.models.select[!is.na(MDM.models.select)])  
  for (veh.n in MDM.models.select){
    my <- strsplit(as.character(ModelList.all[veh.n,5]),",")[[1]]
    makename <- as.character(ModelList.all[veh.n,2])
    modelname <- as.character(ModelList.all[veh.n,4])
    veh <- paste(tolower(makename), '/', tolower(modelname), '/', sep = '')
    warr.URLname.List <- paste("http://www.cars.com/", veh, my, '/warranty/', sep = '')
    
    for (warr.n in 1:length(warr.URLname.List)){ 
      warr.URLname <- warr.URLname.List[warr.n]
      warr.URLname <- gsub(" +", "-", warr.URLname)
      warr.URLname <- gsub("&", "and", warr.URLname)
      warr <- getURL(warr.URLname)
      warr = iconv(warr, 'gbk', 'utf-8')
      warr1 = htmlParse(warr, asText = TRUE, encoding = 'UTF-8')
      
      warr.specname <- getNodeSet(doc = warr1, path = '//div[@id="warranty"]//div[@class="module-body"]/div//span[@class="spec-name"]')
      if (length(warr.specname)==0){
        print(paste('No records', segname, warr.URLname))
        Warr.NoRecords.List <- rbind(Warr.NoRecords.List, paste(segname, warr.URLname))
      } else {
        warr.specdetail <- getNodeSet(doc = warr1, path = '//div[@id="warranty"]//div[@class="module-body"]/div//span[@class="spec-detail float-right"]')
        spec.warr.name <- sapply(warr.specname,xmlValue)
        spec.warr.detail <- sapply(warr.specdetail,xmlValue)
        warr_final <- cbind(spec.warr.name, spec.warr.detail)
        warr_final <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",warr_final)
        colnames(warr_final) <- c("WarrantyName", "WarrantyDetail")
        # export data to excel file
        file.name <- paste(paste("Warranty", segname, makename, modelname), ".xlsx", sep = "")
        sheet.name  <- my[warr.n]
        xls <- loadWorkbook(file.name, create=TRUE)
        createSheet(xls, name=sheet.name)
        writeWorksheet(xls, warr_final, sheet.name, header=TRUE)
        setColumnWidth(xls, sheet.name, column = 1:2, width = -1)
        saveWorkbook(xls)
        print(paste(segname, warr.URLname))        
      }
    }        
  }    
}
  (UseTime <- Sys.time() - StartTime)
Warr.NoRecords.List <- as.data.frame(Warr.NoRecords.List)
write.table(Warr.NoRecords.List, "Warr-NoRecords-List.txt")

################# get Safety Rating ----
setwd("M:/Car.com/MDM Segment")
dir.create("Safety Rating")
setwd("M:/Car.com/MDM Segment/Safety Rating")

StartTime <- Sys.time()

rate.NoRecords.List <- character(0)
NHTSA.NoRecords.List <- character(0)
IIHS.NoRecords.List <- character(0)
for (segid in 1:1){
  segname <- MDM.segment[segid,]
  MDM.models.select <- MDM.models[MDM.models[,"segment"] == MDM.segment[segid,],"ModelID"]
  MDM.models.select <- unique(MDM.models.select[!is.na(MDM.models.select)])  
  for (veh.n in MDM.models.select){ 
#  for (veh.n in 10:10){
  my <- strsplit(as.character(ModelList.all[veh.n,5]),",")[[1]]
  makename <- as.character(ModelList.all[veh.n,2])
  modelname <- as.character(ModelList.all[veh.n,4])
  veh <- paste(tolower(makename), '/', tolower(modelname), '/', sep = '')
  rate.URLname.List <- paste("http://www.cars.com/", veh, my, '/safety-ratings/', sep = '')
  
  for (rate.n in 1:length(rate.URLname.List)){
    rate.URLname <- rate.URLname.List[rate.n]
    rate.URLname <- gsub(" +", "-", rate.URLname)
    rate.URLname <- gsub("&", "and", rate.URLname)
    rate <- getURL(rate.URLname)
    rate = iconv(rate, 'gbk', 'utf-8')
    rate1 = htmlParse(rate, asText = TRUE, encoding = 'UTF-8')
    
    rate.specname <- getNodeSet(doc = rate1, path = '//div[@id="nhtsa-ratings"]//div[@class="module-body"]//div[@class="rowbottomborder nhtsaDataStyle"]|//div[@class="nhtsaDataStyle"]')
    rate.specname22 <- getNodeSet(doc = rate1, path = '//div[@id="iihs-ratings"]//div[@class="module-body"]//div[@class="iihsDataStyle"]|//div[@class="rowbottomborder"]')
    if (length(rate.specname)==0 & length(rate.specname22)==0) {
      print(paste('All No records', segname, rate.URLname))
      rate.NoRecords.List <- rbind(rate.NoRecords.List, paste(segname, rate.URLname))
    } else {
      # NHTSA Crash-Test Ratings
      if (length(rate.specname)==0){
        print(paste('NHTSA No records', segname, rate.URLname))
        NHTSA.NoRecords.List <- rbind(NHTSA.NoRecords.List, paste(segname, rate.URLname))
      } else {
        rate.specdetail <- getNodeSet(doc = rate1, path = '//div[@id="nhtsa-ratings"]//div[@class="module-body"]//div[@class="rowbottomborder nhtsaDataStyle"]/div/span|//div[@class="nhtsaDataStyle"]/div/span')
        spec.rate.name <- sapply(rate.specname,xmlValue)
        spec.rate.detail <- sapply(seq(1, length(rate.specdetail)),
                                   function(i){substr(xmlAttrs(rate.specdetail[[i]])[['style']], 8, 11)})
        rate.header <- sapply(getNodeSet(doc = rate1, path = '//div[@id="nhtsa-ratings"]//div[@class="module-header"]/h3'),xmlValue)
        rate.type <- strsplit(rate.header, " ")[[1]][1]
        rate_final <- cbind(rate.type, spec.rate.name, spec.rate.detail)
        rate_final <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",rate_final)
        colnames(rate_final) <- c("RatingType", "RatingName", "Rating")
        # change to 5-4-3-2-1
        rate_final[rate_final[,3]=="83.7", 3]=5
        rate_final[rate_final[,3]=="67.0", 3]=4
        rate_final[rate_final[,3]=="50.2", 3]=3
        rate_final[rate_final[,3]=="33.5", 3]=2
        rate_final[rate_final[,3]=="16.7", 3]=1
      }
      
      # IIHS Crash-Test Data
      if (length(rate.specname22)==0){
        print(paste('IIHS No records', segname, rate.URLname))
        IIHS.NoRecords.List <- rbind(IIHS.NoRecords.List, paste(segname, rate.URLname))
        
      } else {
        rate.specdetail22 <- getNodeSet(doc = rate1, path = '//div[@id="iihs-ratings"]//div[@class="module-body"]//div[@class="iihsDataStyle"]|//div[@id="iihs-ratings"]//div[@class="module-body"]/div/span')
        spec.rate.name22 <- sapply(rate.specname22,xmlValue)
        spec.rate.detail22 <- sapply(rate.specdetail22,xmlValue)
        rate.header22 <- sapply(getNodeSet(doc = rate1, path = '//div[@id="iihs-ratings"]//div[@class="module-header"]/h3'),xmlValue)
        rate.type22 <- strsplit(rate.header22, " ")[[1]][1]
        rate_final22 <- cbind(rate.type22, spec.rate.name22, spec.rate.detail22)  
        rate_final22 <- gsub('\\r|\\n|\\"|(^\\s*)|(\\s*$)', "",rate_final22)
        colnames(rate_final22) <- c("RatingType", "RatingName", "Rating")
      }
      flag <- paste(length(rate.specname)==0, length(rate.specname22)==0, sep = " and ")
      rate_final33 <- switch(flag,
                             "FALSE and TRUE" = rate_final,
                             "TRUE and FALSE" = rate_final22,
                             "FALSE and FALSE" = rbind(rate_final, rate_final22))
      # export data to excel file
      file.name <- paste(paste("Safety-Ratings", segname, makename, modelname), ".xlsx", sep = "")
      sheet.name  <- my[rate.n]
      xls <- loadWorkbook(file.name, create=TRUE)
      createSheet(xls, name=sheet.name)
      writeWorksheet(xls, rate_final33, sheet.name, header=TRUE)
      setColumnWidth(xls, sheet.name, column = 1:2, width = -1)
      saveWorkbook(xls)
      print(paste(segname, rate.URLname))
    }
  }
}
}
(UseTime <- Sys.time() - StartTime)

write.table(rate.NoRecords.List, "Allrate-NoRecords-List.txt")
write.table(NHTSA.NoRecords.List, "NHTSA-NoRecords-List.txt")
write.table(IIHS.NoRecords.List, "IIHS-NoRecords-List.txt")



# change ford name
library(stringr)
setwd("M:/Car.com/Safety Rating")
fordnames <- list.files()
MDM.models0 <- MDM.models[str_trim(MDM.models[,"Make"])=="FORD",]
MDM.models1 <- unique(cbind(paste(paste(MDM.models0[,6], "Safety-Ratings"), ".xlsx", sep=""), 
                            paste(paste("Safety-Ratings", MDM.models0[,2], MDM.models0[,6]), ".xlsx", sep = ""))) 
for (i in 1:dim(MDM.models1)[1]){
  eval(parse(text = paste0("file.rename('", MDM.models1[i,1], "' , '", MDM.models1[i,2], "')")))
}

# change ford name in Specification
library(stringr)
file_names <- "M:/Car.com/MDM Segment/Specification/SPD_TRUCK/Ford F350"
setwd(file_names)
(fordnames <- list.files())
(newfordnames <- paste("Spec SPD_TRUCK", gsub(" Spec", "", fordnames)))
for (i in 1:length(fordnames)){
  eval(parse(text = paste0("file.rename('", fordnames[i], "' , '", newfordnames[i], "')")))
}
